{-# LANGUAGE RecordWildCards #-}
module HiFi.TcPlugin.FoldFields
  ( buildFoldFieldsExpr
  ) where

import           Control.Monad
import           Data.Either
import           Data.Functor ((<&>))
import           Data.Maybe

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs
import           HiFi.TcPlugin.RecordParts
import           HiFi.TcPlugin.Utils (makeWantedCt)

buildFoldFieldsExpr
  :: PluginInputs
  -> Ghc.EvBindsVar
  -> [Ghc.Ct]
  -> Ghc.CtLoc
  -> Ghc.Type
  -> Ghc.Type
  -> Ghc.Class
  -> [Ghc.Type]
  -> [(Ghc.FastString, FieldParts)]
  -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.CoreExpr)
buildFoldFieldsExpr inp@MkPluginInputs{..} evBindsVar givens ctLoc recordTy effectConTy predClass predArgs fields = do
  xTyVarName <- Ghc.unsafeTcPluginTcM
              $ Ghc.newName (Ghc.mkOccName Ghc.varName "x")

  let xTyVar = Ghc.mkTyVar xTyVarName Ghc.liftedTypeKind
      hkdTy = Ghc.mkTyConApp hkdTyCon [recordTy, effectConTy]

  fieldGenBndr <- mkFieldGenBndr inp effectConTy predClass hkdTy xTyVar predArgs

  eFieldGenExprs <-
    traverse
      (mkFieldGenExpr
        inp
        evBindsVar
        givens
        fieldGenBndr
        hkdTy
        ctLoc
        predClass
        predArgs
        effectConTy
        recordTy
      )
      fields

  case partitionEithers eFieldGenExprs of
    ([], fieldGenExprs) -> Right <$> mkFoldFieldsExpr xTyVar fieldGenBndr fieldGenExprs
    (wanteds, _) -> pure . Left $ concat wanteds

-- | Make the binder for the function that produces the x terms
mkFieldGenBndr
  :: PluginInputs
  -> Ghc.Type
  -> Ghc.Class
  -> Ghc.Type
  -> Ghc.TyVar
  -> [Ghc.Type]
  -> Ghc.TcPluginM Ghc.Id
mkFieldGenBndr inp effectConTy predClass hkdTy xTyVar predArgs = do
    fieldGenName <- Ghc.unsafeTcPluginTcM
                  $ Ghc.newName (Ghc.mkOccName Ghc.varName "fieldGen")

    fieldTyVarName <- Ghc.unsafeTcPluginTcM
                    $ Ghc.newName (Ghc.mkOccName Ghc.varName "a")

    let tyVar = Ghc.mkTyVar fieldTyVarName Ghc.liftedTypeKind
        fieldTy = Ghc.mkTyConApp
                    (fieldTyTyCon inp)
                    [effectConTy, Ghc.mkTyVarTy tyVar]
        -- forall a. (C (FieldTy f a)
        --   => String
        --   -> (HKD rec f -> FieldTy f a)
        --   -> x
        fieldGenTy = Ghc.mkSigmaTy forallBndrs preds tyBody
          where
            forallBndrs = [ Ghc.mkTyCoVarBinder Ghc.Required tyVar ]
            preds = [ Ghc.mkClassPred predClass
                        $ predArgs
                       ++ [ fieldTy ]
                    ]
            tyBody = Ghc.stringTy
                   `Ghc.mkVisFunTyMany`
                     ( hkdTy
                     `Ghc.mkVisFunTyMany`
                       fieldTy
                     )
                   `Ghc.mkVisFunTyMany`
                     Ghc.mkTyVarTy xTyVar

    pure $ Ghc.mkLocalIdOrCoVar fieldGenName Ghc.Many fieldGenTy

-- | Make the expr that results from applying all arguments (including the dict)
-- to the user supplied function that generates a value for each field.
mkFieldGenExpr
  :: PluginInputs
  -> Ghc.EvBindsVar
  -> [Ghc.Ct]
  -> Ghc.Id
  -> Ghc.Type
  -> Ghc.CtLoc
  -> Ghc.Class
  -> [Ghc.Type]
  -> Ghc.Type
  -> Ghc.Type
  -> (Ghc.FastString, FieldParts)
  -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.CoreExpr)
mkFieldGenExpr inp evBindsVar givens fieldGenBndr hkdTy ctLoc predClass predArgs effectConTy recordTy (fieldName, fieldParts) = do
  hkdName <- Ghc.unsafeTcPluginTcM
           $ Ghc.newName (Ghc.mkOccName Ghc.varName "hkd")

  let hkdBndr = Ghc.mkLocalIdOrCoVar hkdName Ghc.Many hkdTy
      getterExpr =
        Ghc.mkCoreLams [hkdBndr] $
          case fieldNesting fieldParts of
            Unnested idx ->
              Ghc.mkCoreApps (Ghc.Var $ indexArrayId inp)
                             [ Ghc.Type recordTy
                             , Ghc.Type effectConTy
                             , Ghc.Var hkdBndr
                             , Ghc.mkUncheckedIntExpr idx
                             ]
            Nested offset len innerRecTy _ ->
              Ghc.mkCoreApps (Ghc.Var $ getInnerRecId inp)
                             [ Ghc.Type recordTy
                             , Ghc.Type effectConTy
                             , Ghc.Type innerRecTy
                             , Ghc.Var hkdBndr
                             , Ghc.mkUncheckedIntExpr offset
                             , Ghc.mkUncheckedIntExpr len
                             ]
      predClassArgs =
        predArgs ++
        [Ghc.mkTyConApp (fieldTyTyCon inp) [effectConTy, fieldType fieldParts]]

  (predCt, predDest) <- makeWantedCt ctLoc predClass predClassArgs

  fieldNameExpr <- Ghc.mkStringExprFS' fieldName

  ePredDict <- do
    Ghc.unsafeTcPluginTcM . Ghc.runTcSWithEvBinds evBindsVar $ do
      -- Add givens back in
      Ghc.solveSimpleGivens givens
      -- Try to solve the constraint with both top level instances and givens
      void $ Ghc.solveSimpleWanteds (Ghc.singleCt predCt)
    -- Check if GHC produced evidence
    mEvTerm <- Ghc.unsafeTcPluginTcM $ lookupEvTerm evBindsVar predDest
    case mEvTerm of
      Just (Ghc.EvExpr evExpr) -> do
        evBindsMap <- Ghc.unsafeTcPluginTcM $ Ghc.getTcEvBindsMap evBindsVar
        let freeVars =
              filter (\x -> not (isDFunId x)
                         || isNothing (Ghc.lookupEvBind evBindsMap x)
                     )
                     $ Ghc.exprFreeVarsList evExpr
        if null freeVars
           then pure $ Right evExpr
           else do
             mCts <- traverse (mkNewWantedFromExpr ctLoc) freeVars
             pure . Left $ catMaybes mCts
      _ -> pure $ Left [predCt]

  pure $ ePredDict <&> \predDict ->
    Ghc.mkCoreApps (Ghc.Var fieldGenBndr) $
      [ Ghc.Type $ fieldType fieldParts
      ] ++ [predDict] ++
      [ fieldNameExpr
      , getterExpr
      ]

-- | Puts the pieces together to form the resulting expr
mkFoldFieldsExpr :: Ghc.TyVar -> Ghc.Id -> [Ghc.CoreExpr] -> Ghc.TcPluginM Ghc.CoreExpr
mkFoldFieldsExpr xTyVar fieldGenBndr fieldGenExprs = do
  initAccName <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "initAcc")
  accTyVarName <- Ghc.unsafeTcPluginTcM
                $ Ghc.newName (Ghc.mkOccName Ghc.varName "acc")
  accumulatorName <- Ghc.unsafeTcPluginTcM
                   $ Ghc.newName (Ghc.mkOccName Ghc.varName "accumulator")


  let accumulatorTy = Ghc.mkTyVarTy xTyVar
                    `Ghc.mkVisFunTyMany`
                      Ghc.mkTyVarTy accTyVar
                    `Ghc.mkVisFunTyMany`
                      Ghc.mkTyVarTy accTyVar

      accumulatorBndr = Ghc.mkLocalIdOrCoVar accumulatorName Ghc.Many accumulatorTy
      accTyVar = Ghc.mkTyVar accTyVarName Ghc.liftedTypeKind
      initAccBndr = Ghc.mkLocalIdOrCoVar initAccName Ghc.Many (Ghc.mkTyVarTy accTyVar)
      lamArgs = [ accTyVar
                , xTyVar
                , fieldGenBndr
                , initAccBndr
                , accumulatorBndr
                ]

  bodyExpr <- Ghc.unsafeTcPluginTcM $
    Ghc.mkFoldrExpr (Ghc.mkTyVarTy xTyVar)
                    (Ghc.mkTyVarTy accTyVar)
                    (Ghc.Var accumulatorBndr)
                    (Ghc.Var initAccBndr)
                    (Ghc.mkListExpr (Ghc.mkTyVarTy xTyVar) fieldGenExprs)

  pure $ Ghc.mkCoreLams lamArgs bodyExpr

isDFunId :: Ghc.EvVar -> Bool
isDFunId var =
  Ghc.isId var &&
     case Ghc.idDetails var of
       Ghc.DFunId{} -> True
       _ -> False

mkNewWantedFromExpr
  :: Ghc.CtLoc
  -> Ghc.EvVar
  -> Ghc.TcPluginM (Maybe Ghc.Ct)
mkNewWantedFromExpr ctLoc evVar
  | let exprTy = Ghc.exprType (Ghc.Var evVar)
  , Just (tyCon, args) <- Ghc.tcSplitTyConApp_maybe exprTy
  , Just cls <- Ghc.tyConClass_maybe tyCon
  = Just . fst <$> makeWantedCt ctLoc cls args
  | otherwise = pure Nothing

-- | Look up whether a 'TcEvDest' has been filled with evidence.
lookupEvTerm
  :: Ghc.EvBindsVar
  -> Ghc.TcEvDest
  -> Ghc.TcM (Maybe Ghc.EvTerm)
lookupEvTerm _ (Ghc.HoleDest (Ghc.CoercionHole { Ghc.ch_ref = ref } ) ) = do
  mb_co <- Ghc.readTcRef ref
  case mb_co of
    Nothing -> pure Nothing
    Just co -> pure . Just $ Ghc.evCoercion co
lookupEvTerm evBindsVar (Ghc.EvVarDest ev_var) = do
  evBindsMap <- Ghc.getTcEvBindsMap evBindsVar
  let
    mEvBind :: Maybe Ghc.EvBind
    mEvBind = Ghc.lookupEvBind evBindsMap ev_var
  case mEvBind of
    Nothing     -> pure Nothing
    Just evBind -> pure . Just $ Ghc.eb_rhs evBind
