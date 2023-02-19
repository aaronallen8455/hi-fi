{-# LANGUAGE RecordWildCards #-}
module HiFi.TcPlugin.FoldFields
  ( buildFoldFieldsExpr
  ) where

import           Control.Monad
import           Data.Either
import           Data.Functor ((<&>))
import qualified Data.List as List
import           Data.Maybe

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs
import           HiFi.TcPlugin.RecordParts
import           HiFi.TcPlugin.Utils (makeWantedCt)

buildFoldFieldsExpr
  :: PluginInputs
  -> Ghc.CtLoc
  -> Ghc.Type
  -> Ghc.Type
  -> Ghc.Class
  -> [Ghc.Type]
  -> [(Ghc.FastString, FieldParts)]
  -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.CoreExpr)
buildFoldFieldsExpr inp@MkPluginInputs{..} ctLoc recordTy effectConTy predClass predArgs fields = do
  xTyVarName <- Ghc.unsafeTcPluginTcM
              $ Ghc.newName (Ghc.mkOccName Ghc.varName "x")

  let xTyVar = Ghc.mkTyVar xTyVarName Ghc.liftedTypeKind
      hkdTy = Ghc.mkTyConApp hkdTyCon [recordTy, effectConTy]

  fieldGenBndr <- mkFieldGenBndr inp effectConTy predClass hkdTy xTyVar predArgs

  eFieldGenExprs <-
    traverse
      (mkFieldGenExpr inp fieldGenBndr hkdTy ctLoc predClass predArgs effectConTy recordTy)
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
  -> Ghc.Id
  -> Ghc.Type
  -> Ghc.CtLoc
  -> Ghc.Class
  -> [Ghc.Type]
  -> Ghc.Type
  -> Ghc.Type
  -> (Ghc.FastString, FieldParts)
  -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.CoreExpr)
mkFieldGenExpr inp fieldGenBndr hkdTy ctLoc predClass predArgs effectConTy recordTy (fieldName, fieldParts) = do
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

  predCt <- makeWantedCt ctLoc predClass predClassArgs

  (_, evBindMap)
    <- Ghc.unsafeTcPluginTcM
     . Ghc.runTcS
     . Ghc.solveSimpleWanteds
     $ Ghc.singleCt predCt

  let evVar = Ghc.ctEvEvId $ Ghc.ctEvidence predCt
  ePredDict <- buildEvExprFromMap ctLoc evVar evBindMap
  fieldNameExpr <- Ghc.mkStringExprFS' fieldName

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

-- | The output of solving wanted contains references to variables that are not
-- in scope so an expr must be constructed that binds those variables locally.
buildEvExprFromMap
  :: Ghc.CtLoc
  -> Ghc.EvVar
  -> Ghc.EvBindMap
  -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.EvExpr)
buildEvExprFromMap ctLoc evVar evBindMap = do
  let evBinds = Ghc.dVarEnvElts $ Ghc.ev_bind_varenv evBindMap
  mResult <- case List.partition ((== evVar) . Ghc.eb_lhs) evBinds of
    ([m], rest) -> Ghc.unsafeTcPluginTcM . Ghc.initDsTc' $ do
      baseDict <- Ghc.dsEvTerm $ Ghc.eb_rhs m
      binds <- forM rest $ \evBind -> do
        evExpr <- Ghc.dsEvTerm $ Ghc.eb_rhs evBind
        pure (Ghc.eb_lhs evBind, evExpr)
      pure $ Ghc.mkLetRec binds baseDict
    _ -> pure Nothing
  case mResult of
        Nothing -> do
          mCt <- mkNewWantedFromExpr ctLoc evVar
          pure . Left $ maybeToList mCt
        Just result -> do
          -- Filter out DFunId vars, which are valid even though they are
          -- considered free vars by GHC.
          let freeVars = filter (not . isDFunId) $ Ghc.exprFreeVarsList result
          if null freeVars
             then pure $ Right result
             else do
               mCts <- traverse (mkNewWantedFromExpr ctLoc) freeVars
               pure . Left $ catMaybes mCts

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
  = Just <$> makeWantedCt ctLoc cls args
  | otherwise = pure Nothing
