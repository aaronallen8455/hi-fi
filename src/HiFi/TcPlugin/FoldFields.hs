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
  accTyVarName <- Ghc.unsafeTcPluginTcM
                $ Ghc.newName (Ghc.mkOccName Ghc.varName "acc")
  xTyVarName <- Ghc.unsafeTcPluginTcM
              $ Ghc.newName (Ghc.mkOccName Ghc.varName "x")
  accumulatorName <- Ghc.unsafeTcPluginTcM
                   $ Ghc.newName (Ghc.mkOccName Ghc.varName "accumulator")
  initAccName <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "initAcc")
  hkdName <- Ghc.unsafeTcPluginTcM
           $ Ghc.newName (Ghc.mkOccName Ghc.varName "hkd")

  let accTyVar = Ghc.mkTyVar accTyVarName Ghc.liftedTypeKind
      xTyVar = Ghc.mkTyVar xTyVarName Ghc.liftedTypeKind
      hkdTy = Ghc.mkTyConApp hkdTyCon [recordTy, effectConTy]

  fieldGenBndr <- mkFieldGenBndr inp effectConTy predClass hkdTy xTyVar predArgs

  let initAccBndr = Ghc.mkLocalIdOrCoVar initAccName Ghc.Many (Ghc.mkTyVarTy accTyVar)

      accumulatorTy = Ghc.mkTyVarTy xTyVar
                    `Ghc.mkVisFunTyMany`
                      Ghc.mkTyVarTy accTyVar
                    `Ghc.mkVisFunTyMany`
                      Ghc.mkTyVarTy accTyVar

      accumulatorBndr = Ghc.mkLocalIdOrCoVar accumulatorName Ghc.Many accumulatorTy

      accTyVarBndr = Ghc.mkTyVar accTyVarName Ghc.liftedTypeKind
      xTyVarBndr = Ghc.mkTyVar xTyVarName Ghc.liftedTypeKind
      hkdBndr = Ghc.mkLocalIdOrCoVar hkdName Ghc.Many hkdTy

  let mkFieldGenExpr
        :: (Ghc.FastString, FieldParts)
        -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.CoreExpr)
      mkFieldGenExpr (fieldName, fieldParts) = do
        fieldNameExpr <- Ghc.mkStringExprFS' fieldName
        let getterExpr =
              Ghc.mkCoreLams [hkdBndr] $
                case fieldNesting fieldParts of
                  Unnested idx ->
                    Ghc.mkCoreApps (Ghc.Var indexArrayId)
                                   [ Ghc.Type recordTy
                                   , Ghc.Type effectConTy
                                   , Ghc.Var hkdBndr
                                   , Ghc.mkUncheckedIntExpr idx
                                   ]
                  Nested offset len innerRecTy _ ->
                    Ghc.mkCoreApps (Ghc.Var getInnerRecId)
                                   [ Ghc.Type recordTy
                                   , Ghc.Type effectConTy
                                   , Ghc.Type innerRecTy
                                   , Ghc.Var hkdBndr
                                   , Ghc.mkUncheckedIntExpr offset
                                   , Ghc.mkUncheckedIntExpr len
                                   ]
            predClassArgs =
              predArgs ++
              [Ghc.mkTyConApp fieldTyTyCon [effectConTy, fieldType fieldParts]]

        predCt <- makeWantedCt ctLoc predClass predClassArgs

        (_, evBindMap)
          <- Ghc.unsafeTcPluginTcM
           . Ghc.runTcS
           . Ghc.solveSimpleWanteds
           $ Ghc.singleCt predCt

        let evVar = Ghc.ctEvEvId $ Ghc.ctEvidence predCt
        ePredDict <- buildEvExprFromMap ctLoc evVar evBindMap

        pure $ ePredDict <&> \predDict ->
          Ghc.mkCoreApps (Ghc.Var fieldGenBndr) $
            [ Ghc.Type $ fieldType fieldParts
            ] ++ [predDict] ++
            [ fieldNameExpr
            , getterExpr
            ]

      lamArgs = [ accTyVarBndr
                , xTyVarBndr
                , fieldGenBndr
                , initAccBndr
                , accumulatorBndr
                ]

  result <- traverse mkFieldGenExpr fields
  case partitionEithers result of
    ([], fieldGenExprs) -> do
      bodyExpr <- Ghc.unsafeTcPluginTcM $
        Ghc.mkFoldrExpr (Ghc.mkTyVarTy xTyVar)
                        (Ghc.mkTyVarTy accTyVar)
                        (Ghc.Var accumulatorBndr)
                        (Ghc.Var initAccBndr)
                        (Ghc.mkListExpr (Ghc.mkTyVarTy xTyVar) fieldGenExprs)

      pure . Right $ Ghc.mkCoreLams lamArgs bodyExpr
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

-- | The output of solving wanted contains references to variables that are not
-- in scope so an expr must be constructed that binds those variables locally.
-- The solver seems to always output them in reverse dependency order, hence
-- using a left fold to build the bindings.
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
      let go :: Ghc.EvExpr -> Ghc.EvBind -> Ghc.DsM Ghc.EvExpr
          go acc x = do
            evExpr <- Ghc.dsEvTerm $ Ghc.eb_rhs x
            pure $ Ghc.bindNonRec (Ghc.eb_lhs x) evExpr acc
       in foldM go baseDict rest
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
