{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.TcPlugin
  ( tcPlugin
  ) where

import           Control.Monad
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Data.Either (partitionEithers)
import           Data.Functor ((<&>))
import qualified Data.List as List
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Traversable (for)

import qualified HiFi.GhcFacade as Ghc

import           Debug.Trace

tcPlugin :: Ghc.TcPlugin
tcPlugin = Ghc.TcPlugin
  { Ghc.tcPluginInit = lookupInputs
  , Ghc.tcPluginSolve = tcSolver
  , Ghc.tcPluginRewrite = mempty
  , Ghc.tcPluginStop = \_ -> pure ()
  }

data PluginInputs =
  MkPluginInputs
    { indexOfFieldClass   :: !Ghc.Class
    , fieldGettersName    :: !Ghc.Name
    , toRecordName        :: !Ghc.Name
    , foldFieldsName      :: !Ghc.Name
    , instantiateName     :: !Ghc.Name
    , indexArrayId        :: !Ghc.Id
    , recArrayTyCon       :: !Ghc.TyCon
    , fieldNameTyCon      :: !Ghc.TyCon
    , mkFieldNameDataCon  :: !Ghc.DataCon
    , arrayFromListId     :: !Ghc.Id
    , fieldTypeCheckClass :: !Ghc.Class
    , hasFieldClass       :: !Ghc.Class
    , knownSymbolClass    :: !Ghc.Class
    , hkdTyCon            :: !Ghc.TyCon
    , missingFieldClass   :: !Ghc.Class
    , unknownFieldClass   :: !Ghc.Class
    }

findModule :: String -> Ghc.TcPluginM Ghc.Module
findModule name = do
  findResult <- Ghc.findImportedModule (Ghc.mkModuleName name) Ghc.NoPkgQual
  case findResult of
    Ghc.Found _ res -> pure res
    _               -> error "preposterous!"

lookupInputs :: Ghc.TcPluginM PluginInputs
lookupInputs = do
  hiFiMod <- findModule "HiFi.Internal.Types"

  indexOfFieldClass <- Ghc.tcLookupClass =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "IndexOfField")
  fieldGettersName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldGetters")
  toRecordName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "ToRecord")
  foldFieldsName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FoldFields")
  instantiateName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "Instantiate")
  indexArrayId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "indexArray")
  recArrayTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "RecArray")
  fieldNameTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldName")
  mkFieldNameDataCon <- Ghc.tcLookupDataCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkDataOcc "MkFieldName")
  arrayFromListId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "arrayFromList")
  fieldTypeCheckClass <- Ghc.tcLookupClass =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldTypeCheck")
  hasFieldClass <- Ghc.tcLookupClass Ghc.hasFieldClassName
  knownSymbolClass <- Ghc.tcLookupClass Ghc.knownSymbolClassName
  hkdTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "HKD")
  missingFieldClass <- Ghc.tcLookupClass =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "MissingField")
  unknownFieldClass <- Ghc.tcLookupClass =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "UnknownField")
  pure MkPluginInputs{..}

tcSolver :: PluginInputs -> Ghc.TcPluginSolver
tcSolver inp@MkPluginInputs{..} _env _givens wanteds = do
  results <- for wanteds $ \case
    ct@Ghc.CDictCan{ cc_class, cc_tyargs } -> do
      let clsName = Ghc.getName cc_class

         -- IndexOfField
      if | clsName == Ghc.getName indexOfFieldClass
         , [ getStrTyLitVal -> Just fieldName
           , fmap (map fst . recordFields) . getRecordFields -> Just fields
           ] <- cc_tyargs -> pure $ Just
             ( do
               ix <- List.elemIndex fieldName fields
               let expr = Ghc.mkUncheckedIntExpr $ fromIntegral ix
               Just $ Ghc.EvExpr expr
             , []
             , ct
             )

         -- FieldGetters
         | clsName == fieldGettersName
         , [ recordTy ] <- cc_tyargs
         , Just fields <- map (snd . snd) . recordFields <$> getRecordFields recordTy
         -> do
             selVars <- map Ghc.Var <$> traverse Ghc.tcLookupId fields
             let funTy = Ghc.mkVisFunTyMany recordTy (Ghc.anyTypeOfKind Ghc.liftedTypeKind)
             pure $ Just (Just (Ghc.EvExpr $ Ghc.mkListExpr funTy selVars), [], ct)

         -- ToRecord
         | clsName == toRecordName
         , [ recordTy ] <- cc_tyargs
         , Just RecordParts{..} <- getRecordFields recordTy
         , Just arrType <- Ghc.synTyConRhs_maybe recArrayTyCon
         -> do
             arrBindName <- Ghc.unsafeTcPluginTcM
                          $ Ghc.newName (Ghc.mkOccName Ghc.varName "arr")

             let arrBind = Ghc.mkLocalIdOrCoVar arrBindName Ghc.Many arrType

                 accessor ix =
                   Ghc.mkCoreApps (Ghc.Var indexArrayId)
                     [Ghc.Var arrBind, Ghc.mkUncheckedIntExpr ix]
                 result =
                   Ghc.mkCoreLams [arrBind] $
                     Ghc.mkCoreConApps recordCon
                       $ (Ghc.Type <$> recordTyArgs) ++ do
                           (_, ix) <- recordFields `zip` [0..]
                           [accessor ix]
             pure $ Just (Just (Ghc.EvExpr result), [], ct)

         | clsName == foldFieldsName
         , [ predConTy, recordTy, effectConTy ] <- cc_tyargs
         , Just predTyCon <- Ghc.tyConAppTyCon_maybe predConTy
         , Just fields <- recordFields <$> getRecordFields recordTy -> do
             predClass <- Ghc.tcLookupClass $ Ghc.getName predTyCon
             result
               <- buildFoldFieldsExpr
                    inp
                    (Ghc.ctLoc ct)
                    recordTy
                    effectConTy
                    predClass
                    (map (fmap fst) fields)
             pure $ case result of
               Left newWanteds -> Just (Nothing, newWanteds, ct)
               Right expr -> Just (Just (Ghc.EvExpr expr), [], ct)

         | clsName == instantiateName
         , [ recTy
           , effectCon
           , tupleTy
           ] <- cc_tyargs
         , Just fields <- recordFields <$> getRecordFields recTy -> do
             let fieldNames = fst <$> fields
             (tuplePairs, instantiateExpr)
               <- gatherTupleFieldsAndBuildExpr inp fieldNames tupleTy
             let recordFieldMap = Ghc.listToUFM
                                $ (\(label, t) -> (label, (label, fst t)))
                              <$> fields
             newWanteds <-
               mkFieldTypeCheckWanteds
                 inp
                 (Ghc.ctLoc ct)
                 recTy
                 recordFieldMap
                 tuplePairs
                 effectCon
             pure $ Just (Just (Ghc.EvExpr instantiateExpr), newWanteds, ct)

         | otherwise -> pure Nothing
    _ -> pure Nothing

  let newWanteds = do
        Just (Just _, ws, _) <- results
        ws
      insolubles = do
        Just (Nothing, ws, _) <- results
        ws
      solveds = do
        Just (mR, _, ct) <- results
        case mR of
          Nothing | not (null insolubles) ->
            -- Emit a bogus dict if there are insoluables, otherwise they
            -- don't get reported.
            [(Ghc.EvExpr Ghc.unitExpr, ct)]
          Just r -> [(r, ct)]
          _ -> []

  pure Ghc.TcPluginSolveResult
    { tcPluginInsolubleCts = insolubles
    , tcPluginSolvedCts = solveds
    , tcPluginNewCts = newWanteds
    }

getStrTyLitVal :: Ghc.Type -> Maybe Ghc.FastString
getStrTyLitVal = \case
  Ghc.LitTy (Ghc.StrTyLit fs) -> Just fs
  _ -> Nothing

data RecordParts = RecordParts
  { recordCon :: Ghc.DataCon
  , recordTyArgs :: [Ghc.Type]
  , recordFields :: [(Ghc.FastString, (Ghc.Type, Ghc.Name))]
  }

getRecordFields :: Ghc.Type -> Maybe RecordParts
getRecordFields = \case
  Ghc.TyConApp tyCon args
    | Ghc.isAlgTyCon tyCon
    , Ghc.DataTyCon{..} <- Ghc.algTyConRhs tyCon
    , [dataCon] <- data_cons -> do
      -- Don't allow existential ty vars
      guard $ length (Ghc.dataConUnivAndExTyCoVars dataCon) == length args
      -- Don't allow contexts
      guard . null $ Ghc.dataConTheta dataCon ++ Ghc.dataConStupidTheta dataCon
      let fieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon args
          fieldLabels = Ghc.flLabel <$> Ghc.dataConFieldLabels dataCon
          fieldSelectors = Ghc.flSelector <$> Ghc.dataConFieldLabels dataCon
      guard . not $ null fieldTys
      guard $ length fieldTys == length fieldLabels
      -- add a guard for equality of rep arity and source arity?
      Just RecordParts
        { recordCon = dataCon
        , recordTyArgs = args
        , recordFields = zip fieldLabels $ zip fieldTys fieldSelectors
        }
  _ -> Nothing

data LabelMatchResult
  = Match Ghc.FastString Ghc.Type Ghc.Type
  | Missing Ghc.FastString
  | Extra Ghc.FastString

mkFieldTypeCheckWanteds
  :: PluginInputs
  -> Ghc.CtLoc
  -> Ghc.Type
  -> Ghc.UniqFM Ghc.FastString (Ghc.FastString, Ghc.Type)
  -> [(Ghc.FastString, Ghc.Type)]
  -> Ghc.Type -- effect constructor
  -> Ghc.TcPluginM [Ghc.Ct]
mkFieldTypeCheckWanteds inp ctLoc recTy recordFieldMap tuplePairs effectCon = do
  let matchResults = getFieldMatchResults recordFieldMap tuplePairs
  -- TODO generate error messages from missing or extra fields instead of just failing
  let go = \case
        Match labelFs recFieldTy tupleTy -> do
          let fieldNameTy = Ghc.LitTy $ Ghc.StrTyLit labelFs
              classArgs = [ fieldNameTy
                          , effectCon
                          , recFieldTy
                          , tupleTy
                          ]
          makeWantedCt ctLoc (fieldTypeCheckClass inp) classArgs

        Missing labelFs -> do
          let fieldNameTy = Ghc.LitTy $ Ghc.StrTyLit labelFs
              classArgs = [ fieldNameTy
                          , recTy
                          ]
          makeWantedCt ctLoc (missingFieldClass inp) classArgs
        Extra labelFs -> do
          let fieldNameTy = Ghc.LitTy $ Ghc.StrTyLit labelFs
              classArgs = [ fieldNameTy
                          , recTy
                          ]
          makeWantedCt ctLoc (unknownFieldClass inp) classArgs
  traverse go $ Ghc.nonDetEltsUFM matchResults

makeWantedCt :: Ghc.CtLoc -> Ghc.Class -> [Ghc.Type] -> Ghc.TcPluginM Ghc.Ct
makeWantedCt ctLoc clss classArgs = do
  let classPred = Ghc.mkClassPred clss classArgs
  evidence <- Ghc.newWanted ctLoc classPred
  pure Ghc.CDictCan
    { Ghc.cc_ev = evidence
    , Ghc.cc_class = clss
    , Ghc.cc_tyargs = classArgs
    , Ghc.cc_pend_sc = False
    , Ghc.cc_fundeps = False
    }

getFieldMatchResults
  :: Ghc.UniqFM Ghc.FastString (Ghc.FastString, Ghc.Type)
  -> [(Ghc.FastString, Ghc.Type)]
  -> Ghc.UniqFM Ghc.FastString LabelMatchResult
getFieldMatchResults recordFieldMap tuplePairs = do
  let tupleFieldMap = Ghc.listToUFM $ zip (fst <$> tuplePairs) tuplePairs

      inBoth (_, x) (label, y) = Just $ Match label x y
      onlyInRecord = fmap (\(label, _) -> Missing label)
      onlyInTuple = fmap (\(label, _) -> Extra label)

   in Ghc.mergeUFM
        inBoth
        onlyInRecord
        onlyInTuple
        recordFieldMap
        tupleFieldMap

gatherTupleFieldsAndBuildExpr
  :: PluginInputs
  -> [Ghc.FastString] -- field names in order
  -> Ghc.Type -- tuple
  -> Ghc.TcPluginM ([(Ghc.FastString, Ghc.Type)], Ghc.CoreExpr)
gatherTupleFieldsAndBuildExpr MkPluginInputs{..} fieldNames tupleTy = do
    tupleName <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "tuple")
    let tupleBind = Ghc.mkLocalIdOrCoVar tupleName Ghc.Many tupleTy

    (pairs, expr) <- go [] tupleBind tupleTy
    pure (pairs, Ghc.mkCoreLams [tupleBind] expr)
  where
    getFieldPairFromTy ty = StateT $ \exprBuilder -> case ty of
      Ghc.TyConApp _ [fieldNameTy@(Ghc.TyConApp fnTyCon fieldNameLit), fieldTy]
        | Ghc.getName fnTyCon == Ghc.getName fieldNameTyCon
        , [Ghc.LitTy (Ghc.StrTyLit fs)] <- fieldNameLit -> do
          fieldVarName
            <- Ghc.unsafeTcPluginTcM
             $ Ghc.newName (Ghc.mkOccName Ghc.varName "field")
          let fieldVarId = Ghc.mkLocalIdOrCoVar fieldVarName Ghc.Many fieldTy

          fieldPairName
            <- Ghc.unsafeTcPluginTcM
             $ Ghc.newName (Ghc.mkOccName Ghc.varName "fieldPair")
          let fieldPairId = Ghc.mkLocalIdOrCoVar fieldPairName Ghc.Many ty
              mkPairCase =
                Ghc.mkSingleAltCase
                  (Ghc.Var fieldPairId)
                  (Ghc.mkWildValBinder Ghc.Many ty)
                  (Ghc.DataAlt $ Ghc.tupleDataCon Ghc.Boxed 2)
                  [Ghc.mkWildValBinder Ghc.Many fieldNameTy, fieldVarId]

          pure ( (fieldPairId, (fs, (fieldVarId, fieldTy)))
               , mkPairCase . exprBuilder
               )

      _ -> fail "unexpected tuple structure in gatherTupleFieldsAndBuildExpr"

    go ids tupleBind ty = case ty of
      Ghc.TyConApp tyCon (restTy : fieldTys)
        | Ghc.isAlgTyCon tyCon
        , Ghc.TupleTyCon{} <- Ghc.algTyConRhs tyCon -> do
            (fields, exprBuilder)
              <- (`runStateT` id)
               $ traverse getFieldPairFromTy fieldTys

            restVarName
              <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "rest")

            let restVarId = Ghc.mkLocalIdOrCoVar restVarName Ghc.Many restTy

            (resultIds, nextExpr)
              <- go (map snd fields ++ ids)
                    restVarId
                    restTy

            let expr = exprBuilder nextExpr
                tupleCase =
                  Ghc.mkSingleAltCase
                    (Ghc.Var tupleBind)
                    (Ghc.mkWildValBinder Ghc.Many ty)
                    (Ghc.DataAlt $ Ghc.tupleDataCon Ghc.Boxed (length fieldTys + 1))
                    (restVarId : (fst <$> fields))
                    expr

            pure (resultIds, tupleCase)

      -- the tuple nesting ends in a unit
      Ghc.TyConApp tyCon []
        | tyCon == Ghc.unitTyCon
        -> do
          let idsMap = Ghc.listToUFM (map (fmap fst) ids)
              fieldExprsInOrder :: [Ghc.CoreExpr]
              fieldExprsInOrder = do
                label <- fieldNames
                Just fieldId <- [Ghc.lookupUFM idsMap label] -- actual field validation is done elsewhere
                [Ghc.Var fieldId]
              fieldListExpr =
                Ghc.mkListExpr (Ghc.anyTypeOfKind Ghc.liftedTypeKind) fieldExprsInOrder
              arrExpr =
                Ghc.mkCoreApps (Ghc.Var arrayFromListId) [fieldListExpr]
          pure (map (fmap snd) ids, arrExpr)

      _ -> fail "garbage input to 'gatherTupleFieldsAndBuildExpr'"

buildFoldFieldsExpr
  :: PluginInputs
  -> Ghc.CtLoc
  -> Ghc.Type
  -> Ghc.Type
  -> Ghc.Class
  -> [(Ghc.FastString, Ghc.Type)]
  -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.CoreExpr)
buildFoldFieldsExpr MkPluginInputs{..} ctLoc recordTy effectConTy predClass fields = do
  accTyVarName <- Ghc.unsafeTcPluginTcM
                $ Ghc.newName (Ghc.mkOccName Ghc.varName "acc")
  xTyVarName <- Ghc.unsafeTcPluginTcM
              $ Ghc.newName (Ghc.mkOccName Ghc.varName "x")

  fieldGenName <- Ghc.unsafeTcPluginTcM
                $ Ghc.newName (Ghc.mkOccName Ghc.varName "fieldGen")

  fieldTyVarName <- Ghc.unsafeTcPluginTcM
                  $ Ghc.newName (Ghc.mkOccName Ghc.varName "a")
  accumulatorName <- Ghc.unsafeTcPluginTcM
                   $ Ghc.newName (Ghc.mkOccName Ghc.varName "accumulator")
  initAccName <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "initAcc")
  hkdName <- Ghc.unsafeTcPluginTcM
           $ Ghc.newName (Ghc.mkOccName Ghc.varName "hkd")

  let fieldTyVar = Ghc.mkTyVar fieldTyVarName Ghc.liftedTypeKind
      accTyVar = Ghc.mkTyVar accTyVarName Ghc.liftedTypeKind
      xTyVar = Ghc.mkTyVar xTyVarName Ghc.liftedTypeKind

      hkdTy = Ghc.mkTyConApp hkdTyCon [recordTy, effectConTy]
      fieldGenTy = Ghc.mkSigmaTy forallBndrs preds tyBody
        where
          forallBndrs = [ Ghc.mkTyCoVarBinder Ghc.Required fieldTyVar ]
          preds = [ Ghc.mkClassPred predClass
                      [Ghc.mkAppTys effectConTy [Ghc.mkTyVarTy fieldTyVar]]
                  ]
          tyBody = Ghc.stringTy
                 `Ghc.mkVisFunTyMany`
                   ( hkdTy
                   `Ghc.mkVisFunTyMany`
                     Ghc.mkAppTys effectConTy [Ghc.mkTyVarTy fieldTyVar]
                   )
                 `Ghc.mkVisFunTyMany`
                   Ghc.mkTyVarTy xTyVar
      fieldGenBndr = Ghc.mkLocalIdOrCoVar fieldGenName Ghc.Many fieldGenTy
      initAccBndr = Ghc.mkLocalIdOrCoVar initAccName Ghc.Many (Ghc.mkTyVarTy accTyVar)

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
        :: (Integer, (Ghc.FastString, Ghc.Type))
        -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.CoreExpr)
      mkFieldGenExpr (idx, (fieldName, fieldTy)) = do
        stringIds <- Ghc.getMkStringIds Ghc.tcLookupId
        let fieldNameExpr = Ghc.mkStringExprFSWith stringIds fieldName
            getterExpr =
              Ghc.mkCoreLams [hkdBndr] $
                Ghc.mkCoreApps (Ghc.Var indexArrayId)
                               [Ghc.Var hkdBndr, Ghc.mkUncheckedIntExpr idx]
            predClassArgs = [Ghc.mkAppTys effectConTy [fieldTy]]

        predCt <- makeWantedCt ctLoc predClass predClassArgs

        (_, evBindMap)
          <- Ghc.unsafeTcPluginTcM
           . Ghc.runTcS
           . Ghc.solveSimpleWanteds
           $ Ghc.singleCt predCt

        let evVar = Ghc.ctEvEvId $ Ghc.ctEvidence predCt
        ePredDict <- buildEvExprFromMap ctLoc evVar evBindMap

        traceM $ Ghc.showSDocUnsafe $ Ghc.ppr fieldNameExpr
        pure $ ePredDict <&> \predDict ->
          Ghc.mkCoreApps (Ghc.Var fieldGenBndr) $
            [ Ghc.Type fieldTy
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

  result <- traverse mkFieldGenExpr (zip [0..] fields)
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

evExprFromEvTerm :: Ghc.EvTerm -> Maybe Ghc.EvExpr
evExprFromEvTerm (Ghc.EvExpr x) = Just x
evExprFromEvTerm _ = Nothing

-- | The output of solving wanted contains references to variables that are not
-- in scope so an expr must be constructed that binds those variables locally.
-- The solver seems to always output them in reverse dependency order, hence
-- using a left fold to build the bindings.
buildEvExprFromMap
  :: Ghc.CtLoc
  -> Ghc.EvVar
  -> Ghc.EvBindMap
  -> Ghc.TcPluginM (Either [Ghc.Ct] Ghc.EvExpr)
buildEvExprFromMap ctLoc evVar evBindMap =
  let evBinds = Ghc.dVarEnvElts $ Ghc.ev_bind_varenv evBindMap
      mResult = case List.partition ((== evVar) . Ghc.eb_lhs) evBinds of
        ([m], rest) -> do
          baseDict <- evExprFromEvTerm $ Ghc.eb_rhs m
          let go acc x = do
                evExpr <- evExprFromEvTerm $ Ghc.eb_rhs x
                Just $ Ghc.bindNonRec (Ghc.eb_lhs x) evExpr acc
           in foldM go baseDict rest
        _ -> Nothing
   in case mResult of
        Nothing -> do
          mCt <- mkNewWantedFromExpr ctLoc evVar
          pure . Left $ maybeToList mCt
        Just result -> do
          let freeVars = Ghc.exprFreeVars result
          if Ghc.isEmptyUniqSet freeVars
             then pure $ Right result
             else do
               mCts <- traverse (mkNewWantedFromExpr ctLoc)
                                (Ghc.nonDetEltsUniqSet freeVars)
               pure . Left $ catMaybes mCts

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

