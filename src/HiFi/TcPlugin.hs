{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.TcPlugin
  ( tcPlugin
  ) where

import           Control.Monad.Trans.Except
import           Data.Traversable (for)

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.FieldGetters (mkFieldGetters)
import           HiFi.TcPlugin.FoldFields (buildFoldFieldsExpr)
import           HiFi.TcPlugin.HkdHasField (mkHkdHasFieldExpr)
import           HiFi.TcPlugin.HkdSetField (mkHkdSetFieldExpr)
import           HiFi.TcPlugin.Instantiate (gatherTupleFieldsAndBuildExpr, mkFieldTypeCheckWanteds)
import           HiFi.TcPlugin.PluginInputs
import           HiFi.TcPlugin.RecordParts
import           HiFi.TcPlugin.ToRecord (mkToRecordExpr)
import           HiFi.TcPlugin.Utils (makeWantedCt)

tcPlugin :: Ghc.TcPlugin
tcPlugin = Ghc.TcPlugin
  { Ghc.tcPluginInit = lookupInputs
  , Ghc.tcPluginSolve = tcSolver
#if MIN_VERSION_ghc(9,4,0)
  , Ghc.tcPluginRewrite = mempty
#endif
  , Ghc.tcPluginStop = \_ -> pure ()
  }

tcSolver :: PluginInputs -> Ghc.TcPluginSolver
tcSolver inp@MkPluginInputs{..} = Ghc.adaptSolver $ \env givens wanteds -> do
  results <- for wanteds $ \case
    ct@Ghc.CDictCan{ cc_class, cc_tyargs } -> do
      let clsName = Ghc.getName cc_class

         -- FieldGetters
      if | clsName == fieldGettersName
         , [ recordTy ] <- cc_tyargs
         -> guardSupportedRecord inp recordTy ct $ \recordParts -> do
              let fields = map snd $ recordFields recordParts
              mExprs <- fmap concat . sequence <$>
                traverse (mkFieldGetters recordTy (recordTyVarSubst recordParts))
                         fields
              for mExprs $ \exprs -> do
                let funTy = Ghc.mkVisFunTyMany recordTy (Ghc.anyTypeOfKind Ghc.liftedTypeKind)
                pure (Just (Ghc.EvExpr $ Ghc.mkListExpr funTy exprs), [], ct)

         -- ToRecord
         | clsName == toRecordName
         , [ recordTy ] <- cc_tyargs
         , Just arrType <- Ghc.synTyConRhs_maybe recArrayTyCon
         -> guardSupportedRecord inp recordTy ct $ \recordParts -> do
              arrBindName <- Ghc.unsafeTcPluginTcM
                           $ Ghc.newName (Ghc.mkOccName Ghc.varName "arr")

              let arrBind = Ghc.mkLocalIdOrCoVar arrBindName Ghc.Many arrType
                  expr = mkToRecordExpr inp recordTy recordParts arrBind 0
                  result = Ghc.mkCoreLams [arrBind] expr
              pure $ Just (Just (Ghc.EvExpr result), [], ct)

         -- FoldFields
         | clsName == foldFieldsName
         , [ predConTy, recordTy, effectConTy ] <- cc_tyargs
         , Just (predTyCon, predArgs) <- Ghc.tcSplitTyConApp_maybe predConTy
         -> guardSupportedRecord inp recordTy ct $ \recordParts -> do
              let fields = recordFields recordParts
              predClass <- Ghc.tcLookupClass $ Ghc.getName predTyCon
              result
                <- buildFoldFieldsExpr
                     inp
                     env
                     givens
                     (Ghc.ctLoc ct)
                     recordTy
                     effectConTy
                     predClass
                     predArgs
                     fields
              pure $ case result of
                Left newWanteds -> Just (Nothing, newWanteds, ct)
                Right expr -> Just (Just (Ghc.EvExpr expr), [], ct)

         -- Instantiate
         | clsName == instantiateName
         , [ recTy
           , effectCon
           , tupleTy
           ] <- cc_tyargs
         -> guardSupportedRecord inp recTy ct $ \recordParts -> do
              let fields = recordFields recordParts
              (tuplePairs, instantiateExpr)
                <- gatherTupleFieldsAndBuildExpr inp fields recTy tupleTy effectCon
              let recordFieldMap = Ghc.listToUFM
                                 $ (\(label, parts) -> (label, (label, fieldType parts)))
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

         -- HkdHasField
         | clsName == hkdHasFieldName
         , [ getStrTyLitVal -> Just fieldName
           , recTy
           , effectTy
           , _fieldTy
           ] <- cc_tyargs
         -> guardSupportedRecord inp recTy ct $ \recordParts -> do
              let fields = recordFields recordParts
                  namesToIndexes = fmap fieldNesting <$> fields
              for (lookup fieldName namesToIndexes) $ \idx -> do
                getterExpr <- mkHkdHasFieldExpr inp recTy effectTy idx
                pure (Just (Ghc.EvExpr getterExpr), [], ct)

         -- HkdSetField
         | clsName == hkdSetFieldName
         , [ getStrTyLitVal -> Just fieldName
           , recTy
           , effectTy
           , fieldTy
           ] <- cc_tyargs
         -> guardSupportedRecord inp recTy ct $ \recordParts -> do
              let fields = recordFields recordParts
                  namesToIndexes = fmap fieldNesting <$> fields
              for (lookup fieldName namesToIndexes) $ \idx -> do
                setterExpr <- mkHkdSetFieldExpr inp recTy effectTy fieldTy idx
                pure (Just (Ghc.EvExpr setterExpr), [], ct)

         | otherwise -> pure Nothing
    _ -> pure Nothing

  let newWanteds = do
        Just (Just _, ws, _) <- results
        ws
      insolubles = do
        Just (Nothing, ws, ct) <- results
        ws ++ [ct]
      solveds = do
        Just (mR, _, ct) <- results
        case mR of
          Just r -> [(r, ct)]
          _ -> []

  pure $ Ghc.mkTcPluginSolveResult newWanteds insolubles solveds

getStrTyLitVal :: Ghc.Type -> Maybe Ghc.FastString
getStrTyLitVal = \case
  Ghc.LitTy (Ghc.StrTyLit fs) -> Just fs
  _ -> Nothing

-- | Emit the UnsupportedRecord wanted constraint if the type is not supported
-- for HKD promotion.
guardSupportedRecord
  :: PluginInputs
  -> Ghc.Type
  -> Ghc.Ct
  -> (RecordParts -> Ghc.TcPluginM (Maybe (Maybe Ghc.EvTerm, [Ghc.Ct], Ghc.Ct)))
  -> Ghc.TcPluginM (Maybe (Maybe Ghc.EvTerm, [Ghc.Ct], Ghc.Ct))
guardSupportedRecord inp recordTy ct k =
  -- If the record type is a type variable, return nothing so that the constraint
  -- will be attempted again when the variable is instantiated.
  if Ghc.tcIsTyVarTy recordTy
     then pure Nothing
     else do
      eParts <- runExceptT $ getRecordParts inp mempty recordTy
      case eParts of
        Left (UnsupportedTy ty) -> do
          -- use the place holder evidence for the wanted constraint so that the
          -- desired error message will be displayed.
          newWanted <- fst <$> makeWantedCt (Ghc.ctLoc ct) (unsupportedRecordClass inp) [ty]
          pure $ Just (Just $ Ghc.ctEvTerm $ Ghc.ctEvidence ct, [newWanted], ct)
        Left (DataConNotInScope dataCon) -> do
          newWanted <- fst <$> makeWantedCt (Ghc.ctLoc ct) (dataConNotInScopeClass inp)
                         [Ghc.mkTyConTy $ Ghc.promoteDataCon dataCon]
          pure $ Just (Just $ Ghc.ctEvTerm $ Ghc.ctEvidence ct, [newWanted], ct)
        Right recParts -> k recParts
