{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.TcPlugin
  ( tcPlugin
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.Trans.Except
import           Data.Traversable (for)

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.Equality (nestHKDEquality)
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
    ct@Ghc.CIrredCan{ cc_ev }
      | Just (_, ty1, ty2) <- Ghc.getEqPredTys_maybe (Ghc.ctEvPred cc_ev)
      ->
        for (nestHKDEquality inp ty1 ty2 <|> nestHKDEquality inp ty2 ty1) $ \case
          Left eqPred -> do
            let ctLoc = Ghc.ctLoc ct
            evidence <- Ghc.newWanted ctLoc eqPred
            pure MkResult
              { evidence = Nothing
              , newWanted = [Ghc.mkNonCanonical evidence]
              , subject = ct
              }
          Right evTerm ->
            pure MkResult
              { evidence = Just evTerm
              , newWanted = []
              , subject = ct
              }

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
                pure MkResult
                  { evidence = Just . Ghc.EvExpr $ Ghc.mkListExpr funTy exprs
                  , newWanted = []
                  , subject = ct
                  }

         -- ToRecord
         | clsName == toRecordName
         , [ recordTy ] <- cc_tyargs
         , Just arrType <- Ghc.synTyConRhs_maybe recArrayTyCon
         -> guardSupportedRecord inp recordTy ct $ \recordParts -> do
              arrBindName <- Ghc.unsafeTcPluginTcM
                           $ Ghc.newName (Ghc.mkOccName Ghc.varName "arr")

              let arrBind = Ghc.mkLocalIdOrCoVar arrBindName Ghc.ManyTy' arrType
                  expr = mkToRecordExpr inp recordTy recordParts arrBind 0
                  result = Ghc.mkCoreLams [arrBind] expr
              pure $ Just MkResult
                { evidence = Just (Ghc.EvExpr result)
                , newWanted = []
                , subject = ct
                }

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
                Left newWanteds -> Just MkResult
                  { evidence = Nothing
                  , newWanted = newWanteds
                  , subject = ct
                  }
                Right expr -> Just MkResult
                  { evidence = Just (Ghc.EvExpr expr)
                  , newWanted = []
                  , subject = ct
                  }

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
              pure $ Just MkResult
                { evidence = Just (Ghc.EvExpr instantiateExpr)
                , newWanted = newWanteds
                , subject = ct
                }

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
                pure MkResult
                  { evidence = Just (Ghc.EvExpr getterExpr)
                  , newWanted = []
                  , subject = ct
                  }

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
                pure MkResult
                  { evidence = Just (Ghc.EvExpr setterExpr)
                  , newWanted = []
                  , subject = ct
                  }

         | otherwise -> pure Nothing
    _ -> pure Nothing

  let newWanteds = do
        Just MkResult { evidence = Just _, newWanted = ws } <- results
        ws
      insolubles = do
        Just MkResult { evidence = Nothing, newWanted = ws, subject = ct } <- results
        ws ++ [ct]
      solveds = do
        Just MkResult { evidence = mEv, subject = ct } <- results
        case mEv of
          Just r -> [(r, ct)]
          _ -> []

  pure $ Ghc.mkTcPluginSolveResult newWanteds insolubles solveds

data Result = MkResult
  { evidence :: !(Maybe Ghc.EvTerm) -- ^ Resulting dict, if successful
  , newWanted :: ![Ghc.Ct] -- ^ New wanted to emit
  , subject :: !Ghc.Ct -- ^ Constraint being solved
  }

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
  -> (RecordParts -> Ghc.TcPluginM (Maybe Result))
  -> Ghc.TcPluginM (Maybe Result)
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
          pure $ Just MkResult
            { evidence = Just $ Ghc.ctEvTerm $ Ghc.ctEvidence ct
            , newWanted = [newWanted]
            , subject = ct
            }
        Left (DataConNotInScope dataCon) -> do
          newWanted <- fst <$> makeWantedCt (Ghc.ctLoc ct) (dataConNotInScopeClass inp)
                         [Ghc.mkTyConTy $ Ghc.promoteDataCon dataCon]
          pure $ Just MkResult
            { evidence = Just $ Ghc.ctEvTerm $ Ghc.ctEvidence ct
            , newWanted = [newWanted]
            , subject = ct
            }
        Right recParts -> k recParts
