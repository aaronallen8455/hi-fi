{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.TcPlugin
  ( tcPlugin
  ) where

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
tcSolver inp@MkPluginInputs{..} _env _givens wanteds = do
  results <- for wanteds $ \case
    ct@Ghc.CDictCan{ cc_class, cc_tyargs } -> do
      let clsName = Ghc.getName cc_class

         -- FieldGetters
      if | clsName == fieldGettersName
         , [ recordTy ] <- cc_tyargs
         , Just recordParts <- getRecordFields inp recordTy
         , let fields = map snd $ recordFields recordParts
         -> do
             mExprs <- fmap concat . sequence <$>
               traverse (mkFieldGetters recordTy (recordTyVarSubst recordParts))
                        fields
             for mExprs $ \exprs -> do
               let funTy = Ghc.mkVisFunTyMany recordTy (Ghc.anyTypeOfKind Ghc.liftedTypeKind)
               pure (Just (Ghc.EvExpr $ Ghc.mkListExpr funTy exprs), [], ct)

         -- ToRecord
         | clsName == toRecordName
         , [ recordTy ] <- cc_tyargs
         , Just recordParts <- getRecordFields inp recordTy
         , Just arrType <- Ghc.synTyConRhs_maybe recArrayTyCon
         -> do
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
         , Just fields <- recordFields <$> getRecordFields inp recordTy -> do
             predClass <- Ghc.tcLookupClass $ Ghc.getName predTyCon
             result
               <- buildFoldFieldsExpr
                    inp
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
         , Just fields <- recordFields <$> getRecordFields inp recTy -> do
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
         , Just fields <- recordFields <$> getRecordFields inp recTy
         , let namesToIndexes = fmap fieldNesting <$> fields
         , Just idx <- lookup fieldName namesToIndexes -> do
             getterExpr <- mkHkdHasFieldExpr inp recTy effectTy idx
             pure $ Just (Just (Ghc.EvExpr getterExpr), [], ct)

         -- HkdSetField
         | clsName == hkdSetFieldName
         , [ getStrTyLitVal -> Just fieldName
           , recTy
           , effectTy
           , fieldTy
           ] <- cc_tyargs
         , Just fields <- recordFields <$> getRecordFields inp recTy
         , let namesToIndexes = fmap fieldNesting <$> fields
         , Just idx <- lookup fieldName namesToIndexes -> do
             setterExpr <- mkHkdSetFieldExpr inp recTy effectTy fieldTy idx
             pure $ Just (Just (Ghc.EvExpr setterExpr), [], ct)

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
