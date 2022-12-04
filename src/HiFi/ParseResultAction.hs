{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.ParseResultAction
  ( parseResultAction
  ) where

import qualified Data.Generics as Syb

import qualified HiFi.GhcFacade as Ghc

parseResultAction :: Ghc.ModSummary -> Ghc.ParsedResult -> Ghc.Hsc Ghc.ParsedResult
parseResultAction _modSummary parsedResult = do
  -- TODO check if source code actually contains "mkHKD" before doing a traversal
  let parsedModule = Ghc.parsedResultModule parsedResult
      applyTransform mo =
        mo { Ghc.hsmodDecls = Syb.everywhere (Syb.mkT transformMkHKD)
                            $ Ghc.hsmodDecls mo
           }
      newModule = applyTransform <$> Ghc.hpm_module parsedModule
  pure parsedResult
    { Ghc.parsedResultModule = parsedModule { Ghc.hpm_module = newModule } }

transformMkHKD :: Ghc.HsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
transformMkHKD = \case
    Ghc.RecordUpd{..}
      | checkExpr (Ghc.unLoc rupd_expr)
      , Left fields <- rupd_flds
      -> let fieldPairs = getFieldPair . Ghc.unLoc <$> fields
             tuple = mkTupleFromFields fieldPairs
          in Ghc.unLoc $ Ghc.nlHsApp rupd_expr tuple
    other -> other
  where
    checkExpr = \case
      Ghc.HsVar _ (Ghc.unLoc -> updVar)
        | extractName updVar == Ghc.mkFastString "mkHKD"
        -> True
      Ghc.HsPar _ _ expr _ -> checkExpr $ Ghc.unLoc expr
      Ghc.HsAppType _ expr _ -> checkExpr $ Ghc.unLoc expr
      _ -> False

getFieldPair
  :: Ghc.HsRecUpdField Ghc.GhcPs
  -> (Ghc.FastString, Ghc.HsExpr Ghc.GhcPs)
getFieldPair Ghc.HsFieldBind {..} = (getFieldName $ Ghc.unLoc hfbLHS, Ghc.unLoc hfbRHS)

extractName :: Ghc.RdrName -> Ghc.FastString
extractName = Ghc.occNameFS . Ghc.rdrNameOcc

getFieldName :: Ghc.AmbiguousFieldOcc Ghc.GhcPs -> Ghc.FastString
getFieldName = extractName . Ghc.rdrNameAmbiguousFieldOcc

mkTupleFromFields
  :: [(Ghc.FastString, Ghc.HsExpr Ghc.GhcPs)]
  -> Ghc.LHsExpr Ghc.GhcPs
mkTupleFromFields [] = unitHsExpr
mkTupleFromFields ((fieldName, expr) : rest) =
  Ghc.mkLHsTupleExpr
    [ pairTuple, mkTupleFromFields rest ]
    mempty
  where
    pairTuple =
      Ghc.mkLHsTupleExpr
        [Ghc.noLocA fieldNameExpr, Ghc.noLocA expr]
        mempty
    fieldNameExpr :: Ghc.HsExpr Ghc.GhcPs
    fieldNameExpr =
      Ghc.HsAppType
        Ghc.noSrcSpan
        (Ghc.noLocA $
          Ghc.HsVar Ghc.noExtField $
            Ghc.L Ghc.noSrcSpanA . Ghc.mkUnqual Ghc.dataName $
              Ghc.mkFastString "MkFieldName"
        )
        (Ghc.HsWC Ghc.NoExtField . Ghc.noLocA $
          Ghc.HsTyLit Ghc.NoExtField
            (Ghc.HsStrTy Ghc.NoSourceText fieldName)
        )

unitHsExpr :: Ghc.LHsExpr Ghc.GhcPs
unitHsExpr = Ghc.nlHsVar (Ghc.Exact $ Ghc.getName Ghc.unitDataCon)
