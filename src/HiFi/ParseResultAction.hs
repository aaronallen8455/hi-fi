{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.ParseResultAction
  ( parseResultAction
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Generics as Syb

import qualified HiFi.GhcFacade as Ghc

#if MIN_VERSION_ghc(9,4,0)
parseResultAction :: Ghc.ModSummary -> Ghc.ParsedResult -> Ghc.Hsc Ghc.ParsedResult
#elif MIN_VERSION_ghc(9,2,0)
parseResultAction :: Ghc.ModSummary -> Ghc.HsParsedModule -> Ghc.Hsc Ghc.HsParsedModule
#endif
parseResultAction modSummary parsedResult = do
  let applyTransform mo =
        mo { Ghc.hsmodDecls = Syb.everywhere (Syb.mkT transformMkHKD)
                            $ Ghc.hsmodDecls mo
           }
      mSrcCodeBuffer = Ghc.ms_hspp_buf modSummary
#if MIN_VERSION_ghc(9,4,0)
      parsedModule = Ghc.parsedResultModule parsedResult
#elif MIN_VERSION_ghc(9,2,0)
      parsedModule = parsedResult
#endif
      newModule = applyTransform <$> Ghc.hpm_module parsedModule

  -- To prevent doing a traversal of the AST in vain, check if the source code
  -- contains the target string, otherwise no-op
  case containsMatch "mkHKD" <$> mSrcCodeBuffer of
    Just False ->
      pure parsedResult
    _ ->
#if MIN_VERSION_ghc(9,4,0)
      pure parsedResult
        { Ghc.parsedResultModule = parsedModule { Ghc.hpm_module = newModule } }
#elif MIN_VERSION_ghc(9,2,0)
      pure parsedResult { Ghc.hpm_module = newModule }
#endif

containsMatch :: BS.ByteString -> Ghc.StringBuffer -> Bool
containsMatch needle buf =
  not . BS.null . snd . BS.breakSubstring needle $ bufToByteString buf

bufToByteString :: Ghc.StringBuffer -> BS.ByteString
bufToByteString (Ghc.StringBuffer buf len cur) = BSI.PS buf cur len

transformMkHKD :: Ghc.HsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
transformMkHKD = \case
    Ghc.RecordUpd{..}
      | checkExpr (Ghc.unLoc rupd_expr)
      , Left fields <- rupd_flds
      -> let fieldPairs = Ghc.getFieldPair . Ghc.unLoc <$> fields
             tuple = mkTupleFromFields fieldPairs
          in Ghc.unLoc $ Ghc.nlHsApp rupd_expr tuple
    other -> other
  where
    checkExpr = \case
      Ghc.HsVar _ (Ghc.unLoc -> updVar)
        | Ghc.extractName updVar == Ghc.mkFastString "mkHKD"
        -> True
      Ghc.HsPar' expr -> checkExpr $ Ghc.unLoc expr
      Ghc.HsAppType _ expr _ -> checkExpr $ Ghc.unLoc expr
      _ -> False

mkTupleFromFields
  :: [(Ghc.FastString, Ghc.HsExpr Ghc.GhcPs)]
  -> Ghc.LHsExpr Ghc.GhcPs
mkTupleFromFields [] = unitHsExpr
mkTupleFromFields (splitAt (Ghc.mAX_TUPLE_SIZE - 1) -> (fields, rest)) =
    Ghc.mkLHsTupleExpr
      (mkTupleFromFields rest : map mkPairTuple fields)
      mempty
  where
    mkPairTuple (fieldName, expr) =
      Ghc.mkLHsTupleExpr
        [Ghc.noLocA $ mkFieldNameExpr fieldName, Ghc.noLocA expr]
        mempty
    mkFieldNameExpr fieldName =
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
