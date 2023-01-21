{-# LANGUAGE RecordWildCards #-}
module HiFi.TcPlugin.ToRecord
  ( mkToRecordExpr
  ) where

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs
import           HiFi.TcPlugin.RecordParts

mkToRecordExpr
  :: PluginInputs
  -> Ghc.Type
  -> RecordParts
  -> Ghc.Var
  -> Integer
  -> Ghc.CoreExpr
mkToRecordExpr inputs@MkPluginInputs{..} recordTy recordParts arrBind !ixOffset =
  let mkFieldVal (_, fieldParts) = case fieldNesting fieldParts of
        Unnested n ->
          Ghc.mkCoreApps (Ghc.Var indexArrayId)
            [ Ghc.Type recordTy
            , Ghc.Type $ Ghc.tyConNullaryTy identityTyCon
            , Ghc.Var arrBind
            , Ghc.mkUncheckedIntExpr $! n + ixOffset
            ]
        Nested offset _ innerRecTy innerRecParts ->
          mkToRecordExpr
            inputs
            innerRecTy
            innerRecParts
            arrBind
            (ixOffset + offset)

      fieldVals = mkFieldVal <$> recordFields recordParts

   in Ghc.mkCoreConApps (recordCon recordParts)
      $ (Ghc.Type <$> recordTyArgs recordParts) ++ fieldVals
