{-# LANGUAGE RecordWildCards #-}
module HiFi.TcPlugin.HkdHasField
  ( mkHkdHasFieldExpr
  ) where

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs
import           HiFi.TcPlugin.RecordParts

mkHkdHasFieldExpr
  :: PluginInputs
  -> Ghc.Type
  -> Ghc.Type
  -> FieldNesting
  -> Ghc.TcPluginM Ghc.CoreExpr
mkHkdHasFieldExpr MkPluginInputs{..} recTy effectTy idx = do
  hkdName <- Ghc.unsafeTcPluginTcM
           $ Ghc.newName (Ghc.mkOccName Ghc.varName "hkd")
  let hkdTy = Ghc.mkTyConApp hkdTyCon [recTy, effectTy]
      hkdBndr = Ghc.mkLocalIdOrCoVar hkdName Ghc.ManyTy' hkdTy
      getterExpr =
        Ghc.mkCoreLams [hkdBndr] $
          case idx of
            Unnested ix ->
              Ghc.mkCoreApps (Ghc.Var indexArrayId)
                             [ Ghc.Type recTy
                             , Ghc.Type effectTy
                             , Ghc.Var hkdBndr
                             , Ghc.mkUncheckedIntExpr ix
                             ]
            Nested offset len innerRecTy _ ->
              Ghc.mkCoreApps (Ghc.Var getInnerRecId)
                             [ Ghc.Type recTy
                             , Ghc.Type effectTy
                             , Ghc.Type innerRecTy
                             , Ghc.Var hkdBndr
                             , Ghc.mkUncheckedIntExpr offset
                             , Ghc.mkUncheckedIntExpr len
                             ]
  pure getterExpr
