{-# LANGUAGE RecordWildCards #-}
module HiFi.TcPlugin.HkdSetField
  ( mkHkdSetFieldExpr
  ) where

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs
import           HiFi.TcPlugin.RecordParts

mkHkdSetFieldExpr
  :: PluginInputs
  -> Ghc.Type
  -> Ghc.Type
  -> Ghc.Type
  -> FieldNesting
  -> Ghc.TcPluginM Ghc.CoreExpr
mkHkdSetFieldExpr MkPluginInputs{..} recTy effectTy fieldTy idx = do
  hkdName <- Ghc.unsafeTcPluginTcM
           $ Ghc.newName (Ghc.mkOccName Ghc.varName "hkd")
  elName <- Ghc.unsafeTcPluginTcM
          $ Ghc.newName (Ghc.mkOccName Ghc.varName "el")

  let hkdTy = Ghc.mkTyConApp hkdTyCon [recTy, effectTy]
      hkdBndr = Ghc.mkLocalIdOrCoVar hkdName Ghc.Many hkdTy
      setterExpr =
        case idx of
          Unnested ix ->
            let elTy = Ghc.mkAppTys effectTy [Ghc.anyTypeOfKind Ghc.liftedTypeKind]
                elBndr = Ghc.mkLocalIdOrCoVar elName Ghc.Many elTy
             in Ghc.mkCoreLams [elBndr, hkdBndr]
                $ Ghc.mkCoreApps (Ghc.Var writeArrayId)
                  [ Ghc.Type recTy
                  , Ghc.Type effectTy
                  , Ghc.Type fieldTy
                  , Ghc.Var hkdBndr
                  , Ghc.mkUncheckedIntExpr ix
                  , Ghc.Var elBndr
                  ]
          Nested offset len innerRecTy _ ->
            let innerRecBndr = Ghc.mkLocalIdOrCoVar elName Ghc.Many fieldTy
             in Ghc.mkCoreLams [innerRecBndr, hkdBndr]
                $ Ghc.mkCoreApps (Ghc.Var setInnerRecId)
                  [ Ghc.Type recTy
                  , Ghc.Type effectTy
                  , Ghc.Type innerRecTy
                  , Ghc.Var hkdBndr
                  , Ghc.mkUncheckedIntExpr offset
                  , Ghc.mkUncheckedIntExpr len
                  , Ghc.Var innerRecBndr
                  ]
  pure setterExpr
