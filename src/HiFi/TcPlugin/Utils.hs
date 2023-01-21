{-# LANGUAGE CPP #-}
module HiFi.TcPlugin.Utils
  ( makeWantedCt
  ) where

import qualified HiFi.GhcFacade as Ghc

makeWantedCt :: Ghc.CtLoc -> Ghc.Class -> [Ghc.Type] -> Ghc.TcPluginM Ghc.Ct
makeWantedCt ctLoc clss classArgs = do
  let classPred = Ghc.mkClassPred clss classArgs
  evidence <- Ghc.newWanted ctLoc classPred
  pure Ghc.CDictCan
    { Ghc.cc_ev = evidence
    , Ghc.cc_class = clss
    , Ghc.cc_tyargs = classArgs
    , Ghc.cc_pend_sc = False
#if MIN_VERSION_ghc(9,4,0)
    , Ghc.cc_fundeps = False
#endif
    }

