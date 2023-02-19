module HiFi.TcPlugin.Utils
  ( makeWantedCt
  ) where

import qualified HiFi.GhcFacade as Ghc

makeWantedCt
  :: Ghc.CtLoc
  -> Ghc.Class
  -> [Ghc.Type]
  -> Ghc.TcPluginM (Ghc.Ct, Ghc.TcEvDest)
makeWantedCt ctLoc clss classArgs = do
  let classPred = Ghc.mkClassPred clss classArgs
  evidence <- Ghc.newWanted ctLoc classPred
  pure (Ghc.mkNonCanonical evidence, Ghc.ctev_dest evidence)
