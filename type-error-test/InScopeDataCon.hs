
#!/usr/bin/env cabal
{- cabal:
build-depends:
    base,
-}

{-# OPTIONS_GHC -fplugin HiFi #-}

import           HiFi
import           Record

hkd :: HKD R2 Maybe
hkd = mkHKD
  { foo2 = Just False
  , bar2 = Just "test"
  }

main :: IO ()
main = pure ()
