#!/usr/bin/env cabal
{- cabal:
build-depends:
    base,
-}

{-# OPTIONS_GHC -fplugin HiFi #-}

import           HiFi
import           Record

hkd :: HKD R Maybe
hkd = mkHKD
  { foo = Just False
  , bar = Just "test"
  }

main :: IO ()
main = pure ()
