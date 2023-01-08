#!/usr/bin/env cabal
{- cabal:
build-depends:
    base
-}

{-# OPTIONS_GHC -fplugin HiFi #-}

import           HiFi

data TestRec = TestRec
  { f1 :: Bool
  , f2 :: Int
  , f3 :: String
  }

hkd :: HKD TestRec Maybe
hkd = mkHKD
  { f1 = Just True
  , f2 = Just 9
  }

main :: IO ()
main = pure ()
