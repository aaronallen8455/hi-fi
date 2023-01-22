#!/usr/bin/env cabal
{- cabal:
build-depends:
    base
-}

{-# OPTIONS_GHC -fplugin HiFi #-}

import           HiFi

data TestRec = TestRec
  { f1 :: Bool
  , f2 :: NestHKD Inner
  }

data Inner = Inner
  { i1 :: NestHKD TestRec }

hkd :: HKD TestRec Maybe
hkd = mkHKD
  { f1 = Just True
  , f2 = mkHKD { i1 = hkd }
  }

main :: IO ()
main = pure ()
