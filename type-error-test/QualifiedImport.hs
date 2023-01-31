#!/usr/bin/env cabal
{- cabal:
build-depends:
    base,
-}

{-# OPTIONS_GHC -fplugin HiFi #-}

import qualified HiFi

data TestRec = TestRec
  { f1 :: Bool
  , f2 :: Int
  , f3 :: String
  }

hkd :: HiFi.HKD TestRec Maybe
hkd = HiFi.mkHKD
  { f1 = Just True
  , f2 = Just 9
  , f3 = Nothing
  }

main :: IO ()
main = pure ()
