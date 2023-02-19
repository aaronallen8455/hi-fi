{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import           Data.Aeson
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           HiFi
import           HiFi.Aeson

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree = testGroup "tests"
  [ testProperty "trip json" tripJson
  , testCase "optional field" optionalField
  ]

tripJson :: Property
tripJson =
  forAll (arbitrary @Test1) $ \x -> Just x === decode (encode x)

data Test1 = Test1
  { t11 :: Bool
  , t12 :: Int
  , t13 :: [Double]
  -- , t14 :: NestHKD Test2
  } deriving (Show, Eq)
    deriving (FromJSON, ToJSON) via HkdJSON "Test1" Test1

instance Arbitrary Test1 where
  arbitrary = fromHKD <$> fillC @Arbitrary arbitrary

data Test2 = Test2
  { t21 :: Maybe Bool
  , t22 :: Maybe Int
  } deriving (Show, Eq)
    deriving (FromJSON, ToJSON) via HkdJSON "Test2" Test2

instance Arbitrary Test2 where
  arbitrary = fromHKD <$> fillC @Arbitrary arbitrary

optionalField :: Assertion
optionalField =
  let r = Test2 (Just True) Nothing
   in Just r @=? decode "{\"t21\":true}"
