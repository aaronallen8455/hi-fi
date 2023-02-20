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
  , testCase "optional nested field" optionalNestedField
  ]

tripJson :: Property
tripJson =
  forAll (arbitrary @Test1) $ \x -> Just x === decode (encode x)

data Test1 = Test1
  { t11 :: Bool
  , t12 :: Int
  , t13 :: [Double]
  , t14 :: Test2
  , t15 :: NestHKD Test2
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

instance Arbitrary (HKD Test2 Identity) where
  arbitrary = toHKD <$> arbitrary @Test2

optionalField :: Assertion
optionalField =
  let r = Test2 (Just True) Nothing
   in Just r @=? decode "{\"t21\":true}"

optionalNestedField :: Assertion
optionalNestedField =
  let r = Test1 True 1 [2] (Test2 Nothing Nothing) (NestHKD (Test2 Nothing (Just 1)))
   in Just r @=? decode
        "{\"t11\":true,\"t12\":1,\"t13\":[2],\"t14\":{},\"t15\":{\"t22\":1}}"
