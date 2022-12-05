{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Monoid
import           Data.String
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Test.QuickCheck.Property.Monoid

import           HiFi

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testProperty "trip record" tripRecord
  , testCase "getters" testGetters
  , testCase "setters" testSetters
  , testCase "sequence" testSequence
  , testGroup "instantiation"
    [ testCase "basic" testInstantiation
    , testCase "literals" testLiterals
    , testCase "parameterized" testParameterized
    ]
  , testCase "effect mapping" testEffectMap
  , testCase "zipping" testZipping
  , testCase "fill" testFill
  , testProperty "Monoid" (eq $ prop_Monoid (T @(HKD Test2 Identity)))
  , testProperty "Eq" testEq
  , testProperty "Ord" testOrd
  ]

tripRecord :: Property
tripRecord =
  forAll
    (arbitrary @Test1) $ \x -> x === toRecord (fromRecord x)

testGetters :: Assertion
testGetters = do
  let hkd = fromRecord testInput1
  Identity testInput1.t1a @?= hkd.t1a
  Identity testInput1.t1b @?= hkd.t1b
  Identity testInput1.t1c @?= hkd.t1c
  Identity testInput1.t1d @?= hkd.t1d
  Identity testInput1.t1e @?= hkd.t1e

testSetters :: Assertion
testSetters = do
  let hkd = setField @"t1a" (Identity False)
          . setField @"t1b" (Identity 2)
          . setField @"t1c" (Identity 5.1)
          . setField @"t1d" (Identity (-9.1))
          . setField @"t1e" (Identity (UnicodeString "dsa"))
          $ fromRecord testInput1
  Test1 False 2 5.1 (-9.1) (UnicodeString "dsa")
    @?= toRecord hkd

testSequence :: Assertion
testSequence = do
  let a = [True, False]
      b = [3, -5]
      c = [9.2, 1.1]
      d = [10, -23.5]
      e = UnicodeString <$> ["test", "abc"]
      hkd = mkHKD { t1a = a
                  , t1b = b
                  , t1c = c
                  , t1d = d
                  , t1e = e
                  }
      app = Test1 <$> a <*> b <*> c <*> d <*> e
  app @?= recSequence hkd

testInstantiation :: Assertion
testInstantiation = do
  let hkd = mkHKD { t1a = pure testInput1.t1a
                  , t1c = pure testInput1.t1c
                  , t1e = pure testInput1.t1e
                  , t1b = pure testInput1.t1b
                  , t1d = pure testInput1.t1d
                  }
  Just testInput1 @?= recSequence hkd

testLiterals :: Assertion
testLiterals = do
  let hkd = mkHKD { t1a = pure True
                  , t1b = pure 5
                  , t1c = pure 5
                  , t1d = pure 5
                  , t1e = pure "hello"
                  }
      rec = Test1 { t1a = True
                  , t1b = 5
                  , t1c = 5
                  , t1d = 5
                  , t1e = UnicodeString "hello"
                  }
  rec @?= toRecord hkd

testParameterized :: Assertion
testParameterized = do
  let hkd = mkHKD { t3a = [True]
                  , t3b = [[Just ()]]
                  }
      r = Test3 { t3a = True
                , t3b = [Just ()]
                }
  [r] @?= recSequence hkd

testEffectMap :: Assertion
testEffectMap = do
  let hkd = mapEffect (Just . runIdentity) $ fromRecord testInput1
  Just testInput1 @?= recSequence hkd

testZipping :: Assertion
testZipping = do
  let upd = mkHKD { t1a = Just False
                  , t1b = Nothing
                  , t1c = Nothing
                  , t1d = Just 9.99
                  , t1e = Just $ UnicodeString "cde"
                  }
      new = recZipWith (`maybe` Identity) (fromRecord testInput1) upd
      expected = testInput1 { t1a = False, t1d = 9.99, t1e = "cde" }
  expected @?= toRecord new

testFill :: Assertion
testFill = do
  let hkd = fill @Test1 []
  [] @?= recSequence hkd

testEq :: Property
testEq =
  forAll (arbitrary @Test1) $ \x ->
    forAll arbitrary $ \y ->
      (x == y) === (fromRecord x == fromRecord y)

testOrd :: Property
testOrd =
  forAll (arbitrary @Test1) $ \x ->
    forAll arbitrary $ \y ->
      compare x y === compare (fromRecord x) (fromRecord y)

data Test1 = Test1
  { t1a :: Bool
  , t1b :: Int
  , t1c :: Double
  , t1d :: Float
  , t1e :: UnicodeString
  } deriving (Eq, Show, Ord)

instance Arbitrary Test1 where
  arbitrary =
    Test1 <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary

testInput1 :: Test1
testInput1 =
  Test1 True 1 3.4 4.5 (UnicodeString "abc")

data Test2 = Test2
  { t2a :: Maybe (Product Int)
  , t2b :: Any
  , t2c :: Sum Int
  } deriving (Eq, Show)

instance Arbitrary (HKD Test2 Identity) where
  arbitrary = recSequenceShallow mkHKD
    { t2a = Compose (Identity <$> arbitrary)
    , t2b = Compose (Identity <$> arbitrary)
    , t2c = Compose (Identity <$> arbitrary)
    }

data Test3 a b = Test3
  { t3a :: a
  , t3b :: [b]
  } deriving (Eq, Show)

instance IsString UnicodeString where
  fromString = UnicodeString
