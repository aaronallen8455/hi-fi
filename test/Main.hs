{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main, Test4(..)) where

import           Data.Foldable
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
    , testCase "type applications" testTypeApplications
    , testCase "large record" testLargeRecord
    ]
  , testCase "effect mapping" testEffectMap
  , testCase "zipping" testZipping
  , testCase "fill" testFill
  , testProperty "Monoid" (eq $ prop_Monoid (T @(HKD Test2 Identity)))
  , testProperty "Eq" testEq
  , testProperty "Ord" testOrd
  , testProperty "Show / Read" testShowRead
  , testProperty "Show / Read single field" testShowReadSingle
  , testCase "distribute" testDistribute
  , testCase "hkd distribute" testHkdDistribute
  ]

tripRecord :: Property
tripRecord =
  forAll
    (arbitrary @Test1) $ \x -> x === toRecord (fromRecord x)

testGetters :: Assertion
testGetters = do
  let hkd = fromRecord testInput1
  Identity testInput1.t1a @=? hkd.t1a
  Identity testInput1.t1b @=? hkd.t1b
  Identity testInput1.t1c @=? hkd.t1c
  Identity testInput1.t1d @=? hkd.t1d
  Identity testInput1.t1e @=? hkd.t1e

testSetters :: Assertion
testSetters = do
  let hkd = setField @"t1a" (Identity False)
          . setField @"t1b" (Identity 2)
          . setField @"t1c" (Identity 5.1)
          . setField @"t1d" (Identity (-9.1))
          . setField @"t1e" (Identity (UnicodeString "dsa"))
          $ fromRecord testInput1
  Test1 False 2 5.1 (-9.1) (UnicodeString "dsa")
    @=? toRecord hkd

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
  app @=? hkdSequence hkd

testInstantiation :: Assertion
testInstantiation = do
  let hkd = mkHKD { t1a = pure testInput1.t1a
                  , t1c = pure testInput1.t1c
                  , t1e = pure testInput1.t1e
                  , t1b = pure testInput1.t1b
                  , t1d = pure testInput1.t1d
                  }
  Just testInput1 @=? hkdSequence hkd

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
  rec @=? toRecord hkd

testParameterized :: Assertion
testParameterized = do
  let hkd = mkHKD { t3a = [True]
                  , t3b = [[Just ()]]
                  }
      r = Test3 { t3a = True
                , t3b = [Just ()]
                }
  [r] @=? hkdSequence hkd

testTypeApplications :: Assertion
testTypeApplications = do
  let hkd1 = (mkHKD @(Test3 Bool (Maybe ())))
              { t3a = [True]
              , t3b = [[Just ()]]
              }
      hkd2 = (mkHKD @(Test3 Bool (Maybe ())) @[])
              { t3a = [True]
              , t3b = [[Just ()]]
              }
  hkd1 @=? hkd2

testLargeRecord :: Assertion
testLargeRecord = do
  let hkd :: HKD Test4 Maybe
      hkd = mkHKD
        { t41 = Just 41 , t42 = Just 42 , t43 = Just 43 , t44 = Just 44
        , t45 = Just 45 , t46 = Just 46 , t47 = Just 47 , t48 = Just 48
        , t49 = Just 49 , t410 = Just 410 , t411 = Just 411 , t412 = Just 412
        , t413 = Just 413 , t414 = Just 414 , t415 = Just 415 , t416 = Just 416
        , t417 = Just 417 , t418 = Just 418 , t419 = Just 419 , t420 = Just 420
        , t421 = Just 421 , t422 = Just 422 , t423 = Just 423 , t424 = Just 424
        , t425 = Just 425 , t426 = Just 426 , t427 = Just 427 , t428 = Just 428
        , t429 = Just 429 , t430 = Just 430 , t431 = Just 431 , t432 = Just 432
        , t433 = Just 433 , t434 = Just 434 , t435 = Just 435 , t436 = Just 436
        , t437 = Just 437 , t438 = Just 438 , t439 = Just 439 , t440 = Just 440
        , t441 = Just 441 , t442 = Just 442 , t443 = Just 443 , t444 = Just 444
        , t445 = Just 445 , t446 = Just 446 , t447 = Just 447 , t448 = Just 448
        , t449 = Just 449 , t450 = Just 450 , t451 = Just 451 , t452 = Just 452
        , t453 = Just 453 , t454 = Just 454 , t455 = Just 455 , t456 = Just 456
        , t457 = Just 457 , t458 = Just 458 , t459 = Just 459 , t460 = Just 460
        , t461 = Just 461 , t462 = Just 462 , t463 = Just 463 , t464 = Just 464
        , t465 = Just 465 , t466 = Just 466 , t467 = Just 467 , t468 = Just 468
        , t469 = Just 469 , t470 = Just 470 , t471 = Just 471 , t472 = Just 472
        , t473 = Just 473 , t474 = Just 474 , t475 = Just 475 , t476 = Just 476
        , t477 = Just 477 , t478 = Just 478 , t479 = Just 479 , t480 = Just 480
        , t481 = Just 481 , t482 = Just 482 , t483 = Just 483 , t484 = Just 484
        , t485 = Just 485 , t486 = Just 486 , t487 = Just 487 , t488 = Just 488
        , t489 = Just 489 , t490 = Just 490 , t491 = Just 491 , t492 = Just 492
        , t493 = Just 493 , t494 = Just 494 , t495 = Just 495 , t496 = Just 496
        , t497 = Just 497 , t498 = Just 498 , t499 = Just 499 , t4100 = Just 4100
        , t4101 = Just 4101 , t4102 = Just 4102 , t4103 = Just 4103 , t4104 = Just 4104
        , t4105 = Just 4105 , t4106 = Just 4106 , t4107 = Just 4107 , t4108 = Just 4108
        , t4109 = Just 4109 , t4110 = Just 4110 , t4111 = Just 4111 , t4112 = Just 4112
        , t4113 = Just 4113 , t4114 = Just 4114 , t4115 = Just 4115 , t4116 = Just 4116
        , t4117 = Just 4117 , t4118 = Just 4118 , t4119 = Just 4119 , t4120 = Just 4120
        , t4121 = Just 4121 , t4122 = Just 4122 , t4123 = Just 4123 , t4124 = Just 4124
        , t4125 = Just 4125 , t4126 = Just 4126 , t4127 = Just 4127 , t4128 = Just 4128
        , t4129 = Just 4129 , t4130 = Just 4130 , t4131 = Just 4131 , t4132 = Just 4132
        , t4133 = Just 4133 , t4134 = Just 4134 , t4135 = Just 4135 , t4136 = Just 4136
        , t4137 = Just 4137 , t4138 = Just 4138 , t4139 = Just 4139 , t4140 = Just 4140
        , t4141 = Just 4141 , t4142 = Just 4142 , t4143 = Just 4143 , t4144 = Just 4144
        , t4145 = Just 4145 , t4146 = Just 4146 , t4147 = Just 4147 , t4148 = Just 4148
        , t4149 = Just 4149 , t4150 = Just 4150 , t4151 = Just 4151 , t4152 = Just 4152
        , t4153 = Just 4153 , t4154 = Just 4154 , t4155 = Just 4155 , t4156 = Just 4156
        , t4157 = Just 4157 , t4158 = Just 4158 , t4159 = Just 4159 , t4160 = Just 4160
        , t4161 = Just 4161 , t4162 = Just 4162 , t4163 = Just 4163 , t4164 = Just 4164
        , t4165 = Just 4165 , t4166 = Just 4166 , t4167 = Just 4167 , t4168 = Just 4168
        , t4169 = Just 4169 , t4170 = Just 4170 , t4171 = Just 4171 , t4172 = Just 4172
        , t4173 = Just 4173 , t4174 = Just 4174 , t4175 = Just 4175 , t4176 = Just 4176
        , t4177 = Just 4177 , t4178 = Just 4178 , t4179 = Just 4179 , t4180 = Just 4180
        , t4181 = Just 4181 , t4182 = Just 4182 , t4183 = Just 4183 , t4184 = Just 4184
        , t4185 = Just 4185 , t4186 = Just 4186 , t4187 = Just 4187 , t4188 = Just 4188
        , t4189 = Just 4189 , t4190 = Just 4190 , t4191 = Just 4191 , t4192 = Just 4192
        , t4193 = Just 4193 , t4194 = Just 4194 , t4195 = Just 4195 , t4196 = Just 4196
        , t4197 = Just 4197 , t4198 = Just 4198 , t4199 = Just 4199 , t4200 = Just 4200
        }
  let bools =
        [ hkd.t41 == Just 41
        , hkd.t42 == Just 42
        , hkd.t43 == Just 43
        , hkd.t44 == Just 44
        , hkd.t45 == Just 45
        , hkd.t46 == Just 46
        , hkd.t47 == Just 47
        , hkd.t48 == Just 48
        , hkd.t49 == Just 49
        , hkd.t410 == Just 410
        , hkd.t411 == Just 411
        , hkd.t412 == Just 412
        , hkd.t413 == Just 413
        , hkd.t414 == Just 414
        , hkd.t415 == Just 415
        , hkd.t416 == Just 416
        , hkd.t417 == Just 417
        , hkd.t418 == Just 418
        , hkd.t419 == Just 419
        , hkd.t420 == Just 420
        , hkd.t421 == Just 421
        , hkd.t422 == Just 422
        , hkd.t423 == Just 423
        , hkd.t424 == Just 424
        , hkd.t425 == Just 425
        , hkd.t426 == Just 426
        , hkd.t427 == Just 427
        , hkd.t428 == Just 428
        , hkd.t429 == Just 429
        , hkd.t430 == Just 430
        , hkd.t431 == Just 431
        , hkd.t432 == Just 432
        , hkd.t433 == Just 433
        , hkd.t434 == Just 434
        , hkd.t435 == Just 435
        , hkd.t436 == Just 436
        , hkd.t437 == Just 437
        , hkd.t438 == Just 438
        , hkd.t439 == Just 439
        , hkd.t440 == Just 440
        , hkd.t441 == Just 441
        , hkd.t442 == Just 442
        , hkd.t443 == Just 443
        , hkd.t444 == Just 444
        , hkd.t445 == Just 445
        , hkd.t446 == Just 446
        , hkd.t447 == Just 447
        , hkd.t448 == Just 448
        , hkd.t449 == Just 449
        , hkd.t450 == Just 450
        , hkd.t451 == Just 451
        , hkd.t452 == Just 452
        , hkd.t453 == Just 453
        , hkd.t454 == Just 454
        , hkd.t455 == Just 455
        , hkd.t456 == Just 456
        , hkd.t457 == Just 457
        , hkd.t458 == Just 458
        , hkd.t459 == Just 459
        , hkd.t460 == Just 460
        , hkd.t461 == Just 461
        , hkd.t462 == Just 462
        , hkd.t463 == Just 463
        , hkd.t464 == Just 464
        , hkd.t465 == Just 465
        , hkd.t466 == Just 466
        , hkd.t467 == Just 467
        , hkd.t468 == Just 468
        , hkd.t469 == Just 469
        , hkd.t470 == Just 470
        , hkd.t471 == Just 471
        , hkd.t472 == Just 472
        , hkd.t473 == Just 473
        , hkd.t474 == Just 474
        , hkd.t475 == Just 475
        , hkd.t476 == Just 476
        , hkd.t477 == Just 477
        , hkd.t478 == Just 478
        , hkd.t479 == Just 479
        , hkd.t480 == Just 480
        , hkd.t481 == Just 481
        , hkd.t482 == Just 482
        , hkd.t483 == Just 483
        , hkd.t484 == Just 484
        , hkd.t485 == Just 485
        , hkd.t486 == Just 486
        , hkd.t487 == Just 487
        , hkd.t488 == Just 488
        , hkd.t489 == Just 489
        , hkd.t490 == Just 490
        , hkd.t491 == Just 491
        , hkd.t492 == Just 492
        , hkd.t493 == Just 493
        , hkd.t494 == Just 494
        , hkd.t495 == Just 495
        , hkd.t496 == Just 496
        , hkd.t497 == Just 497
        , hkd.t498 == Just 498
        , hkd.t499 == Just 499
        , hkd.t4100 == Just 4100
        , hkd.t4101 == Just 4101
        , hkd.t4102 == Just 4102
        , hkd.t4103 == Just 4103
        , hkd.t4104 == Just 4104
        , hkd.t4105 == Just 4105
        , hkd.t4106 == Just 4106
        , hkd.t4107 == Just 4107
        , hkd.t4108 == Just 4108
        , hkd.t4109 == Just 4109
        , hkd.t4110 == Just 4110
        , hkd.t4111 == Just 4111
        , hkd.t4112 == Just 4112
        , hkd.t4113 == Just 4113
        , hkd.t4114 == Just 4114
        , hkd.t4115 == Just 4115
        , hkd.t4116 == Just 4116
        , hkd.t4117 == Just 4117
        , hkd.t4118 == Just 4118
        , hkd.t4119 == Just 4119
        , hkd.t4120 == Just 4120
        , hkd.t4121 == Just 4121
        , hkd.t4122 == Just 4122
        , hkd.t4123 == Just 4123
        , hkd.t4124 == Just 4124
        , hkd.t4125 == Just 4125
        , hkd.t4126 == Just 4126
        , hkd.t4127 == Just 4127
        , hkd.t4128 == Just 4128
        , hkd.t4129 == Just 4129
        , hkd.t4130 == Just 4130
        , hkd.t4131 == Just 4131
        , hkd.t4132 == Just 4132
        , hkd.t4133 == Just 4133
        , hkd.t4134 == Just 4134
        , hkd.t4135 == Just 4135
        , hkd.t4136 == Just 4136
        , hkd.t4137 == Just 4137
        , hkd.t4138 == Just 4138
        , hkd.t4139 == Just 4139
        , hkd.t4140 == Just 4140
        , hkd.t4141 == Just 4141
        , hkd.t4142 == Just 4142
        , hkd.t4143 == Just 4143
        , hkd.t4144 == Just 4144
        , hkd.t4145 == Just 4145
        , hkd.t4146 == Just 4146
        , hkd.t4147 == Just 4147
        , hkd.t4148 == Just 4148
        , hkd.t4149 == Just 4149
        , hkd.t4150 == Just 4150
        , hkd.t4151 == Just 4151
        , hkd.t4152 == Just 4152
        , hkd.t4153 == Just 4153
        , hkd.t4154 == Just 4154
        , hkd.t4155 == Just 4155
        , hkd.t4156 == Just 4156
        , hkd.t4157 == Just 4157
        , hkd.t4158 == Just 4158
        , hkd.t4159 == Just 4159
        , hkd.t4160 == Just 4160
        , hkd.t4161 == Just 4161
        , hkd.t4162 == Just 4162
        , hkd.t4163 == Just 4163
        , hkd.t4164 == Just 4164
        , hkd.t4165 == Just 4165
        , hkd.t4166 == Just 4166
        , hkd.t4167 == Just 4167
        , hkd.t4168 == Just 4168
        , hkd.t4169 == Just 4169
        , hkd.t4170 == Just 4170
        , hkd.t4171 == Just 4171
        , hkd.t4172 == Just 4172
        , hkd.t4173 == Just 4173
        , hkd.t4174 == Just 4174
        , hkd.t4175 == Just 4175
        , hkd.t4176 == Just 4176
        , hkd.t4177 == Just 4177
        , hkd.t4178 == Just 4178
        , hkd.t4179 == Just 4179
        , hkd.t4180 == Just 4180
        , hkd.t4181 == Just 4181
        , hkd.t4182 == Just 4182
        , hkd.t4183 == Just 4183
        , hkd.t4184 == Just 4184
        , hkd.t4185 == Just 4185
        , hkd.t4186 == Just 4186
        , hkd.t4187 == Just 4187
        , hkd.t4188 == Just 4188
        , hkd.t4189 == Just 4189
        , hkd.t4190 == Just 4190
        , hkd.t4191 == Just 4191
        , hkd.t4192 == Just 4192
        , hkd.t4193 == Just 4193
        , hkd.t4194 == Just 4194
        , hkd.t4195 == Just 4195
        , hkd.t4196 == Just 4196
        , hkd.t4197 == Just 4197
        , hkd.t4198 == Just 4198
        , hkd.t4199 == Just 4199
        , hkd.t4200 == Just 4200
        ]
  traverse_ (assertBool "") bools

testEffectMap :: Assertion
testEffectMap = do
  let hkd = hkdMap (Just . runIdentity) $ fromRecord testInput1
  Just testInput1 @=? hkdSequence hkd

testZipping :: Assertion
testZipping = do
  let upd :: HKD Test1 Maybe
      upd = mkHKD { t1a = Just False
                  , t1b = Nothing
                  , t1c = Nothing
                  , t1d = Just 9.99
                  , t1e = Just $ UnicodeString "cde"
                  }
      new = hkdZipWith (`maybe` Identity) (fromRecord testInput1) upd
      expected = testInput1 { t1a = False, t1d = 9.99, t1e = "cde" }
  show expected @=? show (toRecord new)

testFill :: Assertion
testFill = do
  let hkd = fill @Test1 []
  [] @=? hkdSequence hkd

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

testShowRead :: Property
testShowRead =
  forAll (arbitrary @Test1) $ \x ->
    x === toRecord (read (show $ fromRecord x))

testShowReadSingle :: Property
testShowReadSingle =
  forAll (arbitrary @Test5) $ \x ->
    x === toRecord (read (show $ fromRecord x))

testDistribute :: Assertion
testDistribute = do
  let recs = [ testInput1
             , Test1 { t1a = False
                     , t1b = 99
                     , t1c = 1.1
                     , t1d = 9.9
                     , t1e = UnicodeString "..."
                     }
             ]
      hkd = mkHKD { t1a = [True, False]
                  , t1b = [1, 99]
                  , t1c = [3.4, 1.1]
                  , t1d = [4.5, 9.9]
                  , t1e = [UnicodeString "abc", UnicodeString "..."]
                  }
  hkd @=? hkdDistribute recs

testHkdDistribute :: Assertion
testHkdDistribute = do
  let hkds = [ mkHKD { t1a = Just False
                     , t1b = Just 99
                     , t1c = Just 1.1
                     , t1d = Just 9.9
                     , t1e = Nothing
                     }
             , mkHKD { t1a = Just True
                     , t1b = Just 20
                     , t1c = Nothing
                     , t1d = Nothing
                     , t1e = Nothing
                     }
             ]
      hkd = (mkHKD @Test1)
              { t1a = Compose [Just False, Just True]
              , t1b = Compose [Just 99, Just 20]
              , t1c = Compose [Just 1.1, Nothing]
              , t1d = Compose [Just 9.9, Nothing]
              , t1e = Compose [Nothing, Nothing]
              }
  hkd @=? hkdDistributeShallow hkds

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
  arbitrary = fromRecord <$> hkdSequence mkHKD
    { t2a = arbitrary
    , t2b = arbitrary
    , t2c = arbitrary
    }

data Test3 a b = Test3
  { t3a :: a
  , t3b :: [b]
  } deriving (Eq, Show)

data Test4 = Test4
  { t41 :: Int
  , t42 :: Int
  , t43 :: Int
  , t44 :: Int
  , t45 :: Int
  , t46 :: Int
  , t47 :: Int
  , t48 :: Int
  , t49 :: Int
  , t410 :: Int
  , t411 :: Int
  , t412 :: Int
  , t413 :: Int
  , t414 :: Int
  , t415 :: Int
  , t416 :: Int
  , t417 :: Int
  , t418 :: Int
  , t419 :: Int
  , t420 :: Int
  , t421 :: Int
  , t422 :: Int
  , t423 :: Int
  , t424 :: Int
  , t425 :: Int
  , t426 :: Int
  , t427 :: Int
  , t428 :: Int
  , t429 :: Int
  , t430 :: Int
  , t431 :: Int
  , t432 :: Int
  , t433 :: Int
  , t434 :: Int
  , t435 :: Int
  , t436 :: Int
  , t437 :: Int
  , t438 :: Int
  , t439 :: Int
  , t440 :: Int
  , t441 :: Int
  , t442 :: Int
  , t443 :: Int
  , t444 :: Int
  , t445 :: Int
  , t446 :: Int
  , t447 :: Int
  , t448 :: Int
  , t449 :: Int
  , t450 :: Int
  , t451 :: Int
  , t452 :: Int
  , t453 :: Int
  , t454 :: Int
  , t455 :: Int
  , t456 :: Int
  , t457 :: Int
  , t458 :: Int
  , t459 :: Int
  , t460 :: Int
  , t461 :: Int
  , t462 :: Int
  , t463 :: Int
  , t464 :: Int
  , t465 :: Int
  , t466 :: Int
  , t467 :: Int
  , t468 :: Int
  , t469 :: Int
  , t470 :: Int
  , t471 :: Int
  , t472 :: Int
  , t473 :: Int
  , t474 :: Int
  , t475 :: Int
  , t476 :: Int
  , t477 :: Int
  , t478 :: Int
  , t479 :: Int
  , t480 :: Int
  , t481 :: Int
  , t482 :: Int
  , t483 :: Int
  , t484 :: Int
  , t485 :: Int
  , t486 :: Int
  , t487 :: Int
  , t488 :: Int
  , t489 :: Int
  , t490 :: Int
  , t491 :: Int
  , t492 :: Int
  , t493 :: Int
  , t494 :: Int
  , t495 :: Int
  , t496 :: Int
  , t497 :: Int
  , t498 :: Int
  , t499 :: Int
  , t4100 :: Int
  , t4101 :: Int
  , t4102 :: Int
  , t4103 :: Int
  , t4104 :: Int
  , t4105 :: Int
  , t4106 :: Int
  , t4107 :: Int
  , t4108 :: Int
  , t4109 :: Int
  , t4110 :: Int
  , t4111 :: Int
  , t4112 :: Int
  , t4113 :: Int
  , t4114 :: Int
  , t4115 :: Int
  , t4116 :: Int
  , t4117 :: Int
  , t4118 :: Int
  , t4119 :: Int
  , t4120 :: Int
  , t4121 :: Int
  , t4122 :: Int
  , t4123 :: Int
  , t4124 :: Int
  , t4125 :: Int
  , t4126 :: Int
  , t4127 :: Int
  , t4128 :: Int
  , t4129 :: Int
  , t4130 :: Int
  , t4131 :: Int
  , t4132 :: Int
  , t4133 :: Int
  , t4134 :: Int
  , t4135 :: Int
  , t4136 :: Int
  , t4137 :: Int
  , t4138 :: Int
  , t4139 :: Int
  , t4140 :: Int
  , t4141 :: Int
  , t4142 :: Int
  , t4143 :: Int
  , t4144 :: Int
  , t4145 :: Int
  , t4146 :: Int
  , t4147 :: Int
  , t4148 :: Int
  , t4149 :: Int
  , t4150 :: Int
  , t4151 :: Int
  , t4152 :: Int
  , t4153 :: Int
  , t4154 :: Int
  , t4155 :: Int
  , t4156 :: Int
  , t4157 :: Int
  , t4158 :: Int
  , t4159 :: Int
  , t4160 :: Int
  , t4161 :: Int
  , t4162 :: Int
  , t4163 :: Int
  , t4164 :: Int
  , t4165 :: Int
  , t4166 :: Int
  , t4167 :: Int
  , t4168 :: Int
  , t4169 :: Int
  , t4170 :: Int
  , t4171 :: Int
  , t4172 :: Int
  , t4173 :: Int
  , t4174 :: Int
  , t4175 :: Int
  , t4176 :: Int
  , t4177 :: Int
  , t4178 :: Int
  , t4179 :: Int
  , t4180 :: Int
  , t4181 :: Int
  , t4182 :: Int
  , t4183 :: Int
  , t4184 :: Int
  , t4185 :: Int
  , t4186 :: Int
  , t4187 :: Int
  , t4188 :: Int
  , t4189 :: Int
  , t4190 :: Int
  , t4191 :: Int
  , t4192 :: Int
  , t4193 :: Int
  , t4194 :: Int
  , t4195 :: Int
  , t4196 :: Int
  , t4197 :: Int
  , t4198 :: Int
  , t4199 :: Int
  , t4200 :: Int
  }

data Test5 = Test5 { t51 :: Int } deriving (Show, Eq)

instance Arbitrary Test5 where
  arbitrary = hkdSequence mkHKD { t51 = arbitrary }

instance IsString UnicodeString where
  fromString = UnicodeString
