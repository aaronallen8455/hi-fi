{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HiFi.Api
  ( mapEffect
  , hkdSequenceShallow
  , hkdSequence
  , hkdDistribute
  , hkdDistributeShallow
  , hkdTraverse
  , hkdCotraverse
  , hkdZipWith
  , hkdPure
  , toRecord
  , fromRecord
  , mkHKD
  , setField
  , getField
  , fill
  , atField
  , StringSing(..)
  , toFieldName
  ) where

import           Control.Monad.ST
import           Data.Coerce (coerce)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import qualified Data.Primitive.Array as A
import           GHC.Records
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

import           HiFi.Internal.Types (FieldGetters(..), HKD(..), IndexOfField(..), Instantiate(..), ToRecord(..))
import           HiFi.StringSing (StringSing(..), toFieldName)

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

mapEffect :: (forall a. f a -> g a) -> HKD rec f -> HKD rec g
mapEffect f (UnsafeMkHKD arr) = UnsafeMkHKD $ f <$> arr

hkdSequenceShallow :: forall f g rec. Applicative f
                   => HKD rec (Compose f g) -> f (HKD rec g)
hkdSequenceShallow (UnsafeMkHKD arr) = UnsafeMkHKD <$> traverse coerce arr

hkdSequence :: forall f rec. (Applicative f, ToRecord rec) => HKD rec f -> f rec
hkdSequence = fmap toRecord
            . hkdSequenceShallow @f @Identity
            . mapEffect (coerce . fmap Identity)

hkdDistribute :: forall rec f. (FieldGetters rec, Functor f)
              => f rec -> HKD rec f
hkdDistribute fRec =
  let getters = fieldGetters @rec
      fields = (`fmap` fRec) <$> getters
   in coerce $ A.arrayFromList fields

hkdDistributeShallow
  :: forall rec f g. (FieldGetters rec, Functor f, Functor g)
  => f (HKD rec g)
  -> HKD rec (Compose f g)
hkdDistributeShallow fHKD =
  let getters = fieldGetters @rec
   in coerce . A.arrayFromList $ do
        idx <- fst <$> zip [0..] getters
        pure . Compose $ fmap (\(UnsafeMkHKD arr) -> A.indexArray arr idx) fHKD

hkdTraverse
  :: Applicative t
  => (forall a. f a -> t (g a))
  -> HKD rec f
  -> t (HKD rec g)
hkdTraverse f = hkdSequenceShallow . mapEffect (coerce . f)

hkdCotraverse
  :: (Functor f, Functor t, FieldGetters rec)
  => (forall a. t (f a) -> g a)
  -> t (HKD rec f)
  -> HKD rec g
hkdCotraverse f = mapEffect (f . getCompose) . hkdDistributeShallow

hkdZipWith
  :: (forall a. f a -> g a -> h a)
  -> HKD rec f
  -> HKD rec g
  -> HKD rec h
hkdZipWith f (UnsafeMkHKD a) (UnsafeMkHKD b) = UnsafeMkHKD . A.arrayFromList $ do
  i <- [0 .. A.sizeofArray a - 1]
  [ f (A.indexArray a i) (A.indexArray b i) ]

hkdPure :: (Applicative f, FieldGetters rec) => rec -> HKD rec f
hkdPure = mapEffect (pure . coerce) . fromRecord

toRecord :: ToRecord rec => HKD rec Identity -> rec
toRecord (UnsafeMkHKD arr) = toRecord' $ coerce arr

-- This function must be inlined, otherwise it behaves incorrectly with >O0
{-# INLINE fromRecord #-}
fromRecord :: forall rec. FieldGetters rec => rec -> HKD rec Identity
fromRecord rec =
  let getters = fieldGetters @rec
   in UnsafeMkHKD $ A.createArray (length getters) (error "fromRecord: impossible") $ \arr -> do
    let go !ix (x:xs) = do
          A.writeArray arr ix (coerce $ x rec)
          go (ix + 1) xs
        go _ [] = pure ()
    go 0 getters

mkHKD :: forall rec f tuple. (Instantiate rec f tuple) => tuple -> HKD rec f
mkHKD = instantiate @rec @f @tuple

setField
  :: forall name rec a f
   . (HasField name rec a, IndexOfField name rec)
  => f a
  -> HKD rec f
  -> HKD rec f
setField val (UnsafeMkHKD arr) = UnsafeMkHKD $ runST $ do
  marr <- A.unsafeThawArray arr
  A.writeArray marr (indexOfField @name @rec) (unsafeCoerce val)
  A.unsafeFreezeArray marr

-- | Instantiate a HKD using the same value for every field.
fill :: forall rec f. FieldGetters rec => (forall a. f a) -> HKD rec f
fill x = UnsafeMkHKD . A.arrayFromList $ unsafeCoerce x <$ fieldGetters @rec

-- | A lens focusing a specific field in a HKD.
atField :: forall (name :: Symbol) rec effect f a
         . (HasField name rec a, IndexOfField name rec, Functor f)
        => (effect a -> f (effect a))
        -> HKD rec effect -> f (HKD rec effect)
atField afa rec =
  flip (setField @name) rec <$> afa (getField @name rec)

