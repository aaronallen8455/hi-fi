{-# LANGUAGE AllowAmbiguousTypes #-}
module HiFi.Api
  ( mapEffect
  , recSequenceShallow
  , recSequence
  , recZipWith
  , toRecord
  , fromRecord
  , mkHKD
  , setField
  , fill
  , atField
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

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

mapEffect :: (forall a. f a -> g a) -> HKD rec f -> HKD rec g
mapEffect f (UnsafeMkHKD arr) = UnsafeMkHKD $ f <$> arr

recSequenceShallow :: forall f g rec. Applicative f
                   => HKD rec (Compose f g) -> f (HKD rec g)
recSequenceShallow (UnsafeMkHKD arr) = UnsafeMkHKD <$> traverse getCompose arr

recSequence :: forall f rec. (Applicative f, ToRecord rec) => HKD rec f -> f rec
recSequence = fmap toRecord
            . recSequenceShallow @f @Identity
            . mapEffect (Compose . fmap Identity)

recZipWith
  :: (forall a. f a -> g a -> h a)
  -> HKD rec f
  -> HKD rec g
  -> HKD rec h
recZipWith f (UnsafeMkHKD a) (UnsafeMkHKD b) = UnsafeMkHKD . A.arrayFromList $ do
  i <- [0 .. A.sizeofArray a]
  [ f (A.indexArray a i) (A.indexArray b i) ]

toRecord :: ToRecord rec => HKD rec Identity -> rec
toRecord (UnsafeMkHKD arr) = toRecord' $ coerce arr

fromRecord :: forall rec. FieldGetters rec => rec -> HKD rec Identity
fromRecord rec =
  UnsafeMkHKD
    . A.arrayFromList
    . fmap (Identity . ($ rec))
    $ fieldGetters @rec

-- NB: need to be cognizant of the max tuple size restriction. mAX_TUPLE_SIZE
-- Should sort by the field name
-- Then have a mapping to "unsort" the fields to the correct order for the record con
-- Still need the source code field names in the tuple to check that they match
-- the actual field names.
-- Should probably make the field names a Proxy with type level string to make
-- it easier to work with since value level strings will probably be difficult
-- to work with.
mkHKD :: forall rec f tuple. Instantiate rec f tuple => tuple -> HKD rec f
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

-- TODO should this be parameterized over a constraint?
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

