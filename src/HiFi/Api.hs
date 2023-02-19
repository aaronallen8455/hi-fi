{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HiFi.Api
  ( hkdMap
  , hkdSequenceShallow
  , hkdSequence
  , hkdDistribute
  , hkdDistributeShallow
  , hkdTraverse
  , hkdCotraverse
  , hkdZipWith
  , hkdZipWith3
  , hkdPure
  , fromHKD
  , toHKD
  , mkHKD
  , setField
  , getField
  , fill
  , fillC
  , withInstances
  , atField
  , hkdApplyUpdate
  , StringSing(..)
  , toFieldName
  , NestHKD(..)
  , ToHkdFields(..)
  , FoldFields(..)
  , FieldTy
  , ToRecord
  , FieldGetters
  , WithHkdFields
  -- * Re-exports
  , Identity(..)
  ) where

import           Control.Applicative (liftA2)
import           Data.Coerce (coerce)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Product (Product(..))
import qualified Data.Primitive.Array as A
import qualified GHC.Exts as Exts
import           GHC.Records
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

import           HiFi.Internal.Types (FieldGetters(..), FieldTy, FoldFields(..), HKD(..), HkdHasField, HkdSetField, HkdSetField(..), Instantiate(..), NestHKD(..), ToHkdFields(..), ToRecord(..), WithHkdFields)
import           HiFi.StringSing (StringSing(..), toFieldName)

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

hkdMap :: (forall a. f a -> g a) -> HKD rec f -> HKD rec g
hkdMap f (UnsafeMkHKD arr) = UnsafeMkHKD $ f <$> arr

hkdSequenceShallow :: forall f g rec. Applicative f
                   => HKD rec (Compose f g) -> f (HKD rec g)
hkdSequenceShallow (UnsafeMkHKD arr) = UnsafeMkHKD <$> traverse coerce arr

hkdSequence :: forall f rec. (Applicative f, ToRecord rec) => HKD rec f -> f rec
hkdSequence = fmap fromHKD
            . hkdSequenceShallow @f @Identity
            . hkdMap (coerce . fmap Identity)

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
hkdTraverse f = hkdSequenceShallow . hkdMap (coerce . f)

hkdCotraverse
  :: (Functor f, Functor t, FieldGetters rec)
  => (forall a. t (f a) -> g a)
  -> t (HKD rec f)
  -> HKD rec g
hkdCotraverse f = hkdMap (f . getCompose) . hkdDistributeShallow

hkdZipWith
  :: (forall a. f a -> g a -> h a)
  -> HKD rec f
  -> HKD rec g
  -> HKD rec h
hkdZipWith f (UnsafeMkHKD a) (UnsafeMkHKD b) = UnsafeMkHKD . A.arrayFromList $ do
  i <- [0 .. A.sizeofArray a - 1]
  [ f (A.indexArray a i) (A.indexArray b i) ]

hkdZipWith3
  :: (forall a. f a -> g a -> h a -> i a)
  -> HKD rec f
  -> HKD rec g
  -> HKD rec h
  -> HKD rec i
hkdZipWith3 f fa ga ha =
  hkdZipWith (\a (Pair b c) -> f a b c) fa
  $ hkdZipWith Pair ga ha

hkdPure :: (Applicative f, FieldGetters rec) => rec -> HKD rec f
hkdPure = hkdMap (pure . coerce) . toHKD

fromHKD :: ToRecord rec => HKD rec Identity -> rec
fromHKD = toRecord' . coerce

-- This function must be inlined, otherwise it behaves incorrectly with >O0
{-# INLINE toHKD #-}
toHKD :: forall rec. FieldGetters rec => rec -> HKD rec Identity
toHKD rec =
  let getters = fieldGetters @rec
   in UnsafeMkHKD $ A.createArray (length getters) (error "toHKD: impossible") $ \arr -> do
    let go !ix (x:xs) = do
          A.writeArray arr ix (coerce $ x rec)
          go (ix + 1) xs
        go _ [] = pure ()
    go 0 getters

mkHKD :: forall rec f tuple. (Instantiate rec f tuple) => tuple -> HKD rec f
mkHKD = instantiate @rec @f @tuple

setField
  :: forall name rec f a
   . (HasField name rec a, HkdSetField name rec f a)
  => FieldTy f a
  -> HKD rec f
  -> HKD rec f
setField = hkdSetField @name @rec @f @a

-- | Instantiate a HKD using the same value for every field.
fill :: forall rec f. FieldGetters rec => (forall a. f a) -> HKD rec f
fill x = UnsafeMkHKD . A.arrayFromList $ unsafeCoerce x <$ fieldGetters @rec

fillC :: forall c rec f
       . (Applicative f, FoldFields (WithHkdFields c Identity) rec Identity)
      => (forall a. c a => f a)
      -> f (HKD rec Identity)
fillC fa = withInstances @c (\_ _ -> fa)

-- | Accepts a handler that is invoked for each field of the record, providing
-- an instance of the given class as along with the field name and a getter in
-- order to construct a result. The results from each field are then aggregated
-- to form a complete record in HKD form.
--
-- This can be used to implement some type classes in a record generic way.
withInstances
  :: forall c f g rec
   . (Applicative f, FoldFields (WithHkdFields c g) rec g)
  => (forall a. c (FieldTy g a)
        => String -- ^ field name
        -> (HKD rec g -> FieldTy g a) -- ^ getter
        -> f (FieldTy g a)
     )
  -> f (HKD rec g)
withInstances k =
  let go :: forall a. (c (FieldTy g a), ToHkdFields g (FieldTy g a))
         => String
         -> (HKD rec g -> FieldTy g a)
         -> f [g Exts.Any]
      go name getter = toHkdFields <$> k @a name getter
   in coerce . A.arrayFromList <$>
        foldFields @(WithHkdFields c g) @rec @g
          go
          (pure [] :: f [g Exts.Any])
          (liftA2 (++))

-- | A lens focusing a specific field in a HKD.
atField :: forall (name :: Symbol) rec effect f a
         . ( HasField name rec a
           , Functor f
           , HkdHasField name rec effect a
           , HkdSetField name rec effect a
           )
        => (FieldTy effect a -> f (FieldTy effect a))
        -> HKD rec effect -> f (HKD rec effect)
atField afa rec =
  flip (setField @name) rec <$> afa (getField @name rec)

hkdApplyUpdate :: (ToRecord rec, FieldGetters rec) => HKD rec Maybe -> rec -> rec
hkdApplyUpdate upd rec =
  fromHKD $ hkdZipWith (\m i -> maybe i coerce m) upd (toHKD rec)
