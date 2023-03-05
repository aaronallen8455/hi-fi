{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module HiFi.Api
  ( -- * HKD API
    hkdMap
  , hkdSequenceOuter
  , hkdSequence
  , hkdDistribute
  , hkdDistributeOuter
  , hkdTraverse
  , hkdCotraverse
  , hkdZip
  , hkdUnzip
  , hkdZipWith
  , hkdZipWith3
  , hkdPure
  -- * Conversion
  , fromHKD
  , toHKD
  -- * Field access
  , setField
  , getField
  , modifyField
  , atField
  , atFieldI
  -- * Instantiation
  , mkHKD
  , fill
  , fillC
  , withInstances
  , buildWithInstances
  -- * Utilities
  , hkdApplyUpdate
  , hkdApplyModifier
  -- * HKD Nesting
  , NestHKD(..)
  -- * Internals
  , StringSing(..)
  , FieldName(..)
  , toFieldName
  , ToHkdFields(..)
  , FoldFields
  , FieldTy
  , ToRecord
  , FieldGetters
  , WithHkdFields
  , OverFieldTy
  , Recurse
  , HKD
  -- * Re-exports
  , I
  , Identity(I, ..)
  , Endo(..)
  , Compose(..)
  ) where

import           Control.Applicative (liftA2)
import           Data.Coerce (coerce)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Product (Product(..))
import           Data.Monoid (Endo(..))
import qualified Data.Primitive.Array as A
import qualified GHC.Exts as Exts
import           GHC.Records
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

import           HiFi.Internal.Types (FieldGetters(..), FieldName(..), FieldTy, FoldFields(..), HKD(..), HkdHasField, HkdSetField, HkdSetField(..), Instantiate(..), NestHKD(..), OverFieldTy, ToHkdFields(..), ToRecord(..), WithHkdFields)
import           HiFi.StringSing (StringSing(..), toFieldName)

--------------------------------------------------------------------------------
-- HKD API
--------------------------------------------------------------------------------

-- | Uses a natural transformation to change the functor parameter of a HKD.
-- This is the HKD analog of 'fmap'.
--
-- @since 0.1.0.0
hkdMap :: (forall a. f a -> g a) -> HKD rec f -> HKD rec g
hkdMap f (UnsafeMkHKD arr) = UnsafeMkHKD $ f <$> arr

-- | Uses the 'Applicative' instance of the HKD's functor to transform it to
-- a normal record inside that functor. This is analogous to the @sequenceA@ function.
--
-- @since 0.1.0.0
hkdSequence :: forall f rec. (Applicative f, ToRecord rec) => HKD rec f -> f rec
hkdSequence = fmap fromHKD
            . hkdSequenceOuter @f @Identity
            . hkdMap (coerce . fmap Identity)

-- | Like 'hkdSequence' but only sequences over the outer functor in the
-- composition of two functors.
--
-- @since 0.1.0.0
hkdSequenceOuter :: forall f g rec. Applicative f
                 => HKD rec (Compose f g) -> f (HKD rec g)
hkdSequenceOuter (UnsafeMkHKD arr) = UnsafeMkHKD <$> traverse coerce arr

-- | Takes a normal record wrapped in a functor and produces a HKD
-- parameterized by that functor. This is the dual of 'hkdSequence'.
--
-- @since 0.1.0.0
hkdDistribute :: forall rec f. (FieldGetters rec, Functor f)
              => f rec -> HKD rec f
hkdDistribute fRec =
  let getters = fieldGetters @rec
      fields = (`fmap` fRec) <$> getters
   in coerce $ A.arrayFromList fields

-- | Takes a HKD wrapped in a functor and produces a HKD parameterized by the
-- composition of that functor with the functor from the original HKD. This is
-- the dual of 'hkdSequenceOuter'.
--
-- @since 0.1.0.0
hkdDistributeOuter
  :: forall rec f g. (FieldGetters rec, Functor f, Functor g)
  => f (HKD rec g)
  -> HKD rec (Compose f g)
hkdDistributeOuter fHKD =
  let getters = fieldGetters @rec
   in coerce . A.arrayFromList $ do
        idx <- fst <$> zip [0..] getters
        pure . Compose $ fmap (\(UnsafeMkHKD arr) -> A.indexArray arr idx) fHKD

-- | This is the HKD analog of the 'traverse' function.
--
-- @since 0.1.0.0
hkdTraverse
  :: Applicative t
  => (forall a. f a -> t (g a))
  -> HKD rec f
  -> t (HKD rec g)
hkdTraverse f = hkdSequenceOuter . hkdMap (coerce . f)

-- | The dual of 'hkdTraverse'.
--
-- @since 0.1.0.0
hkdCotraverse
  :: (Functor f, Functor t, FieldGetters rec)
  => (forall a. t (f a) -> g a)
  -> t (HKD rec f)
  -> HKD rec g
hkdCotraverse f = hkdMap (f . getCompose) . hkdDistributeOuter

-- | Analog of the 'zip' function that works over HKDs instead of lists.
--
-- @since 0.1.0.0
hkdZip :: HKD rec f -> HKD rec g -> HKD rec (Product f g)
hkdZip = hkdZipWith Pair

-- | Analog of the 'unzip' function that works over HKDs instead of a list.
--
-- @since 0.1.0.0
hkdUnzip :: HKD rec (Product f g) -> (HKD rec f, HKD rec g)
hkdUnzip hkd = (hkdMap (\(Pair x _) -> x) hkd, hkdMap (\(Pair _ x) -> x) hkd)

-- | Analog of the 'zipWith' function that works over HKDs instead of lists.
--
-- @since 0.1.0.0
hkdZipWith
  :: (forall a. f a -> g a -> h a)
  -> HKD rec f
  -> HKD rec g
  -> HKD rec h
hkdZipWith f (UnsafeMkHKD a) (UnsafeMkHKD b) = UnsafeMkHKD . A.arrayFromList $ do
  i <- [0 .. A.sizeofArray a - 1]
  [ f (A.indexArray a i) (A.indexArray b i) ]

-- | Analog of the 'zipWith3' function that works over HKDs instead of lists.
--
-- @since 0.1.0.0
hkdZipWith3
  :: (forall a. f a -> g a -> h a -> i a)
  -> HKD rec f
  -> HKD rec g
  -> HKD rec h
  -> HKD rec i
hkdZipWith3 f fa ga ha =
  hkdZipWith (\a (Pair b c) -> f a b c) fa
  $ hkdZip ga ha

-- | Uses an Applicative instance to transform a normal record into a HKD
-- parameterized by that Applicative. This is the HKD analog of 'pure'.
--
-- @since 0.1.0.0
hkdPure :: (Applicative f, FieldGetters rec) => rec -> HKD rec f
hkdPure = hkdMap (pure . coerce) . toHKD

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- | Convert a HKD parameterized by 'Identity' to a normal record.
--
-- @since 0.1.0.0
fromHKD :: ToRecord rec => HKD rec Identity -> rec
fromHKD = toRecord' . coerce

-- This function must be inlined, otherwise it behaves incorrectly with >O0
{-# INLINE toHKD #-}
-- | Convert a normal record to a HKD parameterized by 'Identity'.
--
-- @since 0.1.0.0
toHKD :: forall rec. FieldGetters rec => rec -> HKD rec Identity
toHKD rec =
  let getters = fieldGetters @rec
   in UnsafeMkHKD $ A.createArray (length getters) (error "toHKD: impossible") $ \arr -> do
    let go !ix (x:xs) = do
          A.writeArray arr ix (coerce $ x rec)
          go (ix + 1) xs
        go _ [] = pure ()
    go 0 getters

--------------------------------------------------------------------------------
-- Field access
--------------------------------------------------------------------------------

-- | Replace the value of a specific field in a HKD.
--
-- @
--   setField @"fieldName" newValue hkd
-- @
--
-- @since 0.1.0.0
setField
  :: forall name rec f a
   . (HasField name rec a, HkdSetField name rec f a)
  => FieldTy f a
  -> HKD rec f
  -> HKD rec f
setField = hkdSetField @name @rec @f @a

-- | Apply a function to the value of a specific field in a HKD.
--
-- @
--   modifyField @"fieldName" modifier hkd
-- @
--
-- @since 0.1.0.0
modifyField
  :: forall name rec f a
   . (HasField name rec a, HkdSetField name rec f a, HkdHasField name rec f a)
  => (FieldTy f a -> FieldTy f a)
  -> HKD rec f
  -> HKD rec f
modifyField f hkd = setField @name (f (getField @name hkd)) hkd

-- | A lens focusing a specific field in a HKD.
--
-- @since 0.1.0.0
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

-- | A lens that focuses a specific field in a HKD parameterized by the 'Identity'
-- functor which looks through the 'Identity' wrapper. It is a type error to
-- use this lens for a 'NestHKD' field, use 'atField' instead.
--
-- @since 0.1.0.0
atFieldI :: forall (name :: Symbol) rec f a
          . ( HasField name rec a
            , Functor f
            , HkdHasField name rec Identity a
            , HkdSetField name rec Identity a
            , FieldTy Identity a ~ Identity a
            )
         => (a -> f a)
         -> HKD rec Identity -> f (HKD rec Identity)
atFieldI = atField @name . (\f -> fmap coerce . f . coerce)

--------------------------------------------------------------------------------
-- Instantiation
--------------------------------------------------------------------------------

-- | A special function that allows record syntax to be used to instantiate the
-- HKD version of a record.
--
-- @
-- data MyRec = MyRec
--   { f1 :: Int
--   , f2 :: Bool
--   , f3 :: String
--   }
--
-- let hkd = mkHKD
--       { f1 = Just 3
--       , f2 = Nothing
--       , f3 = Just ""
--       }
-- @
--
-- @since 0.1.0.0
mkHKD :: forall rec f tuple. (Instantiate rec f tuple) => tuple -> HKD rec f
mkHKD = instantiate @rec @f @tuple

-- | Instantiate a HKD using the same value for every field.
--
-- @
--   fill Nothing :: HKD Record Maybe
-- @
--
-- @since 0.1.0.0
fill :: forall rec f. FieldGetters rec => (forall a. f a) -> HKD rec f
fill x = UnsafeMkHKD . A.arrayFromList $ unsafeCoerce x <$ fieldGetters @rec

-- | Instantiate a HKD using a value that has access to a class instance for
-- each field in the record.
--
-- @
-- instance Arbitrary (HKD MyRecord Identity) where
--   arbitrary = fromHKD <$> fillC @Arbitrary arbitrary
-- @
--
-- @since 0.1.0.0
fillC :: forall c rec f
       . (Applicative f, FoldFields (WithHkdFields (OverFieldTy c Identity) Identity) rec Identity)
      => (forall a. c (FieldTy Identity a) => f (FieldTy Identity a))
      -> f (HKD rec Identity)
fillC fa =
  withInstances @(OverFieldTy c Identity) @f @I @rec (\_ _ _ -> fa)

-- | Accepts a handler that is invoked for each field of the record, providing
-- an instance of the given class as along with the field name and getters in
-- order to construct a result. The results from each field are then aggregated
-- using an Applicative to form a complete record in HKD form.
--
-- This can be used to implement some type classes in a record generic way.
--
-- @since 0.1.0.0
withInstances
  :: forall c f g rec
   . (Applicative f, FoldFields (WithHkdFields c g) rec g)
  => (forall a. c a
        => String -- ^ field name
        -> (HKD rec g -> FieldTy g a) -- ^ getter
        -> (rec -> a)
        -> f (FieldTy g a)
     )
  -> f (HKD rec g)
withInstances k =
  let go :: forall a. (c a, ToHkdFields g (FieldTy g a))
         => String
         -> (HKD rec g -> FieldTy g a)
         -> (rec -> a)
         -> f [g Exts.Any]
      go name getter recGetter = toHkdFields <$> k @a name getter recGetter
   in coerce . A.arrayFromList <$>
        foldFields @(WithHkdFields c g) @rec @g
          go
          (pure [] :: f [g Exts.Any])
          (liftA2 (++))

data Evidence c f a fty where
  NotNested :: c a => Evidence c f a (f a)
  Nested :: forall rec c f. FoldFields (WithHkdFields (Recurse c f) f) rec f
         => Evidence c f (NestHKD rec) (HKD rec f)

class Recurse' c f a (FieldTy f a) => Recurse c f a where
instance Recurse' c f a (FieldTy f a) => Recurse c f a where

class FieldTy f a ~ fty => Recurse' c f a fty where
  recurse :: Evidence c f a fty

instance (FieldTy f a ~ f a, c a) => Recurse' c f a (f a) where
  recurse = NotNested

instance FoldFields (WithHkdFields (Recurse c f) f) rec f
    => Recurse' c f (NestHKD rec) (HKD rec f) where
  recurse = Nested

-- | Similar to 'withInstances' but constructs an HKD directly rather than
-- having it wrapped in an Appicative.
--
-- @since 0.1.0.0
buildWithInstances
  :: forall c g rec
   . FoldFields (WithHkdFields (Recurse c g) g) rec g
  => (forall a
         . c a
        => String
        -> (HKD rec g -> g a)
        -> (rec -> a)
        -> g a
     )
  -> HKD rec g
buildWithInstances k =
  let go :: forall a. Recurse c g a
         => String
         -> (HKD rec g -> FieldTy g a)
         -> (rec -> a)
         -> Identity (FieldTy g a)
      go fieldName getter recGetter =
        let nestedRec :: forall nestedRec
                       . ( FoldFields (WithHkdFields (Recurse c g) g) nestedRec g
                         , a ~ NestHKD nestedRec
                         )
                      => HKD nestedRec g
            nestedRec =
              buildWithInstances @c @g @nestedRec
                ( \name innerGetter innerRecGetter ->
                    k name (innerGetter . getter)
                           (innerRecGetter . coerce . recGetter)
                )
         in Identity $
           case recurse @c @g @a of
             NotNested -> k fieldName getter recGetter
             Nested -> nestedRec
   in runIdentity $ withInstances @(Recurse c g) @I @g @rec go

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Use a HKD parameterized by 'Maybe' as an update where 'Just' values replace
-- the value in the updated record and 'Nothing' no-ops.
--
-- @since 0.1.0.0
hkdApplyUpdate :: (ToRecord rec, FieldGetters rec) => HKD rec Maybe -> rec -> rec
hkdApplyUpdate upd rec =
  fromHKD $ hkdZipWith (\m i -> maybe i coerce m) upd (toHKD rec)

-- | Use a HKD parameterized by 'Endo' to apply a record of modifier functions
-- to a plain record.
--
-- @since 0.1.0.0
hkdApplyModifier :: (ToRecord rec, FieldGetters rec) => HKD rec Endo -> rec -> rec
hkdApplyModifier m rec =
  fromHKD $ hkdZipWith (appEndo . coerce) m (toHKD rec)

-- type family RemoveConApp f a where
--   RemoveConApp f (f a) = a
-- 
-- class NoNestedHKD f a where
--   noNestedHkdProof :: FieldTy f (RemoveConApp f a) :~: a
-- instance FieldTy f a ~ f a => NoNestedHKD f (f a) where
--   noNestedHkdProof = Refl
-- instance (TypeError (Text "Nested HKD fields not allowed here"))
--   => NoNestedHKD f (HKD a f) where
--   noNestedHkdProof = error "impossible"

--------------------------------------------------------------------------------
-- Synonyms
--------------------------------------------------------------------------------

type I = Identity
pattern I :: a -> Identity a
pattern I a = Identity a
{-# COMPLETE I #-}
