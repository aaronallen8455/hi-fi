{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module HiFi.Internal.Types
  ( HKD(..)
  , RecArray
  , FieldName(..)
  , FieldType(..)
  , IndexOfField(..)
  , FieldGetters(..)
  , Instantiate(..)
  , ToRecord(..)
  , FoldFields(..)
  , indexArray
  , arrayFromList
  ) where

import           Data.Kind
import qualified Data.List as List
import qualified Data.Primitive.Array as A
import qualified GHC.Exts as Exts
import           GHC.Records
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

type HKD :: Type -> (Type -> Type) -> Type
newtype HKD rec f =
  UnsafeMkHKD (A.Array (f Exts.Any))

data FieldName (symbol :: Symbol) = MkFieldName

data FieldType (a :: Type) = MkFieldType

type RecArray = A.Array Exts.Any

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance FoldFields Semigroup rec f => Semigroup (HKD rec f) where
  a <> b =
    let go _ _ getter =
          unsafeCoerce $ getter a <> getter b
        -- TODO would be more efficient to not construct the intermediate list
        fields =
          foldFields @Semigroup @rec @f go [] (:)
     in UnsafeMkHKD $ A.arrayFromList fields

instance (FoldFields Semigroup rec f, FoldFields Monoid rec f) => Monoid (HKD rec f) where
  mempty =
    let go _ (MkFieldType :: FieldType a) _ = unsafeCoerce $ mempty @(f a)
        fields =
          foldFields @Monoid @rec @f go [] (:)
     in UnsafeMkHKD $ A.arrayFromList fields

instance FoldFields Eq rec f => Eq (HKD rec f) where
  a == b =
    let go _ _ getter = getter a == getter b
     in foldFields @Eq @rec @f go True (&&)

instance FoldFields Show rec f => Show (HKD rec f) where
  show rec =
    let go fieldName _ getter =
          fieldName <> " = " <> show (getter rec)
     in "HKD {" <> List.intercalate ", " (foldFields @Show @rec @f go [] (:)) <> "}"

instance (FoldFields Eq rec f, FoldFields Ord rec f) => Ord (HKD rec f) where
  compare a b =
    let go _ _ getter = compare (getter a) (getter b)
     in foldFields @Ord @rec @f go mempty (<>)

instance (HasField (name :: Symbol) rec a, IndexOfField name rec)
    => HasField name (HKD rec f) (f a) where
  getField (UnsafeMkHKD arr) =
    unsafeCoerce $ A.indexArray arr (indexOfField @name @rec)

--------------------------------------------------------------------------------
-- Magic type classes
--------------------------------------------------------------------------------

type IndexOfField :: Symbol -> Type -> Constraint
class IndexOfField name rec where
  indexOfField :: Int

class FieldGetters rec where
  fieldGetters :: [rec -> Exts.Any]

-- Could build a core expr that applies the dataCon to each element from the
-- array. Will have to lookup the `indexArray` Id to do this.
-- Might be easier to construct an rn expr and then typecheck and desugar it.
-- Core might be easier because won't have to deal with coercions
class ToRecord rec where
  toRecord' :: RecArray -> rec

-- | Right fold over the fields of a record
type FoldFields :: (Type -> Constraint) -> Type -> (Type -> Type) -> Constraint
class FoldFields c rec f where
  foldFields :: forall acc x.
                (forall a.
                  c (f a) => String -> FieldType a -> (HKD rec f -> f a) -> x
                )
             -> acc
             -> (x -> acc -> acc)
             -> acc

type Instantiate :: Type -> (Type -> Type) -> Type -> Constraint
class Instantiate rec f tuple where
  instantiate :: tuple -> HKD rec f

type FieldTypeCheck :: Symbol -> (Type -> Type) -> Type -> Type -> Constraint
class FieldTypeCheck fieldName f recordTy userTy where

-- This equality constraint helps resolve ambiguous terms such as literals and Nothing
instance a ~ b => FieldTypeCheck fieldName f a (f b)

instance {-# INCOHERENT #-}
  TypeError
       (Text "Expected '" :<>: ShowType (f a)
   :<>: Text "', got '" :<>: ShowType b
   :<>: Text "' for field '" :<>: Text fieldName :<>: Text "'")
  => FieldTypeCheck fieldName f a b

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

indexArray :: RecArray -> Int -> Exts.Any
indexArray = A.indexArray

arrayFromList :: [Exts.Any] -> RecArray
arrayFromList = A.arrayFromList
