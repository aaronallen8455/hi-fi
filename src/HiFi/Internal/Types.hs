{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module HiFi.Internal.Types
  ( HKD(..)
  , RecArray
  , FieldName(..)
  , IndexOfField(..)
  , FieldGetters(..)
  , Instantiate(..)
  , ToRecord(..)
  , FoldFields(..)
  , MissingField
  , UnknownField
  , indexArray
  , arrayFromList
  , unsafeCoerceF
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (void, when)
import           Control.Monad.ST (ST)
import           Data.Kind
import qualified Data.List as List
import qualified Data.Primitive.Array as A
import qualified GHC.Exts as Exts
import           GHC.Read (readPrec)
import           GHC.Records
import           GHC.TypeLits
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import           Unsafe.Coerce (unsafeCoerce)

type HKD :: Type -> (Type -> Type) -> Type
newtype HKD rec f =
  UnsafeMkHKD (A.Array (f Exts.Any))

data FieldName (symbol :: Symbol) = MkFieldName

type RecArray = A.Array Exts.Any

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance FoldFields Semigroup rec f => Semigroup (HKD rec f) where
  a@(UnsafeMkHKD arr) <> b =
    let builder :: forall s. A.MutableArray s (f Exts.Any) -> ST s ()
        builder newArr = do
          let go :: forall a. Semigroup (f a) => String -> (HKD rec f -> f a) -> Int -> ST s Int
              go _ getter !idx = do
                A.writeArray newArr idx . unsafeCoerce $ getter a <> getter b
                pure $ idx - 1
          void $ foldFields @Semigroup @rec @f go (pure (A.sizeofArray arr - 1)) (=<<)
     in UnsafeMkHKD $ A.createArray (A.sizeofArray arr) (error "Semigroup: impossible") builder

instance (FoldFields Semigroup rec f, FoldFields Monoid rec f) => Monoid (HKD rec f) where
  mempty =
    let go :: forall a. Monoid (f a) => String -> (HKD rec f -> f a) -> f Exts.Any
        go _ _ = unsafeCoerce $ mempty @(f a)
        fields =
          foldFields @Monoid @rec @f go [] (:)
     in UnsafeMkHKD $ A.arrayFromList fields

instance FoldFields Eq rec f => Eq (HKD rec f) where
  a == b =
    let go :: forall a. Eq (f a) => String -> (HKD rec f -> f a) -> Bool
        go _ getter = getter a == getter b
     in foldFields @Eq @rec @f go True (&&)

instance FoldFields Show rec f => Show (HKD rec f) where
  show rec =
    let go :: forall a. Show (f a) => String -> (HKD rec f -> f a) -> String
        go fieldName getter =
          fieldName <> " = " <> show (getter rec)
     in "HKD {" <> List.intercalate ", " (foldFields @Show @rec @f go [] (:)) <> "}"

instance FoldFields Read rec f => Read (HKD rec f) where
  readPrec = do
    let go :: forall a. Read (f a) => String -> (HKD rec f -> f a) -> Bool -> ReadPrec.ReadPrec (f Exts.Any)
        go fieldName _ checkComma = do
          ReadPrec.lift $ do
            ReadP.string fieldName *> ReadP.skipSpaces
            ReadP.char '=' *> ReadP.skipSpaces
          x <- readPrec @(f a)
          ReadPrec.lift $ do
            ReadP.skipSpaces
            when checkComma $ ReadP.char ',' *> ReadP.skipSpaces
          pure $ unsafeCoerce x
    ReadPrec.lift $ ReadP.string "HKD {" *> ReadP.skipSpaces
    fields <- sequence . fst
            $ foldFields @Read @rec @f
                go
                ([], False)
                (\x (acc, b) -> (x b : acc, True))
    ReadPrec.lift $ ReadP.char '}' *> ReadP.eof
    pure . UnsafeMkHKD $ A.fromList fields

instance (FoldFields Eq rec f, FoldFields Ord rec f) => Ord (HKD rec f) where
  compare a b =
    let go :: forall a. Ord (f a) => String -> (HKD rec f -> f a) -> Ordering
        go _ getter = compare (getter a) (getter b)
     in foldFields @Ord @rec @f go mempty (<>)

instance FoldFields NFData rec f => NFData (HKD rec f) where
  rnf hkd =
    let go :: forall a. NFData (f a) => String -> (HKD rec f -> f a) -> ()
        go _ getter = rnf (getter hkd)
     in foldFields @NFData @rec @f go () (\() () -> ())

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

class ToRecord rec where
  toRecord' :: RecArray -> rec

-- | Right fold over the fields of a record
type FoldFields :: (Type -> Constraint) -> Type -> (Type -> Type) -> Constraint
class FoldFields c rec f where
  foldFields :: forall acc x.
                (forall a.
                  c (f a) => String -> (HKD rec f -> f a) -> x
                )
             -> acc
             -> (x -> acc -> acc)
             -> acc

type Instantiate :: Type -> (Type -> Type) -> Type -> Constraint
class Instantiate rec f tuple where
  instantiate :: tuple -> HKD rec f

type FieldTypeCheck :: Symbol -> (Type -> Type) -> Type -> Type -> Constraint
class FieldTypeCheck fieldName f recordTy userTy

-- This equality constraint helps resolve ambiguous terms such as literals and Nothing.
instance (f a ~ b) => FieldTypeCheck fieldName f a b

type MissingField :: Symbol -> Type -> Constraint
class MissingField fieldName rec
instance
  TypeError (Text "Missing field '" :<>: Text fieldName
        :<>: Text "' of '" :<>: ShowType rec :<>: Text "'")
  => MissingField fieldName rec

type UnknownField :: Symbol -> Type -> Constraint
class UnknownField fieldName rec
instance
  TypeError (Text "Unknown field '" :<>: Text fieldName
        :<>: Text "' for '" :<>: ShowType rec :<>: Text "'")
  => UnknownField fieldName rec

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

indexArray :: forall (rec :: Type) (f :: Type -> Type). HKD rec f -> Int -> f Exts.Any
indexArray (UnsafeMkHKD arr) = A.indexArray arr

arrayFromList :: forall rec (f :: Type -> Type). [f Exts.Any] -> HKD rec f
arrayFromList = UnsafeMkHKD . A.arrayFromList

unsafeCoerceF :: forall (f :: Type -> Type) (a :: Type). f a -> f Exts.Any
unsafeCoerceF = unsafeCoerce
