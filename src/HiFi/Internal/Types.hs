{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module HiFi.Internal.Types
  ( HKD(..)
  , RecArray
  , FieldName(..)
  , FieldGetters(..)
  , HkdHasField(..)
  , HkdSetField(..)
  , Instantiate(..)
  , ToRecord(..)
  , FoldFields(..)
  , ToHkdFields(..)
  , WithHkdFields
  , MissingField
  , UnknownField
  , UnsupportedRecord
  , indexArray
  , writeArray
  , getInnerRec
  , setInnerRec
  , arrayFromList
  , unsafeCoerceF
  , NestHKD(..)
  , FieldTy
  , OverFieldTy
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (foldM, void, when)
import           Control.Monad.ST (ST)
import           Data.Coerce (coerce)
import           Data.Kind
import qualified Data.List as List
import qualified Data.Primitive.Array as A
import           Foreign.Storable (Storable)
import qualified GHC.Exts as Exts
import           GHC.Generics (Generic)
import           GHC.Read (readPrec)
import           GHC.Records
import           GHC.TypeLits
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import           Unsafe.Coerce (unsafeCoerce)

type role HKD representational representational
type HKD :: Type -> (Type -> Type) -> Type
newtype HKD rec f =
  UnsafeMkHKD (A.Array (f Exts.Any))

data FieldName (symbol :: Symbol) = MkFieldName

type RecArray = A.Array Exts.Any

--------------------------------------------------------------------------------
-- Record Nesting
--------------------------------------------------------------------------------

-- | Marks a nested record field so that it inherits the higher kindedness of
-- the ambient record. To reduce extraneous noise, this type is treated
-- specially by the plugin so that you don't need to apply it to a field when
-- constructing a record or unwrap it when accessing the field.
--
-- @
--   data Foo = Foo { f1 :: Int, f2 :: Bool }
--   data Bar = Bar { b1 :: NestHKD Foo, b2 :: Foo }
--
--   nestedHKD :: HKD Bar Maybe -> HKD Foo Maybe
--   nestedHKD = getField @"b1"
--
--   unnested :: HKD Bar Maybe -> Maybe Foo
--   unnested = getField @"b2"
--
--   autoUnwrap :: Bar -> Foo
--   autoUnwrap = b1
--
--   autoWrap :: Foo -> Bar
--   autoWrap foo = Bar { b1 = foo, b2 = foo}
-- @
--
-- __WARNING:__ The special treatment of 'NestHKD' by the plugin can possibly
-- result in unsoundness if it is used for things other than simply tagging a
-- field in a record. Specifically, it should __not__ be used in a phantom type
-- parameter position.
--
-- @since 0.1.0.0
newtype NestHKD a = NestHKD { unNestHKD :: a }
  deriving newtype
    ( Eq
    , Generic
    , Semigroup
    , Monoid
    , Ord
    , Storable
    , Show
    , Read
    )
  deriving stock
    ( Functor
    , Foldable
    , Traversable
    )

instance Applicative NestHKD where
  pure = NestHKD
  (<*>) = coerce

instance Monad NestHKD where
  m >>= k = k $ coerce m

type FieldTy :: (Type -> Type) -> Type -> Type
type family FieldTy f a = r | r -> f a where
  FieldTy f (NestHKD a) = HKD a f
  FieldTy f a = f a

-- | Utility class used in FoldFields to allow a new HKD to be constructed
-- from the result of the fold.
type ToHkdFields :: (Type -> Type) -> Type -> Constraint
class ToHkdFields f a where
  toHkdFields :: a -> [f Exts.Any]

instance ToHkdFields f (f a) where
  toHkdFields x = [unsafeCoerce x]

instance ToHkdFields f (HKD rec f) where
  toHkdFields (UnsafeMkHKD arr) = Exts.toList arr

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

type WithHkdFields :: (Type -> Constraint) -> (Type -> Type) -> Type -> Constraint
class (c a, ToHkdFields f (FieldTy f a)) => WithHkdFields c f a
instance (c a, ToHkdFields f (FieldTy f a)) => WithHkdFields c f a

instance FoldFields (WithHkdFields (OverFieldTy Semigroup f) f) rec f => Semigroup (HKD rec f) where
  a@(UnsafeMkHKD arr) <> b =
    let builder :: forall s. A.MutableArray s (f Exts.Any) -> ST s ()
        builder newArr = do
          let go :: forall a. (Semigroup (FieldTy f a), ToHkdFields f (FieldTy f a))
                 => String
                 -> (HKD rec f -> FieldTy f a)
                 -> (rec -> a)
                 -> Int
                 -> ST s Int
              go _ getter _ !idx = do
                let writeElems i x = do
                      A.writeArray newArr i x
                      pure $! i - 1
                    fields = toHkdFields $ getter a <> getter b
                foldM writeElems idx $ reverse fields
          void $ foldFields @(WithHkdFields (OverFieldTy Semigroup f) f) @rec @f
                   go
                   (pure (A.sizeofArray arr - 1))
                   (=<<)
     in coerce $ A.createArray (A.sizeofArray arr) (error "Semigroup: impossible") builder

instance ( FoldFields (WithHkdFields (OverFieldTy Semigroup f) f) rec f
         , FoldFields (WithHkdFields (OverFieldTy Monoid f) f) rec f
         )
    => Monoid (HKD rec f) where
  mempty =
    let go :: forall a. (Monoid (FieldTy f a), ToHkdFields f (FieldTy f a))
           => String -> (HKD rec f -> FieldTy f a) -> (rec -> a) -> [f Exts.Any]
        go _ _ _ = toHkdFields $ mempty @(FieldTy f a)
        fields =
          foldFields @(WithHkdFields (OverFieldTy Monoid f) f) @rec @f go [] (++)
     in coerce $ A.arrayFromList fields

instance FoldFields (OverFieldTy Eq f) rec f => Eq (HKD rec f) where
  a == b =
    let go :: forall a. Eq (FieldTy f a) => String -> (HKD rec f -> FieldTy f a) -> (rec -> a) -> Bool
        go _ getter _ = getter a == getter b
     in foldFields @(OverFieldTy Eq f) @rec @f go True (&&)

instance FoldFields (OverFieldTy Show f) rec f => Show (HKD rec f) where
  show rec =
    let go :: forall a. Show (FieldTy f a) => String -> (HKD rec f -> FieldTy f a) -> (rec -> a) -> String
        go fieldName getter _ =
          fieldName <> " = " <> show (getter rec)
     in "HKD {" <> List.intercalate ", " (foldFields @(OverFieldTy Show f) @rec @f go [] (:)) <> "}"

instance FoldFields (WithHkdFields (OverFieldTy Read f) f) rec f => Read (HKD rec f) where
  readPrec = do
    let go :: forall a. (Read (FieldTy f a), ToHkdFields f (FieldTy f a))
           => String
           -> (HKD rec f -> FieldTy f a)
           -> (rec -> a)
           -> Bool
           -> ReadPrec.ReadPrec [f Exts.Any]
        go fieldName _ _ checkComma = do
          ReadPrec.lift $ do
            ReadP.string fieldName *> ReadP.skipSpaces
            ReadP.char '=' *> ReadP.skipSpaces
          x <- ReadPrec.step $ readPrec @(FieldTy f a)
          ReadPrec.lift $ do
            ReadP.skipSpaces
            when checkComma $ ReadP.char ',' *> ReadP.skipSpaces
          pure $ toHkdFields x
    ReadPrec.lift $ ReadP.string "HKD" *> ReadP.skipSpaces *> ReadP.char '{' *> ReadP.skipSpaces
    fields <- sequence . fst
            $ foldFields @(WithHkdFields (OverFieldTy Read f) f) @rec @f
                go
                ([], False)
                (\x (acc, b) -> (x b : acc, True))
    ReadPrec.lift $ ReadP.char '}' *> ReadP.skipSpaces
    ReadPrec.readP_to_Prec $ \n ->
      when (n == ReadPrec.minPrec) ReadP.eof
    pure . UnsafeMkHKD $ A.fromList (concat fields)

instance (FoldFields (OverFieldTy Eq f) rec f, FoldFields (OverFieldTy Ord f) rec f) => Ord (HKD rec f) where
  compare a b =
    let go :: forall a. Ord (FieldTy f a) => String -> (HKD rec f -> FieldTy f a) -> (rec -> a) -> Ordering
        go _ getter _ = compare (getter a) (getter b)
     in foldFields @(OverFieldTy Ord f) @rec @f go mempty (<>)

instance FoldFields (OverFieldTy NFData f) rec f => NFData (HKD rec f) where
  rnf hkd =
    let go :: forall a. NFData (FieldTy f a) => String -> (HKD rec f -> FieldTy f a) -> (rec -> a) -> ()
        go _ getter _ = rnf (getter hkd)
     in foldFields @(OverFieldTy NFData f) @rec @f go () seq

instance (HasField (name :: Symbol) rec a, HkdHasField name rec f a, result ~ FieldTy f a)
    => HasField name (HKD rec f) result where
  getField = hkdGetField @name @rec @f @a

--------------------------------------------------------------------------------
-- Magic type classes
--------------------------------------------------------------------------------

type HkdHasField :: Symbol -> Type -> (Type -> Type) -> Type -> Constraint
class HkdHasField name rec f a where
  hkdGetField :: HKD rec f -> FieldTy f a

type HkdSetField :: Symbol -> Type -> (Type -> Type) -> Type -> Constraint
class HkdSetField name rec f a where
  hkdSetField :: FieldTy f a -> HKD rec f -> HKD rec f

type FieldGetters :: Type -> Constraint
class FieldGetters rec where
  fieldGetters :: [rec -> Exts.Any]

type ToRecord :: Type -> Constraint
class ToRecord rec where
  toRecord' :: RecArray -> rec

-- | Right fold over the fields of a record
type FoldFields :: (Type -> Constraint) -> Type -> (Type -> Type) -> Constraint
class FoldFields c rec f where
  foldFields :: forall acc x.
                (forall a.
                  c a
                    => String
                    -> (HKD rec f -> FieldTy f a)
                    -> (rec -> a)
                    -> x
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
instance (FieldTy f a ~ b) => FieldTypeCheck fieldName f a b

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

type UnsupportedRecord :: Type -> Constraint
class UnsupportedRecord rec
instance
  TypeError (Text "Unsupported type for HKD promotion: " :<>: ShowType rec :<>: Text "."
        :$$: Text "Only record types without existentials or constraint contexts can be promoted to HKDs."
        :$$: Text "Additionally, nested HKDs are not allowed to be infinite or type family applications."
            )
  => UnsupportedRecord rec

type DataConNotInScope :: Type -> Constraint
class DataConNotInScope dataCon
instance
  TypeError (Text "Data constructor must be in scope for HKD promotion: "
        :<>: ShowType dataCon
#if !MIN_VERSION_ghc(9,6,0)
        :<>: Text "'"
#endif
            )
  => DataConNotInScope dataCon

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

indexArray :: forall (rec :: Type) (f :: Type -> Type). HKD rec f -> Int -> f Exts.Any
indexArray (UnsafeMkHKD arr) = A.indexArray arr

writeArray
  :: forall (rec :: Type) (f :: Type -> Type) (a :: Type)
   . HKD rec f
  -> Int
  -> f a
  -> HKD rec f
writeArray (UnsafeMkHKD arr) ix a = UnsafeMkHKD $ A.runArray $ do
  arrM <- A.thawArray arr 0 (A.sizeofArray arr)
  A.writeArray arrM ix (unsafeCoerce a)
  pure arrM

getInnerRec
  :: forall (rec :: Type) (f :: Type -> Type) (innerRec :: Type)
   . HKD rec f
  -> Int
  -> Int
  -> HKD innerRec f
getInnerRec (UnsafeMkHKD arr) offset len =
  UnsafeMkHKD (A.cloneArray arr offset len)

setInnerRec
  :: forall (rec :: Type) (f :: Type -> Type) (innerRec :: Type)
   . HKD rec f
  -> Int
  -> Int
  -> HKD innerRec f
  -> HKD rec f
setInnerRec (UnsafeMkHKD arr) offset len (UnsafeMkHKD innerRec) =
  UnsafeMkHKD $ A.runArray $ do
    arrMut <- A.thawArray arr 0 (A.sizeofArray arr)
    A.copyArray arrMut offset innerRec 0 len
    pure arrMut

arrayFromList :: forall rec (f :: Type -> Type). [f Exts.Any] -> HKD rec f
arrayFromList = UnsafeMkHKD . A.arrayFromList

unsafeCoerceF :: forall (f :: Type -> Type) (a :: Type). f a -> f Exts.Any
unsafeCoerceF = unsafeCoerce

type OverFieldTy :: (Type -> Constraint) -> (Type -> Type) -> Type -> Constraint
class c (FieldTy f a) => OverFieldTy c f a
instance c (FieldTy f a) => OverFieldTy c f a
