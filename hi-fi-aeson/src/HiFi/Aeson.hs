{-# OPTIONS_GHC -fplugin HiFi -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module HiFi.Aeson
  ( HkdJSON(..)
  , ObjectParser(..)
  , parserHKD
  , ObjectEncoder(..)
  , encoderHKD
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor.Const
import           Data.Kind
import           Data.Proxy
import           Data.String
import           Data.Typeable (Typeable, eqT, (:~:)(..))
import           GHC.TypeLits
import           HiFi

newtype HkdJSON (name :: Symbol) rec = HkdJSON { unHkdJSON :: rec }

type And :: (Type -> Constraint) -> (Type -> Constraint) -> Type -> Constraint
class (a x, a (StripMaybe x), b x, b (StripMaybe x)) => And a b x
instance (a x, a (StripMaybe x), b x, b (StripMaybe x)) => And a b x

type StripMaybe :: Type -> Type
type family StripMaybe a where
  StripMaybe (Identity (Maybe a)) = a
  StripMaybe (Maybe a) = a
  StripMaybe x = x

instance ( KnownSymbol name
         , FoldFields (WithHkdFields (OverFieldTy (FromJSON `And` Typeable) Identity) Identity) rec Identity
         , ToRecord rec
         )
    => FromJSON (HkdJSON name rec) where
  parseJSON = fmap (HkdJSON . fromHKD) . hkdParseJSON (symbolVal $ Proxy @name)

-- Orphan instance
instance (FoldFields (WithHkdFields (OverFieldTy (FromJSON `And` Typeable) f) f) rec f)
    => FromJSON (HKD rec f) where
  parseJSON = hkdParseJSON "HKD"

instance (FoldFields (WithHkdFields (OverFieldTy ToJSON Identity) Identity) rec Identity, FieldGetters rec)
    => ToJSON (HkdJSON name rec) where
  toJSON (HkdJSON rec) = hkdToJSON $ toHKD rec
  toEncoding (HkdJSON rec) = hkdToEncoding $ toHKD rec

-- Orphan instance
instance (FoldFields (WithHkdFields (OverFieldTy ToJSON f) f) rec f, FieldGetters rec)
  => ToJSON (HKD rec f) where
  toJSON = hkdToJSON
  toEncoding = hkdToEncoding

hkdParseJSON :: forall rec f
              . FoldFields (WithHkdFields (OverFieldTy (FromJSON `And` Typeable) f) f) rec f
             => String -> Value -> Parser (HKD rec f)
hkdParseJSON name = withObject name $ \o ->
  let go :: forall a. ( Typeable (FieldTy f a)
                      , Typeable (StripMaybe (FieldTy f a))
                      , FromJSON (FieldTy f a)
                      , FromJSON (StripMaybe (FieldTy f a))
                      )
         => String
         -> (HKD rec f -> FieldTy f a)
         -> (rec -> a)
         -> Parser (FieldTy f a)
      go fieldName _ _ =
        case eqT @(FieldTy f a) @(Maybe (StripMaybe (FieldTy f a))) of
          Just Refl -> o .:? fromString fieldName
          Nothing ->
            case eqT @(FieldTy f a)
                     @(Identity (Maybe (StripMaybe (FieldTy f a)))) of
              Just Refl -> Identity <$> o .:? fromString fieldName
              Nothing -> o .: fromString fieldName
   in withInstances @(OverFieldTy (FromJSON `And` Typeable) f) go

hkdToJSON :: forall rec f. FoldFields (WithHkdFields (OverFieldTy ToJSON f) f) rec f
          => HKD rec f -> Value
hkdToJSON hkd =
  let go :: forall a. ToJSON (FieldTy f a)
         => String
         -> (HKD rec f -> FieldTy f a)
         -> (rec -> a)
         -> Const [Pair] (FieldTy f a)
      go fieldName getter _ =
        Const [((.=) @Pair @(FieldTy f a)) (fromString fieldName) (getter hkd)]
   in object . getConst $ withInstances @(OverFieldTy ToJSON f) go

hkdToEncoding :: forall rec f. FoldFields (WithHkdFields (OverFieldTy ToJSON f) f) rec f
              => HKD rec f -> Encoding
hkdToEncoding hkd =
  let go :: forall a. ToJSON (FieldTy f a)
         => String
         -> (HKD rec f -> FieldTy f a)
         -> (rec -> a)
         -> Const Series (FieldTy f a)
      go fieldName getter _ =
        Const $ ((.=) @Series @(FieldTy f a)) (fromString fieldName) (getter hkd)
   in pairs . getConst $ withInstances @(OverFieldTy ToJSON f) go

newtype ObjectParser a = OP { runOP :: Object -> Parser a }
  deriving (Functor, Applicative) via Compose ((->) Object) Parser

parserHKD :: FoldFields (WithHkdFields (Recurse (FromJSON `And` Typeable) ObjectParser) ObjectParser) rec ObjectParser
          => HKD rec ObjectParser
parserHKD =
  buildWithInstances @(FromJSON `And` Typeable) $ \fieldName _ (_ :: rec -> a) ->
    OP $
      case eqT @a @(Maybe (StripMaybe a)) of
        Just Refl -> (.:? fromString fieldName)
        Nothing -> (.: fromString fieldName)

newtype ObjectEncoder record a = OE { runOE :: record -> [Pair] }
  deriving (Functor, Applicative) via Compose ((->) record) (Const [Pair])

encoderHKD :: FoldFields (WithHkdFields (Recurse ToJSON (ObjectEncoder rec)) (ObjectEncoder rec)) rec (ObjectEncoder rec)
           => HKD rec (ObjectEncoder rec)
encoderHKD =
  buildWithInstances @ToJSON $ \fieldName _ getter ->
    OE $ pure . (fromString fieldName .=) . getter
