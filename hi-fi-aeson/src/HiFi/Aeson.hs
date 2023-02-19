{-# OPTIONS_GHC -fplugin HiFi #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
module HiFi.Aeson
  ( HkdJSON(..)
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
  StripMaybe x = x

instance ( KnownSymbol name
         , FoldFields (WithHkdFields (FromJSON `And` Typeable) Identity) rec Identity
         , ToRecord rec
         )
    => FromJSON (HkdJSON name rec) where
  parseJSON = withObject (symbolVal $ Proxy @name) $ \o ->
    let go :: forall a. ( Typeable (FieldTy Identity a)
                        , Typeable (StripMaybe (FieldTy Identity a))
                        , FromJSON (FieldTy Identity a)
                        , FromJSON (StripMaybe (FieldTy Identity a))
                        )
           => String
           -> (HKD rec Identity -> FieldTy Identity a)
           -> Parser (FieldTy Identity a)
        go fieldName _ =
          case eqT @(FieldTy Identity a)
                   @(Identity (Maybe (StripMaybe (FieldTy Identity a)))) of
            Nothing -> o .: fromString fieldName
            Just Refl -> Identity <$> o .:? fromString fieldName
     in HkdJSON . fromHKD <$> withInstances @(FromJSON `And` Typeable) go

instance (FoldFields (WithHkdFields ToJSON Identity) rec Identity, FieldGetters rec)
    => ToJSON (HkdJSON name rec) where
  toJSON (HkdJSON rec) =
    let hkd = toHKD rec
        go :: forall a. ToJSON (FieldTy Identity a)
           => String
           -> (HKD rec Identity -> FieldTy Identity a)
           -> Const [Pair] (FieldTy Identity a)
        go fieldName getter =
          Const [((.=) @Pair @(FieldTy Identity a)) (fromString fieldName) (getter hkd)]
     in object . getConst $ withInstances @ToJSON go
  toEncoding (HkdJSON rec) =
    let hkd = toHKD rec
        go :: forall a. ToJSON (FieldTy Identity a)
           => String
           -> (HKD rec Identity -> FieldTy Identity a)
           -> Const Series (FieldTy Identity a)
        go fieldName getter =
          Const $ ((.=) @Series @(FieldTy Identity a)) (fromString fieldName) (getter hkd)
     in pairs . getConst $ withInstances @ToJSON go
