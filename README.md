# Hi-Fi

This is a plugin based library that facilitates using the higher-kinded
data pattern with record types without adding parameters or using
generics. You can even use record syntax!

```haskell
data MyRecord =
  MkMyRecord
    { foo :: Bool
    , bar :: Int
    , baz :: String
    }

recordUpdate :: HKD MyRecord Maybe
recordUpdate =
  mkHKD { foo = Just True
        , bar = Nothing
        , baz = Just "new"
        }
```