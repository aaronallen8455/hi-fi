# Hi-Fi

This is a plugin based library that facilitates using the higher-kinded
data pattern with record types without adding parameters or using
generics.

Record syntax is used to instantiate the higher kinded version of a record,
providing good ergonomics while maintaining the familiarity of constructing
standard records.

The main benefit of this approach over a generics based approach is far less
compile time overhead. Generics code is well known culprit of slow compilation
due to the strain it puts on the simplifier. The use of a plugin largely
sidesteps this issue.

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

- Does not work with `NamedFieldPuns` or `RecordWildCards`
- You need `DataKinds` and `TypeApplications` turned on when working with
  records that have non-standard characters in field names.
- Currently nested records are not recursively made higher kinded.
