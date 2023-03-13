<h1 style="display: flex;align-items: center;">
  <img src="logo.svg" alt="hi-fi logo" width="80" height="80"/>
  Hi-Fi
</h1>

### Intro

This is a plugin based library that facilitates using the [higher-kinded data
pattern](https://reasonablypolymorphic.com/blog/higher-kinded-data/) with
record types without adding parameters or using generics.

The plugin allows for record syntax to be used to instantiate the higher kinded
version of a record, providing good ergonomics while maintaining the
familiarity of constructing records the standard way.

Another benefit of this plugin over a generics based approach is that there is
far less compile time overhead. Generics code is a well known culprit of slow
compilation due to the strain it puts on the simplifier. The use of a plugin
largely sidesteps this issue.

### Getting Started

To use this library, simply add `hi-fi` as a package dependency and then add
the following to the `ghc-options` stanza of your `package.yaml` or `*.cabal`
file to enable the plugin:

```
ghc-options: -fplugin HiFi
```

Everything you need is exported by the `HiFi` module. This module can be
imported either qualified or unqualified, but using an explicit import list for
it is not recommended.

### Instantiation

The special `mkHKD` function is used to instantiate the higher kinded data
version of a record. It is used with record syntax to explicitly assign each
field of the record:

```haskell
data MyRecord =
  MkMyRecord
    { foo :: Bool
    , bar :: Int
    , baz :: String
    }

-- A higher kinded data version of 'MyRecord' using the 'Maybe' functor.
recordUpdate :: HKD MyRecord Maybe
recordUpdate =
  mkHKD { foo = Just True
        , bar = Nothing
        , baz = Just "new"
        }
```

If a field is missing or if there is an extra field, then you will get a custom
error message calling out the field by name. Likewise, if the type of a field
is incorrect, the error message will contain a reference to the offending field.

If GHC is not able to infer the type of the HKD (or if you prefer to give the
type explicitly) then the `TypeApplications` extension can be used to supply
the record type as well as the functor type like so:

```haskell
{-# LANGUAGE TypeApplications #-}

hkd1 =
  (mkHKD @MyRecord)
    { foo = Just True
    , bar = Just 1
    , baz = Nothing
    }

hkd2 =
  (mkHKD @MyRecord @Maybe)
    { foo = pure True
    , bar = pure 1
    , baz = empty
    }
```

### Conversion

Converting to and from the higher kinded version of a record is done using the
`toHKD` and `fromHKD` functions respectively. In order to use `fromHKD`, the
HKD must be using `Data.Functor.Identity` as the functor parameter.

```
hkd :: HKD MyRecord Identity
hkd = toHKD (MyRecord { foo = True, bar = 1, baz = "..." })

record :: MyRecord
record = fromHKD hkd
```

The `hkdSequence` function allows you to turn an HKD parameterized by an
`Applicative` effect into the base record wrapped in that effect. As the name
suggests, it is analogous to the `sequenceA` function.

```haskell
recordList :: [MyRecord]
recordList = hkdSequence $
  mkHKD { foo = [True, False]
        , bar = [1, 2, 3]
        , baz = ["..."]
        }
```

### Accessing Fields

The `HKD` type is equipped with `HasField` instances for each field in the base
record, which means you can use the `OverloadedRecordDot` extension to access
fields:

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

recordUpdate.foo :: Maybe Bool
recordUpdate.bar :: Maybe Int
recordUPdate.baz :: Maybe String
```

Use the `setField` function for setting field values:

```haskell
modifiedRecordUpdate :: HKD MyRecord Maybe
modifiedRecordUpdate = setField @"foo" Nothing recordUpdate
```

Additionally, there is a lens called `atField` which can be used with most
optics libraries.
```haskell
modifiedRecordUpdate :: HKD MyRecord Maybe
modifiedRecordUpdate = recordUpdate & atField @"foo" ?~ True
```

### API

A rich API for working with HKDs is provided. There are also many useful type
class instances for the `HKD` type, such as `Show`, `Eq`, `Ord`, `Semigroup`,
and `Monoid`.
```haskell
data Person =
  MkPerson
    { name :: String
    , age :: Int
    } deriving Show

sara :: Person
sara = MkPerson "Sara" 40

tom :: Person
tom = MkPerson "Tom" 36

update :: HKD Person Maybe
update = mkHKD {name = Just "Bob", age = Nothing}

-- Example GHCi session:
>>> hkdDistribute [sara, tom]
HKD {name = ["Sara","Tom"], age = [40,36]}

>>> hkdSequence $ hkdDistribute [sara, tom]
[ MkPerson {name = "Sara", age = 40}
, MkPerson {name = "Sara", age = 36}
, MkPerson {name = "Tom", age = 40}
, MkPerson {name = "Tom", age = 36}
]

>>> hkdPure @Last sara <> hkdPure tom
HKD {name = Last {getLast = Just "Tom"}, age = Last {getLast = Just 36}}

>>> hkdApplyUpdate update tom
MkPerson {name = "Bob", age = 36}
```

### Applicative Expressions

`Applicative` effects are commonly applied to records using syntax such as
```haskell
Foo <$> expr1
    <*> expr2
    <*> expr3
    ...
```
This idiom has a number of down-sides:
- It looks bizarre to the uninitiated.
- You must know the order of fields in the record to tell which expression is
  mapping to which field. It's a common mistake to get the ordering wrong, which
  won't be caught at compile time if the fields are of the same type.
- The error messages resulting from missing fields or mismatched types can be
  verbose and confusing.

using `hi-fi` you can instead write applicative expressions using record syntax:
```haskell
hkdSequence (mkHKD @Foo) -- type application optional in most cases
  { field1 = expr1
  , field2 = expr2
  , field3 = expr3
  ...
  }
```
This is advantageous because having field assignments greatly improves the
readability of the code. You also don't need to worry about putting fields in
a particular order. Additionally, error messages will reference the relevant field by name.

### Meta Programming Alternative

The `hi-fi` plugin provides a number of type classes that are able to tap into
GHC's internal representation in order to work with records in ways that are
not normally possible without meta programming techniques such as generics or
template haskell.

As an example, let's see how `hi-fi` can be used to implement the
`FromNamedRecord` and `ToNamedRecord` type classes from the `cassava` library
in a record generic way.

```haskell
import qualified Data.Csv as Csv
import           Data.Functor.Identity
import           Data.Functor.Const
import qualified HiFi

newtype HkdCsv record = MkHkdCsv record

instance ( HiFi.FoldFields (HiFi.WithHkdFields (HiFi.OverFieldTy Csv.FromField Identity) Identity) record Identity
         , HiFi.ToRecord record
         )
    => Csv.FromNamedRecord (HkdCsv record) where
  parseNamedRecord m =
    let lookupField fieldName _ _ = Csv.lookup m (fromString fieldName)
     in MkHkdCsv . HiFi.fromHKD
          <$> HiFi.withInstances @(HiFi.OverFieldTy Csv.FromField Identity) lookupField

instance ( HiFi.FoldFields (HiFi.WithHkdFields (HiFi.OverFieldTy Csv.ToField Identity) Identity) record Identity
         , HiFi.FieldGetters record
         )
    => Csv.ToNamedRecord (HkdCsv record) where
  toNamedRecord (MkHkdCsv rec) =
    let hkd = toHKD rec
        mkField fieldName getter _ =
          Const [ Csv.namedField (fromString fieldName) (getter hkd) ]
     in Csv.namedRecord . getConst
          $ HiFi.withInstances @(OverFieldTy Csv.ToField Identity) mkField

data Person =
  MkPerson
    { name :: String
    , age :: Int
    } deriving Csv.FromNamedRecord via (HkdCsv Person)
      deriving Csv.ToNamedRecord via (HkdCsv Person)
```

### Nested Records

It's possible to have a record as a field in another record and have the inner
record promoted to its higher kinded data version along with the parent record.
This is done using the `NestHKD` type, which is a newtype wrapper akin to
`Data.Functor.Identity`.

```haskell
data Outer = Outer
  { foo :: NestHKD Inner
  , bar :: Bool
  }

data Inner = Inner
  { baz :: String
  }

hkd :: HKD Outer Maybe
hkd = mkHKD
  { foo = mkHKD
      { baz = Just "inner record"
      }
  , bar = Nothing
  }
```

### Limitations
- Currently supports GHC 9.2.x, 9.4.x, and 9.6.x
- Records must follow certain rules to be promotable to HKDs:
  - Nested records cannot result in inifinite recursion
  - Types of nested records cannot be type family applications
  - Existential type variables or constraint contexts are not allowed
