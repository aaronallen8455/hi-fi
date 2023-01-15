{-# LANGUAGE DataKinds #-}
module HiFi.StringSing
  ( StringSing(..)
  , toFieldName
  , allowedChar
  ) where

import           Data.Char (ord)
import           Data.Kind (Type)
import           GHC.TypeLits (ConsSymbol, Symbol)

import           HiFi.Internal.Types (FieldName(..))

toFieldName :: StringSing str -> FieldName str
toFieldName _ = MkFieldName

allowedChar :: Char -> Bool
allowedChar c =
  let i = ord c
   in (i >= 97 && i <= 122) -- lowercase letters
   || (i >= 65 && i <= 90) -- uppercase letters
   || i == 95 -- underscore
   || i == 39 -- apostrophe
   || (i >= 48 && i <= 57) -- digits

type StringSing :: Symbol -> Type
-- | Singleton for type level strings. This allows the parse result plugin to
-- generate a value whose type reflects a field name, so that the DataKinds
-- extension is not required. Only the subset of common characters is
-- represented and if any other character appears in a field name then
-- DataKinds will be necessary.
data StringSing str where
  SSA :: StringSing str -> StringSing (ConsSymbol 'A' str)
  SSB :: StringSing str -> StringSing (ConsSymbol 'B' str)
  SSC :: StringSing str -> StringSing (ConsSymbol 'C' str)
  SSD :: StringSing str -> StringSing (ConsSymbol 'D' str)
  SSE :: StringSing str -> StringSing (ConsSymbol 'E' str)
  SSF :: StringSing str -> StringSing (ConsSymbol 'F' str)
  SSG :: StringSing str -> StringSing (ConsSymbol 'G' str)
  SSH :: StringSing str -> StringSing (ConsSymbol 'H' str)
  SSI :: StringSing str -> StringSing (ConsSymbol 'I' str)
  SSJ :: StringSing str -> StringSing (ConsSymbol 'J' str)
  SSK :: StringSing str -> StringSing (ConsSymbol 'K' str)
  SSL :: StringSing str -> StringSing (ConsSymbol 'L' str)
  SSM :: StringSing str -> StringSing (ConsSymbol 'M' str)
  SSN :: StringSing str -> StringSing (ConsSymbol 'N' str)
  SSO :: StringSing str -> StringSing (ConsSymbol 'O' str)
  SSP :: StringSing str -> StringSing (ConsSymbol 'P' str)
  SSQ :: StringSing str -> StringSing (ConsSymbol 'Q' str)
  SSR :: StringSing str -> StringSing (ConsSymbol 'R' str)
  SSS :: StringSing str -> StringSing (ConsSymbol 'S' str)
  SST :: StringSing str -> StringSing (ConsSymbol 'T' str)
  SSU :: StringSing str -> StringSing (ConsSymbol 'U' str)
  SSV :: StringSing str -> StringSing (ConsSymbol 'V' str)
  SSW :: StringSing str -> StringSing (ConsSymbol 'W' str)
  SSX :: StringSing str -> StringSing (ConsSymbol 'X' str)
  SSY :: StringSing str -> StringSing (ConsSymbol 'Y' str)
  SSZ :: StringSing str -> StringSing (ConsSymbol 'Z' str)
  SSa :: StringSing str -> StringSing (ConsSymbol 'a' str)
  SSb :: StringSing str -> StringSing (ConsSymbol 'b' str)
  SSc :: StringSing str -> StringSing (ConsSymbol 'c' str)
  SSd :: StringSing str -> StringSing (ConsSymbol 'd' str)
  SSe :: StringSing str -> StringSing (ConsSymbol 'e' str)
  SSf :: StringSing str -> StringSing (ConsSymbol 'f' str)
  SSg :: StringSing str -> StringSing (ConsSymbol 'g' str)
  SSh :: StringSing str -> StringSing (ConsSymbol 'h' str)
  SSi :: StringSing str -> StringSing (ConsSymbol 'i' str)
  SSj :: StringSing str -> StringSing (ConsSymbol 'j' str)
  SSk :: StringSing str -> StringSing (ConsSymbol 'k' str)
  SSl :: StringSing str -> StringSing (ConsSymbol 'l' str)
  SSm :: StringSing str -> StringSing (ConsSymbol 'm' str)
  SSn :: StringSing str -> StringSing (ConsSymbol 'n' str)
  SSo :: StringSing str -> StringSing (ConsSymbol 'o' str)
  SSp :: StringSing str -> StringSing (ConsSymbol 'p' str)
  SSq :: StringSing str -> StringSing (ConsSymbol 'q' str)
  SSr :: StringSing str -> StringSing (ConsSymbol 'r' str)
  SSs :: StringSing str -> StringSing (ConsSymbol 's' str)
  SSt :: StringSing str -> StringSing (ConsSymbol 't' str)
  SSu :: StringSing str -> StringSing (ConsSymbol 'u' str)
  SSv :: StringSing str -> StringSing (ConsSymbol 'v' str)
  SSw :: StringSing str -> StringSing (ConsSymbol 'w' str)
  SSx :: StringSing str -> StringSing (ConsSymbol 'x' str)
  SSy :: StringSing str -> StringSing (ConsSymbol 'y' str)
  SSz :: StringSing str -> StringSing (ConsSymbol 'z' str)
  SS0 :: StringSing str -> StringSing (ConsSymbol '0' str)
  SS1 :: StringSing str -> StringSing (ConsSymbol '1' str)
  SS2 :: StringSing str -> StringSing (ConsSymbol '2' str)
  SS3 :: StringSing str -> StringSing (ConsSymbol '3' str)
  SS4 :: StringSing str -> StringSing (ConsSymbol '4' str)
  SS5 :: StringSing str -> StringSing (ConsSymbol '5' str)
  SS6 :: StringSing str -> StringSing (ConsSymbol '6' str)
  SS7 :: StringSing str -> StringSing (ConsSymbol '7' str)
  SS8 :: StringSing str -> StringSing (ConsSymbol '8' str)
  SS9 :: StringSing str -> StringSing (ConsSymbol '9' str)
  SS_ :: StringSing str -> StringSing (ConsSymbol '_' str)
  SS' :: StringSing str -> StringSing (ConsSymbol '\'' str)
  SSNil :: StringSing ""
