module Record
  ( R
  , R2(..)
  ) where

data R = MkR { foo :: Bool, bar :: String }
data R2 = MkR2 { foo2 :: Bool, bar2 :: String }
