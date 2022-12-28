module HiFi
  ( -- * Types
    HKD
  , FieldName(..)
  -- * API
  , module Api
  -- * Plugin
  , plugin
  -- delete
  , arrayFromList
  , unsafeCoerceF
  , instantiate
  ) where

import           HiFi.Api as Api
import qualified HiFi.GhcFacade as Ghc
import           HiFi.ParseResultAction (parseResultAction)
import           HiFi.TcPlugin (tcPlugin)
import           HiFi.Internal.Types (FieldName(..), HKD, arrayFromList, unsafeCoerceF, instantiate)

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.tcPlugin = const $ Just tcPlugin
  , Ghc.parsedResultAction = const parseResultAction
  }
