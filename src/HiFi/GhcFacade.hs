module HiFi.GhcFacade
  ( module Ghc
  ) where

import           GHC.Plugins as Ghc hiding (DefaultingPlugin, TcPlugin, varName)
import           GHC.Tc.Types as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Core.Reduction as Ghc
import           GHC.Tc.Plugin as Ghc
import           GHC.Tc.Types.Evidence as Ghc
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Types.TyThing as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (getEnvs, getTopEnv, newUnique, getPrintUnqualified, getSrcSpanM)
import           GHC.Types.Name as Ghc
import           GHC.Core.Class as Ghc
import           GHC.Core.Predicate as Ghc
