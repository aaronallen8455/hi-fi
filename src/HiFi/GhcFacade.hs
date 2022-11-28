module HiFi.GhcFacade
  ( module Ghc
  ) where

import           GHC.Plugins as Ghc hiding (DefaultingPlugin, TcPlugin, varName, substTy, isInScope, extendTvSubst)
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
import           Language.Haskell.Syntax.Expr as Ghc
import           Language.Haskell.Syntax.Extension as Ghc
import           Language.Haskell.Syntax.Type as Ghc
import           Language.Haskell.Syntax.Pat as Ghc
import           GHC.Hs.Extension as Ghc
import           GHC.Types.SourceText as Ghc
import           GHC.Hs.Type as Ghc
import           GHC.Parser.Annotation as Ghc (noSrcSpanA, noLocA)
import           GHC.Hs.Utils as Ghc
import           GHC.Hs as Ghc (HsParsedModule(..), HsModule(..))
import           GHC.Tc.Utils.TcType as Ghc
import           GHC.Builtin.Names as Ghc
import           GHC.Core.InstEnv as Ghc
import           GHC.Tc.Instance.Class as Ghc
import           GHC.Tc.Solver.Monad as Ghc (runTcS)
import           GHC.Tc.Solver.Interact as Ghc (solveSimpleWanteds)
