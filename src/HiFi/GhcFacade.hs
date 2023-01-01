{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module HiFi.GhcFacade
  ( module Ghc
  , getFieldPair
  , extractName
  , pattern HsPar'
  , mkStringExprFS'
  , mkTcPluginSolveResult
  , findImportedModule'
  , initDsTc'
  ) where

#if MIN_VERSION_ghc(9,4,0)
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
import           GHC.Settings.Constants as Ghc
import           GHC.Data.StringBuffer as Ghc
import           GHC.HsToCore.Binds as Ghc
import           GHC.HsToCore.Monad as Ghc hiding (newUnique)

#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Plugins as Ghc hiding (TcPlugin, varName, substTy, isInScope, extendTvSubst)
import           GHC.Tc.Types as Ghc
import           GHC.Core.TyCo.Rep as Ghc
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
import           GHC.Settings.Constants as Ghc
import           GHC.Data.StringBuffer as Ghc
import           GHC.HsToCore.Binds as Ghc
import           GHC.HsToCore.Monad as Ghc hiding (newUnique)

#endif

getFieldPair
  :: Ghc.HsRecUpdField Ghc.GhcPs
  -> (Ghc.FastString, Ghc.HsExpr Ghc.GhcPs)
#if MIN_VERSION_ghc(9,4,0)
getFieldPair Ghc.HsFieldBind {..} =
  ( extractFieldName $ Ghc.unLoc hfbLHS
  , Ghc.unLoc hfbRHS
  )
#elif MIN_VERSION_ghc(9,2,0)
getFieldPair Ghc.HsRecField {..} =
  ( extractFieldName $ Ghc.unLoc hsRecFieldLbl
  , Ghc.unLoc hsRecFieldArg
  )
#endif

extractFieldName :: Ghc.AmbiguousFieldOcc Ghc.GhcPs -> Ghc.FastString
extractFieldName = extractName . Ghc.rdrNameAmbiguousFieldOcc

extractName :: Ghc.RdrName -> Ghc.FastString
extractName = Ghc.occNameFS . Ghc.rdrNameOcc

pattern HsPar' :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
#if MIN_VERSION_ghc(9,4,0)
pattern HsPar' expr <- Ghc.HsPar _ _ expr _
#elif MIN_VERSION_ghc(9,2,0)
pattern HsPar' expr <- Ghc.HsPar _ expr
#endif

mkStringExprFS' :: Ghc.FastString -> Ghc.TcPluginM Ghc.CoreExpr
mkStringExprFS' fs = do
#if MIN_VERSION_ghc(9,4,0)
  stringIds <- Ghc.getMkStringIds Ghc.tcLookupId
  pure $ Ghc.mkStringExprFSWith stringIds fs
#elif MIN_VERSION_ghc(9,2,0)
  Ghc.unsafeTcPluginTcM $ Ghc.mkStringExprFS fs
#endif

mkTcPluginSolveResult
  :: [Ghc.Ct] -> [Ghc.Ct] -> [(Ghc.EvTerm, Ghc.Ct)]
#if MIN_VERSION_ghc(9,4,0)
  -> Ghc.TcPluginSolveResult
#elif MIN_VERSION_ghc(9,2,0)
  -> Ghc.TcPluginResult
#endif
mkTcPluginSolveResult newWanteds insolubles solveds =
#if MIN_VERSION_ghc(9,4,0)
  Ghc.TcPluginSolveResult
    { tcPluginInsolubleCts = insolubles
    , tcPluginSolvedCts = solveds
    , tcPluginNewCts = newWanteds
    }
#elif MIN_VERSION_ghc(9,2,0)
  if null solveds
     then TcPluginContradiction insolubles
     else TcPluginOk solveds newWanteds
#endif

findImportedModule' :: Ghc.ModuleName -> Ghc.TcPluginM Ghc.FindResult
findImportedModule' modName =
#if MIN_VERSION_ghc(9,4,0)
  Ghc.findImportedModule modName Ghc.NoPkgQual
#elif MIN_VERSION_ghc(9,2,0)
  Ghc.findImportedModule modName Nothing
#endif

initDsTc' :: Ghc.DsM a -> Ghc.TcM (Maybe a)
#if MIN_VERSION_ghc(9,4,0)
initDsTc' = fmap snd . Ghc.initDsTc
#elif MIN_VERSION_ghc(9,2,0)
initDsTc' = fmap Just . Ghc.initDsTc
#endif
