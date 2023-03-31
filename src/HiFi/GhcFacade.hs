{-# LANGUAGE ViewPatterns #-}
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
  , adaptSolver
  , hsLet'
  , coreView'
  , Subst'
  , pattern ManyTy'
  , pattern HsAppType'
  ) where

#if MIN_VERSION_ghc(9,6,0)
import           GHC as Ghc (lookupName)
import           GHC.Plugins as Ghc hiding (DefaultingPlugin, TcPlugin, varName, isInScope, extendTvSubst)
import           GHC.Tc.Types as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Core.Reduction as Ghc
import           GHC.Core.Type as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Tc.Plugin as Ghc
import           GHC.Tc.Types.Evidence as Ghc
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Types.TyThing as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (getEnvs, getTopEnv, newUnique, getSrcSpanM, getNamePprCtx)
import           GHC.Types.Name as Ghc
import           GHC.Core.Class as Ghc
import           GHC.Core.Predicate as Ghc
import           Language.Haskell.Syntax.Basic as Ghc
import           Language.Haskell.Syntax.Binds as Ghc
import           Language.Haskell.Syntax.Concrete as Ghc
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
import           GHC.Tc.Solver.Monad as Ghc (runTcS, TcS, runTcSWithEvBinds)
import           GHC.Tc.Solver.Interact as Ghc (solveSimpleWanteds, solveSimpleGivens)
import           GHC.Settings.Constants as Ghc
import           GHC.Data.StringBuffer as Ghc
import           GHC.HsToCore.Binds as Ghc
import           GHC.HsToCore.Monad as Ghc hiding (newUnique)
import           GHC.Tc.Types.Origin as Ghc
import           GHC.Parser.Annotation as Ghc (noComments, noAnn)
import           GHC.Builtin.Types.Prim as Ghc

#elif MIN_VERSION_ghc(9,4,0)
import           GHC as Ghc (lookupName)
import           GHC.Plugins as Ghc hiding (DefaultingPlugin, TcPlugin, varName, substTy, isInScope, extendTvSubst)
import           GHC.Tc.Types as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Core.Reduction as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Tc.Plugin as Ghc
import           GHC.Tc.Types.Evidence as Ghc
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Types.TyThing as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (getEnvs, getTopEnv, newUnique, getPrintUnqualified, getSrcSpanM)
import           GHC.Types.Name as Ghc
import           GHC.Core.Class as Ghc
import           GHC.Core.Predicate as Ghc
import           Language.Haskell.Syntax.Binds as Ghc
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
import           GHC.Tc.Solver.Monad as Ghc (runTcS, TcS, runTcSWithEvBinds)
import           GHC.Tc.Solver.Interact as Ghc (solveSimpleWanteds, solveSimpleGivens)
import           GHC.Settings.Constants as Ghc
import           GHC.Data.StringBuffer as Ghc
import           GHC.HsToCore.Binds as Ghc
import           GHC.HsToCore.Monad as Ghc hiding (newUnique)
import           GHC.Tc.Types.Origin as Ghc
import           GHC.Parser.Annotation as Ghc (noComments, noAnn)

#elif MIN_VERSION_ghc(9,2,0)
import           GHC as Ghc (lookupName)
import           GHC.Data.Bag as Ghc
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
import           Language.Haskell.Syntax.Binds as Ghc
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
import           GHC.Tc.Solver.Monad as Ghc (runTcS, TcS, runTcSWithEvBinds)
import           GHC.Tc.Solver.Interact as Ghc (solveSimpleWanteds, solveSimpleGivens)
import           GHC.Settings.Constants as Ghc
import           GHC.Data.StringBuffer as Ghc
import           GHC.HsToCore.Binds as Ghc
import           GHC.HsToCore.Monad as Ghc hiding (newUnique)
import           GHC.Tc.Types.Origin as Ghc
import           GHC.Parser.Annotation as Ghc (noComments, noAnn)

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

adaptSolver
  :: ( Ghc.EvBindsVar
    -> [Ghc.Ct]
    -> [Ghc.Ct]
#if MIN_VERSION_ghc(9,4,0)
    -> Ghc.TcPluginM Ghc.TcPluginSolveResult
#elif MIN_VERSION_ghc(9,2,0)
    -> Ghc.TcPluginM Ghc.TcPluginResult
#endif
    )
  -> Ghc.TcPluginSolver
#if MIN_VERSION_ghc(9,4,0)
adaptSolver = id
#elif MIN_VERSION_ghc(9,2,0)
adaptSolver solver given _derived wanted = do
  evBindsVar <- Ghc.getEvBindsTcPluginM
  solver evBindsVar given wanted
#endif

hsLet' :: Ghc.HsLocalBindsLR Ghc.GhcPs Ghc.GhcPs
       -> Ghc.LHsExpr Ghc.GhcPs
       -> Ghc.HsExpr Ghc.GhcPs
hsLet' binds body =
#if MIN_VERSION_ghc(9,4,0)
  Ghc.HsLet Ghc.noAnn Ghc.noHsTok binds Ghc.noHsTok body
#elif MIN_VERSION_ghc(9,2,0)
  Ghc.HsLet Ghc.noAnn binds body
#endif

coreView' :: Ghc.Type -> Maybe Ghc.Type
coreView' =
#if MIN_VERSION_ghc(9,6,0)
  Ghc.coreView
#else
  Ghc.tcView
#endif

type Subst' =
#if MIN_VERSION_ghc(9,6,0)
  Ghc.Subst
#else
  Ghc.TCvSubst
#endif

pattern ManyTy'
#if MIN_VERSION_ghc(9,6,0)
  :: Ghc.Type
pattern ManyTy' = Ghc.ManyTy
#else
  :: Ghc.Mult
pattern ManyTy' = Ghc.Many
#endif

pattern HsAppType' :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.HsWildCardBndrs Ghc.GhcPs (Ghc.LHsType Ghc.GhcPs) -> Ghc.HsExpr Ghc.GhcPs
#if MIN_VERSION_ghc(9,6,0)
pattern HsAppType' expr typeApp <-
  Ghc.HsAppType _ expr _ typeApp
  where
    HsAppType' expr typeApp = Ghc.HsAppType Ghc.NoExtField expr Ghc.noHsTok typeApp
#else
pattern HsAppType' expr typeApp <-
  Ghc.HsAppType _ expr typeApp
  where
    HsAppType' expr typeApp = Ghc.HsAppType Ghc.noSrcSpan expr typeApp
#endif
