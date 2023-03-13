module HiFi.TcPlugin.FieldGetters
  ( mkFieldGetters
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.List as List

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.RecordParts

-- | Produce a list of accessor functions for each field in the HKD, including
-- nested HKDs.
mkFieldGetters
  :: Ghc.Type
  -> Ghc.Subst'
  -> FieldParts
  -> Ghc.TcPluginM (Maybe [Ghc.CoreExpr])
mkFieldGetters recTy subst fp =
    (traverse . traverse) buildExpr
      =<< runMaybeT (collectSels subst fp)
  where
    collectSels :: Ghc.Subst' -> FieldParts -> MaybeT Ghc.TcPluginM [[Ghc.CoreExpr]]
    collectSels tcSubst fieldParts = do
      selId <- lift . Ghc.tcLookupId $ fieldSelName fieldParts
      selExpr <- MaybeT . pure $ instantiateSelector (Ghc.Var selId) tcSubst
      case fieldNesting fieldParts of
        Unnested{} -> pure [[selExpr]]
        Nested _ _ _ recParts
          | let newSubst = Ghc.composeTCvSubst (recordTyVarSubst recParts) tcSubst
          -> fmap (map (selExpr :) . concat)
           . traverse (collectSels newSubst . snd)
           $ recordFields recParts

    buildExpr :: [Ghc.CoreExpr] -> Ghc.TcPluginM Ghc.CoreExpr
    buildExpr [selector] = pure selector
    buildExpr selectors = do
      recName <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "rec")
      let recBind = Ghc.mkLocalIdOrCoVar recName Ghc.ManyTy' recTy
          go acc selector = Ghc.mkCoreApps selector [acc]
          expr = List.foldl' go (Ghc.Var recBind) selectors
      pure $ Ghc.mkCoreLams [recBind] expr

instantiateSelector :: Ghc.CoreExpr -> Ghc.Subst' -> Maybe Ghc.CoreExpr
instantiateSelector sel tcSubst = do
  case Ghc.splitForAllTyCoVar_maybe (Ghc.exprType sel) of
    Nothing -> pure sel
    Just (arg, _) -> do
      guard $ Ghc.isTyVar arg
      sub <- Ghc.lookupTyVar tcSubst arg
      let applied = Ghc.mkCoreApps sel [Ghc.Type sub]
      instantiateSelector applied tcSubst
