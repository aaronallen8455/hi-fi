{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module HiFi.TcPlugin.RecordParts
  ( RecordParts(..)
  , FieldParts(..)
  , FieldNesting(..)
  , InvalidHkdReason(..)
  , getRecordParts
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Control.Applicative (ZipList(..))
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import           Data.Coerce (coerce)
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid (Last(..))
import qualified Data.Set as S

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs

data RecordParts = RecordParts
  { recordCon :: !Ghc.DataCon
  , recordTyArgs :: ![Ghc.Type]
  , recordFields :: ![(Ghc.FastString, FieldParts)]
  , recordTyVarSubst :: !Ghc.TCvSubst -- Used to instantiate polymorphic fields
  }

data FieldParts = FieldParts
  { fieldSelName :: !Ghc.Name -- These must be instantiated if fields are polymorphic
  , fieldType :: !Ghc.Type
  , fieldNesting :: !FieldNesting
  }

data FieldNesting
  = Unnested !Integer
  | Nested
      !Integer -- offset
      !Integer -- length
      !Ghc.Type -- record ty
      !RecordParts

newtype TyOrd = TyOrd Ghc.Type

instance Eq TyOrd where
  (==) = coerce Ghc.eqType

instance Ord TyOrd where
  compare = coerce Ghc.nonDetCmpType

-- This does not resolve type families instances
expandSynonyms :: Ghc.Type -> Ghc.Type
expandSynonyms ty = fromMaybe ty $ Ghc.tcView ty

data InvalidHkdReason
  = UnsupportedTy Ghc.Type
  | DataConNotInScope Ghc.DataCon

getRecordParts
  :: PluginInputs
  -> S.Set TyOrd
  -> Ghc.Type
  -> ExceptT InvalidHkdReason Ghc.TcPluginM RecordParts
getRecordParts inputs visitedTys (expandSynonyms -> recTy) = case recTy of
  Ghc.TyConApp tyCon args
    | Ghc.isAlgTyCon tyCon
    , Ghc.DataTyCon{..} <- Ghc.algTyConRhs tyCon
    -- Don't allow infinite recursion of nested HKDs
    , not $ S.member (TyOrd recTy) visitedTys
    , [dataCon] <- data_cons
      -- Don't allow existential ty vars
    , length (Ghc.dataConUnivAndExTyCoVars dataCon) == length args
      -- Don't allow contexts
    , null $ Ghc.dataConTheta dataCon ++ Ghc.dataConStupidTheta dataCon -> do
      hsc <- lift $ Ghc.tcg_rdr_env . fst <$> Ghc.getEnvs
      let fieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon args
          fieldLabels = Ghc.flLabel <$> Ghc.dataConFieldLabels dataCon
          tcSubst = Ghc.zipTCvSubst (Ghc.dataConUnivAndExTyCoVars dataCon) args
          fieldSelectors = Ghc.flSelector <$> Ghc.dataConFieldLabels dataCon
          newVisitedTys = S.insert (TyOrd recTy) visitedTys

      when (null fieldTys || length fieldTys /= length fieldLabels)
        . throwE $ UnsupportedTy recTy

      -- Data con must be in scope
      when (isNothing . Ghc.lookupGRE_Name hsc $ Ghc.getName dataCon)
        . throwE $ DataConNotInScope dataCon

      fieldNestings <-
        let go :: Ghc.Type -> StateT Integer (ExceptT InvalidHkdReason Ghc.TcPluginM) FieldNesting
            go ty = StateT $ \ !ix ->
              case Ghc.tcSplitTyConApp_maybe ty of
                Just (con, [innerRecTy]) | Ghc.getName con == nestHkdName inputs -> do
                  innerRecParts <- getRecordParts inputs newVisitedTys innerRecTy
                  lastFieldParts <-
                    case getLast . foldMap (Last . Just)
                           $ recordFields innerRecParts of
                      Nothing -> throwE $ UnsupportedTy innerRecTy
                      Just (_, l) -> pure l
                  let len = case fieldNesting lastFieldParts of
                              Unnested i -> i + 1
                              Nested i l _ _ -> i + l
                  pure (Nested ix len innerRecTy innerRecParts, ix + len)
                _ -> pure (Unnested ix, ix + 1)
         in evalStateT (traverse go fieldTys) 0
      -- add a guard for equality of rep arity and source arity?
      pure RecordParts
        { recordCon = dataCon
        , recordTyArgs = args
        , recordFields = zip fieldLabels
                       . getZipList
                       $ FieldParts
                           <$> ZipList fieldSelectors
                           <*> ZipList fieldTys
                           <*> ZipList fieldNestings
        , recordTyVarSubst = tcSubst
        }
  _ -> throwE $ UnsupportedTy recTy
