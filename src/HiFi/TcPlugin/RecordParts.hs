{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.TcPlugin.RecordParts
  ( RecordParts(..)
  , FieldParts(..)
  , FieldNesting(..)
  , getRecordFields
  ) where

import           Control.Monad
import           Control.Applicative (ZipList(..))
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import           Data.Monoid (Last(..))

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

getRecordFields :: PluginInputs -> Ghc.Type -> Maybe RecordParts
getRecordFields inputs = \case
  Ghc.TyConApp tyCon args
    | Ghc.isAlgTyCon tyCon
    , Ghc.DataTyCon{..} <- Ghc.algTyConRhs tyCon
    , [dataCon] <- data_cons -> do
      -- Don't allow existential ty vars
      guard $ length (Ghc.dataConUnivAndExTyCoVars dataCon) == length args
      -- Don't allow contexts
      guard . null $ Ghc.dataConTheta dataCon ++ Ghc.dataConStupidTheta dataCon
      let fieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon args
          fieldLabels = Ghc.flLabel <$> Ghc.dataConFieldLabels dataCon
          tcSubst = Ghc.zipTCvSubst (Ghc.dataConUnivAndExTyCoVars dataCon) args
          fieldSelectors = Ghc.flSelector <$> Ghc.dataConFieldLabels dataCon
      fieldNestings <-
        let go :: Ghc.Type -> StateT Integer Maybe FieldNesting
            go ty = StateT $ \ !ix ->
              case Ghc.tcSplitTyConApp_maybe ty of
                Just (con, [innerRecTy]) | Ghc.getName con == nestHkdName inputs -> do
                  innerRecParts <- getRecordFields inputs innerRecTy
                  (_, lastFieldParts) <-
                    getLast . foldMap (Last . Just) $ recordFields innerRecParts
                  let len = case fieldNesting lastFieldParts of
                              Unnested i -> i + 1
                              Nested i l _ _ -> i + l
                  Just (Nested ix len innerRecTy innerRecParts, ix + len)
                _ -> pure (Unnested ix, ix + 1)
         in evalStateT (traverse go fieldTys) 0
      guard . not $ null fieldTys
      guard $ length fieldTys == length fieldLabels
      -- add a guard for equality of rep arity and source arity?
      Just RecordParts
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
  _ -> Nothing
