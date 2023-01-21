{-# LANGUAGE RecordWildCards #-}
module HiFi.TcPlugin.PluginInputs
  ( PluginInputs(..)
  , lookupInputs
  ) where

import qualified HiFi.GhcFacade as Ghc

data PluginInputs =
  MkPluginInputs
    { fieldGettersName    :: !Ghc.Name
    , toRecordName        :: !Ghc.Name
    , foldFieldsName      :: !Ghc.Name
    , instantiateName     :: !Ghc.Name
    , indexArrayId        :: !Ghc.Id
    , recArrayTyCon       :: !Ghc.TyCon
    , fieldNameTyCon      :: !Ghc.TyCon
    , mkFieldNameDataCon  :: !Ghc.DataCon
    , arrayFromListId     :: !Ghc.Id
    , fieldTypeCheckClass :: !Ghc.Class
    , hasFieldClass       :: !Ghc.Class
    , knownSymbolClass    :: !Ghc.Class
    , hkdTyCon            :: !Ghc.TyCon
    , missingFieldClass   :: !Ghc.Class
    , unknownFieldClass   :: !Ghc.Class
    , unsafeCoerceFId     :: !Ghc.Id
    , identityTyCon       :: !Ghc.TyCon
    , hkdHasFieldName     :: !Ghc.Name
    , hkdSetFieldName     :: !Ghc.Name
    , nestHkdName         :: !Ghc.Name
    , getInnerRecId       :: !Ghc.Id
    , setInnerRecId       :: !Ghc.Id
    , writeArrayId        :: !Ghc.Id
    , fieldTyTyCon        :: !Ghc.TyCon
    }

findModule :: String -> Ghc.TcPluginM Ghc.Module
findModule name = do
  findResult <- Ghc.findImportedModule' (Ghc.mkModuleName name)
  case findResult of
    Ghc.Found _ res -> pure res
    _               -> error "preposterous!"

lookupInputs :: Ghc.TcPluginM PluginInputs
lookupInputs = do
  hiFiMod <- findModule "HiFi.Internal.Types"
  identityMod <- findModule "Data.Functor.Identity"

  fieldGettersName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldGetters")
  toRecordName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "ToRecord")
  foldFieldsName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FoldFields")
  instantiateName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "Instantiate")
  indexArrayId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "indexArray")
  recArrayTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "RecArray")
  fieldNameTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldName")
  mkFieldNameDataCon <- Ghc.tcLookupDataCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkDataOcc "MkFieldName")
  arrayFromListId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "arrayFromList")
  fieldTypeCheckClass <- Ghc.tcLookupClass =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldTypeCheck")
  hasFieldClass <- Ghc.tcLookupClass Ghc.hasFieldClassName
  knownSymbolClass <- Ghc.tcLookupClass Ghc.knownSymbolClassName
  hkdTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "HKD")
  missingFieldClass <- Ghc.tcLookupClass =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "MissingField")
  unknownFieldClass <- Ghc.tcLookupClass =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "UnknownField")
  unsafeCoerceFId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "unsafeCoerceF")
  identityTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig identityMod (Ghc.mkTcOcc "Identity")
  hkdHasFieldName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "HkdHasField")
  hkdSetFieldName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "HkdSetField")
  nestHkdName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "NestHKD")
  getInnerRecId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "getInnerRec")
  setInnerRecId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "setInnerRec")
  writeArrayId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "writeArray")
  fieldTyTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldTy")

  pure MkPluginInputs{..}
