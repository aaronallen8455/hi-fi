{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HiFi
  ( HKD
  -- * API
  , mapEffect
  , recSequenceShallow
  , recSequence
  , toRecord
  , fromRecord
  , mkHKD
  , recZipWith
  , setField
  , fill
  -- * Plugin
  , plugin
  , indexArray
  , type RecArray
  , unsafeCoerceAny
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Coerce (coerce)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Kind
import qualified Data.List as List
import           Data.Maybe (catMaybes)
import qualified Data.Primitive.Array as A
import           Data.Proxy
import           Data.Traversable (for)
import qualified GHC.Exts as Exts
import           GHC.Records
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

import qualified HiFi.GhcFacade as Ghc

-- TODO type roles?
newtype HKD rec f =
  MkHKD (A.Array (f Exts.Any))

instance (HasField (name :: Symbol) rec a, IndexOfField name rec)
    => HasField name (HKD rec f) (f a) where
  getField (MkHKD arr) =
    unsafeCoerce $ A.indexArray arr (indexOfField @name @rec)

--------------------------------------------------------------------------------
-- Magic type classes
--------------------------------------------------------------------------------

type IndexOfField :: Symbol -> Type -> Constraint
class IndexOfField name rec where
  indexOfField :: Int

class FieldGetters rec where
  fieldGetters :: [rec -> Exts.Any]

-- Could build a core expr that applies the dataCon to each element from the
-- array. Will have to lookup the `indexArray` Id to do this.
-- Might be easier to construct an rn expr and then typecheck and desugar it.
-- Core might be easier because won't have to deal with coercions
class ToRecord rec where
  toRecord' :: A.Array Exts.Any -> rec

-- | Right fold over the fields of a record
type FoldFields :: (Type -> Constraint) -> Type -> (Type -> Type) -> Constraint
class FoldFields c rec f where
  foldFields :: (forall (name :: Symbol) a.
                  -- should these be passed as Dicts?
                  (c (f a), HasField name rec a, KnownSymbol name, IndexOfField name rec)
                    => Proxy name -> Proxy a -> x
                )
             -> acc
             -> (x -> acc -> acc)
             -> acc

class Instantiate rec f tuple where
  instantiate :: tuple -> HKD rec f

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

mapEffect :: (forall a. f a -> g a) -> HKD rec f -> HKD rec g
mapEffect f (MkHKD arr) = MkHKD $ f <$> arr

recSequenceShallow :: forall f g rec. Applicative f
                   => HKD rec (Compose f g) -> f (HKD rec g)
recSequenceShallow (MkHKD arr) = MkHKD <$> traverse getCompose arr

recSequence :: forall f rec. (Applicative f, ToRecord rec) => HKD rec f -> f rec
recSequence = fmap toRecord . recSequenceShallow @f @Identity . mapEffect (Compose . fmap Identity)

recZipWith :: (forall a. f a -> g a -> h a) -> HKD rec f -> HKD rec g -> HKD rec h
recZipWith f (MkHKD a) (MkHKD b) = MkHKD . A.arrayFromList $ do
  i <- [0 .. A.sizeofArray a]
  [ f (A.indexArray a i) (A.indexArray b i) ]

toRecord :: ToRecord rec => HKD rec Identity -> rec
toRecord (MkHKD arr) = toRecord' $ coerce arr

fromRecord :: forall rec. FieldGetters rec => rec -> HKD rec Identity
fromRecord rec =
  MkHKD . A.arrayFromList
        . fmap (Identity . ($ rec))
        $ fieldGetters @rec

mkHKD :: forall rec f tuple. Instantiate rec f tuple => tuple -> HKD rec f
mkHKD = instantiate @rec @f @tuple

setField
  :: forall name rec a f
   . (HasField name rec a, IndexOfField name rec)
  => f a
  -> HKD rec f
  -> HKD rec f
setField val (MkHKD arr) = MkHKD $ runST $ do
  marr <- A.unsafeThawArray arr
  A.writeArray marr (indexOfField @name @rec) (unsafeCoerce val)
  A.unsafeFreezeArray marr

fill :: forall rec f. FieldGetters rec => (forall a. f a) -> HKD rec f
fill x = MkHKD . A.arrayFromList $ x <$ fieldGetters @rec

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance FoldFields Semigroup rec f => Semigroup (HKD rec f) where
  a <> b =
    let go (Proxy :: Proxy name) _ =
          unsafeCoerce $ getField @name a <> getField @name b
        -- TODO would be more efficient to not construct the intermediate list
        fields =
          foldFields @Semigroup @rec @f go [] (:)
     in MkHKD $ A.arrayFromList fields

instance (FoldFields Semigroup rec f, FoldFields Monoid rec f) => Monoid (HKD rec f) where
  mempty =
    let go _ (Proxy :: Proxy a) = unsafeCoerce $ mempty @(f a)
        fields =
          foldFields @Monoid @rec @f go [] (:)
     in MkHKD $ A.arrayFromList fields

instance FoldFields Eq rec f => Eq (HKD rec f) where
  a == b =
    let go (Proxy :: Proxy name) _ = getField @name a == getField @name b
     in foldFields @Eq @rec @f go True (&&)

-- How to instantiate using record syntax?
-- Rewrite the record syntax as a tuple of tuples:
-- mkHKD @Rec @f ( ("fieldName", value)
--               , (..)
--               )
-- Can then do some form of type checking in the magic type class. Hopefully
-- there will be some way of emitting errors from the type checker plugin.
--
-- What about nested fields? Could have a magic type family that decides
-- whether to return `f a` or `f (HKD a f)`. Definitely complicates things
-- though and might be better left out for now. Higgledy does not do deep
-- conversion so maybe it is not required to be competitive?

--------------------------------------------------------------------------------
-- Plugin
--------------------------------------------------------------------------------

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.tcPlugin = const $ Just tcPlugin
  }

tcPlugin :: Ghc.TcPlugin
tcPlugin = Ghc.TcPlugin
  { Ghc.tcPluginInit = lookupInputs
  , Ghc.tcPluginSolve = tcSolver
  , Ghc.tcPluginRewrite = mempty -- \defs -> Ghc.unitUFM (remixTyFam defs) (tcPluginRewriter defs)
  , Ghc.tcPluginStop = \_ -> pure ()
  }

data PluginInputs =
  MkPluginInputs
    { indexOfFieldName :: Ghc.Name
    , fieldGettersName :: Ghc.Name
    , toRecordName :: Ghc.Name
    , foldFieldsName :: Ghc.Name
    , instantiateName :: Ghc.Name
    , indexArrayId :: Ghc.Id
    , recArrayTyCon :: Ghc.TyCon
    , unsafeCoerceAnyId :: Ghc.Id
    }

findModule :: String -> Ghc.TcPluginM Ghc.Module
findModule name = do
  findResult <- Ghc.findImportedModule (Ghc.mkModuleName name) Ghc.NoPkgQual
  case findResult of
    Ghc.Found _ res -> pure res
    _               -> error "preposterous!"

type RecArray = A.Array Exts.Any

indexArray :: RecArray -> Int -> Exts.Any
indexArray = A.indexArray

unsafeCoerceAny :: forall a. Exts.Any -> a
unsafeCoerceAny = unsafeCoerce

lookupInputs :: Ghc.TcPluginM PluginInputs
lookupInputs = do
  hiFiMod <- findModule "HiFi"

  indexOfFieldName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "IndexOfField")
  fieldGettersName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FieldGetters")
  toRecordName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "ToRecord")
  foldFieldsName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "FoldFields")
  instantiateName <- Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "Instantiate")
  indexArrayId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "indexArray")
  recArrayTyCon <- Ghc.tcLookupTyCon =<< Ghc.lookupOrig hiFiMod (Ghc.mkTcOcc "RecArray")
  unsafeCoerceAnyId <- Ghc.tcLookupId =<< Ghc.lookupOrig hiFiMod (Ghc.mkVarOcc "unsafeCoerceAny")
  pure MkPluginInputs{..}

tcSolver :: PluginInputs -> Ghc.TcPluginSolver
tcSolver MkPluginInputs{..} _env _givens wanteds = do
  solved <- fmap catMaybes . for wanteds $ \case
    ct@Ghc.CDictCan{ cc_class, cc_tyargs } -> do
      let clsName = Ghc.getName cc_class

         -- IndexOfField
      if | clsName == indexOfFieldName
         , [ getStrTyLitVal -> Just fieldName
           , fmap (map fst . snd) . getRecordFields -> Just fields
           ] <- cc_tyargs -> pure $ do
             ix <- List.elemIndex fieldName fields
             let expr = Ghc.mkUncheckedIntExpr $ fromIntegral ix
             Just (Ghc.EvExpr expr, ct)

         -- FieldGetters
         | clsName == fieldGettersName
         , [ recordTy ] <- cc_tyargs
         , Just fields <- map (snd . snd) . snd <$> getRecordFields recordTy
         -> do
             selVars <- map Ghc.Var <$> traverse Ghc.tcLookupId fields
             let funTy = Ghc.mkVisFunTyMany recordTy Ghc.anyTy
             pure $ Just (Ghc.EvExpr $ Ghc.mkListExpr funTy selVars, ct)

         -- ToRecord
         | clsName == toRecordName
         , [ recordTy ] <- cc_tyargs
         , Just (dataCon, fields) <- getRecordFields recordTy
         , Just arrType <- Ghc.synTyConRhs_maybe recArrayTyCon
         -> do
             arrBindName <- Ghc.unsafeTcPluginTcM
                          $ Ghc.newName (Ghc.mkOccName Ghc.varName "arr")

             let arrBind =
                   Ghc.mkLocalVar
                     Ghc.VanillaId
                     arrBindName
                     Ghc.Many
                     arrType
                     Ghc.vanillaIdInfo

                 accessor fieldTy ix =
                   Ghc.mkCoreApps (Ghc.Var unsafeCoerceAnyId)
                     [ Ghc.Type fieldTy
                     , Ghc.mkCoreApps (Ghc.Var indexArrayId)
                         [Ghc.Var arrBind, Ghc.mkUncheckedIntExpr ix]
                     ]
                 result =
                   Ghc.mkCoreLams [arrBind] $
                     Ghc.mkCoreConApps dataCon
                       $ do
                         (fst . snd -> fieldTy, ix) <- fields `zip` [0..]
                         [accessor fieldTy ix]
             pure $ Just (Ghc.EvExpr result, ct)

         | clsName == foldFieldsName ->
           undefined

         | clsName == instantiateName ->
           undefined

         | otherwise -> pure Nothing
    _ -> pure Nothing

  pure $ Ghc.TcPluginOk solved []

getStrTyLitVal :: Ghc.Type -> Maybe Ghc.FastString
getStrTyLitVal = \case
  Ghc.LitTy (Ghc.StrTyLit fs) -> Just fs
  _ -> Nothing

getRecordFields :: Ghc.Type -> Maybe (Ghc.DataCon, [(Ghc.FastString, (Ghc.Type, Ghc.Name))])
getRecordFields = \case
  Ghc.TyConApp tyCon args
    | Ghc.isAlgTyCon tyCon
    , Ghc.DataTyCon{..} <- Ghc.algTyConRhs tyCon
    , [dataCon] <- data_cons -> do
      let fieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon args
          fieldLabels = Ghc.flLabel <$> Ghc.dataConFieldLabels dataCon
          fieldSelectors = Ghc.flSelector <$> Ghc.dataConFieldLabels dataCon
      guard . not $ null fieldTys
      guard $ length fieldTys == length fieldLabels
      -- add a guard for equality of rep arity and source arity?
      Just (dataCon, zip fieldLabels $ zip fieldTys fieldSelectors)
  _ -> Nothing
