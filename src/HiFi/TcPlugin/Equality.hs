module HiFi.TcPlugin.Equality
  ( nestHKDEquality
  ) where

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs

-- | Produce an equality proof for a ~ NestHKD b or a new wanted if a and b
-- are not equal.
nestHKDEquality
  :: PluginInputs
  -> Ghc.Type
  -> Ghc.Type
  -> Maybe (Either Ghc.PredType Ghc.EvTerm)
nestHKDEquality inp ty1 ty2
  | Ghc.TyConApp ty2TyCon [innerTy2] <- ty2
  , Ghc.tyConName ty2TyCon == nestHkdName inp
  = Just $
    if Ghc.eqType ty1 innerTy2
       then
         let evTerm = Ghc.evCoercion
                    $ Ghc.mkUnivCo (Ghc.PluginProv "NestHKD")
                                   Ghc.Nominal
                                   ty1
                                   innerTy2
          in Right evTerm
       else
         let eqPred = Ghc.mkPrimEqPredRole Ghc.Nominal ty1 innerTy2
          in Left eqPred
  | otherwise = Nothing
