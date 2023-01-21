{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module HiFi.TcPlugin.Instantiate
  ( gatherTupleFieldsAndBuildExpr
  , mkFieldTypeCheckWanteds
  ) where

import           Control.Monad.Trans.State

import qualified HiFi.GhcFacade as Ghc
import           HiFi.TcPlugin.PluginInputs
import           HiFi.TcPlugin.RecordParts
import           HiFi.TcPlugin.Utils (makeWantedCt)

gatherTupleFieldsAndBuildExpr
  :: PluginInputs
  -> [(Ghc.FastString, FieldParts)] -- field names and tyeps in order
  -> Ghc.Type -- record type
  -> Ghc.Type -- tuple
  -> Ghc.Type -- effect constructor
  -> Ghc.TcPluginM ([(Ghc.FastString, Ghc.Type)], Ghc.CoreExpr)
gatherTupleFieldsAndBuildExpr inp@MkPluginInputs{..} fieldNamesTys recTy tupleTy effectConTy = do
    tupleName <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "tuple")
    let tupleBind = Ghc.mkLocalIdOrCoVar tupleName Ghc.Many tupleTy

    (pairs, expr) <- go [] tupleBind tupleTy
    pure (pairs, Ghc.mkCoreLams [tupleBind] expr)
  where
    getFieldPairFromTy ty = StateT $ \exprBuilder -> case ty of
      Ghc.TyConApp _ [fieldNameTy@(Ghc.TyConApp fnTyCon fieldNameLit), fieldTy]
        | Ghc.getName fnTyCon == Ghc.getName fieldNameTyCon
        , [Ghc.LitTy (Ghc.StrTyLit fs)] <- fieldNameLit -> do
          fieldVarName
            <- Ghc.unsafeTcPluginTcM
             $ Ghc.newName (Ghc.mkOccName Ghc.varName "field")
          -- This is variable that the RHS value is bound to
          let fieldVarId = Ghc.mkLocalIdOrCoVar fieldVarName Ghc.Many fieldTy

          fieldPairName
            <- Ghc.unsafeTcPluginTcM
             $ Ghc.newName (Ghc.mkOccName Ghc.varName "fieldPair")
          let fieldPairId = Ghc.mkLocalIdOrCoVar fieldPairName Ghc.Many ty
              mkPairCase =
                Ghc.mkSingleAltCase
                  (Ghc.Var fieldPairId)
                  (Ghc.mkWildValBinder Ghc.Many ty)
                  (Ghc.DataAlt $ Ghc.tupleDataCon Ghc.Boxed 2)
                  [Ghc.mkWildValBinder Ghc.Many fieldNameTy, fieldVarId]

          pure ( (fieldPairId, (fs, (fieldVarId, fieldTy)))
               , mkPairCase . exprBuilder
               )

      _ -> fail "unexpected tuple structure in gatherTupleFieldsAndBuildExpr"

    go ids tupleBind ty = case ty of
      Ghc.TyConApp tyCon (restTy : fieldTys)
        | Ghc.isAlgTyCon tyCon
        , Ghc.TupleTyCon{} <- Ghc.algTyConRhs tyCon -> do
            (fields, exprBuilder)
              <- (`runStateT` id)
               $ traverse getFieldPairFromTy fieldTys

            restVarName
              <- Ghc.unsafeTcPluginTcM
               $ Ghc.newName (Ghc.mkOccName Ghc.varName "rest")

            let restVarId = Ghc.mkLocalIdOrCoVar restVarName Ghc.Many restTy

            (resultIds, nextExpr)
              <- go (map snd fields ++ ids)
                    restVarId
                    restTy

            let expr = exprBuilder nextExpr
                tupleCase =
                  Ghc.mkSingleAltCase
                    (Ghc.Var tupleBind)
                    (Ghc.mkWildValBinder Ghc.Many ty)
                    (Ghc.DataAlt $ Ghc.tupleDataCon Ghc.Boxed (length fieldTys + 1))
                    (restVarId : (fst <$> fields))
                    expr

            pure (resultIds, tupleCase)

      -- the tuple nesting ends in a unit
      Ghc.TyConApp tyCon []
        | tyCon == Ghc.unitTyCon
        -> do
          let idsMap = Ghc.listToUFM (map (fmap fst) ids)
              fieldExprsInOrder :: [Ghc.CoreExpr]
              fieldExprsInOrder = do
                (label, fieldParts) <- fieldNamesTys
                Just fieldId <- [Ghc.lookupUFM idsMap label] -- actual field validation is done elsewhere
                case fieldNesting fieldParts of
                  Unnested _ ->
                    [Ghc.mkCoreApps (Ghc.Var unsafeCoerceFId)
                                    [ Ghc.Type effectConTy
                                    , Ghc.Type $ fieldType fieldParts
                                    , Ghc.Var fieldId
                                    ]]
                  Nested _ len nestRecTy _ ->
                    destructHKD inp nestRecTy effectConTy fieldId len
              fieldListExpr =
                Ghc.mkListExpr
                  (Ghc.mkAppTys effectConTy [Ghc.anyTypeOfKind Ghc.liftedTypeKind])
                  fieldExprsInOrder
              arrExpr =
                Ghc.mkCoreApps
                  (Ghc.Var arrayFromListId)
                  [Ghc.Type recTy, Ghc.Type effectConTy, fieldListExpr]
          pure (map (fmap snd) ids, arrExpr)

      _ -> fail "garbage input to 'gatherTupleFieldsAndBuildExpr'"

-- Given a variable for an HKD, produce exprs for each element in its array
destructHKD
  :: PluginInputs
  -> Ghc.Type
  -> Ghc.Type
  -> Ghc.Var
  -> Integer
  -> [Ghc.CoreExpr]
destructHKD inputs recTy effectTy hkdVar arrSize = do
  ix <- [0..arrSize - 1]
  pure $ Ghc.mkCoreApps (Ghc.Var $ indexArrayId inputs)
    [ Ghc.Type recTy
    , Ghc.Type effectTy
    , Ghc.Var hkdVar
    , Ghc.mkUncheckedIntExpr ix
    ]

mkFieldTypeCheckWanteds
  :: PluginInputs
  -> Ghc.CtLoc
  -> Ghc.Type
  -> Ghc.UniqFM Ghc.FastString (Ghc.FastString, Ghc.Type)
  -> [(Ghc.FastString, Ghc.Type)]
  -> Ghc.Type -- effect constructor
  -> Ghc.TcPluginM [Ghc.Ct]
mkFieldTypeCheckWanteds inp ctLoc recTy recordFieldMap tuplePairs effectCon = do
  let matchResults = getFieldMatchResults recordFieldMap tuplePairs
  let go = \case
        Match labelFs recFieldTy tupleTy -> do
          let fieldNameTy = Ghc.LitTy $ Ghc.StrTyLit labelFs
              classArgs = [ fieldNameTy
                          , effectCon
                          , recFieldTy
                          , tupleTy
                          ]
              ctLocWithFieldName = ctLoc
                { Ghc.ctl_origin = Ghc.HasFieldOrigin labelFs
                }
          makeWantedCt ctLocWithFieldName (fieldTypeCheckClass inp) classArgs

        Missing labelFs -> do
          let fieldNameTy = Ghc.LitTy $ Ghc.StrTyLit labelFs
              classArgs = [ fieldNameTy
                          , recTy
                          ]
          makeWantedCt ctLoc (missingFieldClass inp) classArgs
        Extra labelFs -> do
          let fieldNameTy = Ghc.LitTy $ Ghc.StrTyLit labelFs
              classArgs = [ fieldNameTy
                          , recTy
                          ]
          makeWantedCt ctLoc (unknownFieldClass inp) classArgs
  traverse go $ Ghc.nonDetEltsUFM matchResults

data LabelMatchResult
  = Match Ghc.FastString Ghc.Type Ghc.Type
  | Missing Ghc.FastString
  | Extra Ghc.FastString

getFieldMatchResults
  :: Ghc.UniqFM Ghc.FastString (Ghc.FastString, Ghc.Type)
  -> [(Ghc.FastString, Ghc.Type)]
  -> Ghc.UniqFM Ghc.FastString LabelMatchResult
getFieldMatchResults recordFieldMap tuplePairs = do
  let tupleFieldMap = Ghc.listToUFM $ zip (fst <$> tuplePairs) tuplePairs

      inBoth (_, x) (label, y) = Just $ Match label x y
      onlyInRecord = fmap (\(label, _) -> Missing label)
      onlyInTuple = fmap (\(label, _) -> Extra label)

   in Ghc.mergeUFM
        inBoth
        onlyInRecord
        onlyInTuple
        recordFieldMap
        tupleFieldMap
