{-# LANGUAGE NoImplicitPrelude #-}

module Demo.Validation (eqValidator) where

import LambdaBuffers.Demo.Plutus.PlutusTx (EqDatum, EqRedeemer (EqRedeemer'IsEqual, EqRedeemer'IsNotEqual))
import LambdaBuffers.Runtime.PlutusTx ()
import PlutusLedgerApi.Data.V3 (Datum (getDatum), Redeemer (getRedeemer), scriptContextRedeemer, scriptContextScriptInfo)
import PlutusLedgerApi.Data.V3 qualified as V3Data
import PlutusTx qualified
import PlutusTx.Lift ()
import PlutusTx.Maybe (Maybe (Just, Nothing))
import PlutusTx.Prelude (Bool, BuiltinUnit, Eq ((==)), check, traceError, (/=))

{-# INLINEABLE typedEqValidator #-}
typedEqValidator :: EqDatum -> EqRedeemer -> Bool
typedEqValidator eqDatum eqRedeemer =
  case eqRedeemer of
    EqRedeemer'IsEqual dat -> eqDatum == dat
    EqRedeemer'IsNotEqual dat -> eqDatum /= dat

{-# INLINEABLE eqValidator #-}
eqValidator :: V3Data.ScriptContext -> BuiltinUnit
eqValidator ctx =
  check (typedEqValidator eqDatum eqRedeemer)
  where
    eqDatum :: EqDatum
    eqDatum =
      case scriptContextScriptInfo ctx of
        V3Data.SpendingScript _TxOutRef (Just datum) ->
          case PlutusTx.fromBuiltinData @EqDatum (getDatum datum) of
            Just eqDatum' -> eqDatum'
            Nothing -> traceError "Couldn't parse EqDatum"
        _ -> traceError "Expected SpendingScript with a datum"

    eqRedeemer :: EqRedeemer
    eqRedeemer =
      case PlutusTx.fromBuiltinData @EqRedeemer (getRedeemer (scriptContextRedeemer ctx)) of
        Just eqRedeemer' -> eqRedeemer'
        Nothing -> traceError "Couldn't parse EqRedeemer"
