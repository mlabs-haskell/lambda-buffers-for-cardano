{-# LANGUAGE NoImplicitPrelude #-}

module Demo.Validation (eqValidator) where

import LambdaBuffers.Demo.Plutus (EqDatum, EqRedeemer (EqRedeemer'IsEqual, EqRedeemer'IsNotEqual))
import LambdaBuffers.Runtime.Plutus ()
import PlutusTx (BuiltinData, FromData (fromBuiltinData))
import PlutusTx.Lift ()
import PlutusTx.Maybe (fromMaybe)
import PlutusTx.Prelude (Eq ((==)), not, traceError, ($))

{-# INLINEABLE eqValidator #-}
eqValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
eqValidator datum redeemer _ctx =
  let
    eqDatum = fromMaybe (traceError "[EQ] EqDatum was incorrect") $ fromBuiltinData @EqDatum datum
    eqRedeemer = fromMaybe (traceError "[EQ] EqDatum was incorrect") $ fromBuiltinData @EqRedeemer redeemer
    validates = case eqRedeemer of
      EqRedeemer'IsEqual dat -> (PlutusTx.Prelude.==) eqDatum dat
      EqRedeemer'IsNotEqual dat -> PlutusTx.Prelude.not $ (PlutusTx.Prelude.==) eqDatum dat
   in
    if validates then () else traceError "[EQ] Validation failed"
