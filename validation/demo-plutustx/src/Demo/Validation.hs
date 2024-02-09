{-# LANGUAGE NoImplicitPrelude #-}

module Demo.Validation (eqValidator) where

import LambdaBuffers.Demo.Plutus (EqDatum, EqRedeemer (EqRedeemer'IsEqual, EqRedeemer'IsNotEqual))
import LambdaBuffers.Runtime.Plutus ()
import PlutusTx (BuiltinData, FromData (fromBuiltinData))
import PlutusTx.Lift ()
import PlutusTx.Maybe (Maybe (Just, Nothing))
import PlutusTx.Prelude (Bool (False), Eq ((==)), error, not, trace, ($))

{-# INLINEABLE eqValidator #-}
eqValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
eqValidator datum redeemer _ctx =
  let
    mayEqDatum = fromBuiltinData @EqDatum datum
    mayEqRedeemer = fromBuiltinData @EqRedeemer redeemer
    validates = case mayEqRedeemer of
      Just (EqRedeemer'IsEqual dat) -> (PlutusTx.Prelude.==) mayEqDatum (Just dat)
      Just (EqRedeemer'IsNotEqual dat) -> PlutusTx.Prelude.not $ (PlutusTx.Prelude.==) mayEqDatum (Just dat)
      Nothing -> False
   in
    if validates
      then ()
      else trace "[EQ] Validation failed" (error ())
