-- Demo.Transactions module contains pure functions that build constraint/lookup pairs (ie. they only build transactions)
module Demo.Transactions
  ( createValueTx, inputIsEqualTx, inputIsNotEqualTx
  ) where

import Contract.PlutusData (Datum(..), Redeemer(..), toData)
import Contract.Prelude ((/\))
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (DatumPresence(..), mustPayToScript, mustSpendScriptOutput) as Constraints
import Contract.TxConstraints (TxConstraints)
import Contract.Value (lovelaceValueOf) as Value
import Ctl.Internal.Plutus.Types.Transaction as Plutus
import Ctl.Internal.Types.Scripts (ValidatorHash)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Data.Tuple.Nested (type (/\))
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.Demo.Plutus (EqDatum, EqRedeemer(..))

-- | Transaction that stores a EqDatum value at the Eq Validator.
createValueTx :: ValidatorHash -> EqDatum -> TxConstraints
createValueTx eqValidatorAddress eqDatum =
  Constraints.mustPayToScript
    eqValidatorAddress
    (Datum (toData eqDatum))
    Constraints.DatumInline
    (Value.lovelaceValueOf (BigInt.fromInt 10_000_000))

-- | `inputIsEqualTx eqValidator eqValidatorUtxos txIn eqDatum` make a transaction that checks if the EqDatum stored at the EqValidator's `txIn` is equal to the provided one in `eqDatum`.
inputIsEqualTx :: Validator -> Map TransactionInput Plutus.TransactionOutputWithRefScript -> TransactionInput -> EqDatum -> TxConstraints /\ ScriptLookups
inputIsEqualTx eqValidator eqValidatorUtxos eqValidatorTxIn eqDatum =
  let
    constraints = Constraints.mustSpendScriptOutput eqValidatorTxIn (Redeemer (toData (EqRedeemer'IsEqual eqDatum)))

    lookups = Lookups.unspentOutputs eqValidatorUtxos <> Lookups.validator eqValidator
  in
    constraints /\ lookups

-- | `inputIsNotEqualTx eqValidator eqValidatorUtxos txIn eqDatum` make a transaction that checks if the EqDatum stored at the EqValidator's `txIn` is NOT equal to the provided one in `eqDatum`.
inputIsNotEqualTx :: Validator -> Map TransactionInput Plutus.TransactionOutputWithRefScript -> TransactionInput -> EqDatum -> TxConstraints /\ ScriptLookups
inputIsNotEqualTx eqValidator eqValidatorUtxos eqValidatorTxIn eqDatum =
  let
    constraints = Constraints.mustSpendScriptOutput eqValidatorTxIn (Redeemer (toData (EqRedeemer'IsNotEqual eqDatum)))

    lookups = Lookups.unspentOutputs eqValidatorUtxos <> Lookups.validator eqValidator
  in
    constraints /\ lookups
