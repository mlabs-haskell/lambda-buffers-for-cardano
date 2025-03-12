-- Demo.Transactions module contains pure functions that build constraint/lookup pairs (ie. they only build transactions)
module Demo.Transactions
  ( createValueTx, inputIsEqualTx, inputIsNotEqualTx
  ) where

import Contract.PlutusData (RedeemerDatum(..), toData)
import Contract.Prelude ((/\))
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (DatumPresence(..), mustPayToScript, mustSpendScriptOutput) as Constraints
import Contract.TxConstraints (TxConstraints)
import Contract.Value (lovelaceValueOf) as Value
import Cardano.Types.BigNum as BigNum
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Data.Tuple.Nested (type (/\))
import LambdaBuffers.Demo.Plutus (EqDatum, EqRedeemer(..))

-- | Transaction that stores a EqDatum value at the Eq Validator.
createValueTx :: ScriptHash -> EqDatum -> TxConstraints
createValueTx eqValidatorAddress eqDatum =
  Constraints.mustPayToScript
    eqValidatorAddress
    (toData eqDatum)
    Constraints.DatumInline
    (Value.lovelaceValueOf (BigNum.fromInt 10_000_000))

-- | `inputIsEqualTx eqValidator eqValidatorUtxos txIn eqDatum` make a transaction that checks if the EqDatum stored at the EqValidator's `txIn` is equal to the provided one in `eqDatum`.
inputIsEqualTx :: Validator -> Map TransactionInput TransactionOutput -> TransactionInput -> EqDatum -> TxConstraints /\ ScriptLookups
inputIsEqualTx eqValidator eqValidatorUtxos eqValidatorTxIn eqDatum =
  let
    constraints = Constraints.mustSpendScriptOutput eqValidatorTxIn (RedeemerDatum (toData (EqRedeemer'IsEqual eqDatum)))

    lookups = Lookups.unspentOutputs eqValidatorUtxos <> Lookups.validator eqValidator
  in
    constraints /\ lookups

-- | `inputIsNotEqualTx eqValidator eqValidatorUtxos txIn eqDatum` make a transaction that checks if the EqDatum stored at the EqValidator's `txIn` is NOT equal to the provided one in `eqDatum`.
inputIsNotEqualTx :: Validator -> Map TransactionInput TransactionOutput -> TransactionInput -> EqDatum -> TxConstraints /\ ScriptLookups
inputIsNotEqualTx eqValidator eqValidatorUtxos eqValidatorTxIn eqDatum =
  let
    constraints = Constraints.mustSpendScriptOutput eqValidatorTxIn (RedeemerDatum (toData (EqRedeemer'IsNotEqual eqDatum)))

    lookups = Lookups.unspentOutputs eqValidatorUtxos <> Lookups.validator eqValidator
  in
    constraints /\ lookups
