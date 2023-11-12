-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.Main (main, suite) where

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Prelude (Effect, Unit, bind, discard, either, flip, maybe, mempty, pure, show, unit, unwrap, ($), (/\), (<$>), (=<<), (>>=))
import Contract.Prim.ByteArray (ByteArray(..), byteArrayFromAscii)
import Contract.Scripts (Validator(..), validatorHash)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip (InitialUTxOs, PlutipTest, defaultPlutipConfig, testPlutipContracts, withKeyWallet, withWallets)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Transaction (TransactionInput(..), awaitTxConfirmed, submitTxFromConstraints)
import Contract.Value (adaSymbol)
import Ctl.Internal.Plutus.Types.Address (Address)
import Ctl.Internal.Types.Scripts (plutusV2Script)
import Ctl.Internal.Types.TokenName (TokenName, mkTokenName)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Contract.Utxos (utxosAt)
import Data.Posix.Signal (Signal(SIGINT))
import Data.UInt as UInt
import Demo.Transactions as Transactions
import Effect.Aff (Milliseconds(Milliseconds), cancelWith, effectCanceler, launchAff)
import Effect.Exception (throw)
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.Demo.Config (Config)
import LambdaBuffers.Demo.Plutus (EqDatum(..), Product(..), Sum(..))
import LambdaBuffers.Demo.Plutus as Demo.Plutus
import LambdaBuffers.Plutus.V1 (AssetClass)
import LambdaBuffers.Runtime.Prelude (Bytes(..), fromJsonString)
import Mote (group, test)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NodeFS
import Test.Spec.Runner (defaultConfig)

-- | `readDemoConfig fp` reads the Demo.Config from given a filepath in `fp`.
readDemoConfig :: String -> Effect Config
readDemoConfig fp = do
  configOrErr <- fromJsonString <$> NodeFS.readTextFile UTF8 fp
  either (\err -> throw (show err)) pure configOrErr

-- | `lbBytesToByteArray` makes a CTL ByteArray from the LambdaBuffers Bytes type.
-- TODO(bladyjoker): Purescript ecosystem doesn't have a well agreed upon bytes type, so this is it. Maybe expose such utility?
lbBytesToByteArray :: Bytes -> ByteArray
lbBytesToByteArray (Bytes uint8Array) = ByteArray uint8Array

lbBytesFromByteArray :: ByteArray -> Bytes
lbBytesFromByteArray (ByteArray uint8Array) = Bytes uint8Array

eqValidatorFromConfig :: Config -> Validator
eqValidatorFromConfig config = Validator (plutusV2Script (lbBytesToByteArray (unwrap (unwrap config).eqValidator)))

-- Run with `spago run --main Test.Main`
main :: Effect Unit
main =
  interruptOnSignal SIGINT
    =<< do
        demoPlutarchConfig <- readDemoConfig "data/demo-plutarch-config.json"
        demoPlutusTxConfig <- readDemoConfig "data/demo-plutustx-config.json"
        let
          plutarchEqValidator = eqValidatorFromConfig demoPlutarchConfig

          plutusTxEqValidator = eqValidatorFromConfig demoPlutusTxConfig

          plutarchEqValidatorHash = validatorHash plutarchEqValidator

          plutusTxEqValidatorHash = validatorHash plutusTxEqValidator
        exampleTokenName :: TokenName <- maybe (throw "Wanted to create an example TokenName but failed") pure (byteArrayFromAscii "example token name" >>= mkTokenName)
        examplePlutusBytes :: ByteArray <- maybe (throw "Wanted to create an example Bytes but failed") pure (byteArrayFromAscii "example bytes")
        let
          exampleAssetClass :: AssetClass
          exampleAssetClass = adaSymbol /\ exampleTokenName

          exampleAddress :: Address
          exampleAddress = scriptHashAddress plutarchEqValidatorHash Nothing

          exampleEqDatumA :: EqDatum
          exampleEqDatumA =
            EqDatum
              { rec:
                  Demo.Plutus.Record
                    { bar: exampleAddress
                    , baz: examplePlutusBytes
                    , foo: exampleAssetClass
                    }
              , sum: Sum'Baz examplePlutusBytes
              , prod: Product exampleAssetClass exampleAddress examplePlutusBytes
              }

          exampleEqDatumB :: EqDatum
          exampleEqDatumB =
            EqDatum
              { rec:
                  Demo.Plutus.Record
                    { bar: exampleAddress
                    , baz: examplePlutusBytes
                    , foo: exampleAssetClass
                    }
              , sum: Sum'Foo exampleAssetClass
              , prod: Product exampleAssetClass exampleAddress examplePlutusBytes
              }
        launchAff do
          flip cancelWith (effectCanceler (exitCode 1)) do
            interpretWithConfig
              defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
              $ testPlutipContracts defaultPlutipConfig (suite plutarchEqValidator exampleEqDatumA exampleEqDatumB)

suite :: Validator -> EqDatum -> EqDatum -> TestPlanM PlutipTest Unit
suite eqValidator exampleEqDatumA exampleEqDatumB = do
  group "CTL Demo tests" do
    test "Store an example EqDatum at EqValidator and then check if (not)equal" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do
          let
            createEqDatumATx = Transactions.createValueTx (validatorHash eqValidator) exampleEqDatumA

            createEqDatumBTx = Transactions.createValueTx (validatorHash eqValidator) exampleEqDatumB
          logInfo' "Storing EqDatum A @ EqV"
          txHash <- submitTxFromConstraints mempty createEqDatumATx
          awaitTxConfirmed txHash
          logInfo' $ "Successfully stored EqDatum A @ EqV with " <> show txHash
          let
            eqValidatorAddress = scriptHashAddress (validatorHash eqValidator) Nothing
          eqValidatorUtxos <- utxosAt eqValidatorAddress
          let
            eqDatumAIsEqualTx /\ eqDatumAIsEqualCtx = Transactions.inputIsEqualTx eqValidator eqValidatorUtxos (TransactionInput { transactionId: txHash, index: UInt.fromInt 0 }) exampleEqDatumA
          logInfo' "Checking if EqDatum A is the same as the one previously stored (it should be)"
          txHash' <- submitTxFromConstraints eqDatumAIsEqualCtx eqDatumAIsEqualTx
          awaitTxConfirmed txHash'
          logInfo' "Successfully checked that they are indeed the same"
          logInfo' "Storing EqDatum B @ EqVal"
          txHash'' <- submitTxFromConstraints mempty createEqDatumBTx
          awaitTxConfirmed txHash''
          logInfo' $ "Successfully stored EqDatum B with " <> show txHash
          eqValidatorUtxos' <- utxosAt eqValidatorAddress
          let
            eqDatumAIsDifferentTx /\ eqDatumAIsDifferentCtx = Transactions.inputIsNotEqualTx eqValidator eqValidatorUtxos' (TransactionInput { transactionId: txHash'', index: UInt.fromInt 0 }) exampleEqDatumA
          logInfo' "Checking if Eq Datum A is different to the one previously stored (it should be)"
          txHash''' <- submitTxFromConstraints eqDatumAIsDifferentCtx eqDatumAIsDifferentTx
          awaitTxConfirmed txHash'''
          logInfo' "Successfully check that they are indeed different"
          pure unit
