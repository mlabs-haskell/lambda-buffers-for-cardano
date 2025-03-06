-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.Main where

import Contract.Log (logInfo')
import Ctl.Internal.Contract.ProviderBackend (mkCtlBackendParams)
import Contract.Config (ContractParams, WalletSpec(UseKeys), testnetConfig, PrivatePaymentKeySource(..))
import Contract.Prelude (Effect, Unit, bind, discard, either, flip, maybe, mempty, pure, show, unit, unwrap, ($), (/\), (<$>), (=<<), (>>=))
import Contract.Prim.ByteArray (ByteArray(..), byteArrayFromAscii, rawBytesFromByteArray)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Transaction (TransactionInput(..), awaitTxConfirmed, submitTxFromConstraints)
import Contract.Monad (Contract, runContract)
import Cardano.Plutus.Types.Address (Address, scriptHashAddress)
import Cardano.Plutus.Types.CurrencySymbol (adaSymbol)
import Cardano.Plutus.Types.ValidatorHash (ValidatorHash(..))
import Cardano.Types.PaymentCredential (PaymentCredential(..))
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.Credential (Credential(ScriptHashCredential))
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.PlutusScript (PlutusScript, plutusV3Script)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Plutus.Types.TokenName (TokenName, mkTokenName)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Mote.Monad (mapTest)
import Contract.Utxos (utxosAt)
import Data.Posix.Signal (Signal(SIGINT))
import Data.UInt as UInt
import Demo.Transactions as Transactions
import Effect.Aff (Milliseconds(Milliseconds), cancelWith, effectCanceler, launchAff)
import Effect.Exception (throw)
import LambdaBuffers.Demo.Config (Config)
import LambdaBuffers.Demo.Plutus (EqDatum(..), Product(..), Sum(..))
import LambdaBuffers.Demo.Plutus as Demo.Plutus
import LambdaBuffers.Plutus.V1 (AssetClass)
import LambdaBuffers.Runtime.Prelude (Bytes(..), fromJsonString)
import Ctl.Internal.ServerConfig (defaultOgmiosWsConfig)
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

eqValidatorFromConfig :: Config -> PlutusScript
eqValidatorFromConfig config = plutusV3Script (rawBytesFromByteArray (lbBytesToByteArray (unwrap (unwrap config).eqValidator)))

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

          plutarchEqValidatorHash = ValidatorHash (PlutusScript.hash plutarchEqValidator)
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

          contractParams :: ContractParams
          contractParams =
            testnetConfig
              { walletSpec =
                Just $ UseKeys (PrivatePaymentKeyFile "./wallets/test.skey") Nothing Nothing
              , backendParams =
                mkCtlBackendParams
                  { ogmiosConfig: defaultOgmiosWsConfig
                  , kupoConfig:
                      { port: UInt.fromInt 1442
                      , host: "localhost"
                      , secure: false
                      , path: Nothing
                      }
                  }
              }
        launchAff do
          flip cancelWith (effectCanceler (exitCode 1)) do
            interpretWithConfig
              defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
              $ mapTest (runContract contractParams) (suite plutarchEqValidator plutusTxEqValidator exampleEqDatumA exampleEqDatumB)

suite :: PlutusScript -> PlutusScript -> EqDatum -> EqDatum -> TestPlanM (Contract Unit) Unit
suite plutarchEqValidator plutusTxEqValidator exampleEqDatumA exampleEqDatumB = do
  group "CTL Demo tests" do
    group "Plutarch" $ eqValidatorTest plutarchEqValidator exampleEqDatumA exampleEqDatumB
    group "PlutusTx" $ eqValidatorTest plutusTxEqValidator exampleEqDatumA exampleEqDatumB

eqValidatorTest :: PlutusScript -> EqDatum -> EqDatum -> TestPlanM (Contract Unit) Unit
eqValidatorTest eqValidator exampleEqDatumA exampleEqDatumB =
  test "Store an example EqDatum at EqValidator and then check if (not)equal" do
    let
      eqValidatorHash = PlutusScript.hash eqValidator

      createEqDatumATx = Transactions.createValueTx eqValidatorHash exampleEqDatumA

      createEqDatumBTx = Transactions.createValueTx eqValidatorHash exampleEqDatumB
    logInfo' "Storing EqDatum A @ EqV"
    txHash <- submitTxFromConstraints mempty createEqDatumATx
    awaitTxConfirmed txHash
    logInfo' $ "Successfully stored EqDatum A @ EqV with " <> show txHash
    let
      eqValidatorAddress =
        mkPaymentAddress
          NetworkId.TestnetId
          (PaymentCredential (ScriptHashCredential eqValidatorHash))
          Nothing
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
