{- | Module: Demo.Process

Provides functions for processing requests to responses
-}
module Demo.Process (process) where

import Cardano.Api (PlutusScriptVersion (..), Script (..))
import Cardano.Api qualified
import Data.Coerce qualified as Coerce
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import LambdaBuffers.Demo.Config (Config (..), Script (..))
import LambdaBuffers.Demo.Request (ClaimRequest, DemoRequest (..), LockRequest (..), Request (..))
import LambdaBuffers.Demo.Response (Error (..), Response (..), Result (..))
import PlutusLedgerApi.V1.Bytes qualified as V1.Bytes
import PlutusLedgerApi.V2 (Address (..), Credential (..), Datum (..), Extended (..), Interval (..), LowerBound (..), OutputDatum (..), ScriptHash (..), TxId (..), TxInfo (..), TxOut (..), UpperBound (..))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.IsData.Class qualified as IsData.Class

process :: Config -> DemoRequest -> Response ()
process config demoRequest =
  case Cardano.Api.deserialiseFromRawBytes (Cardano.Api.proxyToAsType Proxy) $ Coerce.coerce $ config'eqValidator config of
    Left err -> Response'Error $ Error'Internal $ Text.pack $ show err
    Right eqValidatorDeserialised ->
      let eqValidatorScript = PlutusScript PlutusScriptV2 eqValidatorDeserialised
          eqValidatorScriptHash = Cardano.Api.hashScript eqValidatorScript
          eqValidatorAddress =
            Address
              { addressCredential =
                  ScriptCredential $
                    ScriptHash
                    -- ByteString --> BuiltinByteString:
                    $
                      V1.Bytes.getLedgerBytes $
                        V1.Bytes.fromBytes
                        -- Cardano.Api.ScriptHash --> ByteString:
                        $
                          Cardano.Api.serialiseToRawBytes eqValidatorScriptHash
              , addressStakingCredential = Nothing
              }
       in case demoRequest of
            DemoRequest'Lock req ->
              let eqValidatorTxOut =
                    TxOut
                      { txOutAddress = eqValidatorAddress
                      , txOutValue = mempty
                      , txOutReferenceScript = Nothing
                      , txOutDatum =
                          OutputDatum $
                            Datum $
                              IsData.Class.toBuiltinData $
                                lockRequest'eqDatum $
                                  request'request
                                    req
                      }
               in Response'Result $
                    Result
                      { result'txInfo =
                          TxInfo
                            { txInfoInputs = [] -- :: [TxInInfo]
                            , txInfoOutputs =
                                [eqValidatorTxOut]
                            , -- :: [TxOut]
                              txInfoFee = mempty -- :: Value
                            , txInfoMint = mempty -- :: Value,
                            , txInfoDCert = [] -- :: [DCert],
                            , txInfoWdrl = AssocMap.empty -- :: [(StakingCredential, Integer)],
                            , txInfoValidRange =
                                Interval
                                  (LowerBound NegInf False)
                                  (UpperBound PosInf False) -- :: POSIXTimeRange,
                            , txInfoSignatories = mempty -- :: [PubKeyHash],
                            , txInfoData = AssocMap.empty -- :: [(DatumHash, Datum)],
                            , txInfoId = TxId mempty -- :: TxId
                            , txInfoReferenceInputs = mempty
                            , txInfoRedeemers = AssocMap.empty
                            }
                      , result'response = ()
                      }
            DemoRequest'Claim req -> undefined
