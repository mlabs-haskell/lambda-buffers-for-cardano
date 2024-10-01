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
import LambdaBuffers.Demo.Request (ClaimRequest (..), DemoRequest (..), LockRequest (..), Request (..))
import LambdaBuffers.Demo.Response (Error (..), Response (..), Result (..))
import PlutusLedgerApi.V1.Bytes qualified as V1.Bytes
import PlutusLedgerApi.V2 (Address (..), Credential (..), Datum (..), Extended (..), Interval (..), LowerBound (..), OutputDatum (..), Redeemer (..), ScriptHash (..), ScriptPurpose (..), TxId (..), TxInInfo (..), TxInfo (..), TxOut (..), UpperBound (..))
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
                  changeTxOut = mkChangeTxOut req
               in Response'Result $
                    Result
                      { result'txInfo =
                          TxInfo
                            { txInfoInputs = request'feeInputs req
                            , txInfoOutputs =
                                [eqValidatorTxOut, changeTxOut]
                            , txInfoFee = mempty
                            , txInfoMint = mempty
                            , txInfoDCert = []
                            , txInfoWdrl = AssocMap.empty
                            , txInfoValidRange =
                                Interval
                                  (LowerBound NegInf False)
                                  (UpperBound PosInf False)
                            , txInfoSignatories = mempty
                            , txInfoData = AssocMap.empty
                            , txInfoId = TxId mempty
                            , txInfoReferenceInputs = mempty
                            , txInfoRedeemers = AssocMap.empty
                            }
                      , result'response = ()
                      }
            DemoRequest'Claim req ->
              let changeTxOut = mkChangeTxOut req
                  lockedUtxo = claimRequest'lockedUtxo (request'request req)
               in if txOutAddress (txInInfoResolved lockedUtxo) == eqValidatorAddress
                    then
                      Response'Result $
                        Result
                          { result'txInfo =
                              TxInfo
                                { txInfoInputs =
                                    lockedUtxo : request'feeInputs req
                                , txInfoOutputs =
                                    [changeTxOut]
                                , txInfoFee = mempty
                                , txInfoMint = mempty
                                , txInfoDCert = []
                                , txInfoWdrl = AssocMap.empty
                                , txInfoValidRange =
                                    Interval
                                      (LowerBound NegInf False)
                                      (UpperBound PosInf False)
                                , txInfoSignatories = mempty
                                , txInfoData = AssocMap.empty
                                , txInfoId = TxId mempty
                                , txInfoReferenceInputs = mempty
                                , txInfoRedeemers =
                                    AssocMap.fromListSafe
                                      [
                                        ( Spending $ txInInfoOutRef lockedUtxo
                                        , Redeemer
                                            { getRedeemer =
                                                IsData.Class.toBuiltinData $
                                                  claimRequest'eqRedeemer $
                                                    request'request req
                                            }
                                        )
                                      ]
                                }
                          , result'response = ()
                          }
                    else Response'Error $ Error'Build "Invalid lockedUtxo. The locked UTxO must be at the eqValidator address"

{- | Creates the change 'TxOut' from a 'Request' which should be the last
element in the list of 'txInfoOutputs' of a 'TxInInfo'
-}
mkChangeTxOut :: Request a -> TxOut
mkChangeTxOut req =
  TxOut
    { txOutAddress = request'changeAddress req
    , txOutValue =
        foldMap (txOutValue . txInInfoResolved) $ request'feeInputs req
    , txOutReferenceScript = Nothing
    , txOutDatum = NoOutputDatum
    }
