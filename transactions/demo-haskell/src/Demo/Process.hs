{- | Module: Demo.Process

Provides functions for processing requests to responses
-}
module Demo.Process (process) where

import LambdaBuffers.Demo.Config (Config (..))
import LambdaBuffers.Demo.Request (ClaimRequest, DemoRequest (..), LockRequest, Request (..))
import LambdaBuffers.Demo.Response (Error (..), Response (..), Result (..))
import PlutusLedgerApi.V1 (TxInfo (..))

-- data TxInfo
--   = TxInfo {txInfoInputs :: [TxInInfo],
--             txInfoOutputs :: [TxOut],
--             txInfoFee :: Value,
--             txInfoMint :: Value,
--             txInfoDCert :: [DCert],
--             txInfoWdrl :: [(StakingCredential, Integer)],
--             txInfoValidRange :: POSIXTimeRange,
--             txInfoSignatories :: [PubKeyHash],
--             txInfoData :: [(DatumHash, Datum)],
--             txInfoId :: TxId}

-- data TxInInfo
--   = TxInInfo {txInInfoOutRef :: TxOutRef, txInInfoResolved :: TxOut}

-- type TxOutRef :: *
-- data TxOutRef
--   = TxOutRef {txOutRefId :: TxId, txOutRefIdx :: Integer}
--   	-- Defined in ‘PlutusLedgerApi.V1.Tx’
--
--   	type TxOut :: *
-- data TxOut
--   = TxOut {txOutAddress :: Address,
--            txOutValue :: Value,
--            txOutDatumHash :: Maybe DatumHash}

process :: Config -> DemoRequest -> Response ()
process _config demoRequest = case demoRequest of
  DemoRequest'Lock req ->
    Response'Result $
      Result
        { result'txInfo =
            TxInfo
              {
              }
        , result'response = ()
        }
  DemoRequest'Claim req -> undefined
