module Demo.Validation (eqValidator) where

import LambdaBuffers.Demo.Plutus.Plutarch (EqDatum, EqRedeemer (EqRedeemer'IsEqual, EqRedeemer'IsNotEqual))
import LambdaBuffers.Runtime.Plutarch ()
import LambdaBuffers.Runtime.Plutarch.LamVal (pfromPlutusDataPTryFrom)
import Plutarch.LedgerApi.Utils (pmaybeDataToMaybe)
import Plutarch.LedgerApi.V1 (PDatum (PDatum), PRedeemer (PRedeemer))
import Plutarch.LedgerApi.V3 qualified as V3
import Plutarch.Prelude (ClosedTerm, PEq ((#==)), PMaybe (PJust), PUnit (PUnit), pcon, perror, pfield, pfromData, pif, plam, pletC, pmatch, pmatchC, pnot, pshow, ptraceC, ptraceInfo, unTermCont, (#), (:-->))

-- | `eqValidator dat rdmr ctx` checks whether the Datum `dat` is (not)equal to the Datum supplied in Redeemer.
eqValidator :: ClosedTerm (V3.PScriptContext :--> PUnit)
eqValidator = plam $ \ctx -> ptraceInfo "[Mint]" $ unTermCont $ do
  V3.PSpendingScript spending <- pmatchC $ pfield @"scriptInfo" # ctx
  maydatum <- pletC $ pfield @"_1" # spending
  PJust datum <- pmatchC $ pmaybeDataToMaybe # maydatum
  PDatum datumInner <- pmatchC datum

  eqDatum <- pletC $ pfromData $ pfromPlutusDataPTryFrom @EqDatum # datumInner
  ptraceC $ "[Mint] Datum is correct " <> pshow eqDatum

  PRedeemer redeemer <- pmatchC $ pfield @"redeemer" # ctx
  eqRedeemer <- pletC $ pfromData $ pfromPlutusDataPTryFrom @EqRedeemer # redeemer
  ptraceC $ "[Mint] Redeemer is correct " <> pshow eqRedeemer

  validates <- pletC $ pmatch eqRedeemer $ \case
    EqRedeemer'IsEqual dat -> eqDatum #== pfromData dat
    EqRedeemer'IsNotEqual dat -> pnot # (eqDatum #== pfromData dat)

  pure $ pif validates (pcon PUnit) (ptraceInfo "[Mint] Validation failed" perror)
