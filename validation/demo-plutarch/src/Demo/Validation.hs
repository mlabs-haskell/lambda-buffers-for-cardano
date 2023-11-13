module Demo.Validation (eqValidator) where

import LambdaBuffers.Demo.Plutus.Plutarch (EqDatum, EqRedeemer (EqRedeemer'IsEqual, EqRedeemer'IsNotEqual))
import LambdaBuffers.Runtime.Plutarch ()
import LambdaBuffers.Runtime.Plutarch.LamVal (pfromPlutusDataPTryFrom)
import Plutarch (ClosedTerm, perror, plam, pmatch, popaque, unTermCont, (#))
import Plutarch.Api.V2 qualified as V2
import Plutarch.Extra.TermCont (pletC, ptraceC)
import Plutarch.Prelude (PEq ((#==)), pconstant, pfromData, pif, pnot, pshow, ptrace)

-- | `eqValidator dat rdmr ctx` checks whether the Datum `dat` is (not)equal to the Datum supplied in Redeemer.
eqValidator :: ClosedTerm V2.PValidator
eqValidator = plam $ \datum redeemer _ctx -> ptrace "[EQ]" $ unTermCont $ do
  eqDatum <- pletC $ pfromData $ pfromPlutusDataPTryFrom @EqDatum # datum
  ptraceC $ "[EQ] Datum is correct " <> pshow eqDatum
  eqRedeemer <- pletC $ pfromData $ pfromPlutusDataPTryFrom @EqRedeemer # redeemer
  ptraceC $ "[EQ] Redeemer is correct " <> pshow eqRedeemer

  validates <- pletC $ pmatch eqRedeemer $ \case
    EqRedeemer'IsEqual dat -> eqDatum #== pfromData dat
    EqRedeemer'IsNotEqual dat -> pnot # (eqDatum #== pfromData dat)

  pure $ pif validates (popaque (pconstant ())) (ptrace "[EQ] Validation failed" perror)
