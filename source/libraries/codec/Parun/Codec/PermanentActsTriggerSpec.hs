module Parun.Codec.PermanentActsTriggerSpec where

import qualified Parun.Codec.PermanentActsTrigger as PermanentActsTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PermanentAction as PermanentAction
import qualified Parun.Types.PermanentActsTrigger as PermanentActsTrigger

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PermanentActsTrigger" $ do
  -- Teferi's Imp: each occurrence writes no batching key.
  Spec.it s "writes no batching for EachOccurrence" $
    Common.assertCodec
      s
      PermanentActsTrigger.codec
      (PermanentActsTrigger.MkPermanentActsTrigger (PermanentActed.MkPermanentActed PermanentAction.PhaseOut ActingPermanent.Self) Batching.EachOccurrence)
      " {\"action\":\"PhaseOut\",\"permanent\":{\"type\":\"Self\"}} "
  -- The War Doctor: one or more other permanents.
  Spec.it s "round-trips OneOrMore" $
    Common.assertCodec
      s
      PermanentActsTrigger.codec
      (PermanentActsTrigger.MkPermanentActsTrigger (PermanentActed.MkPermanentActed PermanentAction.PhaseOut (ActingPermanent.Matching (Filter.Not Filter.IsSource))) Batching.OneOrMore)
      " {\"action\":\"PhaseOut\",\"permanent\":{\"type\":\"Matching\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}},\"batching\":\"OneOrMore\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentActsTrigger.codec
