module Parun.Codec.OtherCandidatesSpec where

import qualified Parun.Codec.OtherCandidates as OtherCandidates
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CandidateScope as CandidateScope
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.OtherCandidates as OtherCandidates

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OtherCandidates" $ do
  Spec.it s "MkOtherCandidates" $
    Common.assertCodec
      s
      OtherCandidates.codec
      (OtherCandidates.MkOtherCandidates Measure.Power CandidateScope.SameController)
      " {\"measure\":\"Power\",\"scope\":\"SameController\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s OtherCandidates.codec
