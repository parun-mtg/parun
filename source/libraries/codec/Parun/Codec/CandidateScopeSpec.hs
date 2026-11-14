module Parun.Codec.CandidateScopeSpec where

import qualified Parun.Codec.CandidateScope as CandidateScope
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CandidateScope as CandidateScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CandidateScope" $ do
  Spec.it s "EveryCandidate" $
    Common.assertCodec
      s
      CandidateScope.codec
      CandidateScope.EveryCandidate
      " \"EveryCandidate\" "
  Spec.it s "SameController" $
    Common.assertCodec
      s
      CandidateScope.codec
      CandidateScope.SameController
      " \"SameController\" "
  Spec.it s "has a schema" $ Common.assertHasSchema s CandidateScope.codec
