module Parun.Types.OtherCandidates where

import qualified Parun.Types.CandidateScope as CandidateScope
import qualified Parun.Types.Measure as Measure

-- | CR 608.2d: a measure of the other candidates an
-- Parun.Types.Operand.OfEachOtherCandidate comparison reads, and which of
-- them it reads.
data OtherCandidates = MkOtherCandidates
  { measure :: Measure.Measure,
    scope :: CandidateScope.CandidateScope
  }
  deriving (Eq, Ord, Show)
