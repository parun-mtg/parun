module Parun.Types.CandidateScope where

-- | CR 608.2d: which other candidates a Parun.Types.OtherCandidates comparison
-- reads.
data CandidateScope
  = -- | Every other candidate the enclosing position offers (Topple).
    EveryCandidate
  | -- | Only those sharing the candidate's controller, "among creatures that player controls" (Kraven the Hunter).
    SameController
  deriving (Bounded, Enum, Eq, Ord, Show)
