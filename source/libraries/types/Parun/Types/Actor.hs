module Parun.Types.Actor where

-- | Which end of a relation atom acted: the candidate, or the evaluation's
-- source (Parun.Types.DealtDamageTo's dealer, Filter.CrewedThisTurn's crewer).
data Actor
  = -- | The candidate itself -- Brine Hag's "each creature that dealt damage to
    -- it", Subterranean Schooner's "creature that crewed it".
    Candidate
  | -- | The evaluation's source -- Steel Hellkite's "dealt combat damage by this
    -- creature", Balthier and Fran's "Vehicle crewed by Balthier and Fran". CR
    -- 400.7's id, so a source that has since left still answers.
    Source
  deriving (Bounded, Enum, Eq, Ord, Show)
