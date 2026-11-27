module Parun.Types.Actor where

import qualified Parun.Types.SlotName as SlotName

-- | Which end of a relation atom acted: the candidate, the evaluation's source,
-- or an object a slot binds (Parun.Types.DealtDamageTo's dealer,
-- Filter.CrewedThisTurn's crewer).
data Actor
  = -- | The candidate itself -- Brine Hag's "each creature that dealt damage to
    -- it", Subterranean Schooner's "creature that crewed it".
    Candidate
  | -- | The evaluation's source -- Steel Hellkite's "dealt combat damage by this
    -- creature", Balthier and Fran's "Vehicle crewed by Balthier and Fran". CR
    -- 400.7's id, so a source that has since left still answers.
    Source
  | -- | An object this slot binds -- Glen Elendra's first target, whose combat
    -- damage names "that player".
    --
    -- Unprinted: a crewer naming a slot; Parun.FilterPositionLintSpec's "CR
    -- 702.122c no card names a slot as a crewer" keeps it out of card data.
    InSlot SlotName.SlotName
  deriving (Eq, Ord, Show)
