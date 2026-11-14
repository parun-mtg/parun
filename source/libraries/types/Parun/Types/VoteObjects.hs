module Parun.Types.VoteObjects where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.SlotName as SlotName

-- | CR 701.38b's OBJECTS, the payload of Parun.Types.VoteChoices' Objects arm:
-- the listed choices are the permanents @filter@ matches, and the objects tied
-- for most votes are bound at @slot@ for a later effect of the same resolution
-- to name (Council's Judgment's "a nonland permanent you don't control", then
-- "exile each permanent from among them"). Each voted-for object's tally rides
-- beside them as Parun.Types.Binding's perMember, for a later loop over them
-- to read as that member's amount.
--
-- The filter is swept ONCE, off the board as the vote begins, which is also
-- what makes every voter's list the same list -- rule 701.38a votes "for one
-- choice from a list", singular -- less what lies outside that voter's range
-- (CR 801.5a).
data VoteObjects = MkVoteObjects
  { filter :: Filter.Filter Keyword.Keyword,
    slot :: SlotName.SlotName,
    -- | Trap the Trespassers' "each creature with one or more votes": every
    -- object voted for is bound, not only those tied for most.
    anyVotes :: Bool
  }
  deriving (Eq, Ord, Show)
