module Parun.Types.CompanionCondition where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Filter as Filter

-- | The payload of Parun.Types.Keyword's Companion arm: CR 702.139a's
-- [Condition], which the rule gives no fixed shape.
--
-- PARAMETRIC in the keyword for Parun.Types.ForetellCost's reason. Only
-- @CompanionCondition Keyword.Keyword@ is ever written.
--
-- Not implemented: Umori, the Collector's shared card type and Jegantha, the
-- Wellspring's repeated mana symbol (#3261).
data CompanionCondition keyword
  = -- | CR 702.139a: every card in the starting deck matches the Filter.
    EachCard (Filter.Filter keyword)
  | -- | CR 702.139a / 100.2: the starting deck holds at least this many cards
    -- more than the game's minimum deck size.
    CardsOverMinimum Natural.Natural
  | -- | CR 702.139a / 201.2b: the cards in the starting deck that match the
    -- Filter have different names.
    DifferentNames (Filter.Filter keyword)
  deriving (Eq, Ord, Show)
