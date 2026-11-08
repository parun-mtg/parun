module Parun.Types.CommanderDeckRule where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | A deck-construction rule a card changes for the deck it is the commander
-- of, and for no other (CR 903.5e).
--
-- Not implemented: Tolabow, Loch Rascal's added colour for instants and
-- sorceries, a deck-construction choice (#6682).
data CommanderDeckRule
  = -- | "A deck with this commander has no maximum deck size." (CR 903.5a)
    NoMaximumDeckSize
  | -- | "A deck with this commander can have [matching] cards of any color
    -- identity": matching cards are exempt from CR 903.5c and 903.5d.
    AnyColorIdentity (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
