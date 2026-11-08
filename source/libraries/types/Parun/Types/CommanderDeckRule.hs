module Parun.Types.CommanderDeckRule where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | A deck-construction rule a card changes for the deck it is the commander
-- of, and for no other (CR 903.5e).
data CommanderDeckRule
  = -- | "A deck with this commander has no maximum deck size." (CR 903.5a)
    NoMaximumDeckSize
  | -- | Matching cards are exempt from CR 903.5c and 903.5d (CR 903.5e).
    AnyColorIdentity (Filter.Filter Keyword.Keyword)
  | -- | Matching cards' identity may add one colour the deck chooses (CR 903.5e).
    ChosenExtraColor (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
