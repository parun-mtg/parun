module Parun.Types.DeckLimit where

import qualified Numeric.Natural as Natural

-- | How many cards of a card's own name a deck may hold, as the card itself
-- prints it, overriding CR 100.2a's four.
data DeckLimit
  = -- | "A deck can have up to N cards named ~." (CR 100.2a)
    UpTo Natural.Natural
  | -- | "A deck can have any number of cards named ~." (CR 100.2a)
    AnyNumber
  deriving (Eq, Ord, Show)
