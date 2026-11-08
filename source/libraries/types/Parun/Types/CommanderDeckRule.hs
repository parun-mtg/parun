module Parun.Types.CommanderDeckRule where

-- | A deck-construction rule a card changes for the deck it is the commander
-- of, and for no other (CR 903.5e).
--
-- Not implemented: the rules other rulebreakers widen, CR 903.5c's colour
-- identity and CR 903.5d's basic lands (#6676).
data CommanderDeckRule
  = -- | "A deck with this commander has no maximum deck size." (CR 903.5a)
    NoMaximumDeckSize
  deriving (Bounded, Enum, Eq, Ord, Show)
