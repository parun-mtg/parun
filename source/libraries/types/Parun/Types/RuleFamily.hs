module Parun.Types.RuleFamily where

-- | CR 100.2: the construction rules a deck is built under.
data RuleFamily
  = -- | CR 100.2a / 100.4a: each player builds their own deck ahead of time.
    Constructed
  | -- | CR 100.2b: each player builds from the same quantity of unopened product.
    Limited
  | -- | CR 903.5: a Commander deck.
    Commander
  | -- | CR 903.12: a Brawl deck.
    Brawl
  deriving (Bounded, Enum, Eq, Ord, Show)
