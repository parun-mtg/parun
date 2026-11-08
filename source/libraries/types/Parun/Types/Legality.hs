module Parun.Types.Legality where

-- | A card's standing in a format's pool, where the pool has the card at all;
-- a card the pool lacks is not legal there.
data Legality
  = -- | Any number up to the rules' copy limit.
    Legal
  | -- | One copy at most, deck and sideboard together.
    Restricted
  | -- | No copy.
    Banned
  deriving (Bounded, Enum, Eq, Ord, Show)
