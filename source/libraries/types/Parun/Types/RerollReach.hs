module Parun.Types.RerollReach where

-- | CR 706.2b's first step: how many of a roll's dice one reroll modifier
-- reaches, for its one cost.
data RerollReach
  = -- | The one die the modifier watched (Clam-I-Am's "reroll that die").
    ThatDie
  | -- | Any number of the dice its payer rolled at the same time, for one
    -- payment (Monitor Monitor's "reroll one or more dice you rolled").
    AnyOfTheDice
  deriving (Bounded, Enum, Eq, Ord, Show)
