module Parun.Types.DraftBooster where

-- | CR 903.13e / 903.13f: a draft booster whose presence in a Commander Draft
-- changes its rules.
data DraftBooster
  = -- | CR 903.13a / 903.13e: Commander Legends, the default.
    CommanderLegends
  | -- | CR 903.13e / 903.13f(3): Commander Masters.
    CommanderMasters
  | -- | CR 903.13e: Commander Legends: Battle for Baldur's Gate.
    BattleForBaldursGate
  deriving (Bounded, Enum, Eq, Ord, Show)
