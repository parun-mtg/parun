module Parun.Types.LoyaltyClause where

import Numeric.Natural (Natural)

-- | Which clause of CR 606.3 a Parun.Types.LoyaltyGrant relaxes.
data LoyaltyClause
  = -- | CR 606.3's window: the player may activate any time they could cast an
    -- instant (CR 117.1a) instead (The Wandering Emperor).
    Window
  | -- | CR 606.3's once-per-turn limit: one more activation this turn for the
    -- grant's holder, each grant counted (The Chain Veil's rulings).
    OncePerTurn
  | -- | CR 606.3's once-per-turn limit raised to this many, not stacking across
    -- grants (Kaito, Dancing Shadow's rulings).
    TimesEachTurn Natural
  deriving (Eq, Ord, Show)
