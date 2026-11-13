module Parun.Types.TotalCap where

import qualified Parun.Types.Quantity as Quantity

-- | CR 608.2d: the most a choice's picks may sum to, beside how many they are
-- (Parun.Types.HowMany, Parun.Types.CastRepetition) -- Michelangelo's
-- Technique's "up to two creature cards with total mana value 6 or less",
-- Destined Confrontation's "any number of creatures they control with total
-- power 4 or less" -- and CR 601.2c's the same over a slot's targets
-- (Parun.Types.TargetSlot).
data TotalCap = MkTotalCap
  { -- | What one pick adds, read against that card's projection: an
    -- unevaluable weight adds nothing (CR 107.2), a negative one counts as it
    -- is (CR 107.1b).
    weight :: Quantity.Quantity,
    -- | The sum the picks may reach and not pass, read as the choice is made
    -- (CR 608.2h, or CR 601.2c for targets); unevaluable or negative is zero.
    most :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
