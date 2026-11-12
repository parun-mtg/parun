module Parun.Types.TotalCap where

import qualified Parun.Types.Quantity as Quantity

-- | CR 608.2d: the most a choice's picks may sum to, beside how many they are
-- (Parun.Types.HowMany, Parun.Types.CastRepetition) -- Michelangelo's
-- Technique's "up to two creature cards with total mana value 6 or less".
data TotalCap = MkTotalCap
  { -- | What one pick adds, read against that card's projection --
    -- Parun.Types.TopOfLibraryUntil's weight, and its clamp (CR 107.2, 107.1b).
    weight :: Quantity.Quantity,
    -- | The sum the picks may reach and not pass, read as the choice is made
    -- (CR 608.2h); unevaluable or negative is zero.
    most :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
