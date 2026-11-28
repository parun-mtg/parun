module Parun.Types.ColorSpread where

import qualified Numeric.Natural as Natural

-- | How CR 105.4's colour choice spreads over the units one AddMana instruction
-- adds (Parun.Types.ManaProduction.AnyColor, Parun.Types.ManaAmong.spread).
data ColorSpread
  = -- | One choice for every unit, "three mana of any one color" (CR 105.4).
    OneColor
  | -- | A choice per unit, "X mana in any combination of colors" (CR 105.4).
    AnyCombination
  | -- | A choice per run of this many units, no colour chosen twice: "two mana of different colors" is runs of one, Open the Omenpaths' "two mana of any one color and two mana of any other color" runs of two (CR 105.4).
    DifferentColors Natural.Natural
  deriving (Eq, Ord, Show)
