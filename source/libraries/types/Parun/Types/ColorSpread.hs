module Parun.Types.ColorSpread where

-- | How CR 105.4's colour choice spreads over the units one AddMana instruction
-- adds (Parun.Types.ManaProduction.AnyColor, Parun.Types.ManaAmong.spread).
data ColorSpread
  = -- | One choice for every unit, "three mana of any one color" (CR 105.4).
    OneColor
  | -- | A choice per unit, "X mana in any combination of colors" (CR 105.4).
    AnyCombination
  | -- | A choice per unit, no colour chosen twice, "two mana of different
    -- colors" (CR 105.4).
    DifferentColors
  deriving (Bounded, Enum, Eq, Ord, Show)
