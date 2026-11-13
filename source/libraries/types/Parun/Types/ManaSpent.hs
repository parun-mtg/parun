module Parun.Types.ManaSpent where

import qualified Parun.Types.CostPortion as CostPortion
import qualified Parun.Types.ManaType as ManaType

-- | The payload of Parun.Types.Quantity's ManaSpent arm: which of the mana spent
-- to pay for the object it counts (Parun.Types.Object.manaSpent).
data ManaSpent
  = -- | CR 601.2h: all of it -- rule 702.191a's "the amount of mana spent to
    -- cast that spell".
    Total
  | -- | CR 601.2h / 400.7d: the mana of that type spent on that portion of the
    -- cost -- Catharsis's "if {W}{W} was spent to cast it", Soul Burn's "the
    -- amount of {B} spent on X".
    OfType ManaType.ManaType CostPortion.CostPortion
  | -- | CR 601.2h / 106.1a: how many of the five colors the mana spent includes --
    -- Converge's "the number of colors of mana spent to cast this spell".
    Colors
  deriving (Eq, Ord, Show)
