module Parun.Types.PerTarget where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Quantity as Quantity

-- | Parun.Types.CostModifier.perTarget: which of CR 601.2c's announced targets
-- repeat a cost change, and how many times each one does (CR 601.2f).
data PerTarget = MkPerTarget
  { -- | The DISTINCT announced targets that count: Battlefield Thaumaturge's
    -- "each creature it targets".
    matching :: Filter.Filter Keyword.Keyword,
    -- | When Just, the times each matching target repeats the change, read
    -- against that target: Warrior's Blades' "for each +1/+1 counter on the
    -- creature it targets". Nothing is once.
    each :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
