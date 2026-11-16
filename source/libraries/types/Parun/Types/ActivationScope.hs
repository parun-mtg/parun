module Parun.Types.ActivationScope where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | The payload of Parun.Types.SpendScope's Activating arm: which activations
-- one CR 609.4b clause speaks about (CR 602.2b).
data ActivationScope = MkActivationScope
  { -- | Matched against the source of the ability being activated (Agatha's
    -- Soul Cauldron's "abilities of creatures you control").
    source :: Filter.Filter Keyword.Keyword,
    -- | CR 613.1f: matched against what lent the ability, or Nothing for any
    -- of the source's abilities: a card (Drana and Linvala's "those
    -- abilities") or a resolving lend's own ability (Grell Philosopher's,
    -- Binding.thisAbility).
    lentBy :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
