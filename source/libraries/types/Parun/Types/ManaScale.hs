module Parun.Types.ManaScale where

import Numeric.Natural (Natural)

-- | CR 106.12b: the amount a ManaR row makes a permanent tapped for mana
-- produce instead.
data ManaScale
  = -- | The amount it would produce (Naked Singularity).
    Keeps
  | -- | This amount, "instead of any other type and amount" (Contamination).
    Exactly Natural
  | -- | This many times the amount so far (Mana Reflection's "twice as much").
    Times Natural
  deriving (Eq, Ord, Show)
