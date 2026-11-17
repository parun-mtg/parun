module Parun.Types.ThrowKind where

import qualified Numeric.Natural as Natural

-- | What a Parun.Types.Throw throws: CR 705.1's coins, CR 706.1's dice or CR
-- 901.3a's planar die.
data ThrowKind
  = -- | CR 705.1: coins.
    Coins
  | -- | CR 706.1a: dice with this many sides.
    Dice Natural.Natural
  | -- | CR 901.3a / 901.9d: the planar die, whose faces are symbols, not numbers.
    PlanarDie
  deriving (Eq, Ord, Show)
