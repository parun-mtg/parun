module Parun.Types.AttackMeasure where

-- | CR 508.3c: what an attack trigger's floor is compared against, among the
-- declared creatures its Filter admits.
data AttackMeasure
  = -- | How many there are (CR 508.3c).
    Creatures
  | -- | Their total power as declared, negative powers included (CR 107.1b).
    TotalPower
  deriving (Bounded, Enum, Eq, Ord, Show)
