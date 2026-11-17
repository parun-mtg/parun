module Parun.Types.RollKind where

-- | Which die rolls a Parun.Types.RollTrigger watches.
data RollKind
  = -- | CR 706.2: any die roll, each die once (Night Shift of the Living Dead).
    AnyRoll
  | -- | CR 701.52a: a roll to visit Attractions ("Lifetime" Pass Holder).
    ToVisit
  deriving (Bounded, Enum, Eq, Ord, Show)
