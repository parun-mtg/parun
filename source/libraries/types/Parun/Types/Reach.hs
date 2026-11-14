module Parun.Types.Reach where

-- | CR 400.7j: how far Parun.Types.FromAnywhere follows the object a slot holds.
data Reach
  = -- | CR 400.7j / 400.2: the one move it made, and only into a public zone.
    PublicZone
  | -- | CR 400.7j with the printed "from anywhere": every logged move, into any zone.
    AnyZone
  deriving (Bounded, Enum, Eq, Ord, Show)
