module Parun.Types.PlaneswalkedFrom where

-- | CR 901.11 / 701.31d: what a planeswalk ending an "until a player
-- planeswalks" duration must walk away from.
data PlaneswalkedFrom
  = -- | CR 901.11: anything, or nothing at all (Agyrem's "until a player planeswalks").
    AnyCard
  | -- | CR 901.11b: a plane card, a phenomenon not counting (Chaotic Aether's
    -- "until a player planeswalks away from a plane").
    APlane
  deriving (Bounded, Enum, Eq, Ord, Show)
