module Parun.Types.PlaneswalkAway where

-- | CR 701.31d: which face-up cards a planeswalk walks away from.
data PlaneswalkAway
  = -- | CR 701.31b: every face-up plane and phenomenon card.
    FromEveryCard
  | -- | Norn's Seedcore's "except don't planeswalk away from any plane": the
    -- face-up phenomena only.
    FromPhenomenaOnly
  deriving (Bounded, Enum, Eq, Ord, Show)
