module Parun.Types.ControllerGrant where

-- | What a permanent's Modification.GrantsController gives the player who
-- controls it: a fact about that player, not about a set of permanents.
data ControllerGrant
  = -- | CR 702.184c: that player's station abilities read the tapped creature's
    -- toughness where it exceeds its power (Tapestry Warden).
    StationsUsingToughness
  | -- | CR 702.179d: that player's speed can increase beyond 4, lifting the
    -- inherent ability's "if your speed is less than 4" (Gomif, Fast Racer).
    SpeedBeyondFour
  deriving (Bounded, Enum, Eq, Ord, Show)
