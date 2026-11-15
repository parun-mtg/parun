module Parun.Types.TurnedFaceUp where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 708.7: a face-down permanent was turned face up -- which permanent, the
-- player who turned it, and the X chosen for the cost that turned it (CR
-- 107.3d).
--
-- The player is CR 701.40b's and CR 702.37e's special action's, or an effect's
-- controller (Growing Dread's "whenever you turn a permanent face up").
--
-- The X is Nothing when the road up paid no cost with an X in it, and Just 0
-- when the player chose zero. Carried on the event rather than read off the
-- permanent, since CR 702.37f and CR 702.168e give it to the permanent's
-- turned-face-up trigger, and CR 117.5's scan may run after state-based actions
-- have already taken the permanent away.
data TurnedFaceUp = MkTurnedFaceUp
  { object :: ObjectId.ObjectId,
    player :: PlayerId.PlayerId,
    announcedX :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)
