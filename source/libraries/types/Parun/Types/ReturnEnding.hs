module Parun.Types.ReturnEnding where

import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.MonarchWatch as MonarchWatch
import qualified Parun.Types.ObjectId as ObjectId

-- | CR 610.3's or 610.4's specified event for one moved or phased-out object,
-- armed from the effect's Parun.Types.MoveDuration with what the game must
-- remember to see it happen.
data ReturnEnding
  = -- | CR 610.3 / 400.7: this incarnation of the move's source leaves the battlefield.
    SourceLeaves ObjectId.ObjectId
  | -- | CR 726: an opponent of the watch's controller becomes the monarch.
    OpponentCrowned MonarchWatch.MonarchWatch
  | -- | CR 901.11: a player planeswalks, in the event group it names once one has.
    Planeswalked (Maybe EventGroup.EventGroup)
  deriving (Eq, Ord, Show)
