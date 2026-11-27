module Parun.Types.Announced where

import qualified Parun.Types.AnnouncedChoice as AnnouncedChoice
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 608.2d: a player announced a choice made for an object.
data Announced = MkAnnounced
  { player :: PlayerId.PlayerId,
    object :: ObjectId.ObjectId,
    choice :: AnnouncedChoice.AnnouncedChoice
  }
  deriving (Eq, Ord, Show)
