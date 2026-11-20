module Parun.Types.EntryLink where

import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.ObjectId as ObjectId

-- | CR 607.1 / 607.5: what a permanent is linked to by entering or by being
-- assimilated (CR 701.72c): the object whose ability put it onto the
-- battlefield or assimilated it, and which acquisition of that
-- object's abilities the ability came with, since only that acquisition's
-- abilities refer to it, and the resolving spell or ability whose effect
-- made the link, since a delayed ability it created reads only what that
-- instance put there (CR 603.7, 608.2c).
data EntryLink = MkEntryLink
  { source :: !ObjectId.ObjectId,
    acquisition :: !Acquisition.Acquisition,
    resolution :: !ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
