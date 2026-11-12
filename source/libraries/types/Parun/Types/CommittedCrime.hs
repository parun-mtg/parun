module Parun.Types.CommittedCrime where

import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 700.13: a player committed a crime -- who cast, activated or put on the
-- stack the spell or ability, and that stack object.
data CommittedCrime = MkCommittedCrime
  { player :: PlayerId.PlayerId,
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
