module Parun.Types.PastRoll where

import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Source as Source

-- | CR 706.1: one roll instruction an ability carried out this turn, kept per
-- roller as the total of the results they kept, for "any other total you have
-- rolled this turn for this ability" (Free-Range Chicken).
data PastRoll = MkPastRoll
  { -- | The rolling ability's Source, which a copy shares (CR 707.10b).
    ability :: Source.Source,
    -- | The ability's object on the stack, one per resolution.
    resolution :: ObjectId.ObjectId,
    -- | CR 706.1: the player who rolled.
    roller :: PlayerId.PlayerId,
    -- | CR 706.2: the sum of the results the roller kept (CR 706.6).
    total :: Integer
  }
  deriving (Eq, Ord, Show)
