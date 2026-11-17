module Parun.Types.LandPlayed where

import qualified Parun.Types.CastOrigin as CastOrigin
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 305.1: a player played a land, the special action of CR 116.2a.
data LandPlayed = MkLandPlayed
  { -- | The player who played it.
    player :: PlayerId.PlayerId,
    -- | The land as it arrived (CR 400.7), or the card played where a
    -- replacement kept it from arriving.
    land :: ObjectId.ObjectId,
    -- | Where it was played from: a zone, or outside the game (CR 400.11).
    from :: CastOrigin.CastOrigin
  }
  deriving (Eq, Ord, Show)
