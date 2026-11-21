module Parun.Types.LandPlayed where

import qualified Parun.Types.CastOrigin as CastOrigin
import qualified Parun.Types.ExileLink as ExileLink
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
    from :: CastOrigin.CastOrigin,
    -- | CR 607.2a: the card's link to the object that exiled it, as it left
    -- exile, since CR 400.7 drops the row with the card (Voltaic Visionary's
    -- "a card exiled with this creature").
    exiledWith :: Maybe ExileLink.ExileLink
  }
  deriving (Eq, Ord, Show)
