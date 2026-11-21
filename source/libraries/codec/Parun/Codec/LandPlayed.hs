{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.LandPlayed where

import qualified Parun.Codec.CastOrigin as CastOrigin
import qualified Parun.Codec.ExileLink as ExileLink
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.LandPlayed as LandPlayed

-- | A bare object keyed by the record's field names, as
-- Parun.Codec.LeftTheGame is. Every key but "exiledWith" is required.
codec :: Codec.Codec LandPlayed.LandPlayed
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec LandPlayed.player
  land <- Fields.required "land" ObjectId.codec LandPlayed.land
  from <- Fields.required "from" CastOrigin.codec LandPlayed.from
  exiledWith <- Fields.defaulted "exiledWith" Nothing (Common.maybe ExileLink.codec) LandPlayed.exiledWith
  pure LandPlayed.MkLandPlayed {LandPlayed.player = player, LandPlayed.land = land, LandPlayed.from = from, LandPlayed.exiledWith = exiledWith}
