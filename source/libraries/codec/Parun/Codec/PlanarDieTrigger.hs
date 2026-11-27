{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlanarDieTrigger where

import qualified Parun.Codec.PlanarDieFace as PlanarDieFace
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlanarDieTrigger as PlanarDieTrigger

-- | A bare object keyed by the record's field names; an absent face matches
-- every face.
codec :: Codec.Codec PlanarDieTrigger.PlanarDieTrigger
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec PlanarDieTrigger.player
  face <- Fields.optional "face" PlanarDieFace.codec PlanarDieTrigger.face
  pure PlanarDieTrigger.MkPlanarDieTrigger {PlanarDieTrigger.player = player, PlanarDieTrigger.face = face}
