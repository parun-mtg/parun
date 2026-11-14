{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayerSlot where

import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayerSlot as PlayerSlot

-- | A bare object keyed by the record's field names, both required: "for each
-- opponent" and "for each player" are different printed sentences.
codec :: Codec.Codec PlayerSlot.PlayerSlot
codec = Fields.object $ do
  players <- Fields.required "players" PlayerRelation.codec PlayerSlot.players
  slot <- Fields.required "slot" SlotName.codec PlayerSlot.slot
  pure
    PlayerSlot.MkPlayerSlot
      { PlayerSlot.players = players,
        PlayerSlot.slot = slot
      }
