{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.BindPlayers where

import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.BindPlayers as BindPlayers

codec :: Codec.Codec BindPlayers.BindPlayers
codec = Fields.object $ do
  players <- Fields.required "players" PlayerRef.codec BindPlayers.players
  slot <- Fields.required "slot" SlotName.codec BindPlayers.slot
  pure
    BindPlayers.MkBindPlayers
      { BindPlayers.players = players,
        BindPlayers.slot = slot
      }
