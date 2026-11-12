{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayerExpends where

import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayerExpends as PlayerExpends

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec PlayerExpends.PlayerExpends
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec PlayerExpends.player
  amount <- Fields.required "amount" Common.natural PlayerExpends.amount
  pure
    PlayerExpends.MkPlayerExpends
      { PlayerExpends.player = player,
        PlayerExpends.amount = amount
      }
