{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Expended where

import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Expended as Expended

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec Expended.Expended
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec Expended.player
  before <- Fields.required "before" Common.natural Expended.before
  after <- Fields.required "after" Common.natural Expended.after
  pure
    Expended.MkExpended
      { Expended.player = player,
        Expended.before = before,
        Expended.after = after
      }
