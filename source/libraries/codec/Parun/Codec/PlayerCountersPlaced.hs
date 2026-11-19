{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayerCountersPlaced where

import qualified Parun.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayerCountersPlaced as PlayerCountersPlaced

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec PlayerCountersPlaced.PlayerCountersPlaced
codec = Fields.object $ do
  putter <- Fields.required "putter" PlayerId.codec PlayerCountersPlaced.putter
  player <- Fields.required "player" PlayerId.codec PlayerCountersPlaced.player
  kind <- Fields.required "kind" PlayerCounterKind.codec PlayerCountersPlaced.kind
  before <- Fields.required "before" Common.natural PlayerCountersPlaced.before
  after <- Fields.required "after" Common.natural PlayerCountersPlaced.after
  pure
    PlayerCountersPlaced.MkPlayerCountersPlaced
      { PlayerCountersPlaced.putter = putter,
        PlayerCountersPlaced.player = player,
        PlayerCountersPlaced.kind = kind,
        PlayerCountersPlaced.before = before,
        PlayerCountersPlaced.after = after
      }
