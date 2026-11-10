{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CountersOnPayer where

import qualified Parun.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CountersOnPayer as CountersOnPayer

codec :: Codec.Codec CountersOnPayer.CountersOnPayer
codec = Fields.object $ do
  kind <- Fields.required "kind" PlayerCounterKind.codec CountersOnPayer.kind
  count <- Fields.required "count" Common.natural CountersOnPayer.count
  pure CountersOnPayer.MkCountersOnPayer {CountersOnPayer.kind = kind, CountersOnPayer.count = count}
