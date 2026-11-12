{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Discovered where

import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Discovered as Discovered

codec :: Codec.Codec Discovered.Discovered
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec Discovered.player
  value <- Fields.required "value" Common.natural Discovered.value
  pure Discovered.MkDiscovered {Discovered.player = player, Discovered.value = value}
