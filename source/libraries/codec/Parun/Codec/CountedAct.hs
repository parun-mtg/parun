{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CountedAct where

import qualified Parun.Codec.PlayerAction as PlayerAction
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CountedAct as CountedAct

codec :: Codec.Codec CountedAct.CountedAct
codec = Fields.object $ do
  action <- Fields.required "action" PlayerAction.codec CountedAct.action
  player <- Fields.required "player" PlayerId.codec CountedAct.player
  value <- Fields.required "value" Common.natural CountedAct.value
  pure CountedAct.MkCountedAct {CountedAct.action = action, CountedAct.player = player, CountedAct.value = value}
