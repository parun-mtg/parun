{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChoosePlayer where

import qualified Parun.Codec.PlayerScope as PlayerScope
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChoosePlayer as ChoosePlayer

-- | A bare object keyed by the record's field names. The scope is REQUIRED
-- rather than defaulted: "choose an opponent" and "choose a player" are
-- different printed sentences, and picking one as the default would let the
-- other be written by omission.
codec :: Codec.Codec ChoosePlayer.ChoosePlayer
codec = Fields.object $ do
  scope <- Fields.required "scope" PlayerScope.codec ChoosePlayer.scope
  slot <- Fields.required "slot" SlotName.codec ChoosePlayer.slot
  excluding <- Fields.defaulted "excluding" Nothing (Common.maybe SlotName.codec) ChoosePlayer.excluding
  attaching <- Fields.defaulted "attaching" False Common.boolean ChoosePlayer.attaching
  pure
    ChoosePlayer.MkChoosePlayer
      { ChoosePlayer.scope = scope,
        ChoosePlayer.slot = slot,
        ChoosePlayer.excluding = excluding,
        ChoosePlayer.attaching = attaching
      }
