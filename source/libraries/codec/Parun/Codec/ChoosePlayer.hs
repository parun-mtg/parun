{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChoosePlayer where

import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.PlayerScope as PlayerScope
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChoosePlayer as ChoosePlayer
import qualified Parun.Types.PlayerRef as PlayerRef.Type
import qualified Parun.Types.PlayerRelation as PlayerRelation

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
  linked <- Fields.defaulted "linked" False Common.boolean ChoosePlayer.linked
  chooser <- Fields.defaulted "chooser" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec ChoosePlayer.chooser
  pure
    ChoosePlayer.MkChoosePlayer
      { ChoosePlayer.scope = scope,
        ChoosePlayer.slot = slot,
        ChoosePlayer.excluding = excluding,
        ChoosePlayer.attaching = attaching,
        ChoosePlayer.linked = linked,
        ChoosePlayer.chooser = chooser
      }
