{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Cloak where

import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Cloak as Cloak

-- | The cards are optional: omitted, each instructed player cloaks the top card
-- of their own library. The order is elided when unshuffled and the slot when
-- absent, ManifestDread's.
codec :: Codec.Codec Cloak.Cloak
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec Cloak.player
  cards <- Fields.defaulted "cards" Nothing (Common.maybe ObjectRef.codec) Cloak.cards
  shuffled <- Fields.defaulted "shuffled" False Common.boolean Cloak.shuffled
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Cloak.slot
  pure
    Cloak.MkCloak
      { Cloak.player = player,
        Cloak.cards = cards,
        Cloak.shuffled = shuffled,
        Cloak.slot = slot
      }
