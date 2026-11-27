module Parun.Codec.ChoosePlayerSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ChoosePlayer as ChoosePlayer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChoosePlayer as ChoosePlayer
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChoosePlayer" $ do
  -- CR 608.2d's two printed sentences, so the pair catches a codec that read
  -- the scope off anything but the wire.
  Spec.it s "MkChoosePlayer, an opponent (Skullwinder)" $
    Common.assertCodec
      s
      ChoosePlayer.codec
      ( ChoosePlayer.MkChoosePlayer
          { ChoosePlayer.scope = PlayerScope.Related PlayerRelation.Opponent,
            ChoosePlayer.slot = SlotName.MkSlotName (Text.pack "opponent"),
            ChoosePlayer.excluding = Nothing,
            ChoosePlayer.attaching = False,
            ChoosePlayer.linked = False
          }
      )
      " {\"scope\":{\"type\":\"Opponent\"},\"slot\":\"opponent\"} "
  Spec.it s "MkChoosePlayer, a player (Stadium Vendors)" $
    Common.assertCodec
      s
      ChoosePlayer.codec
      ( ChoosePlayer.MkChoosePlayer
          { ChoosePlayer.scope = PlayerScope.Related PlayerRelation.AnyPlayer,
            ChoosePlayer.slot = SlotName.MkSlotName (Text.pack "chosen"),
            ChoosePlayer.excluding = Nothing,
            ChoosePlayer.attaching = False,
            ChoosePlayer.linked = False
          }
      )
      " {\"scope\":{\"type\":\"AnyPlayer\"},\"slot\":\"chosen\"} "
  Spec.it s "MkChoosePlayer, a different opponent (Dack Fayden, Helping Hand)" $
    Common.assertCodec
      s
      ChoosePlayer.codec
      ( ChoosePlayer.MkChoosePlayer
          { ChoosePlayer.scope = PlayerScope.Related PlayerRelation.Opponent,
            ChoosePlayer.slot = SlotName.MkSlotName (Text.pack "opponent"),
            ChoosePlayer.excluding = Just (SlotName.MkSlotName (Text.pack "thoseChosenForOthers")),
            ChoosePlayer.attaching = False,
            ChoosePlayer.linked = False
          }
      )
      " {\"excluding\":\"thoseChosenForOthers\",\"scope\":{\"type\":\"Opponent\"},\"slot\":\"opponent\"} "
  Spec.it s "MkChoosePlayer, a player to attach to (Curse of Leeches)" $
    Common.assertCodec
      s
      ChoosePlayer.codec
      ( ChoosePlayer.MkChoosePlayer
          { ChoosePlayer.scope = PlayerScope.Related PlayerRelation.AnyPlayer,
            ChoosePlayer.slot = SlotName.MkSlotName (Text.pack "player"),
            ChoosePlayer.excluding = Nothing,
            ChoosePlayer.attaching = True,
            ChoosePlayer.linked = False
          }
      )
      " {\"attaching\":true,\"scope\":{\"type\":\"AnyPlayer\"},\"slot\":\"player\"} "
  Spec.it s "MkChoosePlayer, the source's chosen player (Imprison This Insolent Wretch)" $
    Common.assertCodec
      s
      ChoosePlayer.codec
      ( ChoosePlayer.MkChoosePlayer
          { ChoosePlayer.scope = PlayerScope.Related PlayerRelation.Opponent,
            ChoosePlayer.slot = SlotName.MkSlotName (Text.pack "opponent"),
            ChoosePlayer.excluding = Nothing,
            ChoosePlayer.attaching = False,
            ChoosePlayer.linked = True
          }
      )
      " {\"linked\":true,\"scope\":{\"type\":\"Opponent\"},\"slot\":\"opponent\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChoosePlayer.codec
