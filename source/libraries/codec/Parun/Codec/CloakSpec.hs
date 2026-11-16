module Parun.Codec.CloakSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Cloak as Cloak
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Cloak as Cloak
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Cloak" $ do
  -- Ransom Note's "cloak the top card of your library": the cards key is
  -- omitted, each instructed player cloaking from their own library.
  Spec.it s "MkCloak from the instructed player's own library omits the key" $
    Common.assertCodec
      s
      Cloak.codec
      (Cloak.MkCloak (PlayerRef.Relative PlayerRelation.You) Nothing False Nothing)
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"}} "
  -- Expose the Culprit's "shuffle that pile, then cloak them".
  Spec.it s "MkCloak of a bound group in a random order" $
    Common.assertCodec
      s
      Cloak.codec
      (Cloak.MkCloak (PlayerRef.Relative PlayerRelation.You) (Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "pile")))) True Nothing)
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"pile\"},\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"shuffled\":true} "
  -- Cryptic Coat's "cloak the top card of your library, then attach this
  -- Equipment to it".
  Spec.it s "MkCloak binding the cloaked permanent" $
    Common.assertCodec
      s
      Cloak.codec
      (Cloak.MkCloak (PlayerRef.Relative PlayerRelation.You) Nothing False (Just (SlotName.MkSlotName (Text.pack "cloaked"))))
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"slot\":\"cloaked\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Cloak.codec
