module Parun.Codec.TapSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Tap as Tap
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Tap as Tap

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Tap" $ do
  Spec.it s "no bound slot: the key is omitted" $
    Common.assertCodec
      s
      Tap.codec
      (Tap.plain (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "a bound slot: the key is written" $
    Common.assertCodec
      s
      Tap.codec
      (Tap.MkTap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Just (SlotName.MkSlotName (Text.pack "tapped"))) Nothing)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"tapped\":\"tapped\"} "
  Spec.it s "a tapper: the key is written" $
    Common.assertCodec
      s
      Tap.codec
      (Tap.MkTap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Nothing (Just (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "player")))))
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"tapper\":{\"type\":\"InSlot\",\"value\":\"player\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Tap.codec
