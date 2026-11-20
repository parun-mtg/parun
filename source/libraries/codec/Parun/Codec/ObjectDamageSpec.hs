module Parun.Codec.ObjectDamageSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ObjectDamage as ObjectDamage
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.ObjectDamage as ObjectDamage.Type
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ObjectDamage" $ do
  Spec.it s "any source omits the filter" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtTo, ObjectDamage.Type.by = Nothing, ObjectDamage.Type.kind = Nothing, ObjectDamage.Type.to = Nothing})
      " {\"direction\":\"DealtTo\"} "
  Spec.it s "sources matching a filter" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtTo, ObjectDamage.Type.by = Just (Filter.HeldByRecipient Holder.Controller), ObjectDamage.Type.kind = Nothing, ObjectDamage.Type.to = Nothing})
      " {\"direction\":\"DealtTo\",\"by\":{\"type\":\"HeldByRecipient\",\"value\":\"Controller\"}} "
  Spec.it s "noncombat damage alone" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtBy, ObjectDamage.Type.by = Nothing, ObjectDamage.Type.kind = Just DamageKind.Noncombat, ObjectDamage.Type.to = Nothing})
      " {\"direction\":\"DealtBy\",\"kind\":\"Noncombat\"} "
  Spec.it s "damage dealt to the players named" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtBy, ObjectDamage.Type.by = Nothing, ObjectDamage.Type.kind = Nothing, ObjectDamage.Type.to = Just (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")))})
      " {\"direction\":\"DealtBy\",\"to\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ObjectDamage.codec
