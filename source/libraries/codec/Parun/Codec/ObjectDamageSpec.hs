module Parun.Codec.ObjectDamageSpec where

import qualified Parun.Codec.ObjectDamage as ObjectDamage
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.ObjectDamage as ObjectDamage.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ObjectDamage" $ do
  Spec.it s "any source omits the filter" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtTo, ObjectDamage.Type.by = Nothing, ObjectDamage.Type.kind = Nothing})
      " {\"direction\":\"DealtTo\"} "
  Spec.it s "sources matching a filter" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtTo, ObjectDamage.Type.by = Just (Filter.HeldByRecipient Holder.Controller), ObjectDamage.Type.kind = Nothing})
      " {\"direction\":\"DealtTo\",\"by\":{\"type\":\"HeldByRecipient\",\"value\":\"Controller\"}} "
  Spec.it s "noncombat damage alone" $
    Common.assertCodec
      s
      ObjectDamage.codec
      (ObjectDamage.Type.MkObjectDamage {ObjectDamage.Type.direction = DamageDirection.DealtBy, ObjectDamage.Type.by = Nothing, ObjectDamage.Type.kind = Just DamageKind.Noncombat})
      " {\"direction\":\"DealtBy\",\"kind\":\"Noncombat\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ObjectDamage.codec
