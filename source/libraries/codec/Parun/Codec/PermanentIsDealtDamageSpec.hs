module Parun.Codec.PermanentIsDealtDamageSpec where

import qualified Parun.Codec.PermanentIsDealtDamage as PermanentIsDealtDamage
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.PermanentIsDealtDamage as PermanentIsDealtDamage

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PermanentIsDealtDamage" $ do
  Spec.it s "the batch reading omits batching" $
    Common.assertCodec
      s
      PermanentIsDealtDamage.codec
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self Nothing Nothing, PermanentIsDealtDamage.batching = Batching.OneOrMore, PermanentIsDealtDamage.excess = False})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"}}} "
  Spec.it s "the per-source reading" $
    Common.assertCodec
      s
      PermanentIsDealtDamage.codec
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self (Just DamageKind.Combat) Nothing, PermanentIsDealtDamage.batching = Batching.EachOccurrence, PermanentIsDealtDamage.excess = False})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"},\"kind\":\"Combat\"},\"batching\":\"EachOccurrence\"} "
  Spec.it s "the excess reading" $
    Common.assertCodec
      s
      PermanentIsDealtDamage.codec
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self (Just DamageKind.Noncombat) Nothing, PermanentIsDealtDamage.batching = Batching.OneOrMore, PermanentIsDealtDamage.excess = True})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"},\"kind\":\"Noncombat\"},\"excess\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentIsDealtDamage.codec
