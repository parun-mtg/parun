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
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self Nothing Nothing, PermanentIsDealtDamage.batching = Batching.OneOrMore, PermanentIsDealtDamage.wholeBatch = False, PermanentIsDealtDamage.excess = False, PermanentIsDealtDamage.atLeast = 0})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"}}} "
  Spec.it s "the per-source reading" $
    Common.assertCodec
      s
      PermanentIsDealtDamage.codec
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self (Just DamageKind.Combat) Nothing, PermanentIsDealtDamage.batching = Batching.EachOccurrence, PermanentIsDealtDamage.wholeBatch = False, PermanentIsDealtDamage.excess = False, PermanentIsDealtDamage.atLeast = 0})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"},\"kind\":\"Combat\"},\"batching\":\"EachOccurrence\"} "
  Spec.it s "the excess reading" $
    Common.assertCodec
      s
      PermanentIsDealtDamage.codec
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self (Just DamageKind.Noncombat) Nothing, PermanentIsDealtDamage.batching = Batching.OneOrMore, PermanentIsDealtDamage.wholeBatch = False, PermanentIsDealtDamage.excess = True, PermanentIsDealtDamage.atLeast = 0})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"},\"kind\":\"Noncombat\"},\"excess\":true} "
  Spec.it s "the whole-batch reading" $
    Common.assertCodec
      s
      PermanentIsDealtDamage.codec
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self (Just DamageKind.Noncombat) Nothing, PermanentIsDealtDamage.batching = Batching.OneOrMore, PermanentIsDealtDamage.wholeBatch = True, PermanentIsDealtDamage.excess = True, PermanentIsDealtDamage.atLeast = 0})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"},\"kind\":\"Noncombat\"},\"wholeBatch\":true,\"excess\":true} "
  Spec.it s "the floor" $
    Common.assertCodec
      s
      PermanentIsDealtDamage.codec
      (PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = DealtDamage.MkDealtDamage ActingPermanent.Self Nothing Nothing, PermanentIsDealtDamage.batching = Batching.OneOrMore, PermanentIsDealtDamage.wholeBatch = False, PermanentIsDealtDamage.excess = False, PermanentIsDealtDamage.atLeast = 3})
      " {\"dealt\":{\"recipient\":{\"type\":\"Self\"}},\"atLeast\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentIsDealtDamage.codec
