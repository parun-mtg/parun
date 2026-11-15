module Parun.Codec.PlayerIsDealtDamageSpec where

import qualified Parun.Codec.PlayerIsDealtDamage as PlayerIsDealtDamage
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.PlayerIsDealtDamage as PlayerIsDealtDamage
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayerIsDealtDamage" $ do
  Spec.it s "the player alone omits orTheirPlaneswalkers" $
    Common.assertCodec
      s
      PlayerIsDealtDamage.codec
      (PlayerIsDealtDamage.watching (DealtDamage.MkDealtDamage (PlayerScope.Related PlayerRelation.You) Nothing Nothing))
      " {\"dealt\":{\"recipient\":{\"type\":\"You\"}}} "
  Spec.it s "the player or their planeswalkers" $
    Common.assertCodec
      s
      PlayerIsDealtDamage.codec
      ((PlayerIsDealtDamage.watching (DealtDamage.MkDealtDamage (PlayerScope.Related PlayerRelation.You) (Just DamageKind.Combat) Nothing)) {PlayerIsDealtDamage.orTheirPlaneswalkers = True})
      " {\"dealt\":{\"recipient\":{\"type\":\"You\"},\"kind\":\"Combat\"},\"orTheirPlaneswalkers\":true} "
  Spec.it s "one or more opponents during your turn" $
    Common.assertCodec
      s
      PlayerIsDealtDamage.codec
      ((PlayerIsDealtDamage.watching (DealtDamage.MkDealtDamage (PlayerScope.Related PlayerRelation.Opponent) (Just DamageKind.Combat) Nothing)) {PlayerIsDealtDamage.batching = Batching.OneOrMore, PlayerIsDealtDamage.scope = TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.You)})
      " {\"dealt\":{\"recipient\":{\"type\":\"Opponent\"},\"kind\":\"Combat\"},\"batching\":\"OneOrMore\",\"scope\":{\"type\":\"PlayersTurn\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerIsDealtDamage.codec
