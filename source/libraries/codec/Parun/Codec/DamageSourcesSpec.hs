module Parun.Codec.DamageSourcesSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.DamageSources as DamageSources
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.DamageSources as DamageSources
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SourceDealtDamage as SourceDealtDamage

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DamageSources" $ do
  Spec.it s "any source of any damage" $
    Common.assertCodec
      s
      DamageSources.codec
      (DamageSources.MkDamageSources (SourceDealtDamage.MkSourceDealtDamage Nothing Set.empty) (Filter.And []))
      " {\"dealt\":{}} "
  Spec.it s "combat damage to a player, by a source the filter admits" $
    Common.assertCodec
      s
      DamageSources.codec
      (DamageSources.MkDamageSources (SourceDealtDamage.MkSourceDealtDamage (Just DamageKind.Combat) (Set.singleton (DamageRecipient.Player (PlayerRef.Relative PlayerRelation.AnyPlayer)))) Filter.IsSource)
      " {\"dealt\":{\"kind\":\"Combat\",\"recipients\":[{\"type\":\"Player\",\"value\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"}}]},\"by\":{\"type\":\"IsSource\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s DamageSources.codec
