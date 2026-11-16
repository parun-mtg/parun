module Parun.Codec.SourceDealtDamageSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.SourceDealtDamage as SourceDealtDamage
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SourceDealtDamage as SourceDealtDamage

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SourceDealtDamage" $ do
  Spec.it s "MkSourceDealtDamage, any damage" $
    Common.assertCodec
      s
      (SourceDealtDamage.codec Keyword.codec)
      (SourceDealtDamage.MkSourceDealtDamage {SourceDealtDamage.kind = Nothing, SourceDealtDamage.recipients = Set.empty})
      " {} "
  Spec.it s "MkSourceDealtDamage, combat damage to a player" $
    Common.assertCodec
      s
      (SourceDealtDamage.codec Keyword.codec)
      (SourceDealtDamage.MkSourceDealtDamage {SourceDealtDamage.kind = Just DamageKind.Combat, SourceDealtDamage.recipients = Set.singleton (DamageRecipient.Player (PlayerRef.Relative PlayerRelation.AnyPlayer))})
      " {\"kind\":\"Combat\",\"recipients\":[{\"type\":\"Player\",\"value\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"}}]} "
  Spec.it s "MkSourceDealtDamage, damage to a creature" $
    Common.assertCodec
      s
      (SourceDealtDamage.codec Keyword.codec)
      (SourceDealtDamage.MkSourceDealtDamage {SourceDealtDamage.kind = Nothing, SourceDealtDamage.recipients = Set.singleton (DamageRecipient.Permanent CardType.Creature (Filter.And []))})
      " {\"recipients\":[{\"type\":\"Permanent\",\"value\":{\"cardType\":\"Creature\"}}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (SourceDealtDamage.codec Keyword.codec)
