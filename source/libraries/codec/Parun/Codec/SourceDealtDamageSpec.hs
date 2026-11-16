module Parun.Codec.SourceDealtDamageSpec where

import qualified Parun.Codec.SourceDealtDamage as SourceDealtDamage
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.RecipientKind as RecipientKind
import qualified Parun.Types.SourceDealtDamage as SourceDealtDamage

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SourceDealtDamage" $ do
  Spec.it s "MkSourceDealtDamage, any damage" $
    Common.assertCodec
      s
      SourceDealtDamage.codec
      (SourceDealtDamage.MkSourceDealtDamage {SourceDealtDamage.kind = Nothing, SourceDealtDamage.recipient = RecipientKind.AnyRecipient})
      " {\"recipient\":\"AnyRecipient\"} "
  Spec.it s "MkSourceDealtDamage, combat damage to a player" $
    Common.assertCodec
      s
      SourceDealtDamage.codec
      (SourceDealtDamage.MkSourceDealtDamage {SourceDealtDamage.kind = Just DamageKind.Combat, SourceDealtDamage.recipient = RecipientKind.PlayerRecipient})
      " {\"kind\":\"Combat\",\"recipient\":\"PlayerRecipient\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SourceDealtDamage.codec
