module Parun.Codec.DamageRecipientSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.DamageRecipient as DamageRecipient
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DamageRecipient" $ do
  -- Questing Beast's "to an opponent".
  Spec.it s "Player" $
    Common.assertCodec
      s
      DamageRecipient.codec
      (DamageRecipient.Player (PlayerRef.Relative PlayerRelation.Opponent))
      " {\"type\":\"Player\",\"value\":{\"type\":\"Relative\",\"value\":\"Opponent\"}} "
  -- Great Train Heist's "to that player".
  Spec.it s "Player in a slot" $
    Common.assertCodec
      s
      DamageRecipient.codec
      (DamageRecipient.Player (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"Player\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- Phage the Untouchable's "to a creature".
  Spec.it s "Permanent" $
    Common.assertCodec
      s
      DamageRecipient.codec
      (DamageRecipient.Permanent CardType.Creature (Filter.And []))
      " {\"type\":\"Permanent\",\"value\":{\"cardType\":\"Creature\"}} "
  -- Acidic Dagger's "to a non-Wall creature".
  Spec.it s "Permanent qualified" $
    Common.assertCodec
      s
      DamageRecipient.codec
      (DamageRecipient.Permanent CardType.Creature (Filter.Not (Filter.HasSubtype Subtype.Wall)))
      " {\"type\":\"Permanent\",\"value\":{\"cardType\":\"Creature\",\"matching\":{\"type\":\"Not\",\"value\":{\"type\":\"HasSubtype\",\"value\":\"Wall\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DamageRecipient.codec
