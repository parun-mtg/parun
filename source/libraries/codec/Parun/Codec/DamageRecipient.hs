module Parun.Codec.DamageRecipient where

import qualified Parun.Codec.CardType as CardType
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CardType as CardType.Type
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.Keyword as Keyword.Type

codec :: Codec.Codec DamageRecipient.DamageRecipient
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Player" PlayerRelation.codec DamageRecipient.Player (\x -> case x of DamageRecipient.Player y -> Just y; _ -> Nothing),
      Arm.payload "Permanent" permanent (uncurry DamageRecipient.Permanent) (\x -> case x of DamageRecipient.Permanent t f -> Just (t, f); _ -> Nothing)
    ]

-- | The card type, and the qualifier, absent where it admits every permanent of it.
permanent :: Codec.Codec (CardType.Type.CardType, Filter.Type.Filter Keyword.Type.Keyword)
permanent =
  Fields.object $
    (,)
      <$> Fields.required "cardType" CardType.codec fst
      <*> Fields.defaulted "matching" (Filter.Type.And []) (Filter.codec Keyword.codec) snd

tagOf :: DamageRecipient.DamageRecipient -> String
tagOf x = case x of
  DamageRecipient.Player {} -> "Player"
  DamageRecipient.Permanent {} -> "Permanent"
