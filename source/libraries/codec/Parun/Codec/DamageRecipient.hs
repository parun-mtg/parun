module Parun.Codec.DamageRecipient where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CardType as CardType
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CardType as CardType.Type
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.Filter as Filter.Type

codec :: (Typeable.Typeable keyword, Ord keyword) => Codec.Codec keyword -> Codec.Codec (DamageRecipient.DamageRecipient keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "Player" PlayerRef.codec DamageRecipient.Player (\x -> case x of DamageRecipient.Player y -> Just y; _ -> Nothing),
      Arm.payload "Permanent" (permanent keywordCodec) (uncurry DamageRecipient.Permanent) (\x -> case x of DamageRecipient.Permanent t f -> Just (t, f); _ -> Nothing)
    ]

-- | The card type, and the qualifier, absent where it admits every permanent of it.
permanent :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CardType.Type.CardType, Filter.Type.Filter keyword)
permanent keywordCodec =
  Fields.object $
    (,)
      <$> Fields.required "cardType" CardType.codec fst
      <*> Fields.defaulted "matching" (Filter.Type.And []) (Filter.codec keywordCodec) snd

tagOf :: DamageRecipient.DamageRecipient keyword -> String
tagOf x = case x of
  DamageRecipient.Player {} -> "Player"
  DamageRecipient.Permanent {} -> "Permanent"
