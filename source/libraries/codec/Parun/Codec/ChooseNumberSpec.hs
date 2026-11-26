module Parun.Codec.ChooseNumberSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ChooseNumber as ChooseNumber
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.ChooseNumber as ChooseNumber
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChooseNumber" $ do
  Spec.it s "unbounded" $
    Common.assertCodec
      s
      ChooseNumber.codec
      (ChooseNumber.MkChooseNumber (SlotName.MkSlotName (Text.pack "number")) Nothing (PlayerRef.Relative PlayerRelation.You) CardSelection.ByChoice)
      " {\"slot\":\"number\"} "
  Spec.it s "bounded" $
    Common.assertCodec
      s
      ChooseNumber.codec
      (ChooseNumber.MkChooseNumber (SlotName.MkSlotName (Text.pack "drawn")) (Just (Quantity.Literal 4)) (PlayerRef.Relative PlayerRelation.You) CardSelection.ByChoice)
      " {\"slot\":\"drawn\",\"upTo\":{\"type\":\"Literal\",\"value\":4}} "
  Spec.it s "a chooser other than you" $
    Common.assertCodec
      s
      ChooseNumber.codec
      (ChooseNumber.MkChooseNumber (SlotName.MkSlotName (Text.pack "drawn")) (Just (Quantity.Literal 2)) (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "player"))) CardSelection.ByChoice)
      " {\"slot\":\"drawn\",\"upTo\":{\"type\":\"Literal\",\"value\":2},\"chooser\":{\"type\":\"InSlot\",\"value\":\"player\"}} "
  Spec.it s "at random" $
    Common.assertCodec
      s
      ChooseNumber.codec
      (ChooseNumber.MkChooseNumber (SlotName.MkSlotName (Text.pack "damage")) (Just (Quantity.Literal 4)) (PlayerRef.Relative PlayerRelation.You) CardSelection.AtRandom)
      " {\"slot\":\"damage\",\"upTo\":{\"type\":\"Literal\",\"value\":4},\"selection\":\"AtRandom\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChooseNumber.codec
