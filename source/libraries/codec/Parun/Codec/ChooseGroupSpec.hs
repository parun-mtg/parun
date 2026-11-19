module Parun.Codec.ChooseGroupSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Parun.Codec.ChooseGroup as ChooseGroup
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChooseGroup as ChooseGroup
import qualified Parun.Types.Grouping as Grouping
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChooseGroup" $ do
  Spec.it s "MkChooseGroup, a sector (Space Beleren)" $
    Common.assertCodec
      s
      ChooseGroup.codec
      (ChooseGroup.MkChooseGroup Grouping.Sectors (SlotName.MkSlotName (Text.pack "sector")) Nothing you)
      " {\"among\":{\"type\":\"Sectors\"},\"slot\":\"sector\"} "
  Spec.it s "MkChooseGroup, a labelled pile (Raging River)" $
    Common.assertCodec
      s
      ChooseGroup.codec
      (ChooseGroup.MkChooseGroup (Grouping.Piles (SlotName.MkSlotName (Text.pack "left") NonEmpty.:| [SlotName.MkSlotName (Text.pack "right")])) (SlotName.MkSlotName (Text.pack "chosenPile")) (Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "attacker")))) you)
      " {\"among\":{\"type\":\"Piles\",\"value\":[\"left\",\"right\"]},\"slot\":\"chosenPile\",\"about\":{\"type\":\"InSlot\",\"value\":\"attacker\"}} "
  Spec.it s "MkChooseGroup, an opponent's pick (Hostile Negotiations)" $
    Common.assertCodec
      s
      ChooseGroup.codec
      (ChooseGroup.MkChooseGroup (Grouping.Piles (SlotName.MkSlotName (Text.pack "first") NonEmpty.:| [SlotName.MkSlotName (Text.pack "second")])) (SlotName.MkSlotName (Text.pack "chosen")) Nothing (PlayerRef.Relative PlayerRelation.Opponent))
      " {\"among\":{\"type\":\"Piles\",\"value\":[\"first\",\"second\"]},\"slot\":\"chosen\",\"chooser\":{\"type\":\"Relative\",\"value\":\"Opponent\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChooseGroup.codec
  where
    you = PlayerRef.Relative PlayerRelation.You
