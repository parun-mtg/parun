module Parun.Codec.ChosenCardFromAmongSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TotalCap as TotalCap

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChosenCardFromAmong" $ do
  -- Commune with the Gods' "a creature or enchantment card from among them": the
  -- slot names the group an earlier clause bound, and the Filter narrows what may
  -- be picked out of it.
  Spec.it s "MkChosenCardFromAmong, the two required keys" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.HasCardType CardType.Creature,
            ChosenCardFromAmong.count = HowMany.one,
            ChosenCardFromAmong.total = Nothing,
            ChosenCardFromAmong.perColor = Nothing,
            ChosenCardFromAmong.chooser = PlayerRef.Relative PlayerRelation.You,
            ChosenCardFromAmong.pileChooser = Nothing,
            ChosenCardFromAmong.faceUpPile = False,
            ChosenCardFromAmong.selection = CardSelection.ByChoice
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  -- Uncovered Clues' "up to two" and Animal Magnetism's opponent: the two keys
  -- the defaults above leave off the wire entirely.
  Spec.it s "MkChosenCardFromAmong, count and chooser" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.HasCardType CardType.Creature,
            ChosenCardFromAmong.count = HowMany.UpTo (Just (Quantity.Literal 2)),
            ChosenCardFromAmong.total = Nothing,
            ChosenCardFromAmong.perColor = Nothing,
            ChosenCardFromAmong.chooser = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "opponent")),
            ChosenCardFromAmong.pileChooser = Nothing,
            ChosenCardFromAmong.faceUpPile = False,
            ChosenCardFromAmong.selection = CardSelection.ByChoice
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"count\":{\"type\":\"UpTo\",\"value\":{\"type\":\"Literal\",\"value\":2}},\"chooser\":{\"type\":\"InSlot\",\"value\":\"opponent\"}} "
  -- Fact or Fiction: an opponent separates, and you take a pile (CR 700.3).
  Spec.it s "MkChosenCardFromAmong, a pile chooser" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.And [],
            ChosenCardFromAmong.count = HowMany.UpTo Nothing,
            ChosenCardFromAmong.total = Nothing,
            ChosenCardFromAmong.perColor = Nothing,
            ChosenCardFromAmong.chooser = PlayerRef.Relative PlayerRelation.Opponent,
            ChosenCardFromAmong.pileChooser = Just (PlayerRef.Relative PlayerRelation.You),
            ChosenCardFromAmong.faceUpPile = False,
            ChosenCardFromAmong.selection = CardSelection.ByChoice
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"And\",\"value\":[]},\"count\":{\"type\":\"UpTo\"},\"chooser\":{\"type\":\"Relative\",\"value\":\"Opponent\"},\"pileChooser\":{\"type\":\"Relative\",\"value\":\"You\"}} "
  -- Curator of Destinies: you separate into a face-up and a face-down pile, and
  -- an opponent takes one (CR 700.3).
  Spec.it s "MkChosenCardFromAmong, a face-up pile" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "looked"),
            ChosenCardFromAmong.filter = Filter.And [],
            ChosenCardFromAmong.count = HowMany.UpTo Nothing,
            ChosenCardFromAmong.total = Nothing,
            ChosenCardFromAmong.perColor = Nothing,
            ChosenCardFromAmong.chooser = PlayerRef.Relative PlayerRelation.You,
            ChosenCardFromAmong.pileChooser = Just (PlayerRef.Relative PlayerRelation.Opponent),
            ChosenCardFromAmong.faceUpPile = True,
            ChosenCardFromAmong.selection = CardSelection.ByChoice
          }
      )
      " {\"slot\":\"looked\",\"filter\":{\"type\":\"And\",\"value\":[]},\"count\":{\"type\":\"UpTo\"},\"pileChooser\":{\"type\":\"Relative\",\"value\":\"Opponent\"},\"faceUpPile\":true} "
  -- Getaway Barrel's "a random creature card from among them".
  Spec.it s "MkChosenCardFromAmong, at random" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.HasCardType CardType.Creature,
            ChosenCardFromAmong.count = HowMany.one,
            ChosenCardFromAmong.total = Nothing,
            ChosenCardFromAmong.perColor = Nothing,
            ChosenCardFromAmong.chooser = PlayerRef.Relative PlayerRelation.You,
            ChosenCardFromAmong.pileChooser = Nothing,
            ChosenCardFromAmong.faceUpPile = False,
            ChosenCardFromAmong.selection = CardSelection.AtRandom
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"selection\":\"AtRandom\"} "
  -- Michelangelo's Technique's "up to two creature cards with total mana value
  -- 6 or less".
  Spec.it s "MkChosenCardFromAmong, a total" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "looked"),
            ChosenCardFromAmong.filter = Filter.HasCardType CardType.Creature,
            ChosenCardFromAmong.count = HowMany.UpTo (Just (Quantity.Literal 2)),
            ChosenCardFromAmong.total = Just TotalCap.MkTotalCap {TotalCap.weight = Quantity.Measure Measure.ManaValue, TotalCap.most = Quantity.Literal 6},
            ChosenCardFromAmong.perColor = Nothing,
            ChosenCardFromAmong.chooser = PlayerRef.Relative PlayerRelation.You,
            ChosenCardFromAmong.pileChooser = Nothing,
            ChosenCardFromAmong.faceUpPile = False,
            ChosenCardFromAmong.selection = CardSelection.ByChoice
          }
      )
      " {\"slot\":\"looked\",\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"count\":{\"type\":\"UpTo\",\"value\":{\"type\":\"Literal\",\"value\":2}},\"total\":{\"weight\":{\"type\":\"Measure\",\"value\":\"ManaValue\"},\"most\":{\"type\":\"Literal\",\"value\":6}}} "
  -- Sanar, Innovative First-Year's "for each of those colors, you may exile a
  -- card of that color".
  Spec.it s "MkChosenCardFromAmong, a card per color" $
    Common.assertCodec
      s
      ChosenCardFromAmong.codec
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          { ChosenCardFromAmong.slot = SlotName.MkSlotName (Text.pack "revealed"),
            ChosenCardFromAmong.filter = Filter.And [],
            ChosenCardFromAmong.count = HowMany.UpTo Nothing,
            ChosenCardFromAmong.total = Nothing,
            ChosenCardFromAmong.perColor = Just (Filter.HasCardType CardType.Creature),
            ChosenCardFromAmong.chooser = PlayerRef.Relative PlayerRelation.You,
            ChosenCardFromAmong.pileChooser = Nothing,
            ChosenCardFromAmong.faceUpPile = False,
            ChosenCardFromAmong.selection = CardSelection.ByChoice
          }
      )
      " {\"slot\":\"revealed\",\"filter\":{\"type\":\"And\",\"value\":[]},\"count\":{\"type\":\"UpTo\"},\"perColor\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenCardFromAmong.codec
