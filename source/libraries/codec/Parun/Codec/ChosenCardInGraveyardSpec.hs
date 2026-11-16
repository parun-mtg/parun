module Parun.Codec.ChosenCardInGraveyardSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Chooser as Chooser
import qualified Parun.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TotalCap as TotalCap
import qualified Parun.Types.ZoneScope as ZoneScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChosenCardInGraveyard" $ do
  -- CR 404.1 with a choice. The chooser and the graveyards' owners are
  -- separate questions -- "target opponent chooses a card in YOUR graveyard"
  -- names two seats -- which is the shape a positional payload could state
  -- backwards.
  Spec.it s "MkChosenCardInGraveyard, every key" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.TheController,
            ChosenCardInGraveyard.players = ZoneScope.Scoped (PlayerScope.Related PlayerRelation.You),
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Creature,
            ChosenCardInGraveyard.count = HowMany.one,
            ChosenCardInGraveyard.total = Nothing
          }
      )
      " {\"chooser\":{\"type\":\"TheController\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  -- The scope's other arm, which is what Grasping Tentacles' "that player's
  -- graveyard" needs: whose is the slot the spell's own target filled, not a
  -- reading CR 109.5 answers.
  Spec.it s "MkChosenCardInGraveyard, a slot-named scope" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.TheController,
            ChosenCardInGraveyard.players = ZoneScope.InSlot (SlotName.MkSlotName (Text.pack "opponent")),
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Artifact,
            ChosenCardInGraveyard.count = HowMany.one,
            ChosenCardInGraveyard.total = Nothing
          }
      )
      " {\"chooser\":{\"type\":\"TheController\"},\"players\":{\"type\":\"InSlot\",\"value\":\"opponent\"},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Artifact\"}} "
  -- The count Fall of the Thran's "two land cards" states: defaulted, so the
  -- printed singular above encodes without it.
  Spec.it s "MkChosenCardInGraveyard, a count above one" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.EachInScope,
            ChosenCardInGraveyard.players = ZoneScope.Scoped (PlayerScope.Related PlayerRelation.AnyPlayer),
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Land,
            ChosenCardInGraveyard.count = HowMany.Exactly (Quantity.Literal 2),
            ChosenCardInGraveyard.total = Nothing
          }
      )
      " {\"chooser\":{\"type\":\"EachInScope\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"AnyPlayer\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"count\":{\"type\":\"Exactly\",\"value\":{\"type\":\"Literal\",\"value\":2}}} "
  -- Liliana, the Necromancer's "up to two creature cards from graveyards".
  Spec.it s "MkChosenCardInGraveyard, an up-to count" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.TheController,
            ChosenCardInGraveyard.players = ZoneScope.Scoped (PlayerScope.Related PlayerRelation.AnyPlayer),
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Creature,
            ChosenCardInGraveyard.count = HowMany.UpTo (Just (Quantity.Literal 2)),
            ChosenCardInGraveyard.total = Nothing
          }
      )
      " {\"chooser\":{\"type\":\"TheController\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"AnyPlayer\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"count\":{\"type\":\"UpTo\",\"value\":{\"type\":\"Literal\",\"value\":2}}} "
  -- Lively Dirge's "with total mana value 4 or less": defaulted, so every
  -- uncapped choice above encodes without it.
  Spec.it s "MkChosenCardInGraveyard, a total" $
    Common.assertCodec
      s
      ChosenCardInGraveyard.codec
      ( ChosenCardInGraveyard.MkChosenCardInGraveyard
          { ChosenCardInGraveyard.chooser = Chooser.TheController,
            ChosenCardInGraveyard.players = ZoneScope.Scoped (PlayerScope.Related PlayerRelation.You),
            ChosenCardInGraveyard.filter = Filter.HasCardType CardType.Creature,
            ChosenCardInGraveyard.count = HowMany.UpTo (Just (Quantity.Literal 2)),
            ChosenCardInGraveyard.total = Just TotalCap.MkTotalCap {TotalCap.weight = Quantity.Measure Measure.ManaValue, TotalCap.most = Quantity.Literal 4}
          }
      )
      " {\"chooser\":{\"type\":\"TheController\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"count\":{\"type\":\"UpTo\",\"value\":{\"type\":\"Literal\",\"value\":2}},\"total\":{\"weight\":{\"type\":\"Measure\",\"value\":\"ManaValue\"},\"most\":{\"type\":\"Literal\",\"value\":4}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenCardInGraveyard.codec
