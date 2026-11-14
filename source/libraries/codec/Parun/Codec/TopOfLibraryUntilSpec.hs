module Parun.Codec.TopOfLibraryUntilSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Parun.Codec.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TopOfLibraryUntil" $ do
  -- Treasure Hunt's "until you reveal a nonland card": one match, written as a
  -- literal.
  Spec.it s "MkTopOfLibraryUntil, all three keys" $
    Common.assertCodec
      s
      TopOfLibraryUntil.codec
      ( TopOfLibraryUntil.MkTopOfLibraryUntil
          { TopOfLibraryUntil.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibraryUntil.filter = Filter.Not (Filter.HasCardType CardType.Land),
            TopOfLibraryUntil.weight = Quantity.Literal 1,
            TopOfLibraryUntil.count = Quantity.Literal 1,
            TopOfLibraryUntil.deck = Nothing
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Land\"}},\"count\":{\"type\":\"Literal\",\"value\":1}} "
  -- Open the Way's "until you reveal X land cards": the count is CR 601.2b's
  -- announced X, read from the slot casting filled.
  Spec.it s "MkTopOfLibraryUntil carries the announced X Open the Way needs" $
    Common.assertCodec
      s
      TopOfLibraryUntil.codec
      ( TopOfLibraryUntil.MkTopOfLibraryUntil
          { TopOfLibraryUntil.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibraryUntil.filter = Filter.HasCardType CardType.Land,
            TopOfLibraryUntil.weight = Quantity.Literal 1,
            TopOfLibraryUntil.count = Quantity.InSlot (SlotName.MkSlotName (Text.pack "X")),
            TopOfLibraryUntil.deck = Nothing
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"count\":{\"type\":\"InSlot\",\"value\":\"X\"}} "
  -- Improvisation Capstone's "until you exile cards with total mana value 4 or
  -- greater": every card matches and weighs its mana value.
  Spec.it s "MkTopOfLibraryUntil carries the weight Improvisation Capstone needs" $
    Common.assertCodec
      s
      TopOfLibraryUntil.codec
      ( TopOfLibraryUntil.MkTopOfLibraryUntil
          { TopOfLibraryUntil.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibraryUntil.filter = Filter.And [],
            TopOfLibraryUntil.weight = Quantity.Measure Measure.ManaValue,
            TopOfLibraryUntil.count = Quantity.Literal 4,
            TopOfLibraryUntil.deck = Nothing
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"filter\":{\"type\":\"And\",\"value\":[]},\"weight\":{\"type\":\"Measure\",\"value\":\"ManaValue\"},\"count\":{\"type\":\"Literal\",\"value\":4}} "
  -- The filter is what a MATCH is, so a ref without one names a walk that never
  -- stops: a decode failure rather than a library taken whole.
  Spec.it s "a walk with no filter is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"count\":{\"type\":\"Literal\",\"value\":1}} ") >>= Codec.decode TopOfLibraryUntil.codec))
      "expected a decode failure"
  -- And a ref with no count does not say how many matches end the walk.
  Spec.it s "a walk with no count is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Land\"}} ") >>= Codec.decode TopOfLibraryUntil.codec))
      "expected a decode failure"
  -- The Fertile Lands of Saulvinia's "until you reveal a plane card" (CR 901.3).
  Spec.it s "MkTopOfLibraryUntil, a side deck" $
    Common.assertCodec
      s
      TopOfLibraryUntil.codec
      ( TopOfLibraryUntil.MkTopOfLibraryUntil
          { TopOfLibraryUntil.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibraryUntil.filter = Filter.HasCardType CardType.Plane,
            TopOfLibraryUntil.weight = Quantity.Literal 1,
            TopOfLibraryUntil.count = Quantity.Literal 1,
            TopOfLibraryUntil.deck = Just SideDeck.Planar
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"filter\":{\"type\":\"HasCardType\",\"value\":\"Plane\"},\"count\":{\"type\":\"Literal\",\"value\":1},\"deck\":\"Planar\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TopOfLibraryUntil.codec
