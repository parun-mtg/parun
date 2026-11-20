module Parun.Codec.SeatViewSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.ObjectViewSpec as ObjectViewSpec
import qualified Parun.Codec.SeatView as SeatView
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Departure as Departure
import qualified Parun.Types.DraftNote as DraftNote
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.PlayerDesignation as PlayerDesignation
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.Status as Status

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SeatView" $ do
  Spec.it s "every key" $
    Common.assertCodec
      s
      SeatView.codec
      testSeat
      testSeatJson
  Spec.it s "CR 905.2b draft notes" $
    Common.assertCodec
      s
      SeatView.codec
      testSeat {SeatView.draftNotes = Map.singleton (CardName.MkCardName (Text.pack "Garbage Fire")) (Seq.fromList [DraftNote.Number 2, DraftNote.Name (CardName.MkCardName (Text.pack "Shock"))])}
      (List.init testSeatJson <> ",\"draftNotes\":[{\"key\":\"Garbage Fire\",\"value\":[{\"type\":\"Number\",\"value\":2},{\"type\":\"Name\",\"value\":\"Shock\"}]}]}")
  -- CR 701.54c / 702.179b / 702.131c / 309.7.
  Spec.it s "the Ring, speed, the city's blessing and completed dungeons" $
    Common.assertCodec
      s
      SeatView.codec
      testSeat {SeatView.ringTemptations = 2, SeatView.speed = Just 3, SeatView.designations = Set.singleton PlayerDesignation.CitysBlessing, SeatView.completedDungeons = 1}
      (List.init testSeatJson <> ",\"ringTemptations\":2,\"speed\":3,\"designations\":[\"CitysBlessing\"],\"completedDungeons\":1}")
  -- Two seats' commanders, one a partner pair, so both levels key apart.
  Spec.it s "CR 903.10a commander damage" $
    Common.assertCodec
      s
      SeatView.codec
      testSeat {SeatView.commanderDamage = Map.fromList [(PlayerId.MkPlayerId 1, Map.fromList [(CardName.MkCardName (Text.pack "Akiri, Line-Slinger"), 3), (CardName.MkCardName (Text.pack "Rograkh, Son of Rohgahh"), 7)]), (PlayerId.MkPlayerId 2, Map.singleton (CardName.MkCardName (Text.pack "Jedit Ojanen")) 5)]}
      (List.init testSeatJson <> ",\"commanderDamage\":{\"1\":[{\"key\":\"Akiri, Line-Slinger\",\"value\":3},{\"key\":\"Rograkh, Son of Rohgahh\",\"value\":7}],\"2\":[{\"key\":\"Jedit Ojanen\",\"value\":5}]}}")
  Spec.it s "CR 800.4a a seat that left the game" $
    Common.assertCodec
      s
      SeatView.codec
      testSeat {SeatView.status = Status.Departed Departure.Conceded}
      (List.init testSeatJson <> ",\"status\":{\"type\":\"Departed\",\"value\":\"Conceded\"}}")
  Spec.it s "has a schema" $ Common.assertHasSchema s SeatView.codec

-- A seat whose hand shows one of its three cards.
testSeat :: SeatView.SeatView
testSeat =
  SeatView.MkSeatView
    { SeatView.player = PlayerId.MkPlayerId 0,
      SeatView.status = Status.Playing,
      SeatView.life = 20,
      SeatView.counters = Map.empty,
      SeatView.handSize = 3,
      SeatView.hand = [ObjectViewSpec.hidden],
      SeatView.librarySize = 40,
      SeatView.library = [],
      SeatView.graveyard = [],
      SeatView.exile = [],
      SeatView.manaPool = Mana.MkMana [],
      SeatView.ringTemptations = 0,
      SeatView.speed = Nothing,
      SeatView.designations = Set.empty,
      SeatView.completedDungeons = 0,
      SeatView.draftNotes = Map.empty,
      SeatView.commanderDamage = Map.empty
    }

testSeatJson :: String
testSeatJson =
  "{\"player\":0,\"life\":20,\"counters\":[],\"handSize\":3,\"hand\":["
    <> ObjectViewSpec.hiddenJson
    <> "],\"librarySize\":40,\"library\":[],\"graveyard\":[],\"exile\":[],\"manaPool\":[]}"
