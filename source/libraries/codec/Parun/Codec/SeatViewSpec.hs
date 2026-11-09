module Parun.Codec.SeatViewSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.ObjectViewSpec as ObjectViewSpec
import qualified Parun.Codec.SeatView as SeatView
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.SeatView as SeatView

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
      testSeat {SeatView.draftNotes = Map.singleton (CardName.MkCardName (Text.pack "Garbage Fire")) (Seq.fromList [2, 5])}
      (List.init testSeatJson <> ",\"draftNotes\":[{\"key\":\"Garbage Fire\",\"value\":[2,5]}]}")
  Spec.it s "has a schema" $ Common.assertHasSchema s SeatView.codec

-- A seat whose hand shows one of its three cards.
testSeat :: SeatView.SeatView
testSeat =
  SeatView.MkSeatView
    { SeatView.player = PlayerId.MkPlayerId 0,
      SeatView.life = 20,
      SeatView.counters = Map.empty,
      SeatView.handSize = 3,
      SeatView.hand = [ObjectViewSpec.hidden],
      SeatView.librarySize = 40,
      SeatView.library = [],
      SeatView.graveyard = [],
      SeatView.exile = [],
      SeatView.manaPool = Mana.MkMana [],
      SeatView.draftNotes = Map.empty
    }

testSeatJson :: String
testSeatJson =
  "{\"player\":0,\"life\":20,\"counters\":[],\"handSize\":3,\"hand\":["
    <> ObjectViewSpec.hiddenJson
    <> "],\"librarySize\":40,\"library\":[],\"graveyard\":[],\"exile\":[],\"manaPool\":[]}"
