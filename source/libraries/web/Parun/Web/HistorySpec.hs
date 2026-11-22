module Parun.Web.HistorySpec where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.History as History
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.History" $ do
  Spec.describe s "codec" $ do
    Spec.it s "reads back the line it writes" $ do
      let line = History.encode game
      Spec.assertEqWith s "one line" (length (Text.lines line)) 1
      Spec.assertEqWith s "round trip" (History.decode line) (Right game)

    Spec.it s "refuses a summary missing its verdict" $ do
      Spec.assertBool s (either (const True) (const False) (History.decode (Text.pack "{\"ended\":1}"))) "refused"

  Spec.describe s "adding" $ do
    Spec.it s "keeps the newest games up to the cap, dropping the oldest" $ do
      let older = [game {History.ended = n} | n <- [1 .. toInteger History.cap]]
          (kept, dropped) = History.adding game {History.ended = 1000} older
      Spec.assertEqWith s "kept" (fmap History.ended kept) (1000 : [toInteger History.cap, toInteger History.cap - 1 .. 2])
      Spec.assertEqWith s "dropped" (fmap History.ended dropped) [1]

    Spec.it s "keeps a game of a key already kept once" $ do
      let (kept, dropped) = History.adding game [game {History.turns = 3}]
      Spec.assertEqWith s "kept" kept [game]
      Spec.assertEqWith s "dropped" dropped []

  Spec.describe s "recordOf" $ do
    let finished verdict = Ending.MkFinished (Ending.MkEnding verdict [Text.pack "Player 1 wins."] scenario Seq.empty (PlayerId.MkPlayerId 0) [])
        recorded = History.recordOf 1795266300000 60 (Text.pack "build") (History.seats game)
    Spec.it s "keeps a game that ended with a result, its turn read off its last entry" $ do
      Spec.assertEqWith s "won" (fmap fst (recorded (finished Ending.MkWon))) (Just game {History.verdict = Ending.MkWon, History.turns = 7})
      Spec.assertEqWith s "the scenario" (fmap snd (recorded (finished Ending.MkDrawn))) (Just scenario)

    Spec.it s "keeps no game given up from the home screen or stopped short" $ do
      Spec.assertEqWith s "given up" (recorded Ending.MkAbandoned) Nothing
      Spec.assertEqWith s "stopped" (recorded (finished Ending.MkStopped)) Nothing

  Spec.describe s "viewerLabel" $ do
    Spec.it s "names the player's seat as the game's scenario does" $ do
      Spec.assertEqWith s "the host" (History.viewerLabel game) (Just (Text.pack "player1"))
      Spec.assertEqWith s "a guest" (History.viewerLabel game {History.seats = reverse (History.seats game)}) (Just (Text.pack "player2"))

  Spec.describe s "date" $ do
    Spec.it s "tells the day and time on the page's clock then" $ do
      Spec.assertEqWith s "ahead of UTC" (History.date game) "21 Nov 2026, 14:05"
      Spec.assertEqWith s "behind it, across a leap day" (History.date game {History.ended = 1709251140000, History.offset = -300}) "29 Feb 2024, 18:59"
      Spec.assertEqWith s "past midnight" (History.date game {History.ended = 1709251140000, History.offset = 60}) "1 Mar 2024, 00:59"

  Spec.describe s "row" $ do
    Spec.it s "says how it ended, the decks and who played them, its turn and date" $ do
      let page = Html.toText (History.row game [])
          has piece = Spec.assertBool s (Text.isInfixOf (Text.pack piece) page) piece
      has "<span class=\"result lost\">Lost</span>"
      has "Ajani (M19)"
      has "Liliana (M19)"
      has "You vs Bot \x00b7 Turn 9 \x00b7 21 Nov 2026, 14:05"

game :: History.Game
game =
  History.MkGame
    { History.ended = 1795266300000,
      History.offset = 60,
      History.build = Text.pack "build",
      History.verdict = Ending.MkLost,
      History.turns = 9,
      History.said = [Text.pack "Player 1 wins."],
      History.seats =
        [ History.MkSeat History.MkYou (Text.pack "Ajani (M19)") (Just (Text.pack "sets/m19/ajani")) (Set.singleton Color.White),
          History.MkSeat History.MkBot (Text.pack "Liliana (M19)") (Just (Text.pack "sets/m19/liliana")) (Set.singleton Color.Black)
        ]
    }

-- A scenario file whose last entry is on turn 7.
scenario :: Text.Text
scenario =
  Text.pack
    "{\"description\":\"Played.\",\"game\":[{\"name\":\"player1\",\"deck\":[\"20 Plains\"]},{\"name\":\"player2\",\"deck\":[\"20 Swamp\"]}],\"timeline\":[{\"turn\":2,\"step\":\"PrecombatMain\",\"player\":\"player1\",\"do\":{\"Answer\":{\"prompt\":\"ChooseAction\",\"with\":0}}},{\"turn\":7,\"step\":\"PrecombatMain\",\"player\":\"player2\",\"do\":{\"Answer\":{\"prompt\":\"ChooseAction\",\"with\":0}}}]}"
