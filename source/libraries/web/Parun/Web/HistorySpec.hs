module Parun.Web.HistorySpec where

import qualified Data.Map.Strict as Map
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

    Spec.it s "reads back each verdict, a shared screen's winner too, writing the others as bare tags" $ do
      let verdicts = [Ending.MkWon, Ending.MkLost, Ending.MkDrawn, Ending.MkWonBy (PlayerId.MkPlayerId 1), Ending.MkStopped]
      Spec.assertEqWith s "round trip" (fmap (\v -> fmap History.verdict (History.decode (History.encode game {History.verdict = v}))) verdicts) (fmap Right verdicts)
      -- As summaries kept before the shared screen's verdict hold them.
      Spec.assertBool s (Text.isInfixOf (Text.pack "\"verdict\":\"MkLost\"") (History.encode game)) "a bare tag"

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

  Spec.describe s "lastPlayed" $ do
    Spec.it s "dates each deck the player played by the newest game, a bot's deck not at all" $ do
      Spec.assertEq s (History.lastPlayed [game, game {History.ended = 5}]) (Map.singleton (Text.pack "sets/m19/ajani") 1795266300000)

  Spec.describe s "loaded" $ do
    Spec.it s "reads the games kept newest first, forgetting a summary that does not read and any past the cap" $ do
      let kept = [(Text.pack "bad", Text.pack "{")] <> [(History.key g, History.encode g) | n <- [1 .. toInteger History.cap + 1], let g = game {History.ended = n}]
          (games, gone) = History.loaded kept
      Spec.assertEqWith s "kept" (fmap History.ended games) [toInteger History.cap + 1, toInteger History.cap .. 2]
      Spec.assertEqWith s "forgotten" gone [Text.pack "bad", Text.pack "1"]

  Spec.describe s "drift" $ do
    Spec.it s "notes a game another build played, and only one" $ do
      Spec.assertEqWith s "this build" (History.drift (Text.pack "build") game) Nothing
      Spec.assertEqWith s "another" (History.drift (Text.pack "newer") game) (Just (Text.pack "Recorded on an older version; it may not replay exactly."))

  Spec.describe s "recordOf" $ do
    let finished verdict = Ending.MkFinished (Ending.MkEnding verdict [Text.pack "Player 1 wins."] scenario Seq.empty (PlayerId.MkPlayerId 0) False [(1, []), (6, []), (7, [])])
        recorded = History.recordOf 1795266300000 60 (Text.pack "build") (History.seats game)
    Spec.it s "keeps a game that ended with a result, in the turn its trail last sampled" $ do
      Spec.assertEqWith s "won" (fmap fst (recorded (finished Ending.MkWon))) (Just game {History.verdict = Ending.MkWon, History.turns = 7})
      Spec.assertEqWith s "the scenario" (fmap snd (recorded (finished Ending.MkDrawn))) (Just scenario)

    Spec.it s "keeps no game given up from the home screen or stopped short" $ do
      Spec.assertEqWith s "given up" (recorded Ending.MkAbandoned) Nothing
      Spec.assertEqWith s "stopped" (recorded (finished Ending.MkStopped)) Nothing

  Spec.describe s "viewerLabel" $ do
    Spec.it s "names the player's seat as the game's scenario does" $ do
      Spec.assertEqWith s "the host" (History.viewerLabel game) (Just (Text.pack "player1"))
      Spec.assertEqWith s "a guest" (History.viewerLabel game {History.seats = reverse (History.seats game)}) (Just (Text.pack "player2"))

  Spec.describe s "seatNames" $ do
    Spec.it s "names each seat as the game did, whoever played it" $ do
      let bots = game {History.seats = History.seats game <> [History.MkSeat History.MkBot (Text.pack "Vivien (M19)") Nothing Set.empty]}
          guest = game {History.seats = History.MkSeat History.MkPerson (Text.pack "Vivien (M19)") Nothing Set.empty : History.seats game}
          shared = bots {History.seats = take 1 (History.seats bots) <> [History.MkSeat History.MkHere (Text.pack "Vivien (M19)") Nothing Set.empty] <> drop 2 (History.seats bots)}
      Spec.assertEqWith s "two seats" (History.seatNames game) (Map.fromList [(Text.pack "player1", Text.pack "You"), (Text.pack "player2", Text.pack "Player 2")])
      Spec.assertEqWith s "two bots" (History.seatNames bots) (Map.fromList [(Text.pack "player1", Text.pack "You"), (Text.pack "player2", Text.pack "Player 2"), (Text.pack "player3", Text.pack "Player 3")])
      Spec.assertEqWith s "a shared screen's" (History.seatNames shared) (Map.fromList [(Text.pack "player1", Text.pack "Player 1"), (Text.pack "player2", Text.pack "Player 2"), (Text.pack "player3", Text.pack "Player 3")])
      Spec.assertEqWith s "a guest's" (History.seatNames guest) (Map.fromList [(Text.pack "player1", Text.pack "Player 1"), (Text.pack "player2", Text.pack "You"), (Text.pack "player3", Text.pack "Player 3")])

  Spec.describe s "date" $ do
    Spec.it s "tells the day and time on the page's clock then" $ do
      Spec.assertEqWith s "ahead of UTC" (History.date game) "21 Nov 2026, 14:05"
      Spec.assertEqWith s "behind it, across a leap day" (History.date game {History.ended = 1709251140000, History.offset = -300}) "29 Feb 2024, 18:59"
      Spec.assertEqWith s "past midnight" (History.date game {History.ended = 1709251140000, History.offset = 60}) "1 Mar 2024, 00:59"

  Spec.describe s "fileName" $ do
    Spec.it s "names the file by when the game ended on the page's clock, to the second" $ do
      Spec.assertEqWith s "ahead of UTC" (History.fileName game {History.ended = 1795266307000}) (Text.pack "parun-game-2026-11-21-140507.json")
      Spec.assertEqWith s "behind it, across a leap day" (History.fileName game {History.ended = 1709251140000, History.offset = -300}) (Text.pack "parun-game-2024-02-29-185900.json")

  Spec.describe s "row" $ do
    Spec.it s "says how it ended, the decks and who played them, its turn and date" $ do
      let page = Html.toText (History.row game [])
          has piece = Spec.assertBool s (Text.isInfixOf (Text.pack piece) page) piece
      has "<span class=\"result lost\">Lost</span>"
      has "Ajani (M19)"
      has "Liliana (M19)"
      has "You vs Bot \x00b7 Turn 9 \x00b7 21 Nov 2026, 14:05"

    Spec.it s "names a shared screen's winner and players without taking a seat's side" $ do
      let page = Html.toText (History.row game {History.verdict = Ending.MkWonBy (PlayerId.MkPlayerId 1), History.seats = History.seats game <> [History.MkSeat History.MkHere (Text.pack "Vivien (M19)") Nothing Set.empty]} [])
      Spec.assertBool s (Text.isInfixOf (Text.pack "<span class=\"result drawn\">Player 2 won</span>") page) (Text.unpack page)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Player 1 vs Bot vs Player 3 \x00b7 Turn 9") page) (Text.unpack page)

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
