module Parun.Web.EndingSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Log as Log

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Ending" $ do
  Spec.describe s "view" $ do
    Spec.it s "says how the game ended once, offers a rematch, a new game and the scenario, and lists the log newest first, as play does" $ do
      let page = Html.toText (Ending.view ending)
          has piece = Spec.assertBool s (Text.isInfixOf (Text.pack piece) page) piece
      has "<section class=\"verdict won\"><h1>You win!</h1><p>Player 2 conceded.</p></section>"
      has ">Rematch</button>"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "?mine=") page)) "no rematch link"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Share this shuffle") page)) "no share link"
      let tapping label = Html.labelled (Text.pack "button") (Text.pack label) (Ending.view ending) >>= \path -> Html.handler (Text.pack "click") path (Ending.view ending)
      Spec.assertEqWith s "rematch" (tapping "Rematch") (Just Ending.MkRematch)
      Spec.assertBool s (not (Text.isInfixOf (Text.pack ">Same shuffle<") page)) "no same-shuffle button"
      Spec.assertEqWith s "new game" (tapping "New game") (Just Ending.MkNewGame)
      has ">New game</button>"
      has "<a href=\"data:application/json;charset=utf-8,%7b%7d\" download=\"parun-game.json\">Save as a scenario</a>"
      has "<h2>Game log</h2>"
      has "<h3 class=\"log-turn-heading\">Turn 2 \x00b7 Player 2</h3><ul class=\"log-list\"><li class=\"minor\">Player 2 concedes.</li><li>Player 2 draws a card.</li></ul>"
      has "<h3 class=\"log-turn-heading\">Turn 1 \x00b7 You</h3><ul class=\"log-list\"><li>You draw a card.</li></ul>"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Turn 3 \x00b7") page)) "no heading for a turn with nothing in it"
      has "<div class=\"confetti\""

    Spec.it s "keeps why play stopped under its headline" $ do
      let page = Html.toText (Ending.view ending {Ending.verdict = Ending.MkStopped, Ending.said = [Text.pack "The game stopped: end of input."]})
      Spec.assertBool s (Text.isInfixOf (Text.pack "<section class=\"verdict stopped\"><h1>Game over</h1><p>The game stopped: end of input.</p></section>") page) (Text.unpack page)

    Spec.it s "names a shared screen's winner and every seat, taking no seat's side, and says whose log it shows" $ do
      let page = Html.toText (Ending.view ending {Ending.verdict = Ending.MkWonBy bot, Ending.said = [Text.pack "Player 2 wins.", Text.pack "Player 1 conceded."], Ending.shared = True})
          has piece = Spec.assertBool s (Text.isInfixOf (Text.pack piece) page) piece
          hasNot piece = Spec.assertBool s (not (Text.isInfixOf (Text.pack piece) page)) piece
          bot = PlayerId.MkPlayerId 1
      has "<section class=\"verdict won\"><h1>Player 2 wins!</h1><p>Player 1 conceded.</p></section>"
      has "<div class=\"confetti\""
      has "Player 1</strong>: ended on 20 life"
      hasNot "You</strong>"
      hasNot "life-line seat-0 you"
      has "<h2>Game log, as Player 1 saw it</h2>"

    Spec.it s "labels the turns between the start and the end at a round step" $ do
      let lives = [(PlayerId.MkPlayerId 0, 20 :: Integer), (PlayerId.MkPlayerId 1, 20)]
          page = Html.toText (Ending.view ending {Ending.trail = [(turn, lives) | turn <- [1 .. 15]]})
          axis x said' = Text.pack ("<text class=\"axis\" x=\"" <> show (x :: Integer) <> "\" y=\"136\" text-anchor=\"middle\" dominant-baseline=\"middle\">" <> said' <> "</text>")
      -- Fifteen samples over 320: turn 5's is the fifth, turn 10's the tenth.
      Spec.assertBool s (Text.isInfixOf (axis 91 "5" <> axis 205 "10" <> Text.pack "<text class=\"axis\" x=\"320\"") page) "turns 5 and 10"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack ">15</text>") page)) "the last turn labelled once"

    Spec.it s "ends the chart on the turn the game ended in, though no life changed in it" $ do
      let lives = [(PlayerId.MkPlayerId 0, 20 :: Integer), (PlayerId.MkPlayerId 1, 20)]
          trail = Ending.sampled 3 lives (Ending.sampled 2 lives (Ending.sampled 1 lives (Ending.sampled 1 lives Seq.empty)))
          page = Html.toText (Ending.view ending {Ending.trail = Foldable.toList trail})
      Spec.assertEqWith s "a sample a turn" (fmap fst trail) (Seq.fromList [1, 2, 3])
      Spec.assertBool s (Text.isInfixOf (Text.pack "text-anchor=\"end\" dominant-baseline=\"middle\">Turn 3</text>") page) "the axis"

    Spec.it s "charts each seat's life and tells the game's numbers" $ do
      let page = Html.toText (Ending.view ending)
          has piece = Spec.assertBool s (Text.isInfixOf (Text.pack piece) page) piece
      -- Life 20 to 17 to 0 for the bot over a chart 120 high whose floor is 0.
      has "<polyline class=\"life-line seat-1\" points=\"0,0 160,18 320,120\""
      -- Each seat in its own colour, the viewer's line bolder.
      has "<polyline class=\"life-line seat-0 you\""
      has "Player 2</strong>: ended on 0 life, lowest 0, biggest hit 17"
      has "Turn 3"
      -- The scale, 20 at the top and 0 at the floor, and each seat's final
      -- life at its line's end in its colour.
      has "<text class=\"scale\" x=\"-6\" y=\"0\" text-anchor=\"end\" dominant-baseline=\"middle\">20</text>"
      has "<text class=\"scale\" x=\"-6\" y=\"120\" text-anchor=\"end\" dominant-baseline=\"middle\">0</text>"
      has "<text class=\"value seat-0 you\" x=\"326\" y=\"0\" text-anchor=\"start\" dominant-baseline=\"middle\">20</text>"
      has "<text class=\"value seat-1\" x=\"326\" y=\"120\" text-anchor=\"start\" dominant-baseline=\"middle\">0</text>"

    Spec.it s "CR 103.4c a seat that never fell to 20 life has a lowest above it" $ do
      let page = Html.toText (Ending.view ending {Ending.trail = [(1, [(you, 40), (bot, 40)]), (2, [(you, 33), (bot, 12)]), (3, [(you, 37), (bot, -8)])]})
          you = PlayerId.MkPlayerId 0
          bot = PlayerId.MkPlayerId 1
      Spec.assertBool s (Text.isInfixOf (Text.pack "You</strong>: ended on 37 life, lowest 33, biggest hit 7") page) (Text.unpack page)
      -- Below 0, the floor gets its own label beside the baseline's.
      Spec.assertBool s (Text.isInfixOf (Text.pack "<text class=\"scale\" x=\"-6\" y=\"120\" text-anchor=\"end\" dominant-baseline=\"middle\">-8</text>") page) "the floor"

    Spec.it s "keeps the final lives' labels apart where the lines end close together" $ do
      let page = Html.toText (Ending.view ending {Ending.trail = [(1, [(you, 20), (bot, 20)]), (2, [(you, 20), (bot, 19)])]})
          you = PlayerId.MkPlayerId 0
          bot = PlayerId.MkPlayerId 1
      -- 19 life is 6 below the top, the label 12 below the one for 20.
      Spec.assertBool s (Text.isInfixOf (Text.pack "<text class=\"value seat-1\" x=\"326\" y=\"12\"") page) (Text.unpack page)

  Spec.describe s "download" $ do
    Spec.it s "percent-encodes the text's UTF-8, a space and a number sign included" $ do
      Spec.assertEq s (Ending.download $ Text.pack "{\"name\": \"J\246tun #1\"}") $
        Text.pack "data:application/json;charset=utf-8,%7b%22name%22:%20%22J%c3%b6tun%20%231%22%7d"

ending :: Ending.Ending
ending =
  Ending.MkEnding
    { Ending.verdict = Ending.MkWon,
      Ending.said = [Text.pack "You win.", Text.pack "Player 2 conceded."],
      Ending.scenario = Text.pack "{}",
      Ending.witnessed = Seq.fromList [Log.MkTurn 1 you, Log.MkEvent Log.MkMajor (Text.pack "You draw a card."), Log.MkTurn 2 bot, Log.MkEvent Log.MkMajor (Text.pack "Player 2 draws a card."), Log.MkEvent Log.MkMinor (Text.pack "Player 2 concedes."), Log.MkTurn 3 you],
      Ending.viewer = you,
      Ending.shared = False,
      Ending.trail = [(1, [(you, 20), (bot, 20)]), (2, [(you, 20), (bot, 17)]), (3, [(you, 20), (bot, 0)])]
    }
  where
    you = PlayerId.MkPlayerId 0
    bot = PlayerId.MkPlayerId 1
