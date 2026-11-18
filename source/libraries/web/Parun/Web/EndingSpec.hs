module Parun.Web.EndingSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Log as Log

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Ending" $ do
  Spec.describe s "view" $ do
    Spec.it s "says how the game ended, links a new game and the scenario, and lists the log" $ do
      Spec.assertEq s (Html.toText . Ending.view $ Ending.MkEnding Ending.MkWon [Text.pack "Player 1 wins.", Text.pack "Player 2 conceded."] (Text.pack "{}") (Seq.fromList [Log.MkTurn 1 (PlayerId.MkPlayerId 0), Log.MkEvent Log.MkMajor (Text.pack "Player 1 draws a card."), Log.MkEvent Log.MkMinor (Text.pack "Player 2 concedes.")]) (PlayerId.MkPlayerId 0) (Text.pack "sets/a/x", Text.pack "sets/b/y")) $
        Text.pack "<main class=\"ending\"><section class=\"verdict won\"><h1>You win!</h1><p>Player 1 wins.</p><p>Player 2 conceded.</p></section><div class=\"ending-actions\"><a class=\"primary\" href=\"?mine=sets/a/x&amp;theirs=sets/b/y\">Rematch</a><a href=\"./\">New game</a><a href=\"data:application/json;charset=utf-8,%7b%7d\" download=\"parun-game.json\">Save as a scenario</a></div><h2>Game log</h2><ol class=\"game-log\"><li class=\"turn\">Turn 1: your turn</li><li>Player 1 draws a card.</li><li class=\"minor\">Player 2 concedes.</li></ol></main>"

  Spec.describe s "download" $ do
    Spec.it s "percent-encodes the text's UTF-8, a space and a number sign included" $ do
      Spec.assertEq s (Ending.download $ Text.pack "{\"name\": \"J\246tun #1\"}") $
        Text.pack "data:application/json;charset=utf-8,%7b%22name%22:%20%22J%c3%b6tun%20%231%22%7d"
