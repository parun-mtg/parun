module Parun.Web.EndingSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Ending" $ do
  Spec.describe s "view" $ do
    Spec.it s "says how the game ended, links the scenario and a new game, and lists the log" $ do
      Spec.assertEq s (Html.toText . Ending.view $ Ending.MkEnding [Text.pack "Player 1 wins.", Text.pack "Player 2 conceded."] (Text.pack "{}") (Seq.fromList [Text.pack "Player 1 draws a card.", Text.pack "Player 2 concedes."])) $
        Text.pack "<main><h1>Game over</h1><p>Player 1 wins.</p><p>Player 2 conceded.</p><p><a href=\"data:application/json;charset=utf-8,%7b%7d\" download=\"parun-game.json\">Save the game as a scenario</a> <a href=\"\">Play again</a></p><h2>Game log</h2><ol><li>Player 1 draws a card.</li><li>Player 2 concedes.</li></ol></main>"

  Spec.describe s "download" $ do
    Spec.it s "percent-encodes the text's UTF-8, a space and a number sign included" $ do
      Spec.assertEq s (Ending.download $ Text.pack "{\"name\": \"J\246tun #1\"}") $
        Text.pack "data:application/json;charset=utf-8,%7b%22name%22:%20%22J%c3%b6tun%20%231%22%7d"
