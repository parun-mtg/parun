module Parun.Web.LobbySpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Line as Line
import qualified Parun.Web.Lobby as Lobby

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Lobby" $ do
  Spec.describe s "deckName" $ do
    Spec.it s "drops the root and the extension" $ do
      Spec.assertEq s (Lobby.deckName "/data/decks" "/data/decks/sets/por/demo-a.txt") $ Text.pack "sets/por/demo-a"

    Spec.it s "keeps a path outside the root" $ do
      Spec.assertEq s (Lobby.deckName "/data/decks" "/elsewhere/a.txt") $ Text.pack "/elsewhere/a"

  Spec.describe s "view" $ do
    Spec.it s "shows the corpus and every deck" $ do
      Spec.assertEq s (Html.toText . Lobby.view . Lobby.MkLobby 2 (Seq.fromList [Text.pack "a", Text.pack "b"]) $ Nothing) $
        Text.pack "<main><h1>Parun</h1><p>2 card files loaded.</p><p>Choose seat 1's deck.</p><ul><li><button>a</button></li><li><button>b</button></li></ul></main>"

    Spec.it s "names the first seat's deck once it is chosen" $ do
      Spec.assertEq s (Html.toText . Lobby.view . Lobby.MkLobby 2 (Seq.fromList [Text.pack "a"]) . Just $ Text.pack "a") $
        Text.pack "<main><h1>Parun</h1><p>2 card files loaded.</p><p>Seat 1 plays a. Choose seat 2's deck.</p><ul><li><button>a</button></li></ul></main>"

    Spec.it s "sends a deck's name when it is tapped" $ do
      Spec.assertEq s (Html.handler (Text.pack "click") (Seq.fromList [3, 1, 0]) . Lobby.view . Lobby.MkLobby 2 (Seq.fromList [Text.pack "a", Text.pack "b"]) $ Nothing) . Just . Line.MkFixed $ Text.pack "b"
