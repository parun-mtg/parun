module Parun.Web.LobbySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Lobby" $ do
  Spec.describe s "deckName" $ do
    Spec.it s "drops the root and the extension" $ do
      Spec.assertEq s (Lobby.deckName "/data/decks" "/data/decks/sets/por/demo-a.txt") $ Text.pack "sets/por/demo-a"

    Spec.it s "keeps a path outside the root" $ do
      Spec.assertEq s (Lobby.deckName "/data/decks" "/elsewhere/a.txt") $ Text.pack "/elsewhere/a"

  Spec.describe s "describe" $ do
    Spec.it s "names the set by its code and the deck in words" $ do
      Spec.assertEq s (Lobby.describe $ Text.pack "sets/m12/entangling-webs") (Text.pack "M12", Text.pack "Entangling Webs")

  Spec.describe s "deckColours" $ do
    Spec.it s "unions the colours of the cards its lines name, skipping the rest" $ do
      let known = Map.fromList [(CardName.MkCardName (Text.pack "Shock"), Set.singleton Color.Red), (CardName.MkCardName (Text.pack "Forest"), Set.empty), (CardName.MkCardName (Text.pack "Giant Growth"), Set.singleton Color.Green)]
      Spec.assertEq s (Lobby.deckColours (\name -> Map.findWithDefault Set.empty name known) (Text.pack "4 Shock (M19) 156\n18 Forest\n# a comment\n2 Giant Growth\n1 Unknown Card")) (Set.fromList [Color.Red, Color.Green])

  Spec.describe s "update" $ do
    Spec.it s "picks your deck, then moves on to the bot's" $ do
      let picked = Lobby.update (Lobby.MkPick b) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.choosing picked) (Just b, Lobby.MkTheirs)

    Spec.it s "picks the bot's deck once you have one" $ do
      let picked = Lobby.update (Lobby.MkPick a) Text.empty $ Lobby.update (Lobby.MkPick b) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.theirs picked) (Just b, Just a)

    Spec.it s "ignores a deck it does not hold" $ do
      Spec.assertEq s (Lobby.update (Lobby.MkPick $ Text.pack "nope") Text.empty lobby) lobby

    Spec.it s "filters by what is typed" $ do
      Spec.assertEq s (Lobby.search $ Lobby.update Lobby.MkSearch (Text.pack "webs") lobby) (Text.pack "webs")

  Spec.describe s "view" $ do
    Spec.it s "lists the decks matching the filter, grouped by set" $ do
      let page = Html.toText . Lobby.view $ lobby {Lobby.search = Text.pack "WEB"}
      Spec.assertBool s (Text.isInfixOf (Text.pack "<h2>M12</h2>") page) "set"
      Spec.assertBool s (Text.isInfixOf (Text.pack "Entangling Webs") page) "match"
      Spec.assertBool s (not $ Text.isInfixOf (Text.pack "Demo A") page) "filtered out"

    Spec.it s "cannot start without your deck" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "disabled=\"\">Start game") . Html.toText $ Lobby.view lobby) "disabled"

    Spec.it s "sends a deck's name when it is tapped" $ do
      let page = Lobby.view lobby
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Entangling Webs") page >>= \path -> Html.handler (Text.pack "click") path page) . Just $ Lobby.MkPick b

a :: Text.Text
a = Text.pack "sets/por/demo-a"

b :: Text.Text
b = Text.pack "sets/m12/entangling-webs"

lobby :: Lobby.Lobby
lobby = Lobby.new 2 $ Seq.fromList [a, b]
