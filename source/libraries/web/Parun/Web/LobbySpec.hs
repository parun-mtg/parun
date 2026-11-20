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
      Spec.assertEq s (Lobby.mine picked, Lobby.choosing picked) (Just b, Lobby.MkTheirs 0)

    Spec.it s "picks the bot's deck once you have one" $ do
      let picked = Lobby.update (Lobby.MkPick a) Text.empty $ Lobby.update (Lobby.MkPick b) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.theirs picked) (Just b, [Just a])

    Spec.it s "ignores a deck it does not hold" $ do
      Spec.assertEq s (Lobby.update (Lobby.MkPick $ Text.pack "nope") Text.empty lobby) lobby

    Spec.it s "drops the bot's deck when your deck is under another format" $ do
      let picked = Lobby.update (Lobby.MkPick c) Text.empty . Lobby.update (Lobby.MkChoose Lobby.MkMine) Text.empty . Lobby.update (Lobby.MkPick a) Text.empty $ Lobby.update (Lobby.MkPick b) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.theirs picked) (Just c, [Nothing])

    Spec.it s "keeps the bot's deck when your deck is under its format" $ do
      let picked = Lobby.update (Lobby.MkPick d) Text.empty . Lobby.update (Lobby.MkChoose Lobby.MkMine) Text.empty . Lobby.update (Lobby.MkPick d) Text.empty $ Lobby.update (Lobby.MkPick c) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.theirs picked) (Just d, [Just d])

    Spec.it s "refuses the bot a deck under another format than yours" $ do
      let mineChosen = Lobby.update (Lobby.MkPick c) Text.empty lobby
      Spec.assertEq s (Lobby.theirs (Lobby.update (Lobby.MkPick a) Text.empty mineChosen)) [Nothing]

    Spec.it s "seats up to three bots, each with its own deck, and unseats the last" $ do
      let added = Lobby.update Lobby.MkAddBot Text.empty
          three = added (added lobby)
          picked = Lobby.update (Lobby.MkPick a) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 2)) Text.empty . Lobby.update (Lobby.MkPick b) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty $ Lobby.update (Lobby.MkPick a) Text.empty three
      Spec.assertEqWith s "three bots" (Lobby.theirs picked) [Just b, Nothing, Just a]
      Spec.assertEqWith s "no fourth" (Lobby.theirs (added three)) (Lobby.theirs three)
      Spec.assertEqWith s "picking yours moves on to the first bot unchosen" (Lobby.choosing (Lobby.update (Lobby.MkPick a) Text.empty (Lobby.update (Lobby.MkChoose Lobby.MkMine) Text.empty picked))) (Lobby.MkTheirs 1)
      let dropped = Lobby.update Lobby.MkDropBot Text.empty picked
      Spec.assertEqWith s "the last unseated, and its slot no longer chosen" (Lobby.theirs dropped, Lobby.choosing dropped) ([Just b, Nothing], Lobby.MkMine)
      Spec.assertEqWith s "one bot stays" (Lobby.theirs (Lobby.update Lobby.MkDropBot Text.empty lobby)) [Nothing]

    Spec.it s "drops each bot's deck under another format than yours" $ do
      let two = Lobby.update (Lobby.MkPick a) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 1)) Text.empty . Lobby.update (Lobby.MkPick c) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty $ Lobby.update Lobby.MkAddBot Text.empty lobby
          picked = Lobby.update (Lobby.MkPick b) Text.empty (Lobby.update (Lobby.MkChoose Lobby.MkMine) Text.empty two)
      Spec.assertEqWith s "the commander deck dropped" (Lobby.theirs picked, Lobby.choosing picked) ([Nothing, Just a], Lobby.MkTheirs 0)

    Spec.it s "filters by what is typed" $ do
      Spec.assertEq s (Lobby.search $ Lobby.update Lobby.MkSearch (Text.pack "webs") lobby) (Text.pack "webs")

  Spec.describe s "opponents" $ do
    Spec.it s "offers the bot the decks under your deck's format" $ do
      Spec.assertEq s (Lobby.opponents (Lobby.update (Lobby.MkPick c) Text.empty lobby)) (Seq.fromList [c, d])
      Spec.assertEq s (Lobby.opponents (Lobby.update (Lobby.MkPick a) Text.empty lobby)) (Seq.fromList [a, b])

  Spec.describe s "withDecks" $ do
    Spec.it s "keeps what was picked and typed" $ do
      let early = (Lobby.update (Lobby.MkPick a) Text.empty lobby) {Lobby.search = Text.pack "web"}
          later = Lobby.withDecks (Seq.fromList [a, b]) Map.empty early
      Spec.assertEqWith s "mine" (Lobby.mine later) (Just a)
      Spec.assertEqWith s "search" (Lobby.search later) (Text.pack "web")
      Spec.assertEqWith s "read" (Lobby.reading later) False

  Spec.describe s "view" $ do
    Spec.it s "lists the decks matching the filter, grouped by set" $ do
      let page = Html.toText . Lobby.view $ lobby {Lobby.search = Text.pack "WEB"}
      Spec.assertBool s (Text.isInfixOf (Text.pack "<h2>M12</h2>") page) "set"
      Spec.assertBool s (Text.isInfixOf (Text.pack "Entangling Webs") page) "match"
      Spec.assertBool s (not $ Text.isInfixOf (Text.pack "Demo A") page) "filtered out"

    Spec.it s "lists only the decks the bot may play while choosing its deck" $ do
      let page = Html.toText . Lobby.view $ Lobby.update (Lobby.MkPick c) Text.empty lobby
      Spec.assertBool s (Text.isInfixOf (Text.pack "Wretched Ranks") page) "your format"
      Spec.assertBool s (not $ Text.isInfixOf (Text.pack "Entangling Webs") page) "another format"

    Spec.it s "names a deck's format" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "Commander") . Html.toText $ Lobby.view lobby) "format"

    Spec.it s "cannot start without your deck" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "disabled=\"\">Start game") . Html.toText $ Lobby.view lobby) "disabled"

    Spec.it s "names each bot by its seat once there are two" $ do
      let one = Html.toText (Lobby.view lobby)
          two = Html.toText (Lobby.view (Lobby.update Lobby.MkAddBot Text.empty lobby))
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Bot<") one && not (Text.isInfixOf (Text.pack "Remove a bot") one)) "one bot"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Bot 1<") two && Text.isInfixOf (Text.pack ">Bot 2<") two && Text.isInfixOf (Text.pack "Remove a bot") two) "two bots"

    Spec.it s "says the decks are being read until they are" $ do
      let page = Html.toText (Lobby.view Lobby.new)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Reading the decks") page) "reading"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "No deck matches") page)) "no empty match"

    Spec.it s "says why the decks did not load in place of reading them" $ do
      let page = Html.toText (Lobby.view Lobby.new {Lobby.problem = Just (Text.pack "Reload to try again.")})
      Spec.assertBool s (Text.isInfixOf (Text.pack "role=\"alert\">Reload to try again.") page) "problem"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Reading the decks") page)) "not reading"

    Spec.it s "goes home from its Home button" $ do
      let page = Lobby.view lobby
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Home") page >>= \path -> Html.handler (Text.pack "click") path page) (Just Lobby.MkHome)

    Spec.it s "sends a deck's name when it is tapped" $ do
      let page = Lobby.view lobby
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Entangling Webs") page >>= \path -> Html.handler (Text.pack "click") path page) . Just $ Lobby.MkPick b

a :: Text.Text
a = Text.pack "sets/por/demo-a"

b :: Text.Text
b = Text.pack "sets/m12/entangling-webs"

c :: Text.Text
c = Text.pack "sets/fdc/calling-all-angels"

d :: Text.Text
d = Text.pack "sets/fdc/wretched-ranks"

lobby :: Lobby.Lobby
lobby = Lobby.withDecks (Seq.fromList [a, b, c, d]) (Map.fromList [(c, Text.pack "commander"), (d, Text.pack "commander")]) Lobby.new
