module Parun.Web.LobbySpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Patch as Patch

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

  Spec.describe s "pairing" $ do
    Spec.it s "offers a Limited deck the decks naming no format that a Limited game takes" $ do
      let holding = (Lobby.withDecks (Seq.fromList [a, b, saved]) (Map.singleton saved (Text.pack "limited")) Lobby.new) {Lobby.sizes = Map.fromList [(a, 40), (b, 30)]}
      Spec.assertEq s (Lobby.opponents (Lobby.update (Lobby.MkPick saved) Text.empty holding)) (Seq.fromList [a, saved])

    Spec.it s "counts a deck list's main deck" $ do
      Spec.assertEq s (Lobby.mainCount (Text.pack "Format: commander\nCommander\n1 Sai\nDeck\n4 Island\n2 Shock\nSideboard\n3 Bolt\n")) 6

  Spec.describe s "flagged decks" $ do
    Spec.it s "badges a saved deck with problems Not legal" $ do
      let holding = (Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Lobby.new) {Lobby.illegal = Map.singleton saved [Text.pack "43 cards in the deck, fewer than 60"]}
      Spec.assertBool s (Text.isInfixOf (Text.pack "Not legal") (Html.toText (Lobby.view holding))) "badge"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Not legal") (Html.toText (Lobby.view holding {Lobby.illegal = Map.empty})))) "no badge"

    Spec.it s "refuses to start a flagged deck, listing why" $ do
      let holding = flaggedMine
          started = Lobby.update Lobby.MkStart Text.empty holding
      Spec.assertEq s (Lobby.startable holding) (Left [Text.pack "43 cards in the deck, fewer than 60"])
      Spec.assertBool s (Text.isInfixOf (Text.pack "43 cards in the deck, fewer than 60") (Html.toText (Lobby.view started))) "reason"

    Spec.it s "starts an unflagged deck" $ do
      Spec.assertEq s (Lobby.startable (Lobby.update (Lobby.MkPick a) Text.empty flaggedMine {Lobby.choosing = Lobby.MkMine})) (Right a)

    Spec.it s "a new pick clears the refusal" $ do
      let started = Lobby.update Lobby.MkStart Text.empty flaggedMine
      Spec.assertEq s (Lobby.refusal (Lobby.update (Lobby.MkPick a) Text.empty (started {Lobby.choosing = Lobby.MkMine}))) Nothing

    Spec.it s "offers Build a deck" $ do
      let page = Lobby.view lobby
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Build a deck") page >>= \path -> Html.handler (Text.pack "click") path page) (Just Lobby.MkOpenBuilder)

  Spec.describe s "edit" $ do
    Spec.it s "offers Edit under a saved deck shown" $ do
      let page = Lobby.view flaggedMine {Lobby.previews = Map.singleton saved []}
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Edit") page >>= \path -> Html.handler (Text.pack "click") path page) (Just (Lobby.MkEdit saved))

    Spec.it s "offers Edit a copy under a precon shown" $ do
      let page = Lobby.view (Lobby.update (Lobby.MkPick a) Text.empty lobby) {Lobby.previews = Map.singleton a []}
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Edit a copy") page >>= \path -> Html.handler (Text.pack "click") path page) (Just (Lobby.MkEdit a))

  Spec.describe s "import" $ do
    Spec.it s "slugs a name as lowercase words joined by dashes" $ do
      Spec.assertEqWith s "words" (Lobby.slugOf (Text.pack "  My  Deck!")) (Text.pack "my-deck")
      Spec.assertEqWith s "nothing left" (Lobby.slugOf (Text.pack "!!!")) Text.empty

    Spec.it s "refuses an empty name and one a saved deck holds" $ do
      let holding = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Lobby.new
      Spec.assertBool s (not (null (Lobby.nameProblems (Lobby.decks holding) (Text.pack "My Deck")))) "taken"
      Spec.assertBool s (not (null (Lobby.nameProblems (Lobby.decks holding) (Text.pack "!!!")))) "empty"
      Spec.assertEqWith s "free" (Lobby.nameProblems (Lobby.decks holding) (Text.pack "Other")) []

    Spec.it s "keeps what is typed into the panel" $ do
      let opened = Lobby.update Lobby.MkOpenImport Text.empty lobby
          typed = Lobby.update Lobby.MkImportText (Text.pack "4 Forest") (Lobby.update Lobby.MkImportName (Text.pack "Mine") opened)
      Spec.assertEqWith s "typed" (fmap (\i -> (Lobby.importName i, Lobby.importText i)) (Lobby.importing typed)) (Just (Text.pack "Mine", Text.pack "4 Forest"))
      Spec.assertEqWith s "closed" (Lobby.importing (Lobby.update Lobby.MkCloseImport Text.empty typed)) Nothing

    Spec.it s "makes an imported deck yours and closes the panel" $ do
      let opened = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty (Lobby.update Lobby.MkOpenImport Text.empty lobby)
          done = Lobby.imported saved opened
      Spec.assertEqWith s "yours" (Lobby.mine done) (Just saved)
      Spec.assertEqWith s "closed" (Lobby.importing done) Nothing

    Spec.it s "empties every seat a deleted deck held" $ do
      let holding = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Lobby.new
          seated = Lobby.update (Lobby.MkPick saved) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty $ Lobby.update (Lobby.MkPick saved) Text.empty holding
          gone = Lobby.deleted saved seated
      Spec.assertEqWith s "seats" (Lobby.mine gone, Lobby.theirs gone) (Nothing, [Nothing])
      Spec.assertEqWith s "decks" (Lobby.decks gone) (Seq.singleton a)

    Spec.it s "plays every deck but a set's whole card list, a saved deck named All included" $ do
      Spec.assertEqWith s "a set's list" (Lobby.playable (Text.pack "sets/m12/all")) False
      Spec.assertEqWith s "a set's deck" (Lobby.playable b) True
      Spec.assertEqWith s "saved" (Lobby.playable (Text.pack "saved/all")) True

    Spec.it s "groups saved decks under their own heading" $ do
      Spec.assertEq s (Lobby.describe saved) (Text.pack "SAVED", Text.pack "My Deck")

    Spec.it s "shows each problem with the deck imported" $ do
      let failed = lobby {Lobby.importing = Just (Lobby.MkImport (Text.pack "x") (Text.pack "1 Nope") [Text.pack "pasted: no card is named Nope"] False)}
      Spec.assertBool s (Text.isInfixOf (Text.pack "no card is named Nope") (Html.toText (Lobby.view failed))) "problem"

    Spec.it s "keeps the panel's fields, and the filter's, when a deck's list appears above them" $ do
      let opened = Lobby.update Lobby.MkOpenImport Text.empty (Lobby.update (Lobby.MkPick a) Text.empty lobby)
          shown = opened {Lobby.previews = Map.fromList [(a, [])]}
          rebuilt = Foldable.toList (Diff.diff (Lobby.view opened) (Lobby.view shown)) >>= rebuilds
      Spec.assertBool s (all (`notElem` rebuilt) [Text.pack "import-text", Text.pack "import-name", Text.pack "deck-search"]) (show rebuilt)

    Spec.it s "keeps the filter's field when the panel opens above it" $ do
      let rebuilt = Foldable.toList (Diff.diff (Lobby.view lobby) (Lobby.view (Lobby.update Lobby.MkOpenImport Text.empty lobby))) >>= rebuilds
      Spec.assertBool s (Text.pack "deck-search" `notElem` rebuilt) (show rebuilt)

    Spec.it s "offers a saved deck chosen as yours for export and deletion, and no other" $ do
      let holding = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Lobby.new
          page deck = Html.toText (Lobby.view (Lobby.update (Lobby.MkPick deck) Text.empty holding) {Lobby.previews = Map.fromList [(deck, [])]})
      Spec.assertBool s (Text.isInfixOf (Text.pack "Delete") (page saved)) "saved"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Delete") (page a))) "shipped"

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

saved :: Text.Text
saved = Text.pack "saved/my-deck"

-- | Your deck a saved one the check flagged.
flaggedMine :: Lobby.Lobby
flaggedMine = Lobby.update (Lobby.MkPick saved) Text.empty (Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Lobby.new) {Lobby.illegal = Map.singleton saved [Text.pack "43 cards in the deck, fewer than 60"]}

lobby :: Lobby.Lobby
lobby = Lobby.withDecks (Seq.fromList [a, b, c, d]) (Map.fromList [(c, Text.pack "commander"), (d, Text.pack "commander")]) Lobby.new

-- The ids of the fields a patch builds afresh or turns another element
-- into: a field built afresh loses what was typed into it.
rebuilds :: Patch.Patch msg -> [Text.Text]
rebuilds patch = case patch of
  Patch.MkReplace _ html -> fields html
  Patch.MkAppend _ htmls -> foldMap fields htmls
  Patch.MkArrange _ sources -> foldMap (\source -> case source of Patch.MkCreated html -> fields html; Patch.MkKept _ -> []) sources
  Patch.MkSetProperty _ name value | name == Text.pack "id" -> [value]
  _ -> []
  where
    fields html = [field | field <- [Text.pack "import-text", Text.pack "import-name", Text.pack "deck-search"], Text.isInfixOf (Text.pack "id=\"" <> field <> Text.pack "\"") (Html.toText html)]
