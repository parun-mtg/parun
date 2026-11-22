module Parun.Web.LobbySpec where

import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Persona as Persona
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Patch as Patch

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Lobby" $ do
  Spec.describe s "deckName" $ do
    Spec.it s "drops the root and the extension" $ do
      Spec.assertEq s (Lobby.deckName "/data/decks" "/data/decks/sets/por/demo-a.txt") $ Text.pack "sets/por/demo-a"

    Spec.it s "keeps a path outside the root" $ do
      Spec.assertEq s (Lobby.deckName "/data/decks" "/elsewhere/a.txt") $ Text.pack "/elsewhere/a"

  Spec.describe s "deckColours" $ do
    Spec.it s "unions the colours of the cards its lines name, skipping the rest" $ do
      let known = Map.fromList [(CardName.MkCardName (Text.pack "Shock"), Set.singleton Color.Red), (CardName.MkCardName (Text.pack "Forest"), Set.empty), (CardName.MkCardName (Text.pack "Giant Growth"), Set.singleton Color.Green)]
      Spec.assertEq s (Lobby.deckColours (\name -> Map.findWithDefault Set.empty name known) (Text.pack "4 Shock (M19) 156\n18 Forest\n# a comment\n2 Giant Growth\n1 Unknown Card")) (Set.fromList [Color.Red, Color.Green])

  Spec.describe s "update" $ do
    Spec.it s "picks your deck and stays on your seat" $ do
      let picked = Lobby.update (Lobby.MkPick b) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.choosing picked) (Just b, Lobby.MkMine)

    Spec.it s "never moves the seat chosen for on a pick" $ do
      let seats = [Lobby.MkMine, Lobby.MkTheirs 0, Lobby.MkTheirs 1, Lobby.MkTheirs 2]
          three = Lobby.update Lobby.MkAddBot Text.empty (Lobby.update Lobby.MkAddBot Text.empty lobby)
          moved = [(slot, deck) | slot <- seats, let held = Lobby.update (Lobby.MkChoose slot) Text.empty three, deck <- [a, b, c, d], Lobby.choosing (Lobby.update (Lobby.MkPick deck) Text.empty held) /= slot]
      Spec.assertEq s moved []

    Spec.it s "picks the bot's deck once its seat is chosen" $ do
      let picked = Lobby.update (Lobby.MkPick a) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty $ Lobby.update (Lobby.MkPick b) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.theirs picked) (Just b, [Just a])

    Spec.it s "ignores a deck it does not hold" $ do
      Spec.assertEq s (Lobby.update (Lobby.MkPick $ Text.pack "nope") Text.empty lobby) lobby

    Spec.it s "drops the bot's deck when your deck is under another format" $ do
      let picked = Lobby.update (Lobby.MkPick c) Text.empty . Lobby.update (Lobby.MkChoose Lobby.MkMine) Text.empty . botPicks a $ Lobby.update (Lobby.MkPick b) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.theirs picked) (Just c, [Nothing])

    Spec.it s "keeps the bot's deck when your deck is under its format" $ do
      let picked = Lobby.update (Lobby.MkPick d) Text.empty . Lobby.update (Lobby.MkChoose Lobby.MkMine) Text.empty . botPicks d $ Lobby.update (Lobby.MkPick c) Text.empty lobby
      Spec.assertEq s (Lobby.mine picked, Lobby.theirs picked) (Just d, [Just d])

    Spec.it s "refuses the bot a deck under another format than yours" $ do
      Spec.assertEq s (Lobby.theirs (botPicks a (Lobby.update (Lobby.MkPick c) Text.empty lobby))) [Nothing]

    Spec.it s "seats up to three bots, each with its own deck, and unseats the last" $ do
      let added = Lobby.update Lobby.MkAddBot Text.empty
          three = added (added lobby)
          picked = Lobby.update (Lobby.MkPick a) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 2)) Text.empty . botPicks b $ Lobby.update (Lobby.MkPick a) Text.empty three
      Spec.assertEqWith s "three bots" (Lobby.theirs picked) [Just b, Nothing, Just a]
      Spec.assertEqWith s "no fourth" (Lobby.theirs (added three)) (Lobby.theirs three)
      let dropped = Lobby.update Lobby.MkDropBot Text.empty picked
      Spec.assertEqWith s "the last unseated, and its slot no longer chosen" (Lobby.theirs dropped, Lobby.choosing dropped) ([Just b, Nothing], Lobby.MkMine)
      Spec.assertEqWith s "one bot stays" (Lobby.theirs (Lobby.update Lobby.MkDropBot Text.empty lobby)) [Nothing]

    Spec.it s "drops each bot's deck under another format than yours" $ do
      let two = Lobby.update (Lobby.MkPick a) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 1)) Text.empty . botPicks c $ Lobby.update Lobby.MkAddBot Text.empty lobby
          picked = Lobby.update (Lobby.MkPick b) Text.empty (Lobby.update (Lobby.MkChoose Lobby.MkMine) Text.empty two)
      Spec.assertEqWith s "the commander deck dropped" (Lobby.theirs picked, Lobby.choosing picked) ([Nothing, Just a], Lobby.MkMine)

    Spec.it s "filters by what is typed" $ do
      Spec.assertEq s (DeckBrowser.search . Lobby.browser $ Lobby.update (Lobby.MkBrowse DeckBrowser.MkSearch) (Text.pack "webs") lobby) (Text.pack "webs")

    Spec.it s "shows only the decks holding every colour toggled on" $ do
      let red = Lobby.update (Lobby.MkBrowse (DeckBrowser.MkColour Color.Red)) Text.empty coloured
          redGreen = Lobby.update (Lobby.MkBrowse (DeckBrowser.MkColour Color.Green)) Text.empty red
      Spec.assertEqWith s "red" (Lobby.shown red) [a, b]
      Spec.assertEqWith s "red and green" (Lobby.shown redGreen) [b]
      Spec.assertEqWith s "toggled off" (Lobby.shown (Lobby.update (Lobby.MkBrowse (DeckBrowser.MkColour Color.Red)) Text.empty red)) (Lobby.shown coloured)
      Spec.assertEqWith s "pressed" (Maybe.isJust (Html.search (pressedWith (Text.pack "Red")) (Lobby.view red)), Maybe.isJust (Html.search (pressedWith (Text.pack "Red")) (Lobby.view coloured))) (True, False)

    Spec.it s "keeps a deck whose colours are still to read shown under a colour toggled on" $ do
      let unread = Lobby.browsed (\b' -> b' {DeckBrowser.palette = Map.delete c (DeckBrowser.palette b')}) (Lobby.update (Lobby.MkBrowse (DeckBrowser.MkColour Color.Red)) Text.empty coloured)
      Spec.assertEq s (Lobby.shown unread) [a, b, c]

    Spec.it s "shows only the set chosen, and every set for none" $ do
      let m12 = Lobby.update (Lobby.MkBrowse DeckBrowser.MkSet) (Text.pack "M12") lobby
      Spec.assertEqWith s "one set" (Lobby.shown m12) [b]
      Spec.assertEqWith s "every set" (Lobby.shown (Lobby.update (Lobby.MkBrowse DeckBrowser.MkSet) Text.empty m12)) [a, b, c, d]
      Spec.assertEqWith s "the sets offered, by code" (DeckBrowser.setsOf (Lobby.browser lobby)) (fmap Text.pack ["FDC", "M12", "POR"])
      Spec.assertBool s (Text.isInfixOf (Text.pack "<option value=\"M12\" selected=\"\">M12</option>") (Html.toText (Lobby.view m12))) "chosen in the menu"

    Spec.it s "offers each bot's seat to a player invited" $ do
      let full = botPicks a (Lobby.update Lobby.MkAddBot Text.empty (Lobby.update Lobby.MkAddBot Text.empty lobby))
          seated = Lobby.update (Lobby.MkPick b) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 2)) Text.empty $ Lobby.update (Lobby.MkPick b) Text.empty (Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 1)) Text.empty full)
          page = Lobby.view seated
          invites = [Html.handler (Text.pack "click") path page | path <- Maybe.maybeToList (Html.labelled (Text.pack "button") (Text.pack "Invite a player instead") page)]
      Spec.assertEqWith s "no seat left to a random deck" (Lobby.inviteTarget seated) Nothing
      Spec.assertEqWith s "the first bot's own" invites [Just (Lobby.MkInviteSlot 0)]
      Spec.assertEqWith s "one per bot" (length (filter (Text.isInfixOf (Text.pack "Invite a player instead")) (Text.splitOn (Text.pack "</button>") (Html.toText page)))) 3

    Spec.it s "sets every bot's level and style by the name chosen, ignoring one it does not know" $ do
      let set event value = Lobby.update event (Text.pack value)
          easy = set Lobby.MkPersona "control" (set Lobby.MkDifficulty "easy" lobby)
      Spec.assertEqWith s "unless told otherwise" (Lobby.difficulty lobby, Lobby.persona lobby) (Difficulty.MkHard, Persona.MkBalanced)
      Spec.assertEqWith s "chosen" (Lobby.difficulty easy, Lobby.persona easy) (Difficulty.MkEasy, Persona.MkControl)
      Spec.assertEqWith s "unknown" (set Lobby.MkDifficulty "impossible" easy) easy
      Spec.assertBool s (Text.isInfixOf (Text.pack "<option value=\"easy\" selected=\"\">Easy</option>") (Html.toText (Lobby.view easy))) "the level chosen in its menu"
      Spec.assertBool s (Text.isInfixOf (Text.pack "<option value=\"control\" selected=\"\">Control</option>") (Html.toText (Lobby.view easy))) "the style chosen in its menu"

    Spec.it s "offers the bots' level and style only while a bot is seated" $ do
      let invited = Lobby.update (Lobby.MkInviteSlot 0) Text.empty lobby
          offers = Text.isInfixOf (Text.pack "bot-settings") . Html.toText . Lobby.view
      Spec.assertBool s (offers lobby) "a bot seated"
      Spec.assertBool s (not (offers invited)) "every seat invited"

    Spec.it s "shows the saved decks apart from the shipped" $ do
      let holding = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty Lobby.new
      Spec.assertEqWith s "precons" (Lobby.shown holding) [a]
      Spec.assertEqWith s "my decks" (Lobby.shown (Lobby.update (Lobby.MkBrowse (DeckBrowser.MkShelf DeckBrowser.MkSaved)) Text.empty holding)) [saved]

    Spec.it s "names the deck chosen as its header does, on its seat and in the match-up" $ do
      let named = Lobby.update (Lobby.MkPick saved) Text.empty (Lobby.withDecks (Seq.fromList [a, saved]) Map.empty (Map.singleton saved (Text.pack "Tiny <b>Bolt</b>")) Lobby.new)
      Spec.assertBool s (Text.isInfixOf (Text.pack "<span class=\"seat-deck-name\">Tiny &lt;b&gt;Bolt&lt;/b&gt;</span>") (Html.toText (Lobby.view named))) "the seat card"
      Spec.assertEqWith s "the match-up" (fmap Link.deck (take 1 (Lobby.placesFor 0 named))) [Text.pack "Tiny <b>Bolt</b>"]

    Spec.it s "invites a player to the bot left to a random deck, else to a new seat" $ do
      let botChosen = botPicks a lobby
      Spec.assertEqWith s "the random bot" (Lobby.inviteTarget lobby) (Just 0)
      Spec.assertEqWith s "a new seat" (Lobby.inviteTarget botChosen) (Just 1)
      let invited = Lobby.update (Lobby.MkInviteSlot 1) Text.empty botChosen
      Spec.assertEqWith s "seated" (Lobby.theirs invited, Map.keys (Lobby.invites invited)) ([Just a, Nothing], [1])

  Spec.describe s "pairing" $ do
    Spec.it s "offers a Limited deck the decks naming no format that a Limited game takes" $ do
      let holding = Lobby.browsed (\b' -> b' {DeckBrowser.sizes = Map.fromList [(a, 40), (b, 30)]}) (Lobby.withDecks (Seq.fromList [a, b, saved]) (Map.singleton saved (Text.pack "limited")) Map.empty Lobby.new)
      Spec.assertEq s (Lobby.opponents (Lobby.update (Lobby.MkPick saved) Text.empty holding)) (Seq.fromList [a, saved])

    Spec.it s "counts a deck list's main deck" $ do
      Spec.assertEq s (Lobby.mainCount (Text.pack "Format: commander\nCommander\n1 Sai\nDeck\n4 Island\n2 Shock\nSideboard\n3 Bolt\n")) 6

  Spec.describe s "mainEntries" $ do
    Spec.it s "keeps the main deck's lines alone" $ do
      Spec.assertEq s (fmap fst (Lobby.mainEntries (Text.pack "Format: commander\nCommander\n1 Sai\nDeck\n4 Island\n2 Shock\nSideboard\n3 Bolt\n"))) [4, 2]

  Spec.describe s "dealt" $ do
    Spec.it s "never deals a bot a deck smaller than its fewest" $ do
      let sizes :: Map.Map Text.Text Natural
          sizes = Map.fromList [(Text.pack "demo-a", 12), (Text.pack "sampler", 10), (Text.pack "starter", 22), (Text.pack "deck-a", 30), (Text.pack "theme", 40), (Text.pack "big", 60)]
          deals yours = Lobby.dealt everyRoll (\name -> [Map.findWithDefault 0 name sizes >= Lobby.fewestDealt yours]) (Seq.fromList (Map.keys sizes))
      Spec.assertEqWith s "against 40" (Set.fromList (deals 40)) (Set.fromList [Just (Text.pack "theme"), Just (Text.pack "big")])
      Spec.assertEqWith s "against 60" (Set.fromList (deals 60)) (Set.fromList [Just (Text.pack "theme"), Just (Text.pack "big")])
      Spec.assertEqWith s "against 30" (Set.fromList (deals 30)) (Set.fromList [Just (Text.pack "deck-a"), Just (Text.pack "theme"), Just (Text.pack "big")])
      Spec.assertEqWith s "against 12" (Set.fromList (deals 12)) (Set.fromList (fmap Just [Text.pack "demo-a", Text.pack "starter", Text.pack "deck-a", Text.pack "theme", Text.pack "big"]))

    Spec.it s "deals nothing once every deck is refused" $ do
      Spec.assertEq s (Lobby.dealt everyRoll (const [False]) (Seq.fromList [a, b])) [Nothing, Nothing]

  Spec.describe s "flagged decks" $ do
    Spec.it s "badges a saved deck with problems Not legal" $ do
      let holding = Lobby.browsed (\b' -> b' {DeckBrowser.illegal = Map.singleton saved [Text.pack "43 cards in the deck, fewer than 60"], DeckBrowser.shelf = DeckBrowser.MkSaved}) (Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty Lobby.new)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Not legal") (Html.toText (Lobby.view holding))) "badge"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Not legal") (Html.toText (Lobby.view (Lobby.browsed (\b' -> b' {DeckBrowser.illegal = Map.empty}) holding))))) "no badge"

    Spec.it s "cannot start a flagged deck, listing why" $ do
      let page = Html.toText (Lobby.view flaggedMine)
      Spec.assertEq s (Lobby.startable flaggedMine) (Left [Text.pack "43 cards in the deck, fewer than 60"])
      Spec.assertBool s (Text.isInfixOf (Text.pack "disabled=\"\">Start game") page) "disabled"
      Spec.assertBool s (Text.isInfixOf (Text.pack "43 cards in the deck, fewer than 60") page) "reason"

    Spec.it s "starts an unflagged deck" $ do
      Spec.assertEq s (Lobby.startable (Lobby.update (Lobby.MkPick a) Text.empty flaggedMine)) (Right a)

    Spec.it s "offers Build a deck" $ do
      let page = Lobby.view lobby
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Build a deck") page >>= \path -> Html.handler (Text.pack "click") path page) (Just Lobby.MkOpenBuilder)

  Spec.describe s "edit" $ do
    Spec.it s "offers Edit under a saved deck shown" $ do
      let page = Lobby.view (previewing (Map.singleton saved []) flaggedMine)
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Edit") page >>= \path -> Html.handler (Text.pack "click") path page) (Just (Lobby.MkEdit saved))

    Spec.it s "offers Edit a copy under a precon shown" $ do
      let page = Lobby.view (previewing (Map.singleton a []) (Lobby.update (Lobby.MkPick a) Text.empty lobby))
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Edit a copy") page >>= \path -> Html.handler (Text.pack "click") path page) (Just (Lobby.MkEdit a))

  Spec.describe s "import" $ do
    Spec.it s "keeps what is typed into the panel" $ do
      let opened = Lobby.update (Lobby.MkBrowse DeckBrowser.MkOpenImport) Text.empty lobby
          typed = Lobby.update (Lobby.MkBrowse DeckBrowser.MkImportText) (Text.pack "4 Forest") (Lobby.update (Lobby.MkBrowse DeckBrowser.MkImportName) (Text.pack "Mine") opened)
      Spec.assertEqWith s "typed" (fmap (\i -> (DeckBrowser.importName i, DeckBrowser.importText i)) (DeckBrowser.importing (Lobby.browser typed))) (Just (Text.pack "Mine", Text.pack "4 Forest"))
      Spec.assertEqWith s "closed" (DeckBrowser.importing (Lobby.browser (Lobby.update (Lobby.MkBrowse DeckBrowser.MkCloseImport) Text.empty typed))) Nothing

    Spec.it s "gives an imported deck to the seat chosen for, shows the saved decks and closes the panel" $ do
      let opened = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty (Lobby.update (Lobby.MkBrowse DeckBrowser.MkOpenImport) Text.empty lobby)
          done = Lobby.imported saved opened
          forBot = Lobby.imported saved (Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty opened)
      Spec.assertEqWith s "yours" (Lobby.mine done, Lobby.choosing done) (Just saved, Lobby.MkMine)
      Spec.assertEqWith s "the bot's" (Lobby.mine forBot, Lobby.theirs forBot, Lobby.choosing forBot) (Nothing, [Just saved], Lobby.MkTheirs 0)
      Spec.assertEqWith s "shelf" (DeckBrowser.shelf (Lobby.browser done)) DeckBrowser.MkSaved
      Spec.assertEqWith s "closed" (DeckBrowser.importing (Lobby.browser done)) Nothing

    Spec.it s "empties every seat a deleted deck held" $ do
      let holding = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty Lobby.new
          seated = Lobby.update (Lobby.MkPick saved) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty $ Lobby.update (Lobby.MkPick saved) Text.empty holding
          gone = Lobby.deleted saved seated
      Spec.assertEqWith s "seats" (Lobby.mine gone, Lobby.theirs gone) (Nothing, [Nothing])
      Spec.assertEqWith s "decks" (Lobby.decks gone) (Seq.singleton a)

    Spec.it s "plays every deck but a set's whole card list, a saved deck named All included" $ do
      Spec.assertEqWith s "a set's list" (Lobby.playable (Text.pack "sets/m12/all")) False
      Spec.assertEqWith s "a set's deck" (Lobby.playable b) True
      Spec.assertEqWith s "saved" (Lobby.playable (Text.pack "saved/all")) True

    Spec.it s "seats an imported deck on yours where the bot chosen for may not play it" $ do
      let opened = Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty . Lobby.update (Lobby.MkBrowse DeckBrowser.MkOpenImport) Text.empty $ Lobby.update (Lobby.MkPick c) Text.empty (Lobby.withDecks (Seq.fromList [a, c, saved]) (Map.singleton c (Text.pack "commander")) Map.empty Lobby.new)
          done = Lobby.imported saved opened
      Spec.assertEq s (Lobby.mine done, Lobby.theirs done, Lobby.choosing done) (Just saved, [Nothing], Lobby.MkMine)

    Spec.it s "shows each problem with the deck imported" $ do
      let failed = Lobby.browsed (\b' -> b' {DeckBrowser.importing = Just (DeckBrowser.MkImport (Text.pack "x") (Text.pack "1 Nope") [Text.pack "pasted: no card is named Nope"] False)}) lobby
      Spec.assertBool s (Text.isInfixOf (Text.pack "no card is named Nope") (Html.toText (Lobby.view failed))) "problem"

    Spec.it s "keeps the panel's fields, and the filter's, when a deck's list appears above them" $ do
      let opened = Lobby.update (Lobby.MkBrowse DeckBrowser.MkOpenImport) Text.empty (Lobby.update (Lobby.MkPick a) Text.empty lobby)
          shown = previewing (Map.fromList [(a, [])]) opened
          rebuilt = Foldable.toList (Diff.diff (Lobby.view opened) (Lobby.view shown)) >>= rebuilds
      Spec.assertBool s (all (`notElem` rebuilt) [Text.pack "import-text", Text.pack "import-name", Text.pack "deck-search"]) (show rebuilt)

    Spec.it s "keeps the filter's field when the panel opens above it" $ do
      let rebuilt = Foldable.toList (Diff.diff (Lobby.view lobby) (Lobby.view (Lobby.update (Lobby.MkBrowse DeckBrowser.MkOpenImport) Text.empty lobby))) >>= rebuilds
      Spec.assertBool s (Text.pack "deck-search" `notElem` rebuilt) (show rebuilt)

    Spec.it s "offers a saved deck chosen as yours for export and deletion, and no other" $ do
      let holding = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty Lobby.new
          page deck = Html.toText (Lobby.view (previewing (Map.fromList [(deck, [])]) (Lobby.update (Lobby.MkPick deck) Text.empty holding)))
      Spec.assertBool s (Text.isInfixOf (Text.pack "Delete") (page saved)) "saved"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Delete") (page a))) "shipped"

  Spec.describe s "opponents" $ do
    Spec.it s "offers the bot the decks under your deck's format" $ do
      Spec.assertEq s (Lobby.opponents (Lobby.update (Lobby.MkPick c) Text.empty lobby)) (Seq.fromList [c, d])
      Spec.assertEq s (Lobby.opponents (Lobby.update (Lobby.MkPick a) Text.empty lobby)) (Seq.fromList [a, b])

  Spec.describe s "rematch" $ do
    Spec.it s "returns to the decks played, the invited slot still connected, and nobody ready" $ do
      let back = Lobby.rematch ((a, Text.pack "list a") NonEmpty.:| [(b, Text.pack "list b"), (c, Text.pack "list c")]) (Set.singleton 1)
      Spec.assertEq s (Lobby.mine back, Lobby.theirs back) (Just a, [Just b, Nothing])
      Spec.assertEq s (Map.toList (fmap (\i -> (Lobby.connected i, Lobby.guestDeck i, Lobby.guestReady i)) (Lobby.invites back))) [(1, (True, Just (c, Text.pack "list c"), False))]
      Spec.assertBool s (not (Lobby.ready back) && not (Lobby.starts back)) "nobody ready"
      Spec.assertBool s (Lobby.starts (Lobby.heard 1 (Link.MkReady True) (Lobby.update Lobby.MkReady Text.empty back))) "starts once both are ready"

  Spec.describe s "withDecks" $ do
    Spec.it s "keeps what was picked and typed" $ do
      let early = Lobby.browsed (\b' -> b' {DeckBrowser.search = Text.pack "web"}) (Lobby.update (Lobby.MkPick a) Text.empty lobby)
          later = Lobby.withDecks (Seq.fromList [a, b]) Map.empty Map.empty early
      Spec.assertEqWith s "mine" (Lobby.mine later) (Just a)
      Spec.assertEqWith s "search" (DeckBrowser.search (Lobby.browser later)) (Text.pack "web")
      Spec.assertEqWith s "read" (DeckBrowser.reading (Lobby.browser later)) False

  Spec.describe s "view" $ do
    Spec.it s "lists the decks matching the filter, each badged with its set" $ do
      let page = Html.toText . Lobby.view $ Lobby.browsed (\b' -> b' {DeckBrowser.search = Text.pack "WEB"}) lobby
      Spec.assertBool s (Text.isInfixOf (Text.pack "<span class=\"set-badge\">M12</span>") page) "badge"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "<h2>M12</h2>") page)) "no heading"
      Spec.assertBool s (Text.isInfixOf (Text.pack "Entangling Webs") page) "match"
      Spec.assertBool s (not $ Text.isInfixOf (Text.pack "Demo A") page) "filtered out"

    Spec.it s "lists only the decks the bot may play while choosing its deck" $ do
      let page = Html.toText . Lobby.view . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty $ Lobby.update (Lobby.MkPick c) Text.empty lobby
      Spec.assertBool s (Text.isInfixOf (Text.pack "Wretched Ranks") page) "your format"
      Spec.assertBool s (not $ Text.isInfixOf (Text.pack "Entangling Webs") page) "another format"

    Spec.it s "heads the decks with the seat chosen for, and marks that seat alone" $ do
      let forBot = Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty lobby
          marked page = [Html.content seat | seat <- seatPicks page, pressed seat]
      Spec.assertEqWith s "yours" (marked (Lobby.view lobby)) [Text.pack "YYouChoosing a deckNo deck yetChoose one to start"]
      Spec.assertEqWith s "the bot's" (marked (Lobby.view forBot)) [Text.pack "BotChoosing a deckRandom deckDrawn when the game starts"]
      Spec.assertBool s (Text.isInfixOf (Text.pack "<h2>Deck for Bot</h2>") (Html.toText (Lobby.view forBot))) "heading"
      Spec.assertEqWith s "once yours is chosen" (marked (Lobby.view (Lobby.update (Lobby.MkPick a) Text.empty lobby))) [Text.pack "YYouDemo APOR"]

    Spec.it s "shows a deck's list to copy only under that deck" $ do
      let holding = Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty Lobby.new
          exported = Lobby.browsed (\b' -> b' {DeckBrowser.exporting = Just (saved, Text.pack "4 Exported Card"), DeckBrowser.previews = Map.fromList [(saved, []), (a, [])]}) (Lobby.update (Lobby.MkPick saved) Text.empty holding)
          elsewhere = Lobby.update (Lobby.MkPick a) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty $ exported
      Spec.assertBool s (Text.isInfixOf (Text.pack "4 Exported Card") (Html.toText (Lobby.view exported))) "its own"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "4 Exported Card") (Html.toText (Lobby.view elsewhere)))) "not the bot's"

    Spec.it s "keeps Ready off, with players invited, while your deck breaks its format" $ do
      let invited = Lobby.linked 0 (\invite -> invite {Lobby.connected = True}) (Lobby.update (Lobby.MkInviteSlot 0) Text.empty flaggedMine)
          fixed = Lobby.browsed (\b' -> b' {DeckBrowser.illegal = Map.empty}) invited
      Spec.assertEqWith s "flagged" (Lobby.pressable invited, Lobby.waiting invited) (False, Just "Your deck breaks its format:")
      Spec.assertEqWith s "only the guest awaited" (Lobby.pressable fixed, Lobby.waiting fixed) (True, Just "Player 2 is choosing a deck.")
      Spec.assertBool s (Text.isInfixOf (Text.pack "disabled=\"\">Ready") (Html.toText (Lobby.view invited))) "drawn disabled"

    Spec.it s "names a deck's format" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "Commander") . Html.toText $ Lobby.view lobby) "format"

    Spec.it s "disables Start, saying why, until every seat is ready" $ do
      let state held = (Text.isInfixOf (Text.pack "disabled=\"\">Start game") (Html.toText (Lobby.view held)), Lobby.waiting held)
          picked = Lobby.update (Lobby.MkPick a) Text.empty lobby
      Spec.assertEqWith s "reading" (state Lobby.new) (True, Just "Reading the decks\x2026")
      Spec.assertEqWith s "no deck" (state lobby) (True, Just "Choose your deck to start.")
      Spec.assertEqWith s "ready" (state picked) (False, Nothing)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Choose your deck to start.") (Html.toText (Lobby.view lobby))) "the reason said"
      Spec.assertBool s (Text.isInfixOf (Text.pack "Every seat has a deck. Ready to start.") (Html.toText (Lobby.view picked))) "ready said"

    Spec.it s "names each bot by its seat once there are two" $ do
      let one = Html.toText (Lobby.view lobby)
          two = Html.toText (Lobby.view (Lobby.update Lobby.MkAddBot Text.empty lobby))
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Bot<") one && not (Text.isInfixOf (Text.pack ">Remove<") one)) "one bot"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Bot 1<") two && Text.isInfixOf (Text.pack ">Bot 2<") two && Text.isInfixOf (Text.pack ">Remove<") two) "two bots"

    Spec.it s "says the decks are being read until they are" $ do
      let page = Html.toText (Lobby.view Lobby.new)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Reading the decks") page) "reading"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "No deck matches") page)) "no empty match"

    Spec.it s "says why the decks did not load in place of reading them" $ do
      let page = Html.toText (Lobby.view (Lobby.browsed (\b' -> b' {DeckBrowser.problem = Just (Text.pack "Reload to try again.")}) Lobby.new))
      Spec.assertBool s (Text.isInfixOf (Text.pack "role=\"alert\">Reload to try again.") page) "problem"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "<p class=\"empty\">Reading the decks") page)) "not reading"

    Spec.it s "sends a deck's name when it is tapped" $ do
      let page = Lobby.view lobby
      Spec.assertEq s (Html.search (tileOf (Text.pack "Entangling Webs")) page >>= \path -> Html.handler (Text.pack "click") path page) . Just $ Lobby.MkPick b

    Spec.it s "draws a deck's curve on its tile once read" $ do
      let page = Html.toText (Lobby.view (Lobby.browsed (\b' -> b' {DeckBrowser.curves = Map.singleton b [0, 4, 8, 6, 2, 0, 1]}) lobby))
      Spec.assertBool s (Text.isInfixOf (Text.pack "class=\"curve spark\" aria-hidden=\"true\"><span class=\"bar\" title=\"0 at mana value 0\"") page && Text.isInfixOf (Text.pack "title=\"8 at mana value 2\"") page) "curve"

    Spec.it s "details the chosen deck: its kinds of card and its curve" $ do
      let lines' = [DeckBrowser.MkLine 4 (CardName.MkCardName (Text.pack "Shock")) Nothing 1 DeckBrowser.MkSpell, DeckBrowser.MkLine 2 (CardName.MkCardName (Text.pack "Bears")) Nothing 2 DeckBrowser.MkCreature, DeckBrowser.MkLine 17 (CardName.MkCardName (Text.pack "Mountain")) Nothing 0 DeckBrowser.MkLand]
          page = Html.toText (Lobby.view (previewing (Map.singleton a lines') (Lobby.update (Lobby.MkPick a) Text.empty lobby)))
      Spec.assertEqWith s "curve" (DeckBrowser.curveOf lines') [0, 4, 2, 0, 0, 0, 0]
      Spec.assertBool s (Text.isInfixOf (Text.pack "<dt>Creatures</dt><dd class=\"muted\">2</dd>") page && Text.isInfixOf (Text.pack "<dt>Lands</dt><dd class=\"muted\">17</dd>") page) "kinds"
      Spec.assertBool s (Text.isInfixOf (Text.pack "See all 23 cards") page) "list"

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
flaggedMine = Lobby.browsed (\b' -> b' {DeckBrowser.illegal = Map.singleton saved [Text.pack "43 cards in the deck, fewer than 60"]}) (Lobby.update (Lobby.MkPick saved) Text.empty (Lobby.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty Lobby.new))

-- | The bot's seat chosen, this deck picked for it.
botPicks :: Text.Text -> Lobby.Lobby -> Lobby.Lobby
botPicks deck = Lobby.update (Lobby.MkPick deck) Text.empty . Lobby.update (Lobby.MkChoose (Lobby.MkTheirs 0)) Text.empty

-- | 'lobby' with each deck's colours read: a red, b red and green, c white,
-- d black.
coloured :: Lobby.Lobby
coloured = Lobby.browsed (\b' -> b' {DeckBrowser.palette = Map.fromList [(a, Set.singleton Color.Red), (b, Set.fromList [Color.Red, Color.Green]), (c, Set.singleton Color.White), (d, Set.singleton Color.Black)]}) lobby

-- | An element's property by name, if it has it.
propertyOf :: String -> Html.Html msg -> Maybe Text.Text
propertyOf name html = case html of
  Html.MkElement node -> Maybe.listToMaybe [v | Attribute.MkProperty k v <- Foldable.toList (Html.attributes node), k == Text.pack name]
  Html.MkText _ -> Nothing

-- | The lobby with these decks' lists read.
previewing :: Map.Map Text.Text [DeckBrowser.Line] -> Lobby.Lobby -> Lobby.Lobby
previewing lists = Lobby.browsed (\b' -> b' {DeckBrowser.previews = lists})

-- | Whether an element is a pressed toggle with this label.
pressedWith :: Text.Text -> Html.Html msg -> Bool
pressedWith label html = propertyOf "aria-label" html == Just label && pressed html

-- | Whether an element is marked pressed.
pressed :: Html.Html msg -> Bool
pressed html = propertyOf "aria-pressed" html == Just (Text.pack "true")

-- | Whether an element is the deck tile naming this deck.
tileOf :: Text.Text -> Html.Html msg -> Bool
tileOf name html = propertyOf "class" html == Just (Text.pack "deck") && Text.isInfixOf name (Html.content html)

-- | Each seat's button choosing it, in order.
seatPicks :: Html.Html msg -> [Html.Html msg]
seatPicks html = case html of
  Html.MkElement node
    | propertyOf "class" html == Just (Text.pack "seat-pick") -> [html]
    | otherwise -> foldMap seatPicks (Html.childList (Html.children node))
  Html.MkText _ -> []

lobby :: Lobby.Lobby
lobby = Lobby.withDecks (Seq.fromList [a, b, c, d]) (Map.fromList [(c, Text.pack "commander"), (d, Text.pack "commander")]) Map.empty Lobby.new

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

-- | Every deck a roll may pick among these, as the list monad's branches,
-- so 'Lobby.dealt' over it is every deal; nothing among none, as the
-- shell's roll.
everyRoll :: Seq.Seq Text.Text -> [Maybe Text.Text]
everyRoll among = if Seq.null among then [Nothing] else fmap Just (Foldable.toList among)
