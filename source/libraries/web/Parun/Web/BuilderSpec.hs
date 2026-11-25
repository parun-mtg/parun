module Parun.Web.BuilderSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.DeckList as DeckList
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.CardIndex as CardIndex
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Builder" $ do
  Spec.describe s "update" $ do
    Spec.it s "adds and removes a copy at a time, dropping a card at none" $ do
      let added = tap (Builder.MkAdd DeckList.Main bolt) (tap (Builder.MkAdd DeckList.Main bolt) fresh)
          once = tap (Builder.MkRemove DeckList.Main bolt) added
          none = tap (Builder.MkRemove DeckList.Main bolt) once
      Spec.assertEq s (fmap Builder.deck [added, once, none]) [Map.singleton bolt 2, Map.singleton bolt 1, Map.empty]

    Spec.it s "an edit marks the deck changed and a save clears it" $ do
      let added = tap (Builder.MkAdd DeckList.Main bolt) fresh
      Spec.assertEq s (Builder.dirty fresh, Builder.dirty added, Builder.dirty (Builder.saved (Text.pack "saved/burn") added)) (False, True, False)

    Spec.it s "toggling a colour twice clears it" $ do
      let once = tap (Builder.MkToggleColour Color.Red) fresh
      Spec.assertEq s (CardIndex.wanted (Builder.query once), CardIndex.wanted (Builder.query (tap (Builder.MkToggleColour Color.Red) once))) (Set.singleton Color.Red, Set.empty)

    Spec.it s "Back with changes asks first; Stay closes the question" $ do
      let asked = tap Builder.MkBack (tap (Builder.MkAdd DeckList.Main bolt) fresh)
      Spec.assertEq s (Builder.leaving (tap Builder.MkBack fresh), Builder.leaving asked, Builder.leaving (tap Builder.MkStay asked)) (False, True, False)

    Spec.it s "takes the format chosen, which counts as a change" $ do
      let chosen = Builder.update Builder.MkFormat (Text.pack "pauper") fresh
      Spec.assertEq s (Builder.format chosen, Builder.dirty chosen) (Text.pack "pauper", True)

  Spec.describe s "view" $ do
    Spec.it s "names each colour filter by its colour, and every button" $ do
      let names = fmap Html.spoken (Html.elements (Text.pack "button") (Builder.view fresh))
      Spec.assertEqWith s "colours" (filter (`elem` fmap (Text.pack . show) [minBound .. maxBound :: Color.Color]) names) (fmap Text.pack ["White", "Blue", "Black", "Red", "Green"])
      Spec.assertEqWith s "unnamed" (filter (Text.null . Text.strip) names) []

    Spec.it s "draws its colour filters and menus as the decks screen does" $ do
      let shown = page fresh
      Spec.assertBool s (all (`Text.isInfixOf` shown) [Text.pack "class=\"colour-toggles\"", Text.pack "class=\"mana-value deck-menu\"", Text.pack "class=\"format deck-menu\""]) "restyled"

  Spec.describe s "checked" $ do
    Spec.it s "holds the check's problems and the cards they name" $ do
      let found = Builder.checked [Text.pack "5 copies"] (Set.singleton bolt) fresh
      Spec.assertEq s (Builder.problems found, Builder.flagged found) ([Text.pack "5 copies"], Set.singleton bolt)

  Spec.describe s "listText" $ do
    Spec.it s "writes no header for a casual deck, which names no format" $ do
      Spec.assertEq s (Builder.listText (tap (Builder.MkAdd DeckList.Main bolt) fresh)) (Text.pack "1 Lightning Bolt\n")

    Spec.it s "offers casual first, and chooses it" $ do
      let shown = Html.toText (Builder.view fresh)
      Spec.assertBool s (Text.isInfixOf (Text.pack "<option value=\"\" selected=\"\">Casual (no format)</option>") shown) "casual chosen"

    Spec.it s "offers each format by its name, and the format chosen though no other is built for it" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "Format: commanderdraft\n1 Island\n") (Builder.new [Text.pack "commander", Text.pack "oldschool"])
          shown = Html.toText (Builder.view opened)
      Spec.assertEq s (fmap (`Text.isInfixOf` shown) [Text.pack "<option value=\"oldschool\">Old School</option>", Text.pack "<option value=\"commanderdraft\" selected=\"\">Commander Draft</option>"]) [True, True]

    Spec.it s "writes the format header, then the main deck by name" $ do
      let built = (tap (Builder.MkAdd DeckList.Main bolt) . tap (Builder.MkAdd DeckList.Main mountain) . tap (Builder.MkAdd DeckList.Main bolt)) constructed
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: constructed\n2 Lightning Bolt\n1 Mountain\n")

    Spec.it s "writes back the rest of a list untouched" $ do
      let opened = constructed {Builder.deck = Map.singleton bolt 4, Builder.suffixes = Map.singleton (DeckList.Main, bolt) [(Text.pack " (M10) 146", 4)], Builder.rest = [Text.pack "Sideboard", Text.pack "2 Shock"]}
      Spec.assertEq s (Builder.listText opened) (Text.pack "Format: constructed\n4 Lightning Bolt (M10) 146\n\nSideboard\n2 Shock\n")

    Spec.it s "parses back to the same deck, a split card included" $ do
      let built = fresh {Builder.deck = Map.fromList [(bolt, 4), (fireIce, 2)]}
      Spec.assertEq s (Map.fromList [(name, n) | (n, name) <- Lobby.entries (Builder.listText built)]) (Builder.deck built)

  Spec.describe s "view" $ do
    Spec.it s "shows the count against the format's minimum" $ do
      let built = indexed {Builder.deck = Map.fromList [(bolt, 4), (mountain, 39)], Builder.least = Just (60, False)}
      Spec.assertBool s (Text.isInfixOf (Text.pack "43 / 60") (page built)) "count"

    Spec.it s "marks a flagged card" $ do
      let built = indexed {Builder.deck = Map.singleton bolt 5}
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "flagged") (page built))) "unflagged"
      Spec.assertBool s (Text.isInfixOf (Text.pack "flagged") (page (Builder.checked [] (Set.singleton bolt) built))) "flagged"

    Spec.it s "lists the check's problems" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "5 copies of Lightning Bolt") (page (Builder.checked [Text.pack "5 copies of Lightning Bolt"] Set.empty indexed))) "problem"

    Spec.it s "says how many matched beyond the cap" $ do
      let many = fresh {Builder.index = Just (Seq.fromList [card ("Card " <> show n) | n <- [1 .. 61 :: Int]])}
      Spec.assertBool s (Text.isInfixOf (Text.pack "Showing 60 of 61") (page many)) "beyond"

    Spec.it s "shows more on a tap, and the first again once the search changes" $ do
      let many = fresh {Builder.index = Just (Seq.fromList [card ("Card " <> show n) | n <- [1 .. 130 :: Int]])}
          more = tap Builder.MkMore many
          tapped label builder = let shown' = Builder.view builder in Html.labelled (Text.pack "button") (Text.pack label) shown' >>= \path -> Html.handler (Text.pack "click") path shown'
      Spec.assertEqWith s "offered" (tapped "Show 60 more" many) (Just Builder.MkMore)
      Spec.assertEqWith s "shown" (Text.isInfixOf (Text.pack "Showing 120 of 130") (page more), tapped "Show 10 more" more) (True, Just Builder.MkMore)
      Spec.assertEqWith s "reset" (Builder.showing (Builder.update Builder.MkSearch (Text.pack "card") more)) CardIndex.cap

    Spec.it s "keeps a result's Add where it was as its card is added" $ do
      let addAt builder = Html.labelled (Text.pack "button") (Text.pack "Add Shock") (Builder.view builder)
          added = tap (Builder.MkAdd DeckList.Main (named "Shock")) indexed
      Spec.assertEqWith s "in deck" (Text.isInfixOf (Text.pack "1 in deck") (page added)) True
      Spec.assertEqWith s "same place" (addAt added) (addAt indexed)

    Spec.it s "drops a save refused for want of cards once a card is added" $ do
      let refused = indexed {Builder.saveProblems = [Text.pack "Add some cards before saving."]}
      Spec.assertEq s (Builder.saveProblems (tap (Builder.MkAdd DeckList.Main bolt) refused)) []

    Spec.it s "draws the curve only once the deck holds a spell" $ do
      Spec.assertEqWith s "empty" (Text.isInfixOf (Text.pack "curve") (page indexed)) False
      Spec.assertEqWith s "a spell" (Text.isInfixOf (Text.pack "curve builder-curve") (page (tap (Builder.MkAdd DeckList.Main bolt) indexed))) True

    Spec.it s "says no card matches" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "No card matches.") (page (Builder.update Builder.MkSearch (Text.pack "zzzz") indexed))) "none"

    Spec.it s "says the cards are being prepared" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "Preparing the cards") (page fresh)) "preparing"

    Spec.it s "offers the five basic lands whatever the search" $ do
      let shown = Builder.view (Builder.update Builder.MkSearch (Text.pack "zzzz") indexed)
          tapped name = Html.labelled (Text.pack "button") (Text.pack name) shown >>= \path -> Html.handler (Text.pack "click") path shown
      Spec.assertEq s (fmap tapped ["Plains", "Island", "Swamp", "Mountain", "Forest"]) (fmap (Just . Builder.MkAdd DeckList.Main . CardName.MkCardName . Text.pack) ["Plains", "Island", "Swamp", "Mountain", "Forest"])

    Spec.it s "adds a result when its name's button is tapped" $ do
      let shown = Builder.view indexed
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Add Lightning Bolt") shown >>= \path -> Html.handler (Text.pack "click") path shown) (Just (Builder.MkAdd DeckList.Main bolt))

    Spec.it s "folds a narrow screen's filters behind a toggle that counts those set" $ do
      let tapped label builder = let shown' = Builder.view builder in Html.labelled (Text.pack "button") (Text.pack label) shown' >>= \path -> Html.handler (Text.pack "click") path shown'
          narrowed = tap (Builder.MkToggleKind CardType.Creature) (tap (Builder.MkToggleColour Color.Red) indexed)
          opened = tap Builder.MkToggleFilters narrowed
      Spec.assertEqWith s "folded" (Text.isInfixOf (Text.pack "class=\"filters\"") (page indexed), tapped "Filters" indexed) (True, Just Builder.MkToggleFilters)
      Spec.assertEqWith s "counted" (tapped "Filters \x00b7 2" narrowed) (Just Builder.MkToggleFilters)
      Spec.assertEqWith s "open" (fmap (`Text.isInfixOf` page opened) [Text.pack "class=\"filters open\"", Text.pack "aria-expanded=\"true\""]) [True, True]
      Spec.assertEqWith s "closed" (Builder.filtersOpen (tap Builder.MkToggleFilters opened)) False

    Spec.it s "holds the deck's count and latest cards in a narrow screen's summary" $ do
      let added = (tap (Builder.MkAdd DeckList.Side mountain) . tap (Builder.MkAdd DeckList.Main (named "Shock")) . tap (Builder.MkAdd DeckList.Main bolt)) indexed
          summaryOf builder = let shown' = Builder.view builder in maybe Text.empty Html.content (Html.search (Text.isPrefixOf (Text.pack "<div class=\"deck-summary panel\"") . Html.toText) shown' >>= \path -> Html.at path shown')
      Spec.assertEqWith s "latest" (Text.isInfixOf (Text.pack "Latest: Mountain (in sideboard), Shock, Lightning Bolt") (page added)) True
      Spec.assertEqWith s "count" (Text.isInfixOf (Text.pack "2 cards in the deck") (summaryOf added)) True
      Spec.assertEqWith s "removed" (Text.isInfixOf (Text.pack "Latest: Mountain (in sideboard), Lightning Bolt") (page (tap (Builder.MkRemove DeckList.Main (named "Shock")) added))) True

    Spec.it s "switches a narrow screen between the search and the deck" $ do
      let shown = Builder.view indexed
          switched = tap Builder.MkToggleDeck indexed
      Spec.assertEqWith s "offered" (Html.labelled (Text.pack "button") (Text.pack "See deck") shown >>= \path -> Html.handler (Text.pack "click") path shown) (Just Builder.MkToggleDeck)
      Spec.assertEqWith s "shown" (fmap (`Text.isInfixOf` page switched) [Text.pack "class=\"builder-body deck-shown\"", Text.pack "Back to cards"]) [True, True]

    Spec.it s "shows the rules text of the card being read" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "deals 3 damage") (page (Builder.update (Builder.MkRead bolt) Text.empty indexed))) "rules"

  Spec.describe s "open" $ do
    Spec.it s "opens a commander precon and writes its commander back" $ do
      let text = "Format: commander\n\nCommander\n1 Sai, Master Thopterist\n\nDeck\n1 Ornithopter\n30 Island\n"
          opened = Builder.open Nothing (Text.pack "Sai") (Text.pack text) fresh
      Spec.assertEq s (Builder.format opened, Builder.deck opened) (Text.pack "commander", Map.fromList [(named "Ornithopter", 1), (named "Island", 30)])
      Spec.assertEq s (Builder.listText opened) (Text.pack "Format: commander\nCommander\n1 Sai, Master Thopterist\n\nDeck\n30 Island\n1 Ornithopter\n")

    Spec.it s "opens a list with no header as a casual main deck" $ do
      let opened = Builder.open Nothing (Text.pack "Mine") (Text.pack "4 Lightning Bolt\n\n20 Mountain\n") fresh
      Spec.assertEq s (Builder.format opened, Builder.deck opened) (Text.empty, Map.fromList [(bolt, 4), (mountain, 20)])

    Spec.it s "sums a card listed twice" $ do
      Spec.assertEq s (Builder.deck (Builder.open Nothing Text.empty (Text.pack "2 Mountain\n3 Mountain\n") fresh)) (Map.singleton mountain 5)

    Spec.it s "keeps a card's printing and annotations" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "4 Lightning Bolt (m10) 146\n1 Paliano Vanguard {color: G}\n") fresh
      Spec.assertEq s (Builder.listText opened) (Text.pack "4 Lightning Bolt (M10) 146\n1 Paliano Vanguard {color: G}\n")

    Spec.it s "keeps a card's lines with different printings apart" $ do
      let text = Text.pack "2 Mountain (ZEN) 246\n2 Mountain (UNH) 140\n"
          opened = Builder.open Nothing Text.empty text fresh
      Spec.assertEq s (Builder.deck opened, Builder.listText opened) (Map.singleton mountain 4, text)

    Spec.it s "keeps a bare line beside a printed one" $ do
      let text = Text.pack "2 Mountain\n1 Mountain (UNH) 140\n"
      Spec.assertEq s (Builder.listText (Builder.open Nothing Text.empty text fresh)) text

    Spec.it s "takes a copy removed from the last line, and adds one to the first" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "2 Mountain (ZEN) 246\n2 Mountain (UNH) 140\n") fresh
      Spec.assertEq s (Builder.listText (tap (Builder.MkRemove DeckList.Main mountain) opened), Builder.listText (tap (Builder.MkAdd DeckList.Main mountain) opened)) (Text.pack "2 Mountain (ZEN) 246\n1 Mountain (UNH) 140\n", Text.pack "3 Mountain (ZEN) 246\n2 Mountain (UNH) 140\n")

    Spec.it s "counts a commander only where the format's size counts it" $ do
      let text = "Commander\n1 Sai, Master Thopterist\nDeck\n59 Island\n"
          opened = Builder.open Nothing Text.empty (Text.pack text) indexed
      Spec.assertBool s (Text.isInfixOf (Text.pack "59 / 60") (page opened {Builder.least = Just (60, False)})) "constructed"

    Spec.it s "an opened saved deck saves over itself" $ do
      let opened = Builder.open (Just (Text.pack "saved/burn")) (Text.pack "Burn") (Text.pack "4 Lightning Bolt\n") fresh
      Spec.assertEq s (Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) opened) (Right (Text.pack "saved/burn"))

    Spec.it s "an opened precon offers its name and refuses a clash" $ do
      let opened = Builder.open Nothing (Text.pack "Burn") (Text.pack "4 Lightning Bolt\n") fresh
      Spec.assertEq s (Builder.saveName opened, Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) opened) (Text.pack "Burn", Left [Text.pack "A saved deck is already named that."])

    Spec.it s "opens with nothing to save" $ do
      Spec.assertEq s (Builder.dirty (Builder.open Nothing (Text.pack "Burn") (Text.pack "4 Lightning Bolt\n") fresh)) False

    Spec.it s "CR 903.5a counts a commander toward the deck, and notes the sections kept" $ do
      let text = "Format: commander\nCommander\n1 Sai, Master Thopterist\nDeck\n99 Island\nSideboard\n2 Shock\n"
          shown = page (Builder.open Nothing Text.empty (Text.pack text) indexed) {Builder.least = Just (100, True)}
      Spec.assertBool s (Text.isInfixOf (Text.pack "100 / 100") shown) "count"
      Spec.assertBool s (Text.isInfixOf (Text.pack "Commander \x00b7 1") shown && Text.isInfixOf (Text.pack "Sideboard \x00b7 2") shown) "kept"

    Spec.it s "shows the name offered in the name field" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "value=\"Entangling Webs\"") (page (Builder.open Nothing (Text.pack "Entangling Webs") (Text.pack "4 Lightning Bolt\n") indexed))) "name"

    Spec.it s "adding then removing a card leaves nothing to save" $ do
      Spec.assertEq s (Builder.dirty (tap (Builder.MkRemove DeckList.Main bolt) (tap (Builder.MkAdd DeckList.Main bolt) fresh))) False

  Spec.describe s "pool" $ do
    Spec.it s "CR 100.2b caps a pooled card at its count" $ do
      let added = iterate (tap (Builder.MkAdd DeckList.Main bolt)) (pooled "2 Lightning Bolt\n") !! 3
      Spec.assertEq s (Builder.deck added) (Map.singleton bolt 2)

    Spec.it s "CR 100.2b never caps a basic land" $ do
      Spec.assertEq s (Builder.deck (tap (Builder.MkAdd DeckList.Main mountain) (pooled "2 Lightning Bolt\n"))) (Map.singleton mountain 1)

    Spec.it s "CR 100.4b chooses Limited and writes the pool's unused cards as the Sideboard" $ do
      let built = tap (Builder.MkAdd DeckList.Main bolt) (pooled "2 Lightning Bolt\n1 Shock\n")
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: limited\n1 Lightning Bolt\n\nSideboard\n1 Lightning Bolt\n1 Shock\n")

    Spec.it s "returns a removed card to the Sideboard" $ do
      let built = tap (Builder.MkRemove DeckList.Main bolt) (tap (Builder.MkAdd DeckList.Main bolt) (pooled "2 Lightning Bolt\n"))
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: limited\n\nSideboard\n2 Lightning Bolt\n")

    Spec.it s "keeps the pool, and its caps, when the search widens to every card" $ do
      let widened = Builder.update Builder.MkPoolChoice Text.empty (pooled "2 Lightning Bolt\n")
          more = iterate (tap (Builder.MkAdd DeckList.Main bolt)) widened !! 3
      Spec.assertEq s (CardIndex.among (Builder.searchQuery widened), Builder.listText more) (Nothing, Text.pack "Format: limited\n2 Lightning Bolt\n")

    Spec.it s "drops the pool, its caps and its Sideboard when asked" $ do
      let dropped = tap Builder.MkDropPool (pooled "1 Lightning Bolt\n")
      Spec.assertEq s (Builder.listText (iterate (tap (Builder.MkAdd DeckList.Main bolt)) dropped !! 2)) (Text.pack "Format: limited\n2 Lightning Bolt\n")

    Spec.it s "resolves a pasted pool's names to the cards' own, whatever their case" $ do
      Spec.assertEq s (Builder.listText (tap (Builder.MkAdd DeckList.Main bolt) (pooled "3 lightning bolt\n"))) (Text.pack "Format: limited\n1 Lightning Bolt\n\nSideboard\n2 Lightning Bolt\n")

    Spec.it s "refuses a pool naming a card it doesn't know, saying which" $ do
      let refused = pooled "1 Lightnin Bolt\n2 Shock\n"
      Spec.assertEq s (Builder.pool refused, Builder.poolProblems refused) (Nothing, [Text.pack "No card is named Lightnin Bolt."])

    Spec.it s "refuses a pool naming no cards" $ do
      Spec.assertEq s (Builder.poolProblems (pooled "\n")) [Text.pack "The pool names no cards."]

    Spec.it s "writes the pool's Sideboard only under Limited" $ do
      Spec.assertEq s (Builder.listText (Builder.update Builder.MkFormat (Text.pack "constructed") (pooled "2 Shock\n"))) (Text.pack "Format: constructed\n")

    Spec.it s "leaves basic lands out of the pool's Sideboard" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "Format: limited\n9 Mountain\n\nSideboard\n1 Shock\n") indexed
      Spec.assertEq s (Builder.listText (iterate (tap (Builder.MkRemove DeckList.Main mountain)) opened !! 9)) (Text.pack "Format: limited\n\nSideboard\n1 Shock\n")

    Spec.it s "reads a pool's Sideboard back under the cards' own names" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "Format: limited\n2 Lightning Bolt\n\nSideboard\n1 lightning bolt\n") indexed
      Spec.assertEq s (Builder.listText opened) (Text.pack "Format: limited\n2 Lightning Bolt\n\nSideboard\n1 Lightning Bolt\n")

    Spec.it s "a new pool is a change to save" $ do
      Spec.assertEq s (Builder.dirty (pooled "2 Shock\n")) True

    Spec.it s "dropping an opened deck's pool is a change to save" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "Format: limited\n1 Lightning Bolt\n\nSideboard\n1 Shock\n") indexed
      Spec.assertEq s (Builder.dirty opened, Builder.dirty (tap Builder.MkDropPool opened)) (False, True)

    Spec.it s "a pasted pool replaces a Sideboard an opened list kept" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "4 Lightning Bolt\nSideboard\n2 Shock\n") indexed
          built = Builder.update Builder.MkUsePool Text.empty (Builder.update Builder.MkPoolText (Text.pack "4 Lightning Bolt\n") opened)
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: limited\n4 Lightning Bolt\n")

    Spec.it s "reads a limited list's main deck and Sideboard back into its pool" $ do
      let text = Text.pack "Format: limited\n1 Lightning Bolt\n\nSideboard\n1 Lightning Bolt\n1 Shock\n"
          opened = Builder.open Nothing Text.empty text indexed
      Spec.assertEq s (Builder.listText opened, Builder.deck (iterate (tap (Builder.MkAdd DeckList.Main bolt)) opened !! 2)) (text, Map.singleton bolt 2)

    Spec.it s "finds a double-faced card a set's list names by both faces" $ do
      let scoped = Builder.setPool (Text.pack "neo") [named "Front Face // Back Face"] indexed
      Spec.assertEq s (fmap (CardName.unwrap . CardIndex.name) (Foldable.toList (snd (CardIndex.search CardIndex.cap (Builder.searchQuery scoped) (Maybe.fromMaybe Seq.empty (Builder.index scoped)))))) [Text.pack "Front Face", Text.pack "Mountain"]

    Spec.it s "a set's cards narrow the search without capping" $ do
      let scoped = Builder.setPool (Text.pack "m10") [bolt] indexed
      Spec.assertEq s (CardIndex.among (Builder.searchQuery scoped), Builder.deck (iterate (tap (Builder.MkAdd DeckList.Main bolt)) scoped !! 5)) (Just (Set.singleton (Text.pack "lightning bolt")), Map.singleton bolt 5)

    Spec.it s "offers every card, each set, and pasting a pool" $ do
      let shown = page indexed {Builder.sets = [(Text.pack "m10", Text.pack "Magic 2010")]}
      Spec.assertBool s (all (`Text.isInfixOf` shown) [Text.pack "<option value=\"\" selected=\"\">Every card</option>", Text.pack "<option value=\"set:m10\">Magic 2010</option>", Text.pack "<option value=\"paste\">Paste a pool"]) "options"

    Spec.it s "uses the pool pasted into its panel" $ do
      let shown = Builder.view (Builder.update Builder.MkPoolChoice (Text.pack "paste") indexed)
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Use this pool") shown >>= \path -> Html.handler (Text.pack "click") path shown) (Just Builder.MkUsePool)

    Spec.it s "shows how many of a pooled card are left" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "1 left") (page (tap (Builder.MkAdd DeckList.Main bolt) (pooled "2 Lightning Bolt\n")))) "left"

  Spec.describe s "kept" $ do
    Spec.it s "counts no cards for the sections whose lines are not cards" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "1 Island\nDraft Notes\n3 Lurking Automaton\nStickers\n1 Ancestral Hot Dog Minotaur\n") indexed
      Spec.assertBool s (Text.isInfixOf (Text.pack "Also kept: Draft Notes, Stickers") (page opened)) "kept"

  Spec.describe s "fidelity" $ do
    Spec.it s "keeps the first Format header, as a deck list reads it" $ do
      Spec.assertEq s (Builder.format (Builder.open Nothing Text.empty (Text.pack "Format: commander\nFormat: standard\n1 Island\n") fresh)) (Text.pack "commander")

    Spec.it s "reads a Name header as no card, and nothing to note" $ do
      let opened = Builder.open Nothing (Text.pack "Kamahl's Temper") (Text.pack "Name: Kamahl's Temper\n1 Mountain\n") fresh
      Spec.assertEqWith s "the deck" (Builder.deck opened) (Map.singleton mountain 1)
      Spec.assertEqWith s "no notice" (Builder.notices opened) []
      Spec.assertEqWith s "the pool" (fmap fst (Builder.entriesOf [Text.pack "Name: Kamahl's Temper", Text.pack "1 Mountain"])) [mountain]

    Spec.it s "drops a line of no copies" $ do
      Spec.assertEq s (Builder.deck (Builder.open Nothing Text.empty (Text.pack "0 Island\n1 Mountain\n") fresh)) (Map.singleton mountain 1)

    Spec.it s "notes a line it cannot read" $ do
      Spec.assertEq s (Builder.notices (Builder.open Nothing Text.empty (Text.pack "4\n1 Mountain\n") fresh)) [Text.pack "Couldn't read this line, which saving drops: 4"]

    Spec.it s "renames an opened deck's cards as the index names them, with nothing to save" $ do
      let renamed = Builder.withIndex (Maybe.fromMaybe Seq.empty (Builder.index indexed)) (Builder.open Nothing Text.empty (Text.pack "4 lightning bolt\n") fresh)
      Spec.assertEq s (Builder.deck renamed, Builder.dirty renamed) (Map.singleton bolt 4, False)

  Spec.describe s "review" $ do
    Spec.it s "CR 207.5 keeps each line's choice when a card's copies circle different colours" $ do
      let text = Text.pack "2 Cryptic Spires {circled: W U}\n1 Cryptic Spires {circled: B R}\n"
          opened = Builder.open Nothing Text.empty text indexed
      Spec.assertEq s (Builder.listText opened, length (Builder.notices opened)) (text, 1)

    Spec.it s "offers a choice for a pool's unused card that makes one" $ do
      let shown = page (pooled "1 Cryptic Spires\n")
      Spec.assertBool s (Text.isInfixOf (Text.pack "Circle two colours") shown) "offered"

    Spec.it s "folds the sideboard into a held pool when the format turns Limited" $ do
      let held = Builder.update Builder.MkFormat Text.empty (pooled "1 Shock\n")
          sided = tap (Builder.MkAdd DeckList.Side bolt) (tap (Builder.MkTarget DeckList.Side) held)
          back = Builder.update Builder.MkFormat (Text.pack "limited") sided
      Spec.assertEq s (Builder.target back, Builder.listText back) (DeckList.Main, Text.pack "Format: limited\n\nSideboard\n1 Lightning Bolt\n1 Shock\n")

    Spec.it s "takes a later Format header over an empty one" $ do
      Spec.assertEq s (Builder.format (Builder.open Nothing Text.empty (Text.pack "Format:\nFormat: modern\n1 Island\n") fresh)) (Text.pack "modern")

    Spec.it s "CR 903.4b replaces a commander's chosen colour" $ do
      let piper = (card "The Prismatic Piper") {CardIndex.choices = Set.singleton (DeckList.Commander, DeckList.CommanderColor)}
          b = tap (Builder.MkAdd DeckList.Commander (named "The Prismatic Piper")) (Builder.withIndex (Seq.singleton piper) fresh)
          picked = tap (Builder.MkChoose DeckList.Commander (named "The Prismatic Piper") Color.Green) (tap (Builder.MkChoose DeckList.Commander (named "The Prismatic Piper") Color.Red) b)
      Spec.assertEq s (Builder.chosenFor DeckList.Commander (named "The Prismatic Piper") picked) (Set.singleton Color.Green)

    Spec.it s "saves a pasted pool before any card is in the deck" $ do
      Spec.assertEq s (Builder.saveTarget Seq.empty (Builder.update Builder.MkSaveName (Text.pack "Sealed") (pooled "1 Shock\n"))) (Right (Text.pack "saved/sealed"))

    Spec.it s "drops a choice a card does not make, saying so" $ do
      let renamed = Builder.withIndex (Maybe.fromMaybe Seq.empty (Builder.index indexed)) (Builder.open Nothing Text.empty (Text.pack "1 Lightning Bolt {circled: W U}\n") fresh)
      Spec.assertEq s (Builder.listText renamed, length (Builder.notices renamed)) (Text.pack "1 Lightning Bolt\n", 1)

    Spec.it s "keeps a bare copy bare when spellings merge" $ do
      let renamed = Builder.withIndex (Maybe.fromMaybe Seq.empty (Builder.index indexed)) (Builder.open Nothing Text.empty (Text.pack "2 Lightning Bolt (M10) 146\n1 lightning bolt\n") fresh)
      Spec.assertEq s (Builder.listText renamed) (Text.pack "2 Lightning Bolt (M10) 146\n1 Lightning Bolt\n")

  Spec.describe s "zones" $ do
    Spec.it s "opens a commander and a sideboard into zones, and edits them" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "Format: commander\nCommander\n1 Sai, Master Thopterist\nDeck\n30 Island\nSideboard\n2 Shock\n") indexed
          edited = tap (Builder.MkAdd DeckList.Side (named "Shock")) (tap (Builder.MkRemove DeckList.Commander (named "Sai, Master Thopterist")) opened)
      Spec.assertEq s (Builder.zoneOf DeckList.Commander opened, Builder.zoneOf DeckList.Side opened) (Map.singleton (named "Sai, Master Thopterist") 1, Map.singleton (named "Shock") 2)
      Spec.assertEq s (Builder.listText edited) (Text.pack "Format: commander\n30 Island\n\nSideboard\n3 Shock\n")

    Spec.it s "writes a commander first, under its own header" $ do
      let built = tap (Builder.MkAdd DeckList.Main mountain) (tap (Builder.MkAdd DeckList.Commander bolt) fresh)
      Spec.assertEq s (Builder.listText built) (Text.pack "Commander\n1 Lightning Bolt\n\nDeck\n1 Mountain\n")

    Spec.it s "adds a search result to the zone chosen" $ do
      let chosen = tap (Builder.MkTarget DeckList.Side) indexed
          shown = Builder.view chosen
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Add Lightning Bolt") shown >>= \path -> Html.handler (Text.pack "click") path shown) (Just (Builder.MkAdd DeckList.Side bolt))

    Spec.it s "says which zone its counts are of: the header the deck's, a result's badge the zone chosen" $ do
      let built = tap (Builder.MkAdd DeckList.Side bolt) (tap (Builder.MkTarget DeckList.Side) (tap (Builder.MkAdd DeckList.Main bolt) indexed))
      Spec.assertBool s (Text.isInfixOf (Text.pack "<h2>1 card in the deck</h2>") (page built)) "header"
      Spec.assertBool s (Text.isInfixOf (Text.pack "class=\"copies\">1 in sideboard<") (page built)) "badge"

    Spec.it s "a sideboard edit is a change to save" $ do
      Spec.assertEq s (Builder.dirty (tap (Builder.MkAdd DeckList.Side bolt) fresh)) True

    Spec.it s "CR 901.3 opens a planar deck into its zone, edits it, and writes it back" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "1 Island\n\nPlanar Deck\n1 Akoum\n1 Chaotic Aether\n") planechase
          edited = tap (Builder.MkAdd Builder.planar (named "Bant")) (tap (Builder.MkRemove Builder.planar (named "Chaotic Aether")) opened)
      Spec.assertEq s (Builder.zoneOf Builder.planar opened, Builder.rest opened) (Map.fromList [(named "Akoum", 1), (named "Chaotic Aether", 1)], [])
      Spec.assertEq s (Builder.listText edited, Builder.dirty edited) (Text.pack "1 Island\n\nPlanar Deck\n1 Akoum\n1 Bant\n", True)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Planar deck \x00b7 2") (page edited)) "shown"

    Spec.it s "CR 901.3 holds one card of a name in a planar deck" $ do
      let once = tap (Builder.MkAdd Builder.planar (named "Akoum")) (tap (Builder.MkTarget Builder.planar) planechase)
          shown = Builder.view once
      Spec.assertEq s (Builder.zoneOf Builder.planar (tap (Builder.MkAdd Builder.planar (named "Akoum")) once)) (Map.singleton (named "Akoum") 1)
      Spec.assertEq s (fmap (Text.isInfixOf (Text.pack "disabled") . Html.toText) (Html.labelled (Text.pack "button") (Text.pack "Add Akoum") shown >>= (`Html.at` shown))) (Just True)

    Spec.it s "CR 901.3 searches a planar deck's cards for it, and only for it" $ do
      let offered builder = [card' | card' <- ["Akoum", "Chaotic Aether", "Lightning Bolt", "Mountain"], Maybe.isJust (Html.labelled (Text.pack "button") (Text.pack ("Add " <> card')) (Builder.view builder))]
      Spec.assertEq s (offered (tap (Builder.MkTarget Builder.planar) planechase), offered planechase) (["Akoum", "Chaotic Aether"], ["Lightning Bolt", "Mountain"])

  Spec.describe s "choices" $ do
    Spec.it s "CR 207.5 reads a circled choice into colours, and writes it back" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "2 Cryptic Spires {circled: U B}\n") indexed
      Spec.assertEq s (Builder.chosenFor DeckList.Main spires opened, Builder.listText opened) (Set.fromList [Color.Blue, Color.Black], Text.pack "2 Cryptic Spires {circled: U B}\n")

    Spec.it s "CR 207.5 toggles a circled colour" $ do
      let picked = tap (Builder.MkChoose DeckList.Main spires Color.Red) (tap (Builder.MkChoose DeckList.Main spires Color.Green) (tap (Builder.MkAdd DeckList.Main spires) indexed))
      Spec.assertEq s (Builder.listText picked) (Text.pack "1 Cryptic Spires {circled: R G}\n")

    Spec.it s "offers the colours to circle on a card that circles them" $ do
      let shown = page (tap (Builder.MkAdd DeckList.Main spires) indexed)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Circle two colours") shown) "offered"

  Spec.describe s "polish" $ do
    Spec.it s "refuses to save an empty deck" $ do
      Spec.assertEq s (Builder.saveTarget Seq.empty (Builder.update Builder.MkSaveName (Text.pack "Empty") fresh)) (Left [Text.pack "Add some cards before saving."])

    Spec.it s "sums the deck's colours" $ do
      let red = (card "Lightning Bolt") {CardIndex.colours = Set.singleton Color.Red}
          built = tap (Builder.MkAdd DeckList.Main bolt) (Builder.withIndex (Seq.singleton red) fresh)
      Spec.assertEq s (Builder.deckColours built) (Set.singleton Color.Red)

    Spec.it s "shows why the cards or rules did not load" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "Couldn't load the rules.") (page indexed {Builder.loadProblem = Just (Text.pack "Couldn't load the rules.")})) "shown"

  Spec.describe s "saveTarget" $ do
    Spec.it s "a first save takes the typed name" $ do
      Spec.assertEq s (Builder.saveTarget Seq.empty (Builder.update Builder.MkSaveName (Text.pack "Red Burn") (tap (Builder.MkAdd DeckList.Main bolt) fresh))) (Right (Text.pack "saved/red-burn"))

    Spec.it s "refuses a first save under a saved deck's name" $ do
      Spec.assertEq s (Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) (Builder.update Builder.MkSaveName (Text.pack "Burn") (tap (Builder.MkAdd DeckList.Main bolt) fresh))) (Left [Text.pack "A saved deck is already named that."])

    Spec.it s "later saves keep the name saved under" $ do
      let again = Builder.update Builder.MkSaveName (Text.pack "Other") (Builder.saved (Text.pack "saved/burn") (tap (Builder.MkAdd DeckList.Main bolt) fresh))
      Spec.assertEq s (Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) again) (Right (Text.pack "saved/burn"))
  where
    page = Html.toText . Builder.view
    card name = CardIndex.MkEntry (CardName.MkCardName (Text.pack name)) [Text.pack name] Nothing 1 Set.empty Set.empty (Text.pack "Instant") [] (Text.toCaseFold (Text.pack name)) False Set.empty
    spires = named "Cryptic Spires"
    indexed = Builder.withIndex (Seq.fromList [(card "Cryptic Spires") {CardIndex.choices = Set.fromList [(section, DeckList.CircledColors) | section <- [DeckList.Main, DeckList.Side, DeckList.Commander]]}, (card "Lightning Bolt") {CardIndex.rules = [(Text.pack "Lightning Bolt", [Text.pack "Lightning Bolt deals 3 damage to any target."])]}, (card "Mountain") {CardIndex.basic = True}, card "Shock", (card "Front Face") {CardIndex.faceNames = [Text.pack "Front Face", Text.pack "Back Face"]}]) fresh
    planechase = Builder.withIndex (Seq.fromList [(card "Akoum") {CardIndex.types = Set.singleton CardType.Plane}, (card "Bant") {CardIndex.types = Set.singleton CardType.Plane}, (card "Chaotic Aether") {CardIndex.types = Set.singleton CardType.Phenomenon}, card "Lightning Bolt", (card "Mountain") {CardIndex.basic = True}]) fresh
    pooled text = Builder.update Builder.MkUsePool Text.empty (Builder.update Builder.MkPoolText (Text.pack text) (Builder.update Builder.MkPoolChoice (Text.pack "paste") indexed))
    constructed = Builder.update Builder.MkFormat (Text.pack "constructed") fresh
    fresh = Builder.new [Text.pack "constructed", Text.pack "pauper"]
    tap event = Builder.update event Text.empty
    bolt = CardName.MkCardName (Text.pack "Lightning Bolt")
    mountain = CardName.MkCardName (Text.pack "Mountain")
    fireIce = CardName.MkCardName (Text.pack "Fire // Ice")
    named = CardName.MkCardName . Text.pack
