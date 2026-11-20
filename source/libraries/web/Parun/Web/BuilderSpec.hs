module Parun.Web.BuilderSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.CardIndex as CardIndex
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Builder" $ do
  Spec.describe s "update" $ do
    Spec.it s "adds and removes a copy at a time, dropping a card at none" $ do
      let added = tap (Builder.MkAdd bolt) (tap (Builder.MkAdd bolt) fresh)
          once = tap (Builder.MkRemove bolt) added
          none = tap (Builder.MkRemove bolt) once
      Spec.assertEq s (fmap Builder.deck [added, once, none]) [Map.singleton bolt 2, Map.singleton bolt 1, Map.empty]

    Spec.it s "an edit marks the deck changed and a save clears it" $ do
      let added = tap (Builder.MkAdd bolt) fresh
      Spec.assertEq s (Builder.dirty fresh, Builder.dirty added, Builder.dirty (Builder.saved (Text.pack "saved/burn") added)) (False, True, False)

    Spec.it s "toggling a colour twice clears it" $ do
      let once = tap (Builder.MkToggleColour Color.Red) fresh
      Spec.assertEq s (CardIndex.wanted (Builder.query once), CardIndex.wanted (Builder.query (tap (Builder.MkToggleColour Color.Red) once))) (Set.singleton Color.Red, Set.empty)

    Spec.it s "Back with changes asks first; Stay closes the question" $ do
      let asked = tap Builder.MkBack (tap (Builder.MkAdd bolt) fresh)
      Spec.assertEq s (Builder.leaving (tap Builder.MkBack fresh), Builder.leaving asked, Builder.leaving (tap Builder.MkStay asked)) (False, True, False)

    Spec.it s "takes the format chosen, which counts as a change" $ do
      let chosen = Builder.update Builder.MkFormat (Text.pack "pauper") fresh
      Spec.assertEq s (Builder.format chosen, Builder.dirty chosen) (Text.pack "pauper", True)

  Spec.describe s "checked" $ do
    Spec.it s "holds the check's problems and the cards they name" $ do
      let found = Builder.checked [Text.pack "5 copies"] (Set.singleton bolt) fresh
      Spec.assertEq s (Builder.problems found, Builder.flagged found) ([Text.pack "5 copies"], Set.singleton bolt)

  Spec.describe s "listText" $ do
    Spec.it s "writes no header for a casual deck, which names no format" $ do
      Spec.assertEq s (Builder.listText (tap (Builder.MkAdd bolt) fresh)) (Text.pack "1 Lightning Bolt\n")

    Spec.it s "offers casual first, and chooses it" $ do
      let shown = Html.toText (Builder.view fresh)
      Spec.assertBool s (Text.isInfixOf (Text.pack "<option value=\"\" selected=\"\">Casual (no format)</option>") shown) "casual chosen"

    Spec.it s "writes the format header, then the main deck by name" $ do
      let built = (tap (Builder.MkAdd bolt) . tap (Builder.MkAdd mountain) . tap (Builder.MkAdd bolt)) constructed
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: constructed\n2 Lightning Bolt\n1 Mountain\n")

    Spec.it s "writes back the rest of a list untouched" $ do
      let opened = constructed {Builder.deck = Map.singleton bolt 4, Builder.suffixes = Map.singleton bolt [(Text.pack " (M10) 146", 4)], Builder.rest = [Text.pack "Sideboard", Text.pack "2 Shock"]}
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

    Spec.it s "says no card matches" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "No card matches.") (page (Builder.update Builder.MkSearch (Text.pack "zzzz") indexed))) "none"

    Spec.it s "says the cards are being prepared" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "Preparing the cards") (page fresh)) "preparing"

    Spec.it s "offers the five basic lands whatever the search" $ do
      let shown = Builder.view (Builder.update Builder.MkSearch (Text.pack "zzzz") indexed)
          tapped name = Html.labelled (Text.pack "button") (Text.pack name) shown >>= \path -> Html.handler (Text.pack "click") path shown
      Spec.assertEq s (fmap tapped ["Plains", "Island", "Swamp", "Mountain", "Forest"]) (fmap (Just . Builder.MkAdd . CardName.MkCardName . Text.pack) ["Plains", "Island", "Swamp", "Mountain", "Forest"])

    Spec.it s "adds a result when its name's button is tapped" $ do
      let shown = Builder.view indexed
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Add Lightning Bolt") shown >>= \path -> Html.handler (Text.pack "click") path shown) (Just (Builder.MkAdd bolt))

    Spec.it s "shows the rules text of the card being read" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "deals 3 damage") (page (Builder.update (Builder.MkRead bolt) Text.empty indexed))) "rules"

  Spec.describe s "open" $ do
    Spec.it s "opens a commander precon and writes its commander back" $ do
      let text = "Format: commander\n\nCommander\n1 Sai, Master Thopterist\n\nDeck\n1 Ornithopter\n30 Island\n"
          opened = Builder.open Nothing (Text.pack "Sai") (Text.pack text) fresh
      Spec.assertEq s (Builder.format opened, Builder.deck opened) (Text.pack "commander", Map.fromList [(named "Ornithopter", 1), (named "Island", 30)])
      Spec.assertEq s (Builder.listText opened) (Text.pack "Format: commander\n30 Island\n1 Ornithopter\n\nCommander\n1 Sai, Master Thopterist\n")

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
      Spec.assertEq s (Builder.listText (tap (Builder.MkRemove mountain) opened), Builder.listText (tap (Builder.MkAdd mountain) opened)) (Text.pack "2 Mountain (ZEN) 246\n1 Mountain (UNH) 140\n", Text.pack "3 Mountain (ZEN) 246\n2 Mountain (UNH) 140\n")

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
      Spec.assertEq s (Builder.dirty (tap (Builder.MkRemove bolt) (tap (Builder.MkAdd bolt) fresh))) False

  Spec.describe s "pool" $ do
    Spec.it s "CR 100.2b caps a pooled card at its count" $ do
      let added = iterate (tap (Builder.MkAdd bolt)) (pooled "2 Lightning Bolt\n") !! 3
      Spec.assertEq s (Builder.deck added) (Map.singleton bolt 2)

    Spec.it s "CR 100.2b never caps a basic land" $ do
      Spec.assertEq s (Builder.deck (tap (Builder.MkAdd mountain) (pooled "2 Lightning Bolt\n"))) (Map.singleton mountain 1)

    Spec.it s "CR 100.4b chooses Limited and writes the pool's unused cards as the Sideboard" $ do
      let built = tap (Builder.MkAdd bolt) (pooled "2 Lightning Bolt\n1 Shock\n")
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: limited\n1 Lightning Bolt\n\nSideboard\n1 Lightning Bolt\n1 Shock\n")

    Spec.it s "returns a removed card to the Sideboard" $ do
      let built = tap (Builder.MkRemove bolt) (tap (Builder.MkAdd bolt) (pooled "2 Lightning Bolt\n"))
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: limited\n\nSideboard\n2 Lightning Bolt\n")

    Spec.it s "keeps the pool, and its caps, when the search widens to every card" $ do
      let widened = Builder.update Builder.MkPoolChoice Text.empty (pooled "2 Lightning Bolt\n")
          more = iterate (tap (Builder.MkAdd bolt)) widened !! 3
      Spec.assertEq s (CardIndex.among (Builder.searchQuery widened), Builder.listText more) (Nothing, Text.pack "Format: limited\n2 Lightning Bolt\n")

    Spec.it s "drops the pool, its caps and its Sideboard when asked" $ do
      let dropped = tap Builder.MkDropPool (pooled "1 Lightning Bolt\n")
      Spec.assertEq s (Builder.listText (iterate (tap (Builder.MkAdd bolt)) dropped !! 2)) (Text.pack "Format: limited\n2 Lightning Bolt\n")

    Spec.it s "resolves a pasted pool's names to the cards' own, whatever their case" $ do
      Spec.assertEq s (Builder.listText (tap (Builder.MkAdd bolt) (pooled "3 lightning bolt\n"))) (Text.pack "Format: limited\n1 Lightning Bolt\n\nSideboard\n2 Lightning Bolt\n")

    Spec.it s "refuses a pool naming a card it doesn't know, saying which" $ do
      let refused = pooled "1 Lightnin Bolt\n2 Shock\n"
      Spec.assertEq s (Builder.pool refused, Builder.poolProblems refused) (Nothing, [Text.pack "No card is named Lightnin Bolt."])

    Spec.it s "refuses a pool naming no cards" $ do
      Spec.assertEq s (Builder.poolProblems (pooled "\n")) [Text.pack "The pool names no cards."]

    Spec.it s "writes the pool's Sideboard only under Limited" $ do
      Spec.assertEq s (Builder.listText (Builder.update Builder.MkFormat (Text.pack "constructed") (pooled "2 Shock\n"))) (Text.pack "Format: constructed\n")

    Spec.it s "leaves basic lands out of the pool's Sideboard" $ do
      let opened = Builder.open Nothing Text.empty (Text.pack "Format: limited\n9 Mountain\n\nSideboard\n1 Shock\n") indexed
      Spec.assertEq s (Builder.listText (iterate (tap (Builder.MkRemove mountain)) opened !! 9)) (Text.pack "Format: limited\n\nSideboard\n1 Shock\n")

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
      Spec.assertEq s (Builder.listText opened, Builder.deck (iterate (tap (Builder.MkAdd bolt)) opened !! 2)) (text, Map.singleton bolt 2)

    Spec.it s "finds a double-faced card a set's list names by both faces" $ do
      let scoped = Builder.setPool (Text.pack "neo") [named "Front Face // Back Face"] indexed
      Spec.assertEq s (fmap (CardName.unwrap . CardIndex.name) (Foldable.toList (snd (CardIndex.search (Builder.searchQuery scoped) (Maybe.fromMaybe Seq.empty (Builder.index scoped)))))) [Text.pack "Front Face", Text.pack "Mountain"]

    Spec.it s "a set's cards narrow the search without capping" $ do
      let scoped = Builder.setPool (Text.pack "m10") [bolt] indexed
      Spec.assertEq s (CardIndex.among (Builder.searchQuery scoped), Builder.deck (iterate (tap (Builder.MkAdd bolt)) scoped !! 5)) (Just (Set.singleton (Text.pack "lightning bolt")), Map.singleton bolt 5)

    Spec.it s "offers every card, each set, and pasting a pool" $ do
      let shown = page indexed {Builder.sets = [Text.pack "m10"]}
      Spec.assertBool s (all (`Text.isInfixOf` shown) [Text.pack "<option value=\"\" selected=\"\">Every card</option>", Text.pack "<option value=\"set:m10\">M10</option>", Text.pack "<option value=\"paste\">Paste a pool"]) "options"

    Spec.it s "uses the pool pasted into its panel" $ do
      let shown = Builder.view (Builder.update Builder.MkPoolChoice (Text.pack "paste") indexed)
      Spec.assertEq s (Html.labelled (Text.pack "button") (Text.pack "Use this pool") shown >>= \path -> Html.handler (Text.pack "click") path shown) (Just Builder.MkUsePool)

    Spec.it s "shows how many of a pooled card are left" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "1 left") (page (tap (Builder.MkAdd bolt) (pooled "2 Lightning Bolt\n")))) "left"

  Spec.describe s "saveTarget" $ do
    Spec.it s "a first save takes the typed name" $ do
      Spec.assertEq s (Builder.saveTarget Seq.empty (Builder.update Builder.MkSaveName (Text.pack "Red Burn") fresh)) (Right (Text.pack "saved/red-burn"))

    Spec.it s "refuses a first save under a saved deck's name" $ do
      Spec.assertEq s (Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) (Builder.update Builder.MkSaveName (Text.pack "Burn") fresh)) (Left [Text.pack "A saved deck is already named that."])

    Spec.it s "later saves keep the name saved under" $ do
      let again = Builder.update Builder.MkSaveName (Text.pack "Other") (Builder.saved (Text.pack "saved/burn") fresh)
      Spec.assertEq s (Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) again) (Right (Text.pack "saved/burn"))
  where
    page = Html.toText . Builder.view
    card name = CardIndex.MkEntry (CardName.MkCardName (Text.pack name)) [Text.pack name] Nothing 1 Set.empty Set.empty (Text.pack "Instant") [] (Text.toCaseFold (Text.pack name)) False
    indexed = Builder.withIndex (Seq.fromList [(card "Lightning Bolt") {CardIndex.rules = [Text.pack "Lightning Bolt deals 3 damage to any target."]}, (card "Mountain") {CardIndex.basic = True}, card "Shock", (card "Front Face") {CardIndex.faceNames = [Text.pack "Front Face", Text.pack "Back Face"]}]) fresh
    pooled text = Builder.update Builder.MkUsePool Text.empty (Builder.update Builder.MkPoolText (Text.pack text) (Builder.update Builder.MkPoolChoice (Text.pack "paste") indexed))
    constructed = Builder.update Builder.MkFormat (Text.pack "constructed") fresh
    fresh = Builder.new [Text.pack "constructed", Text.pack "pauper"]
    tap event = Builder.update event Text.empty
    bolt = CardName.MkCardName (Text.pack "Lightning Bolt")
    mountain = CardName.MkCardName (Text.pack "Mountain")
    fireIce = CardName.MkCardName (Text.pack "Fire // Ice")
    named = CardName.MkCardName . Text.pack
