module Parun.Web.BuilderSpec where

import qualified Data.Map.Strict as Map
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
    Spec.it s "writes the format header, then the main deck by name" $ do
      let built = (tap (Builder.MkAdd bolt) . tap (Builder.MkAdd mountain) . tap (Builder.MkAdd bolt)) fresh
      Spec.assertEq s (Builder.listText built) (Text.pack "Format: constructed\n2 Lightning Bolt\n1 Mountain\n")

    Spec.it s "writes back the rest of a list untouched" $ do
      let opened = fresh {Builder.deck = Map.singleton bolt 4, Builder.suffixes = Map.singleton bolt (Text.pack " (M10) 146"), Builder.rest = [Text.pack "Sideboard", Text.pack "2 Shock"]}
      Spec.assertEq s (Builder.listText opened) (Text.pack "Format: constructed\n4 Lightning Bolt (M10) 146\nSideboard\n2 Shock\n")

    Spec.it s "parses back to the same deck, a split card included" $ do
      let built = fresh {Builder.deck = Map.fromList [(bolt, 4), (fireIce, 2)]}
      Spec.assertEq s (Map.fromList [(name, n) | (n, name) <- Lobby.entries (Builder.listText built)]) (Builder.deck built)

  Spec.describe s "view" $ do
    Spec.it s "shows the count against the format's minimum" $ do
      let built = indexed {Builder.deck = Map.fromList [(bolt, 4), (mountain, 39)], Builder.least = Just 60}
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
    card name = CardIndex.MkEntry (CardName.MkCardName (Text.pack name)) [Text.pack name] Nothing 1 Set.empty Set.empty (Text.pack "Instant") [] (Text.toCaseFold (Text.pack name))
    indexed = fresh {Builder.index = Just (Seq.fromList [(card "Lightning Bolt") {CardIndex.rules = [Text.pack "Lightning Bolt deals 3 damage to any target."]}, card "Mountain"])}
    fresh = Builder.new [Text.pack "constructed", Text.pack "pauper"]
    tap event = Builder.update event Text.empty
    bolt = CardName.MkCardName (Text.pack "Lightning Bolt")
    mountain = CardName.MkCardName (Text.pack "Mountain")
    fireIce = CardName.MkCardName (Text.pack "Fire // Ice")
