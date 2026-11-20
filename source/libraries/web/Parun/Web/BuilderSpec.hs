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

  Spec.describe s "saveTarget" $ do
    Spec.it s "a first save takes the typed name" $ do
      Spec.assertEq s (Builder.saveTarget Seq.empty (Builder.update Builder.MkSaveName (Text.pack "Red Burn") fresh)) (Right (Text.pack "saved/red-burn"))

    Spec.it s "refuses a first save under a saved deck's name" $ do
      Spec.assertEq s (Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) (Builder.update Builder.MkSaveName (Text.pack "Burn") fresh)) (Left [Text.pack "A saved deck is already named that."])

    Spec.it s "later saves keep the name saved under" $ do
      let again = Builder.update Builder.MkSaveName (Text.pack "Other") (Builder.saved (Text.pack "saved/burn") fresh)
      Spec.assertEq s (Builder.saveTarget (Seq.singleton (Text.pack "saved/burn")) again) (Right (Text.pack "saved/burn"))
  where
    fresh = Builder.new [Text.pack "constructed", Text.pack "pauper"]
    tap event = Builder.update event Text.empty
    bolt = CardName.MkCardName (Text.pack "Lightning Bolt")
    mountain = CardName.MkCardName (Text.pack "Mountain")
    fireIce = CardName.MkCardName (Text.pack "Fire // Ice")
