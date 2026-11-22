module Parun.Web.DecksSpec where

import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Decks as Decks
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Decks" $ do
  Spec.describe s "tabs" $ do
    Spec.it s "opens on My decks while any is saved, else on the precons" $ do
      Spec.assertEqWith s "saved" (shelf (screen Nothing)) DeckBrowser.MkSaved
      Spec.assertEqWith s "none saved" (shelf (Decks.withDecks (Seq.fromList [webs, angels]) Map.empty Map.empty (Decks.new Nothing))) DeckBrowser.MkPrecons
      Spec.assertEqWith s "a precon selected" (shelf (screen (Just webs))) DeckBrowser.MkPrecons

    Spec.it s "keeps the tab chosen when the decks are read again" $ do
      let precons = Decks.update (Decks.MkBrowse (DeckBrowser.MkShelf DeckBrowser.MkPrecons)) Text.empty (screen Nothing)
      Spec.assertEq s (shelf (Decks.withDecks (Seq.fromList [webs, angels, mine, other]) Map.empty Map.empty precons)) DeckBrowser.MkPrecons

    Spec.it s "puts My decks first and switches tab on a tap" $ do
      let page = Decks.view (screen Nothing)
          tabs = Text.splitOn (Text.pack "role=\"tab\"") (Html.toText page)
      Spec.assertEqWith s "order" (fmap (Text.isInfixOf (Text.pack "My decks")) (drop 1 tabs)) [True, False]
      Spec.assertEqWith s "tap" (tapping "Precons" (screen Nothing)) (Just (Decks.MkBrowse (DeckBrowser.MkShelf DeckBrowser.MkPrecons)))

  Spec.describe s "filters" $ do
    Spec.it s "lists the decks matching what is typed, the colours toggled and the set chosen" $ do
      let precons = Decks.update (Decks.MkBrowse (DeckBrowser.MkShelf DeckBrowser.MkPrecons)) Text.empty (screen Nothing)
          typed = Decks.update (Decks.MkBrowse DeckBrowser.MkSearch) (Text.pack "webs") precons
          white = Decks.update (Decks.MkBrowse (DeckBrowser.MkColour Color.White)) Text.empty precons
          fdc = Decks.update (Decks.MkBrowse DeckBrowser.MkSet) (Text.pack "FDC") precons
      Spec.assertEqWith s "typed" (has "Entangling Webs" typed, has "Calling All Angels" typed) (True, False)
      Spec.assertEqWith s "colour" (has "Entangling Webs" white, has "Calling All Angels" white) (False, True)
      Spec.assertEqWith s "set" (has "Entangling Webs" fdc, has "Calling All Angels" fdc) (False, True)

  Spec.describe s "selection" $ do
    Spec.it s "selects a deck on a tap, pressed, and details it with its list" $ do
      let selected = Decks.update (Decks.MkSelect mine) Text.empty (screen Nothing)
          page = Decks.view (Decks.browsed (\b -> b {DeckBrowser.previews = Map.singleton mine [DeckBrowser.MkLine 4 (CardName.MkCardName (Text.pack "Shock")) Nothing 1 DeckBrowser.MkSpell]}) selected)
      Spec.assertEqWith s "the tap" (Html.search (tileOf (Text.pack "My Deck")) (Decks.view (screen Nothing)) >>= \path -> Html.handler (Text.pack "click") path (Decks.view (screen Nothing))) (Just (Decks.MkSelect mine))
      Spec.assertEqWith s "pressed" (Maybe.isJust (Html.search (\node -> tileOf (Text.pack "My Deck") node && Text.isInfixOf (Text.pack "aria-pressed=\"true\"") (Html.toText node)) page)) True
      Spec.assertEqWith s "detailed" (Text.isInfixOf (Text.pack "<h2>My Deck</h2>") (Html.toText page) && Text.isInfixOf (Text.pack "See all 4 cards") (Html.toText page)) True
      Spec.assertEqWith s "nothing selected" (has "Pick a deck to see it here." (screen Nothing)) True

    Spec.it s "closes the detail" $ do
      Spec.assertEq s (Decks.selected (Decks.update Decks.MkClose Text.empty (screen (Just mine)))) Nothing

    Spec.it s "drops a selected deck no longer held" $ do
      Spec.assertEq s (Decks.selected (Decks.withDecks (Seq.fromList [webs]) Map.empty Map.empty (screen (Just mine)))) Nothing

  Spec.describe s "actions" $ do
    Spec.it s "offers a saved deck's Play, Edit, Rename, Duplicate, Export and Delete" $ do
      let shown' = screen (Just mine)
      Spec.assertEq s (fmap (`tapping` shown') ["Play", "Edit", "Rename", "Duplicate", "Export", "Delete"]) (fmap Just [Decks.MkPlay mine, Decks.MkEdit mine, Decks.MkRename, Decks.MkDuplicate mine, Decks.MkBrowse (DeckBrowser.MkExport mine), Decks.MkAskDelete mine])

    Spec.it s "offers a precon's Play, Edit a copy and Export, and nothing to rename or delete" $ do
      let shown' = screen (Just webs)
      Spec.assertEqWith s "offered" (fmap (`tapping` shown') ["Play", "Edit a copy", "Export"]) (fmap Just [Decks.MkPlay webs, Decks.MkEdit webs, Decks.MkBrowse (DeckBrowser.MkExport webs)])
      Spec.assertEqWith s "not offered" (fmap (`tapping` shown') ["Rename", "Duplicate", "Delete"]) [Nothing, Nothing, Nothing]

    Spec.it s "offers New deck and Import a deck" $ do
      Spec.assertEq s (fmap (`tapping` screen Nothing) ["New deck", "Import a deck"]) [Just Decks.MkNewDeck, Just (Decks.MkBrowse DeckBrowser.MkOpenImport)]

    Spec.it s "shows a deck's list to copy under it" $ do
      let exported = Decks.browsed (\b -> b {DeckBrowser.exporting = Just (mine, Text.pack "4 Exported Card")}) (screen (Just mine))
      Spec.assertBool s (has "4 Exported Card" exported) "its own"
      Spec.assertBool s (not (has "4 Exported Card" (Decks.update (Decks.MkSelect other) Text.empty exported))) "not another's"

  Spec.describe s "delete" $ do
    Spec.it s "asks before deleting, and deletes only once confirmed" $ do
      let asked = Decks.update (Decks.MkAskDelete mine) Text.empty (screen (Just mine))
      Spec.assertEqWith s "the question" (has "from this browser? This cannot be undone." asked) True
      Spec.assertEqWith s "not before" (has "from this browser?" (screen (Just mine))) False
      Spec.assertEqWith s "confirmed" (tapping "Delete" asked) (Just (Decks.MkBrowse (DeckBrowser.MkDelete mine)))
      Spec.assertEqWith s "cancelled" (Decks.deleting (Decks.update Decks.MkCancelDelete Text.empty asked)) Nothing
      Spec.assertEqWith s "cancel offered" (tapping "Cancel" asked) (Just Decks.MkCancelDelete)

    Spec.it s "drops the deck deleted, and its selection" $ do
      let gone = Decks.deleted mine (Decks.update (Decks.MkAskDelete mine) Text.empty (screen (Just mine)))
      Spec.assertEq s (Decks.selected gone, Decks.deleting gone, DeckBrowser.decks (Decks.browser gone)) (Nothing, Nothing, Seq.fromList [webs, angels, other])

  Spec.describe s "empty" $ do
    Spec.it s "says no deck is saved yet, with New deck and Import a deck" $ do
      let none = Decks.update (Decks.MkBrowse (DeckBrowser.MkShelf DeckBrowser.MkSaved)) Text.empty (Decks.withDecks (Seq.fromList [webs]) Map.empty Map.empty (Decks.new Nothing))
          shelfPage = Maybe.fromMaybe (Html.text Text.empty) (Html.search (Text.isPrefixOf (Text.pack "<div class=\"empty-shelf\"") . Html.toText) (Decks.view none) >>= (`Html.at` Decks.view none))
      Spec.assertEqWith s "the line" (has "No decks saved in this browser yet." none) True
      Spec.assertEqWith s "its buttons" (fmap (\label -> Html.labelled (Text.pack "button") (Text.pack label) shelfPage >>= \path -> Html.handler (Text.pack "click") path shelfPage) ["New deck", "Import a deck"]) [Just Decks.MkNewDeck, Just (Decks.MkBrowse DeckBrowser.MkOpenImport)]
      Spec.assertEqWith s "not while one is saved" (has "No decks saved in this browser yet." (screen Nothing)) False

  Spec.describe s "rename" $ do
    Spec.it s "opens on the deck's name and renames it to the name typed" $ do
      let opened = Decks.update Decks.MkRename Text.empty (screen (Just mine))
          typed = Decks.update Decks.MkRenameName (Text.pack "Red Deck Wins") opened
      Spec.assertEqWith s "its name" (fmap fst (Decks.renaming opened)) (Just (Text.pack "My Deck"))
      Spec.assertEqWith s "renamed" (Decks.renameTarget typed) (Right (mine, Text.pack "saved/red-deck-wins"))
      Spec.assertEqWith s "its own name" (Decks.renameTarget opened) (Right (mine, mine))
      Spec.assertEqWith s "submitted" (tapping "Rename" opened) (Just Decks.MkSubmitRename)

    Spec.it s "refuses another saved deck's name and an empty one, saying why" $ do
      let opened = Decks.update Decks.MkRename Text.empty (screen (Just mine))
          clash = Decks.update Decks.MkRenameName (Text.pack "Other") opened
          refusal = Either.fromLeft [] (Decks.renameTarget clash)
      Spec.assertEqWith s "clash" refusal [Text.pack "A saved deck is already named that."]
      Spec.assertEqWith s "empty" (Decks.renameTarget (Decks.update Decks.MkRenameName (Text.pack "!!") opened)) (Left [Text.pack "Give the deck a name."])
      Spec.assertEqWith s "said" (has "A saved deck is already named that." (Decks.refused refusal clash)) True

    Spec.it s "selects the deck saved under its new name, the box closed" $ do
      let done = Decks.savedAs (Text.pack "saved/red-deck-wins") (Decks.update Decks.MkRename Text.empty (screen (Just mine)))
      Spec.assertEq s (Decks.selected done, Decks.renaming done) (Just (Text.pack "saved/red-deck-wins"), Nothing)

  Spec.describe s "copyName" $ do
    Spec.it s "names a copy after its deck, numbered past those taken" $ do
      let held = Seq.fromList [mine, Text.pack "saved/my-deck-copy"]
      Spec.assertEqWith s "first" (Decks.copyName (Seq.singleton mine) mine) (Text.pack "saved/my-deck-copy")
      Spec.assertEqWith s "second" (Decks.copyName held mine) (Text.pack "saved/my-deck-copy-2")

    Spec.it s "names a copy as its deck is named, then what its file adds" $ do
      let named = Map.singleton mine (Text.pack "Tiny <b>Bolt</b>")
      Spec.assertEqWith s "headed" (Decks.copyTitle named mine (Text.pack "saved/my-deck-copy-2")) (Text.pack "Tiny <b>Bolt</b> Copy 2")
      Spec.assertEqWith s "bare" (Decks.copyTitle Map.empty mine (Text.pack "saved/my-deck-copy")) (Text.pack "My Deck Copy")

  Spec.describe s "returning" $ do
    Spec.it s "selects the deck the builder saved on coming back, else the one it opened" $ do
      Spec.assertEqWith s "unsaved" (Decks.returning (Just webs) (Builder.new [])) (Just webs)
      Spec.assertEqWith s "saved" (Decks.returning (Just webs) (Builder.saved (Text.pack "saved/webs") (Builder.new []))) (Just (Text.pack "saved/webs"))

    Spec.it s "selects an imported deck on My decks, the panel closed" $ do
      let opened = Decks.update (Decks.MkBrowse DeckBrowser.MkOpenImport) Text.empty (Decks.update (Decks.MkBrowse (DeckBrowser.MkShelf DeckBrowser.MkPrecons)) Text.empty (screen Nothing))
          done = Decks.savedAs other opened
      Spec.assertEq s (Decks.selected done, shelf done, DeckBrowser.importing (Decks.browser done)) (Just other, DeckBrowser.MkSaved, Nothing)
  where
    has piece shown' = Text.isInfixOf (Text.pack piece) (Html.toText (Decks.view shown'))
    tapping label shown' =
      let page = Decks.view shown'
       in Html.labelled (Text.pack "button") (Text.pack label) page >>= \path -> Html.handler (Text.pack "click") path page

webs :: Text.Text
webs = Text.pack "sets/m12/entangling-webs"

angels :: Text.Text
angels = Text.pack "sets/fdc/calling-all-angels"

mine :: Text.Text
mine = Text.pack "saved/my-deck"

other :: Text.Text
other = Text.pack "saved/other"

-- | The screen with two precons and two saved decks read, this one
-- selected: webs red and green, angels white.
screen :: Maybe Text.Text -> Decks.Decks
screen chosen =
  Decks.browsed (\b -> b {DeckBrowser.palette = Map.fromList [(webs, Set.fromList [Color.Red, Color.Green]), (angels, Set.singleton Color.White)]}) $
    Decks.withDecks (Seq.fromList [webs, angels, mine, other]) Map.empty Map.empty (Decks.new chosen)

-- | The shelf shown.
shelf :: Decks.Decks -> DeckBrowser.Shelf
shelf = DeckBrowser.shelf . Decks.browser

-- | Whether an element is the deck tile naming this deck.
tileOf :: Text.Text -> Html.Html msg -> Bool
tileOf name html = Text.isPrefixOf (Text.pack "<button class=\"deck") (Html.toText html) && Text.isInfixOf name (Html.content html)
