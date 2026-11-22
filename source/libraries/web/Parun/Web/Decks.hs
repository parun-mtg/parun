-- | The web client's Decks screen (#9430): the decks saved in this browser
-- and the shipped ones, browsed as the lobby browses them
-- (Parun.Web.DeckBrowser); the one selected detailed beside them, with the
-- ways to play, edit, rename, duplicate, export and delete it; and the ways
-- to start a new deck or import one.
module Parun.Web.Decks where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

data Decks = MkDecks
  { -- | The decks, what is read of them, and what they are filtered by.
    browser :: DeckBrowser.DeckBrowser,
    -- | The deck detailed, once a tile is tapped.
    selected :: Maybe Text.Text,
    -- | The name typed for the selected deck and why it was refused, while
    -- it is being renamed.
    renaming :: Maybe (Text.Text, [Text.Text]),
    -- | The deck Delete asks about, until confirmed or cancelled.
    deleting :: Maybe Text.Text
  }
  deriving (Eq, Show)

-- | What a tap or a keystroke on the Decks screen does.
data Event
  = -- | Something done in the deck browser.
    MkBrowse DeckBrowser.Event
  | -- | Details this deck.
    MkSelect Text.Text
  | -- | Closes the detail.
    MkClose
  | -- | Opens the builder on a blank list; the shell does it.
    MkNewDeck
  | -- | Opens the lobby with this deck on your seat; the shell does it.
    MkPlay Text.Text
  | -- | Opens this deck in the builder, a saved one to save over, any other
    -- as a copy; the shell does it.
    MkEdit Text.Text
  | -- | Opens the box the selected deck's new name is typed in.
    MkRename
  | -- | The new name typed.
    MkRenameName
  | -- | Renames the selected deck to the name typed; the shell does it.
    MkSubmitRename
  | -- | Closes the box without renaming.
    MkCancelRename
  | -- | Saves a copy of this saved deck; the shell does it.
    MkDuplicate Text.Text
  | -- | Asks whether to delete this saved deck.
    MkAskDelete Text.Text
  | -- | Keeps the deck asked about.
    MkCancelDelete
  deriving (Eq, Show)

-- | A screen whose decks are still to read, on My decks, this deck
-- selected if any.
new :: Maybe Text.Text -> Decks
new chosen = MkDecks DeckBrowser.new {DeckBrowser.shelf = DeckBrowser.MkSaved} chosen Nothing Nothing

-- | The screen with its deck browser changed.
browsed :: (DeckBrowser.DeckBrowser -> DeckBrowser.DeckBrowser) -> Decks -> Decks
browsed f screen = screen {browser = f (browser screen)}

-- | The screen holding these decks under these formats, what was typed
-- kept: on the first read, on the selected deck's shelf, else on My decks
-- where any is saved and the precons where none is; a selected deck no
-- longer held is dropped.
withDecks :: Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Decks -> Decks
withDecks names named screen =
  let held = Monad.mfilter (`Foldable.elem` names) (selected screen)
      shelf
        | not (DeckBrowser.reading (browser screen)) = DeckBrowser.shelf (browser screen)
        | Just name <- held = shelfOf name
        | any DeckBrowser.isSaved names = DeckBrowser.MkSaved
        | otherwise = DeckBrowser.MkPrecons
   in browsed
        (\b -> (DeckBrowser.withDecks names named b) {DeckBrowser.shelf = shelf})
        screen
          { selected = held,
            renaming = if Maybe.isJust held then renaming screen else Nothing,
            deleting = Monad.mfilter ((== held) . Just) (deleting screen)
          }

-- | The shelf a deck is on.
shelfOf :: Text.Text -> DeckBrowser.Shelf
shelfOf name = if DeckBrowser.isSaved name then DeckBrowser.MkSaved else DeckBrowser.MkPrecons

update :: Event -> Text.Text -> Decks -> Decks
update event typed screen = case event of
  MkBrowse e -> browsed (DeckBrowser.update e typed) screen
  MkSelect name -> screen {selected = Just name, renaming = Nothing, deleting = Nothing, browser = (browser screen) {DeckBrowser.exporting = Nothing}}
  MkClose -> screen {selected = Nothing, renaming = Nothing, deleting = Nothing}
  MkNewDeck -> screen
  MkPlay _ -> screen
  MkEdit _ -> screen
  MkRename -> screen {renaming = fmap (\name -> (snd (DeckBrowser.describe name), [])) (selected screen), deleting = Nothing}
  MkRenameName -> screen {renaming = fmap (\(_, problems) -> (typed, problems)) (renaming screen)}
  MkSubmitRename -> screen
  MkCancelRename -> screen {renaming = Nothing}
  MkDuplicate _ -> screen
  MkAskDelete name -> screen {deleting = Just name, renaming = Nothing}
  MkCancelDelete -> screen {deleting = Nothing}

-- | The selected deck and the name it is to be renamed to, or why it may
-- not be: the name is empty or another saved deck's. Its own name is no
-- clash, and leaves it as it is.
renameTarget :: Decks -> Either [Text.Text] (Text.Text, Text.Text)
renameTarget screen = case (selected screen, renaming screen) of
  (Just name, Just (typed, _))
    | DeckBrowser.isSaved name -> case DeckBrowser.nameProblems (Seq.filter (/= name) (DeckBrowser.decks (browser screen))) typed of
        [] -> Right (name, DeckBrowser.savedPrefix <> DeckBrowser.slugOf typed)
        problems -> Left problems
  _ -> Left [Text.pack "Only a saved deck is renamed."]

-- | The screen with the rename refused for these reasons.
refused :: [Text.Text] -> Decks -> Decks
refused problems screen = screen {renaming = fmap (\(typed, _) -> (typed, problems)) (renaming screen)}

-- | The name a copy of this saved deck is saved under, among these decks:
-- its own with "-copy", then "-copy-2" and on, the first free.
copyName :: Seq.Seq Text.Text -> Text.Text -> Text.Text
copyName names name =
  let base = name <> Text.pack "-copy"
      candidates = base : [base <> Text.pack ("-" <> show n) | n <- [2 :: Int ..]]
   in head' [candidate | candidate <- candidates, Foldable.notElem candidate names]
  where
    head' xs = case xs of
      x : _ -> x
      [] -> name

-- | The screen once this deck is saved here, by import, rename or copy: the
-- saved decks shown, it selected, and any panel closed.
savedAs :: Text.Text -> Decks -> Decks
savedAs name screen = browsed DeckBrowser.imported screen {selected = Just name, renaming = Nothing, deleting = Nothing}

-- | The deck selected on coming back from the builder opened on `opened`:
-- the deck it saved, else that one.
returning :: Maybe Text.Text -> Builder.Builder -> Maybe Text.Text
returning opened built = Builder.savedAs built Applicative.<|> opened

-- | The screen without this deck, nothing selected.
deleted :: Text.Text -> Decks -> Decks
deleted name screen = browsed (DeckBrowser.deleted name) screen {selected = Monad.mfilter (/= name) (selected screen), deleting = Nothing}

view :: Decks -> Html.Html Event
view screen =
  Element.keyed
    "main"
    (["decks-screen"] <> ["detailing" | Maybe.isJust (selected screen)])
    []
    [ (Text.pack "browser", browserView screen),
      (Text.pack "detail", detailView screen)
    ]

-- | The heading and the ways to add a deck, the import panel while open,
-- the shelves, the filters, and the decks shown as tiles.
browserView :: Decks -> Html.Html Event
browserView screen =
  Element.keyed "section" ["browser"] [Element.attr "aria-label" (Text.pack "Decks")] $
    [ ( Text.pack "head",
        Element.el
          "div"
          ["browser-head"]
          []
          [ Element.el "h1" [] [] [Element.string "Decks"],
            Element.button ["primary", "new-deck"] MkNewDeck [Element.string "New deck"],
            Element.button ["open-import"] (MkBrowse DeckBrowser.MkOpenImport) [Element.string "Import a deck"]
          ]
      )
    ]
      <> foldMap (pure . (,) (Text.pack "import") . fmap MkBrowse . DeckBrowser.importView) (DeckBrowser.importing browsing)
      <> [ (Text.pack "shelves", fmap MkBrowse (DeckBrowser.shelvesView [DeckBrowser.MkSaved, DeckBrowser.MkPrecons] browsing)),
           (Text.pack "filters", DeckBrowser.filtersView MkBrowse [] browsing),
           (Text.pack "decks", DeckBrowser.resultsView MkSelect ((== selected screen) . Just) none (DeckBrowser.decks browsing) browsing)
         ]
  where
    browsing = browser screen
    none =
      [ Element.el
          "div"
          ["empty-shelf"]
          []
          [ Element.el "p" [] [] [Element.string "No decks saved in this browser yet."],
            Element.el
              "div"
              ["empty-tools"]
              []
              [ Element.button ["new-deck"] MkNewDeck [Element.string "New deck"],
                Element.button ["open-import"] (MkBrowse DeckBrowser.MkOpenImport) [Element.string "Import a deck"]
              ]
          ]
      ]

-- | The deck selected, detailed (DeckBrowser.detailBody), with its actions:
-- a saved deck's Play, Edit, Rename, Duplicate, Export and Delete, which
-- asks first; a precon's Play, Edit a copy and Export.
detailView :: Decks -> Html.Html Event
detailView screen =
  Element.el "aside" (["detail"] <> ["none" | Maybe.isNothing (selected screen)]) [Element.attr "aria-label" (Text.pack "Deck selected")] $ case selected screen of
    Nothing ->
      [ Element.el "span" ["eyebrow"] [] [Element.string "Deck"],
        Element.el "p" ["muted"] [] [Element.string "Pick a deck to see it here."]
      ]
    Just name ->
      [ Element.el
          "div"
          ["detail-head"]
          []
          [ Element.el "span" ["eyebrow"] [] [Element.string (if DeckBrowser.isSaved name then "Saved deck" else "Precon")],
            Element.el "button" ["quiet", "close-detail"] [Element.attr "type" (Text.pack "button"), Element.on "click" MkClose, Element.attr "aria-label" (Text.pack "Close the deck")] [Element.string "Close"]
          ]
      ]
        <> DeckBrowser.detailBody (browser screen) name
        <> actions name
        <> fmap (fmap MkBrowse) (DeckBrowser.exportedView (browser screen) name)
  where
    actions name
      | deleting screen == Just name =
          [ Element.el
              "div"
              ["confirm-delete"]
              [Element.attr "role" (Text.pack "alertdialog"), Element.attr "aria-label" (Text.pack "Delete this deck?")]
              [ Element.el "p" [] [] [Element.string "Delete ", Element.el "strong" [] [] [Element.text (snd (DeckBrowser.describe name))], Element.string " from this browser? This cannot be undone."],
                Element.el
                  "div"
                  ["saved-tools"]
                  []
                  [ Element.button ["danger", "delete"] (MkBrowse (DeckBrowser.MkDelete name)) [Element.string "Delete"],
                    Element.button ["cancel-delete"] MkCancelDelete [Element.string "Cancel"]
                  ]
              ]
          ]
      | Just (typed, problems) <- renaming screen =
          [ Element.el "div" ["rename"] [] $
              [ Element.el "label" [] [Element.attr "for" (Text.pack "rename-name")] [Element.string "New name"],
                Element.el "input" ["rename-name"] [Element.attr "id" (Text.pack "rename-name"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "value" typed, Element.on "input" MkRenameName] []
              ]
                <> [Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- problems] | not (null problems)]
                <> [ Element.el
                       "div"
                       ["saved-tools"]
                       []
                       [ Element.button ["save-rename"] MkSubmitRename [Element.string "Rename"],
                         Element.button ["cancel-rename"] MkCancelRename [Element.string "Cancel"]
                       ]
                   ]
          ]
      | DeckBrowser.isSaved name =
          [ Element.el
              "div"
              ["saved-tools"]
              []
              [ Element.button ["play"] (MkPlay name) [Element.string "Play"],
                Element.button ["edit"] (MkEdit name) [Element.string "Edit"],
                Element.button ["rename"] MkRename [Element.string "Rename"],
                Element.button ["duplicate"] (MkDuplicate name) [Element.string "Duplicate"],
                Element.button ["export"] (MkBrowse (DeckBrowser.MkExport name)) [Element.string "Export"],
                Element.button ["danger", "delete"] (MkAskDelete name) [Element.string "Delete"]
              ]
          ]
      | otherwise =
          [ Element.el
              "div"
              ["saved-tools"]
              []
              [ Element.button ["play"] (MkPlay name) [Element.string "Play"],
                Element.button ["edit"] (MkEdit name) [Element.string "Edit a copy"],
                Element.button ["export"] (MkBrowse (DeckBrowser.MkExport name)) [Element.string "Export"]
              ]
          ]
