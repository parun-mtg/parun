-- | The deck browser the lobby and the Decks screen share (#9430): every
-- deck and what has been read of each, the shelf shown and what it is
-- filtered by, the panels a deck is imported and exported in, and their
-- views: the shelves, the filters, the decks as tiles, and a deck's detail.
module Parun.Web.DeckBrowser where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana

data DeckBrowser = MkDeckBrowser
  { -- | Whether the deck lists are still to read.
    reading :: Bool,
    -- | Why the deck lists did not load.
    problem :: Maybe Text.Text,
    -- | Every deck, by its path beneath the deck root, less ".txt".
    decks :: Seq.Seq Text.Text,
    -- | The format each deck's header names (Parun.DeckList.formatOf), by
    -- name; a deck naming none is absent.
    formats :: Map.Map Text.Text Text.Text,
    -- | What the deck list is filtered by.
    search :: Text.Text,
    -- | The colours a deck shown must hold, each of them.
    colourFilter :: Set.Set Color.Color,
    -- | The one set whose decks are shown, by its code (describe); every
    -- set's while absent.
    setFilter :: Maybe Text.Text,
    -- | Whether the shipped decks are shown or the saved ones.
    shelf :: Shelf,
    -- | Each deck's colours, by its name.
    palette :: Map.Map Text.Text (Set.Set Color.Color),
    -- | The card lists of the decks detailed so far, by name.
    previews :: Map.Map Text.Text [Line],
    -- | The panel a deck list is pasted into, while open.
    importing :: Maybe Import,
    -- | A deck's name and text, shown to copy.
    exporting :: Maybe (Text.Text, Text.Text),
    -- | Why each saved deck found to break its format does, by name.
    illegal :: Map.Map Text.Text [Text.Text],
    -- | How many cards each deck's main deck holds, by name, as read so far.
    sizes :: Map.Map Text.Text Natural,
    -- | Each deck's spells by mana value (curveOf), by name, as read so far.
    curves :: Map.Map Text.Text [Natural]
  }
  deriving (Eq, Show)

-- | A deck list being pasted in, to save under a name.
data Import = MkImport
  { importName :: Text.Text,
    importText :: Text.Text,
    -- | Why the last import was refused.
    importProblems :: [Text.Text],
    -- | Whether the list is being checked.
    checking :: Bool
  }
  deriving (Eq, Show)

-- | Which decks the browser shows.
data Shelf
  = -- | The decks Parun ships, each a set's.
    MkPrecons
  | -- | The decks saved in this browser.
    MkSaved
  deriving (Eq, Show)

-- | What a tap or a keystroke on the browser does.
data Event
  = -- | Filters the decks by the text typed.
    MkSearch
  | -- | Shows only the decks holding this colour, or stops.
    MkColour Color.Color
  | -- | Shows only the set whose code is chosen, or every set for none.
    MkSet
  | -- | Shows these decks.
    MkShelf Shelf
  | -- | Opens the panel a deck list is pasted into.
    MkOpenImport
  | -- | Closes it.
    MkCloseImport
  | -- | The name typed for the deck pasted.
    MkImportName
  | -- | The deck list typed or pasted.
    MkImportText
  | -- | Checks the list and saves it; the shell does both.
    MkSubmitImport
  | -- | Deletes this saved deck; the shell does it.
    MkDelete Text.Text
  | -- | Shows this deck's list to copy; the shell reads it.
    MkExport Text.Text
  | -- | Hides the list shown to copy.
    MkCloseExport
  deriving (Eq, Show)

-- | A browser whose decks are still to read, on the shipped decks, nothing
-- filtered.
new :: DeckBrowser
new =
  MkDeckBrowser
    { reading = True,
      problem = Nothing,
      decks = Seq.empty,
      formats = Map.empty,
      search = Text.empty,
      colourFilter = Set.empty,
      setFilter = Nothing,
      shelf = MkPrecons,
      palette = Map.empty,
      previews = Map.empty,
      importing = Nothing,
      exporting = Nothing,
      illegal = Map.empty,
      sizes = Map.empty,
      curves = Map.empty
    }

-- | The browser after an event, the value typed being `typed`.
update :: Event -> Text.Text -> DeckBrowser -> DeckBrowser
update event typed browser = case event of
  MkSearch -> browser {search = typed}
  MkColour c -> browser {colourFilter = (if Set.member c (colourFilter browser) then Set.delete c else Set.insert c) (colourFilter browser)}
  MkSet -> browser {setFilter = if Text.null typed then Nothing else Just typed}
  MkShelf which -> browser {shelf = which}
  MkOpenImport -> browser {importing = Just (MkImport Text.empty Text.empty [] False)}
  MkCloseImport -> browser {importing = Nothing}
  MkImportName -> browser {importing = fmap (\i -> i {importName = typed}) (importing browser)}
  MkImportText -> browser {importing = fmap (\i -> i {importText = typed}) (importing browser)}
  MkSubmitImport -> browser
  MkDelete _ -> browser
  MkExport _ -> browser
  MkCloseExport -> browser {exporting = Nothing}

-- | The browser holding these decks under these formats, what was typed
-- kept.
withDecks :: Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> DeckBrowser -> DeckBrowser
withDecks names named browser = browser {decks = names, formats = named, reading = False}

-- | The browser once a deck is imported: the panel closed, the saved decks
-- shown.
imported :: DeckBrowser -> DeckBrowser
imported browser = browser {importing = Nothing, shelf = MkSaved}

-- | The browser without this deck.
deleted :: Text.Text -> DeckBrowser -> DeckBrowser
deleted name browser =
  browser
    { decks = Seq.filter (/= name) (decks browser),
      formats = Map.delete name (formats browser),
      palette = Map.delete name (palette browser),
      previews = Map.delete name (previews browser),
      curves = Map.delete name (curves browser),
      exporting = Nothing
    }

-- | Where saved decks sit beneath the deck root.
savedPrefix :: Text.Text
savedPrefix = Text.pack "saved/"

-- | Whether a deck is one the player saved.
isSaved :: Text.Text -> Bool
isSaved = Text.isPrefixOf savedPrefix

-- | A deck's name as its file's: lowercase letters and digits, each run of
-- anything else one dash, none at either end.
slugOf :: Text.Text -> Text.Text
slugOf = Text.intercalate (Text.pack "-") . filter (not . Text.null) . Text.split (not . Char.isAlphaNum) . Text.toLower

-- | Why a deck may not be saved under this name, among these decks: an
-- empty one, or one a saved deck holds.
nameProblems :: Seq.Seq Text.Text -> Text.Text -> [Text.Text]
nameProblems names typed
  | Text.null slug = [Text.pack "Give the deck a name."]
  | Foldable.elem (savedPrefix <> slug) names = [Text.pack "A saved deck is already named that."]
  | otherwise = []
  where
    slug = slugOf typed

-- | A deck's set and its own name: "sets/m12/entangling-webs" is set "M12",
-- deck "Entangling Webs".
describe :: Text.Text -> (Text.Text, Text.Text)
describe name =
  let parts = Text.splitOn (Text.pack "/") name
      (group, leaf) = case reverse parts of
        l : s : _ -> (Text.toUpper s, l)
        [l] -> (Text.pack "Decks", l)
        [] -> (Text.pack "Decks", name)
   in (group, titled leaf)

-- | "entangling-webs" as "Entangling Webs".
titled :: Text.Text -> Text.Text
titled = Text.unwords . fmap capital . Text.split (\c -> c == '-' || c == '_')
  where
    capital w = case Text.uncons w of
      Just (c, rest) -> Text.cons (Char.toUpper c) rest
      Nothing -> w

-- | The deck as the match-up names it: its own name and its set.
label :: Text.Text -> Text.Text
label name = let (group, deck) = describe name in deck <> Text.pack " (" <> group <> Text.pack ")"

-- | Whether the deck matches what is typed, by its path or its name, any case.
matches :: Text.Text -> Text.Text -> Bool
matches typed name =
  let needle = Text.toCaseFold (Text.strip typed)
   in Text.null needle || Text.isInfixOf needle (Text.toCaseFold name) || Text.isInfixOf needle (Text.toCaseFold (label name))

-- | Those of the decks `offered` on the shelf shown.
shelved :: Seq.Seq Text.Text -> DeckBrowser -> [Text.Text]
shelved offered browser = filter ((== (shelf browser == MkSaved)) . isSaved) (Foldable.toList offered)

-- | Those of the decks `offered` shown: those shelved that match what is
-- typed, hold every colour toggled on (or are still to have their colours
-- read), and, among the shipped, are of the set chosen.
shown :: Seq.Seq Text.Text -> DeckBrowser -> [Text.Text]
shown offered browser =
  let coloured name = maybe True (Set.isSubsetOf (colourFilter browser)) (Map.lookup name (palette browser))
      ofSet name = shelf browser == MkSaved || all (== fst (describe name)) (setFilter browser)
   in filter (\name -> matches (search browser) name && coloured name && ofSet name) (shelved offered browser)

-- | Whether any deck is saved here.
anySaved :: DeckBrowser -> Bool
anySaved = any isSaved . decks

-- | The shelf a deck is on.
shelfOf :: Text.Text -> Shelf
shelfOf name = if isSaved name then MkSaved else MkPrecons

-- | The sets the shipped decks come from, by code, in order.
setsOf :: DeckBrowser -> [Text.Text]
setsOf browser = Set.toList (Set.fromList [fst (describe name) | name <- Foldable.toList (decks browser), not (isSaved name)])

-- | A card of a deck's list, as a deck's detail previews it.
data Line = MkLine
  { copies :: Natural,
    card :: CardName.CardName,
    cost :: Maybe ManaCost.ManaCost,
    -- | CR 202.3.
    value :: Integer,
    kind :: Kind
  }
  deriving (Eq, Show)

-- | Which part of a list a card is shown in.
data Kind
  = MkCreature
  | MkSpell
  | MkLand
  deriving (Eq, Ord, Show)

-- | A deck's spells by mana value, from zero to five, then six and up.
curveOf :: [Line] -> [Natural]
curveOf lines' =
  let spells = [l | l <- lines', kind l /= MkLand]
   in [sum [copies l | l <- spells, value l == n] | n <- [0 .. 5]] <> [sum [copies l | l <- spells, value l >= 6]]

-- | A curve as bars, each labelled with its mana value, with these classes:
-- "spark" draws it small, unlabelled.
curve :: [String] -> [Natural] -> Html.Html msg
curve classes counts =
  let tallest = maximum (1 : counts)
      bar (mv, n) =
        Element.el
          "span"
          ["bar"]
          [Element.attr "title" (Text.pack (show n <> " at mana value " <> mv))]
          [Element.el "span" ["fill"] [Element.attr "style" (Text.pack ("height:" <> show (div (n * 100) tallest) <> "%"))] [], Element.el "span" ["mv"] [] [Element.string mv]]
   in Element.el "span" ("curve" : classes) [Element.attr "aria-hidden" (Text.pack "true")] (fmap bar (zip (fmap show [0 :: Int .. 5] <> ["6+"]) counts))

-- | A deck's cards by kind, each kind under its count.
cardList :: [Line] -> Html.Html msg
cardList lines' =
  let section title k =
        let these = List.sortOn (\l -> (value l, CardName.unwrap (card l))) [l | l <- lines', kind l == k]
         in [ Element.el "div" ["deck-section"] [] $
                Element.el "h3" [] [] [Element.string (title <> " \x00b7 " <> show (sum (fmap copies these)))]
                  : [Element.el "p" ["deck-line"] [] ([Element.el "span" ["copies"] [] [Element.string (show (copies l))], Element.el "span" ["deck-card"] [] [Element.text (CardName.unwrap (card l))]] <> foldMap (pure . Mana.cost) (cost l)) | l <- these]
            | not (null these)
            ]
   in Element.el "div" ["deck-sections"] [] (foldMap (uncurry section) kinds)

-- | Each kind of card, as a deck's detail names it.
kinds :: [(String, Kind)]
kinds = [("Creatures", MkCreature), ("Other spells", MkSpell), ("Lands", MkLand)]

-- | A colour's name.
colourName :: Color.Color -> String
colourName = show

-- | Colours as a deck's detail names them.
colourWords :: Set.Set Color.Color -> String
colourWords colours
  | Set.null colours = "Colourless"
  | otherwise = List.intercalate ", " (fmap colourName (Set.toList colours))

-- | A deck's colours as mana symbols.
pips :: Set.Set Color.Color -> Html.Html msg
pips colours = Element.el "span" ["pips"] [] [Mana.symbol (Text.pack (Web.Card.letterUpper c)) | c <- Set.toList colours]

-- | A deck's set and its card count, as far as read: "M12 · 60 cards".
counted :: DeckBrowser -> Text.Text -> String
counted browser name =
  List.intercalate " \x00b7 " ([Text.unpack (fst (describe name)) | not (isSaved name)] <> ["Saved" | isSaved name] <> [show n <> " cards" | n <- Maybe.maybeToList (Map.lookup name (sizes browser))])

-- | A shelf's name on its tab.
shelfTitle :: Shelf -> String
shelfTitle which = case which of
  MkPrecons -> "Precons"
  MkSaved -> "My decks"

-- | The shelves as tabs, in this order, the one shown marked.
shelvesView :: [Shelf] -> DeckBrowser -> Html.Html Event
shelvesView order browser =
  Element.el
    "div"
    ["shelves"]
    [Element.attr "role" (Text.pack "tablist"), Element.attr "aria-label" (Text.pack "Which decks")]
    (fmap tab order)
  where
    tab which =
      let on = shelf browser == which
       in Element.el
            "button"
            (["tab"] <> ["on" | on])
            [Element.attr "type" (Text.pack "button"), Element.attr "role" (Text.pack "tab"), Element.attr "aria-selected" (Text.pack (if on then "true" else "false")), Element.on "click" (MkShelf which)]
            [Element.string (shelfTitle which)]

-- | The filters: the search box, the colour toggles and the set menu, its
-- events sent as `wrap` makes them, then `extra`.
filtersView :: (Event -> msg) -> [Html.Html msg] -> DeckBrowser -> Html.Html msg
filtersView wrap extra browser =
  Element.el "div" ["deck-tools"] [] $
    fmap
      (fmap wrap)
      [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-search")] [Element.string "Filter decks"],
        Element.el "input" ["search"] [Element.attr "id" (Text.pack "deck-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter decks or sets"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] [],
        Element.el "div" ["colour-toggles"] [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "Colours")] (fmap toggle [minBound .. maxBound]),
        Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-set")] [Element.string "Set"],
        Element.el
          "select"
          ["set-filter"]
          ([Element.attr "id" (Text.pack "deck-set"), Element.on "change" MkSet] <> [Element.attr "disabled" Text.empty | shelf browser == MkSaved])
          (option Text.empty (Text.pack "All sets") : [option code code | code <- setsOf browser])
      ]
      <> extra
  where
    toggle c =
      let on = Set.member c (colourFilter browser)
       in Element.el
            "button"
            (["toggle"] <> ["on" | on])
            [Element.attr "type" (Text.pack "button"), Element.attr "aria-label" (Text.pack (colourName c)), Element.attr "aria-pressed" (Text.pack (if on then "true" else "false")), Element.on "click" (MkColour c)]
            [Mana.symbol (Text.pack (Web.Card.letterUpper c))]
    option value' label' = Element.el "option" [] ([Element.attr "value" value'] <> [Element.attr "selected" Text.empty | setFilter browser == if Text.null value' then Nothing else Just value']) [Element.text label']

-- | Those of the decks `offered` shown, as tiles, each sending `pick` with
-- its name and marked where `picked`, and how many of them are shown; or
-- why there are none: the decks unread or failed, `none` for an empty My
-- decks, or nothing matching.
resultsView :: (Text.Text -> msg) -> (Text.Text -> Bool) -> [Html.Html msg] -> Seq.Seq Text.Text -> DeckBrowser -> Html.Html msg
resultsView pick picked none offered browser =
  Element.el "div" ["deck-results"] [] $ case problem browser of
    Just p -> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p]]
    Nothing
      | reading browser -> [Element.el "p" ["empty"] [] [Element.string "Reading the decks\x2026"]]
      | shelf browser == MkSaved && null (shelved offered browser) -> none
      | null listed -> [Element.el "p" ["empty"] [] [Element.string "No deck matches."]]
      | otherwise ->
          [ Element.keyed "div" ["deck-grid"] [] (fmap (\name -> (name, tile pick (picked name) browser name)) listed),
            Element.el "p" ["deck-count", "muted"] [] [Element.string (if length listed == total then inWords total else show (length listed) <> " of " <> inWords total)]
          ]
  where
    listed = shown offered browser
    total = length (shelved offered browser)
    inWords n = show n <> if n == 1 then " deck" else " decks"

-- | A deck as a tile: its set as a badge ("Saved" for a saved one), its colours, its name, its
-- format, whether it is legal, and its curve once read; pressed where
-- `picked`.
tile :: (Text.Text -> msg) -> Bool -> DeckBrowser -> Text.Text -> Html.Html msg
tile pick picked browser name =
  let (code, own) = describe name
      format = [Element.el "span" ["deck-format"] [] [Element.text (titled f)] | f <- Maybe.maybeToList (Map.lookup name (formats browser))] <> [Element.el "span" ["not-legal"] [] [Element.string "Not legal"] | Map.member name (illegal browser)]
   in Element.el
        "button"
        (["deck"] <> ["picked" | picked])
        [Element.attr "type" (Text.pack "button"), Element.on "click" (pick name), Element.attr "aria-pressed" (Text.pack (if picked then "true" else "false"))]
        ( [ Element.el "span" ["deck-top"] [] [Element.el "span" ["set-badge"] [] [Element.text (if isSaved name then Text.pack "Saved" else code)], pips (Map.findWithDefault Set.empty name (palette browser))],
            Element.el "span" ["deck-name"] [] [Element.text own]
          ]
            <> [Element.el "span" ["deck-badges"] [] format | not (null format)]
            <> [curve ["spark"] counts | counts <- Maybe.maybeToList (Map.lookup name (curves browser))]
        )

-- | A deck's detail: its name, set, size, colours and format, then its
-- curve, its kinds of card and its list once read.
detailBody :: DeckBrowser -> Text.Text -> [Html.Html msg]
detailBody browser name =
  [ Element.el "div" ["detail-name"] [] [pips colours, Element.el "h2" [] [] [Element.text (snd (describe name))]],
    Element.el "p" ["muted"] [] [Element.string (List.intercalate " \x00b7 " ([counted browser name] <> [colourWords colours | Map.member name (palette browser)] <> [Text.unpack (titled f) | f <- Maybe.maybeToList (Map.lookup name (formats browser))]))]
  ]
    <> case Map.lookup name (previews browser) of
      Nothing -> [Element.el "p" ["empty"] [] [Element.string "Reading the list\x2026"]]
      Just lines' ->
        [ Element.el "div" ["panel", "detail-curve"] [] [Element.el "span" ["eyebrow"] [] [Element.string "Mana curve"], curve [] (curveOf lines')],
          Element.el "dl" ["panel", "kinds"] [] [Element.el "div" [] [] [Element.el "dt" [] [] [Element.string title], Element.el "dd" ["muted"] [] [Element.string (show (sum [copies l | l <- lines', kind l == k]))]] | (title, k) <- kinds],
          Element.el "details" ["deck-preview"] [] [Element.el "summary" [] [] [Element.string ("See all " <> show (sum (fmap copies lines')) <> " cards")], cardList lines']
        ]
  where
    colours = Map.findWithDefault Set.empty name (palette browser)

-- | This deck's list to copy, while it is the one exported.
exportedView :: DeckBrowser -> Text.Text -> [Html.Html Event]
exportedView browser name = [exportView shown' | shown'@(exported, _) <- Maybe.maybeToList (exporting browser), exported == name]

-- | The panel a deck list is pasted into: its name, its list, and why the
-- last import was refused.
importView :: Import -> Html.Html Event
importView panel =
  Element.el
    "section"
    ["import"]
    [Element.attr "aria-label" (Text.pack "Import a deck")]
    $ [ Element.el "h2" [] [] [Element.string "Import a deck"],
        Element.el "label" [] [Element.attr "for" (Text.pack "import-name")] [Element.string "Name"],
        Element.el "input" ["import-name"] [Element.attr "id" (Text.pack "import-name"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkImportName] [],
        Element.el "label" [] [Element.attr "for" (Text.pack "import-text")] [Element.string "Deck list"],
        Element.el "textarea" ["import-text"] [Element.attr "id" (Text.pack "import-text"), Element.attr "rows" (Text.pack "12"), Element.attr "spellcheck" (Text.pack "false"), Element.attr "placeholder" (Text.pack "Format: constructed\n24 Forest\n4 Llanowar Elves"), Element.on "input" MkImportText] []
      ]
      <> [Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- importProblems panel] | not (null (importProblems panel))]
      <> [ Element.el
             "div"
             ["import-tools"]
             []
             [ Element.el "button" ["primary"] ([Element.attr "type" (Text.pack "button"), Element.on "click" MkSubmitImport] <> [Element.attr "disabled" Text.empty | checking panel]) [Element.string (if checking panel then "Checking\x2026" else "Import")],
               Element.button [] MkCloseImport [Element.string "Cancel"]
             ]
         ]

-- | A deck's list, to copy.
exportView :: (Text.Text, Text.Text) -> Html.Html Event
exportView (name, contents) =
  Element.el
    "section"
    ["export"]
    [Element.attr "aria-label" (Text.pack "Export a deck")]
    [ Element.el "h2" [] [] [Element.text (snd (describe name))],
      Element.el "textarea" ["export-text"] [Element.attr "readonly" Text.empty, Element.attr "rows" (Text.pack "12"), Element.attr "aria-label" (Text.pack "Deck list")] [Element.text contents],
      Element.button [] MkCloseExport [Element.string "Close"]
    ]
