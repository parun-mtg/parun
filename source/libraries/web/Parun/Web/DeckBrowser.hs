{-# LANGUAGE ApplicativeDo #-}

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
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
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
    -- | The name each deck's header gives it (Parun.DeckList.nameOf), by
    -- name; a deck naming none is absent.
    titles :: Map.Map Text.Text Text.Text,
    -- | What the deck list is filtered by.
    search :: Text.Text,
    -- | The colours a deck shown must hold, each of them.
    colourFilter :: Set.Set Color.Color,
    -- | The one set whose precons are shown, by its code (describe); every
    -- deck while absent. My decks ignore it.
    setFilter :: Maybe Text.Text,
    -- | The one format whose decks are shown, as headers name it; every
    -- deck while absent.
    formatFilter :: Maybe Text.Text,
    -- | How the decks shown are ordered.
    order :: Order,
    -- | Each set's name and release, by its code, as data/decks/sets.json
    -- holds them.
    releases :: Map.Map Text.Text Release,
    -- | When the player last played each deck, in milliseconds since the
    -- Unix epoch (Parun.Web.History.lastPlayed), by name.
    played :: Map.Map Text.Text Integer,
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

-- | A set as a precon names it.
data Release = MkRelease
  { setName :: Text.Text,
    -- | Its release date, as YYYY-MM-DD.
    released :: Text.Text
  }
  deriving (Eq, Show)

-- | The order decks are shown in.
data Order
  = -- | By their own names (describe).
    MkByName
  | -- | Those the player played most recently first, then by name.
    MkByPlayed
  deriving (Bounded, Enum, Eq, Show)

-- | Which decks the browser shows.
data Shelf
  = -- | The decks Parun ships: a set's precons, and format decks.
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
  | -- | Shows only the format chosen, or every format for none.
    MkFormat
  | -- | Orders the decks as chosen (orderKey).
    MkOrder
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
      titles = Map.empty,
      search = Text.empty,
      colourFilter = Set.empty,
      setFilter = Nothing,
      formatFilter = Nothing,
      order = MkByName,
      releases = Map.empty,
      played = Map.empty,
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
  MkFormat -> browser {formatFilter = if Text.null typed then Nothing else Just typed}
  MkOrder -> browser {order = Maybe.fromMaybe MkByName (List.find ((== typed) . orderKey) [minBound .. maxBound])}
  MkShelf which -> browser {shelf = which}
  MkOpenImport -> browser {importing = Just (MkImport Text.empty Text.empty [] False)}
  MkCloseImport -> browser {importing = Nothing}
  MkImportName -> browser {importing = fmap (\i -> i {importName = typed}) (importing browser)}
  MkImportText -> browser {importing = fmap (\i -> i {importText = typed}) (importing browser)}
  MkSubmitImport -> browser
  MkDelete _ -> browser
  MkExport _ -> browser
  MkCloseExport -> browser {exporting = Nothing}

-- | The browser holding these decks under these formats and these names,
-- what was typed kept.
withDecks :: Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Map.Map Text.Text Text.Text -> DeckBrowser -> DeckBrowser
withDecks names named titled' browser = browser {decks = names, formats = named, titles = titled', reading = False}

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
      titles = Map.delete name (titles browser),
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

-- | A precon's set, by its code, and a deck's own name, the one its header
-- gives (`titles`), else its file's: "sets/m12/entangling-webs" is set "M12",
-- deck "Entangling Webs"; "pauper/rakdos-madness" has no set.
describe :: Map.Map Text.Text Text.Text -> Text.Text -> (Maybe Text.Text, Text.Text)
describe named name =
  let parts = Text.splitOn (Text.pack "/") name
      group = case parts of
        [top, code, _] | top == Text.pack "sets" -> Just (Text.toUpper code)
        _ -> Nothing
      leaf = Maybe.fromMaybe name (Maybe.listToMaybe (reverse parts))
   in (group, Map.findWithDefault (titled leaf) name named)

-- | The sets of data/decks/sets.json, by code, or why it does not decode.
releasesOf :: Text.Text -> Either Text.Text (Map.Map Text.Text Release)
releasesOf file = Common.parse file >>= Codec.decode (Common.textMap id Right releaseCodec)

releaseCodec :: Codec.Codec Release
releaseCodec = Fields.object $ do
  name' <- Fields.required "name" Common.text setName
  released' <- Fields.required "releaseDate" Common.text released
  pure (MkRelease name' released')

-- | A set's name, by its code; the code where data/decks/sets.json names
-- none.
setTitle :: DeckBrowser -> Text.Text -> Text.Text
setTitle browser code = maybe code setName (Map.lookup code (releases browser))

-- | "entangling-webs" as "Entangling Webs".
titled :: Text.Text -> Text.Text
titled = Text.unwords . fmap capital . Text.split (\c -> c == '-' || c == '_')
  where
    capital w = case Text.uncons w of
      Just (c, rest) -> Text.cons (Char.toUpper c) rest
      Nothing -> w

-- | The name a held deck's list gives it, by its name, as `titles` holds
-- one read: for a deck sent rather than read here.
titleIn :: (Text.Text, Text.Text) -> Map.Map Text.Text Text.Text
titleIn (name, contents) = foldMap (Map.singleton name) (DeckList.nameOf contents)

-- | The deck as the match-up names it: its own name, and a precon's set's
-- code.
label :: Map.Map Text.Text Text.Text -> Text.Text -> Text.Text
label named name = case describe named name of
  (Just code, deck) -> deck <> Text.pack " (" <> code <> Text.pack ")"
  (Nothing, deck) -> deck

-- | Whether the deck matches what is typed, by its path or its name, any case.
matches :: Map.Map Text.Text Text.Text -> Text.Text -> Text.Text -> Bool
matches named typed name = any (holds typed) [name, label named name]

-- | Whether the text holds what is typed, any case.
holds :: Text.Text -> Text.Text -> Bool
holds typed text =
  let needle = Text.toCaseFold (Text.strip typed)
   in Text.null needle || Text.isInfixOf needle (Text.toCaseFold text)

-- | Those of the decks `offered` on the shelf shown.
shelved :: Seq.Seq Text.Text -> DeckBrowser -> [Text.Text]
shelved offered browser = filter ((== (shelf browser == MkSaved)) . isSaved) (Foldable.toList offered)

-- | Those of the decks `offered` shown, in the order chosen: those shelved
-- that match what is typed or name its set, hold every colour toggled on (or
-- are still to have their colours read), are of the format chosen, and,
-- among the shipped, are of the set chosen.
shown :: Seq.Seq Text.Text -> DeckBrowser -> [Text.Text]
shown offered browser =
  let coloured name = maybe True (Set.isSubsetOf (colourFilter browser)) (Map.lookup name (palette browser))
      found name = matches (titles browser) (search browser) name || any (holds (search browser) . setTitle browser) (fst (describe (titles browser) name))
      ofFormat name = all ((== Map.lookup name (formats browser)) . Just) (formatFilter browser)
      ofSet name = shelf browser == MkSaved || all ((== fst (describe (titles browser) name)) . Just) (setFilter browser)
   in List.sortOn (sortKey browser) (filter (\name -> found name && coloured name && ofFormat name && ofSet name) (shelved offered browser))

-- | Where a deck sorts in the order chosen: by its own name, any case; by
-- play, after every deck the player played more recently.
sortKey :: DeckBrowser -> Text.Text -> (Ord.Down (Maybe Integer), Text.Text, Text.Text)
sortKey browser name =
  ( Ord.Down (if order browser == MkByPlayed then Map.lookup name (played browser) else Nothing),
    Text.toCaseFold (snd (describe (titles browser) name)),
    name
  )

-- | An order as its menu's value.
orderKey :: Order -> Text.Text
orderKey which = Text.pack $ case which of
  MkByName -> "name"
  MkByPlayed -> "played"

-- | An order as its menu names it.
orderTitle :: Order -> String
orderTitle which = case which of
  MkByName -> "Name"
  MkByPlayed -> "Recently played"

-- | Whether any deck is saved here.
anySaved :: DeckBrowser -> Bool
anySaved = any isSaved . decks

-- | The shelf a deck is on.
shelfOf :: Text.Text -> Shelf
shelfOf name = if isSaved name then MkSaved else MkPrecons

-- | The sets the precons come from, by code, newest first; those
-- data/decks/sets.json dates none last, by code.
setsOf :: DeckBrowser -> [Text.Text]
setsOf browser =
  List.sortOn (\code -> (Ord.Down (fmap released (Map.lookup code (releases browser))), code)) $
    Set.toList (Set.fromList [code | name <- Foldable.toList (decks browser), Just code <- [fst (describe (titles browser) name)]])

-- | The formats the decks on the shelf shown name, and the one chosen, by
-- name.
formatsOf :: DeckBrowser -> [Text.Text]
formatsOf browser = Set.toList (Set.fromList (Maybe.maybeToList (formatFilter browser) <> Maybe.mapMaybe (`Map.lookup` formats browser) (shelved (decks browser) browser)))

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

-- | A colour's toggle, its mana symbol named by the colour, pressed when
-- `on`, sending `msg`.
colourToggle :: Bool -> msg -> Color.Color -> Html.Html msg
colourToggle on msg c =
  Element.el
    "button"
    (["toggle"] <> ["on" | on])
    [Element.attr "type" (Text.pack "button"), Element.attr "aria-label" (Text.pack (colourName c)), Element.attr "aria-pressed" (Text.pack (if on then "true" else "false")), Element.on "click" msg]
    [Mana.symbol (Text.pack (Web.Card.letterUpper c))]

-- | Colours as a deck's detail names them.
colourWords :: Set.Set Color.Color -> String
colourWords colours
  | Set.null colours = "Colourless"
  | otherwise = List.intercalate ", " (fmap colourName (Set.toList colours))

-- | A deck's colours as mana symbols.
pips :: Set.Set Color.Color -> Html.Html msg
pips colours = Element.el "span" ["pips"] [] [Mana.symbol (Text.pack (Web.Card.letterUpper c)) | c <- Set.toList colours]

-- | A deck's provenance and its card count, as far as read: "Magic 2012 · 60
-- cards".
counted :: DeckBrowser -> Text.Text -> String
counted browser name =
  List.intercalate " \x00b7 " (fmap Text.unpack (Maybe.maybeToList (provenance browser name)) <> [show n <> " cards" | n <- Maybe.maybeToList (Map.lookup name (sizes browser))])

-- | Where a deck comes from, as its tile's badge says: a precon's set by
-- name, or "Saved"; a shipped format deck says nothing.
provenance :: DeckBrowser -> Text.Text -> Maybe Text.Text
provenance browser name
  | isSaved name = Just (Text.pack "Saved")
  | otherwise = fmap (setTitle browser) (fst (describe (titles browser) name))

-- | A shelf's name on its tab.
shelfTitle :: Shelf -> String
shelfTitle which = case which of
  MkPrecons -> "Precons"
  MkSaved -> "My decks"

-- | The shelves as tabs, in this order, the one shown marked.
shelvesView :: [Shelf] -> DeckBrowser -> Html.Html Event
shelvesView tabs browser =
  Element.el
    "div"
    ["shelves"]
    [Element.attr "role" (Text.pack "tablist"), Element.attr "aria-label" (Text.pack "Which decks")]
    (fmap tab tabs)
  where
    tab which =
      let on = shelf browser == which
       in Element.el
            "button"
            (["tab"] <> ["on" | on])
            [Element.attr "type" (Text.pack "button"), Element.attr "role" (Text.pack "tab"), Element.attr "aria-selected" (Text.pack (if on then "true" else "false")), Element.on "click" (MkShelf which)]
            [Element.string (shelfTitle which)]

-- | The filters every shelf shares: the search box, the colour toggles, the
-- format menu and the order; then, for the precons, the set menu; its events
-- sent as `wrap` makes them, then `extra`.
filtersView :: (Event -> msg) -> [Html.Html msg] -> DeckBrowser -> Html.Html msg
filtersView wrap extra browser =
  Element.el "div" ["deck-tools"] [] $
    fmap
      (fmap wrap)
      ( [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-search")] [Element.string "Filter decks"],
          Element.el "input" ["search"] [Element.attr "id" (Text.pack "deck-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter decks"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] [],
          Element.el "div" ["colour-toggles"] [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "Colours")] (fmap toggle [minBound .. maxBound])
        ]
          <> menu "deck-format" "Format" MkFormat (formatFilter browser) ((Text.empty, Text.pack "Any format") : [(f, titled f) | f <- formatsOf browser])
          <> menu "deck-order" "Order" MkOrder (Just (orderKey (order browser))) [(orderKey o, Text.pack (orderTitle o)) | o <- [minBound .. maxBound]]
          <> (if shelf browser == MkPrecons then menu "deck-set" "Set" MkSet (setFilter browser) ((Text.empty, Text.pack "Any set") : [(code, setTitle browser code) | code <- setsOf browser]) else [])
      )
      <> extra
  where
    toggle c = colourToggle (Set.member c (colourFilter browser)) (MkColour c) c
    menu :: String -> String -> Event -> Maybe Text.Text -> [(Text.Text, Text.Text)] -> [Html.Html Event]
    menu id' title event chosen options =
      [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack id')] [Element.string title],
        Element.el "select" ["deck-menu"] [Element.attr "id" (Text.pack id'), Element.on "change" event] [option chosen value' label' | (value', label') <- options]
      ]
    option chosen value' label' = Element.el "option" [] ([Element.attr "value" value'] <> [Element.attr "selected" Text.empty | chosen == if Text.null value' then Nothing else Just value']) [Element.text label']

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

-- | A deck as a tile: its provenance as a badge, its colours, its name, its
-- format, whether it is legal, and its curve once read; pressed where
-- `picked`.
tile :: (Text.Text -> msg) -> Bool -> DeckBrowser -> Text.Text -> Html.Html msg
tile pick picked browser name =
  let own = snd (describe (titles browser) name)
      format = [Element.el "span" ["deck-format"] [] [Element.text (titled f)] | f <- Maybe.maybeToList (Map.lookup name (formats browser))] <> [Element.el "span" ["not-legal"] [] [Element.string "Not legal"] | Map.member name (illegal browser)]
   in Element.el
        "button"
        (["deck"] <> ["picked" | picked])
        [Element.attr "type" (Text.pack "button"), Element.on "click" (pick name), Element.attr "aria-pressed" (Text.pack (if picked then "true" else "false"))]
        ( [ Element.el "span" ["deck-top"] [] ([Element.el "span" ["set-badge"] [] [Element.text badge] | badge <- Maybe.maybeToList (provenance browser name)] <> [pips (Map.findWithDefault Set.empty name (palette browser))]),
            Element.el "span" ["deck-name"] [] [Element.text own]
          ]
            <> [Element.el "span" ["deck-badges"] [] format | not (null format)]
            <> [curve ["spark"] counts | counts <- Maybe.maybeToList (Map.lookup name (curves browser))]
        )

-- | A deck's detail: its name, provenance, size, colours and format, then its
-- curve, its kinds of card and its list once read.
detailBody :: DeckBrowser -> Text.Text -> [Html.Html msg]
detailBody browser name =
  [ Element.el "div" ["detail-name"] [] [pips colours, Element.el "h2" [] [] [Element.text (snd (describe (titles browser) name))]],
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
exportedView browser name = [exportView (titles browser) shown' | shown'@(exported, _) <- Maybe.maybeToList (exporting browser), exported == name]

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
exportView :: Map.Map Text.Text Text.Text -> (Text.Text, Text.Text) -> Html.Html Event
exportView named (name, contents) =
  Element.el
    "section"
    ["export"]
    [Element.attr "aria-label" (Text.pack "Export a deck")]
    [ Element.el "h2" [] [] [Element.text (snd (describe named name))],
      Element.el "textarea" ["export-text"] [Element.attr "readonly" Text.empty, Element.attr "rows" (Text.pack "12"), Element.attr "aria-label" (Text.pack "Deck list")] [Element.text contents],
      Element.button [] MkCloseExport [Element.string "Close"]
    ]
