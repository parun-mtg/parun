-- | The web client's first screen: the match-up being set, your deck against
-- each bot's, and every deck, grouped by set and filtered by what is typed.
module Parun.Web.Lobby where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana
import qualified Text.Read as Read

data Lobby = MkLobby
  { -- | Whether the deck lists are still to read.
    reading :: Bool,
    -- | Why the deck lists did not load.
    problem :: Maybe Text.Text,
    -- | Every deck, by its path beneath the deck root, less ".txt".
    decks :: Seq.Seq Text.Text,
    -- | The format each deck's header names (Parun.DeckList.formatOf), by
    -- name; a deck naming none is absent.
    formats :: Map.Map Text.Text Text.Text,
    -- | The player's deck, once chosen.
    mine :: Maybe Text.Text,
    -- | Each bot's deck, in seat order, once chosen; a random one otherwise.
    -- There is at least one bot and at most 'mostBots'.
    theirs :: [Maybe Text.Text],
    -- | Which deck a tapped deck becomes.
    choosing :: Slot,
    -- | What the deck list is filtered by.
    search :: Text.Text,
    -- | Each deck's colours, by its name (colours).
    palette :: Map.Map Text.Text (Set.Set Color.Color),
    -- | The card lists of the decks chosen so far, by name.
    previews :: Map.Map Text.Text [Line],
    -- | The panel a deck list is pasted into, while open.
    importing :: Maybe Import,
    -- | A saved deck's name and text, shown to copy.
    exporting :: Maybe (Text.Text, Text.Text),
    -- | Why each saved deck found to break its format does, by name.
    illegal :: Map.Map Text.Text [Text.Text],
    -- | Why Start was last refused, until the next pick.
    refusal :: Maybe [Text.Text]
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

-- | A side of the match-up.
data Slot
  = MkMine
  | -- | The bot at this place in 'theirs', from zero.
    MkTheirs Int
  deriving (Eq, Show)

-- | What a tap or a keystroke on the lobby does.
data Event
  = -- | Makes this deck the chosen slot's.
    MkPick Text.Text
  | -- | Chooses a deck for this slot next.
    MkChoose Slot
  | -- | Filters the decks by the text typed.
    MkSearch
  | -- | A random deck for the slot being chosen; the page picks it.
    MkRandom
  | -- | Seats another bot, with a random deck, up to 'mostBots'.
    MkAddBot
  | -- | Unseats the last bot, while there are two or more.
    MkDropBot
  | -- | Starts the game.
    MkStart
  | -- | Goes back to the home screen.
    MkHome
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
  | -- | Shows this saved deck's list to copy; the shell reads it.
    MkExport Text.Text
  | -- | Hides the list shown to copy.
    MkCloseExport
  | -- | Opens the deck builder; the shell does it.
    MkOpenBuilder
  | -- | Opens this deck in the builder, a saved one to save over, any other
    -- as a copy; the shell does it.
    MkEdit Text.Text
  deriving (Eq, Show)

-- | A lobby whose decks are still to read, nothing chosen yet.
new :: Lobby
new = MkLobby True Nothing Seq.empty Map.empty Nothing [Nothing] MkMine Text.empty Map.empty Map.empty Nothing Nothing Map.empty Nothing

-- | Where saved decks sit beneath the deck root.
savedPrefix :: Text.Text
savedPrefix = Text.pack "saved/"

-- | Whether a deck is one to play: not a set's whole card list.
playable :: Text.Text -> Bool
playable name = isSaved name || not (Text.isSuffixOf (Text.pack "/all") name)

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

-- | The lobby once this deck is imported: the panel closed, the deck yours.
imported :: Text.Text -> Lobby -> Lobby
imported name lobby = (update (MkPick name) Text.empty lobby {choosing = MkMine}) {importing = Nothing}

-- | The lobby without this deck, every seat holding it emptied.
deleted :: Text.Text -> Lobby -> Lobby
deleted name lobby =
  let emptied :: Maybe Text.Text -> Maybe Text.Text
      emptied = Monad.mfilter (/= name)
   in lobby
        { decks = Seq.filter (/= name) (decks lobby),
          formats = Map.delete name (formats lobby),
          palette = Map.delete name (palette lobby),
          previews = Map.delete name (previews lobby),
          mine = emptied (mine lobby),
          theirs = fmap emptied (theirs lobby),
          exporting = Nothing
        }

-- | The lobby holding these decks under these formats, what was chosen and
-- typed kept.
withDecks :: Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Lobby -> Lobby
withDecks names named lobby = lobby {decks = names, formats = named, reading = False}

-- | The most bots a game seats: a table of four.
mostBots :: Int
mostBots = 3

-- | Whether two decks are under one format, and so may meet: the game's
-- format is the one their headers name (Parun.Play.Command.settle), which
-- a Limited deck's makes of a deck naming none.
together :: Lobby -> Text.Text -> Text.Text -> Bool
together lobby one two = case (Map.lookup one (formats lobby), Map.lookup two (formats lobby)) of
  -- CR 100.2b: a Limited deck meets a deck naming no format, which the
  -- game then checks as Limited.
  (Just named, Nothing) | named == limited -> True
  (Nothing, Just named) | named == limited -> True
  (mine', theirs') -> mine' == theirs'
  where
    limited = Text.pack "limited"

-- | The decks the bot may play against yours: every deck, until yours is
-- chosen.
opponents :: Lobby -> Seq.Seq Text.Text
opponents lobby = maybe (decks lobby) (\one -> Seq.filter (together lobby one) (decks lobby)) (mine lobby)

-- | A card's colours as a deck shows them: those of the mana symbols in
-- its faces' costs.
cardColours :: Card.Card -> Set.Set Color.Color
cardColours c = foldMap (foldMap costColours . Face.manaCost) (Card.faces c)

-- | The colours a cost's symbols name (CR 105.2, 202.2).
costColours :: ManaCost.ManaCost -> Set.Set Color.Color
costColours printed =
  let ofType t = case t of
        ManaType.Colored c -> Set.singleton c
        ManaType.Colorless -> Set.empty
      symbol sym = case sym of
        ManaSymbol.Generic _ -> Set.empty
        ManaSymbol.OfType t -> ofType t
        ManaSymbol.Hybrid h -> Set.union (ofType (Hybrid.left h)) (ofType (Hybrid.right h))
        ManaSymbol.MonocoloredHybrid t -> ofType t
        ManaSymbol.Phyrexian c -> Set.singleton c
        ManaSymbol.HybridPhyrexian h -> Set.fromList [HybridPhyrexian.left h, HybridPhyrexian.right h]
        ManaSymbol.Snow -> Set.empty
        ManaSymbol.Variable -> Set.empty
   in foldMap symbol (ManaCost.unwrap printed)

-- | A deck list's colours: those of each card a line names, by `colours`.
deckColours :: (CardName.CardName -> Set.Set Color.Color) -> Text.Text -> Set.Set Color.Color
deckColours colours contents = foldMap colours (Set.fromList (fmap snd (entries contents)))

-- | A deck list's lines that name a card ("4 Forest", "1 Shock (M19) 156"),
-- each as its count and the card; other lines skipped.
entries :: Text.Text -> [(Natural, CardName.CardName)]
entries contents =
  let entry line = do
        let (count, rest) = Text.span Char.isDigit (Text.strip line)
        n <- Read.readMaybe (Text.unpack count)
        pure (n, CardName.MkCardName (Text.strip (fst (Text.breakOn (Text.pack " (") rest))))
   in Maybe.mapMaybe entry (Text.lines contents)

-- | A card of a deck's list, as the lobby previews it.
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

-- | A deck's spells by mana value, as bars, the last counting six and up.
curve :: [Line] -> Html.Html msg
curve lines' =
  let spells = [l | l <- lines', kind l /= MkLand]
      bars = [(mv, sum [copies l | l <- spells, inBar l]) | (mv, inBar) <- [(show n, \l -> value l == n) | n <- [0 .. 5]] <> [("6+", \l -> value l >= 6)]]
      tallest = maximum (1 : fmap snd bars)
      bar (mv, n) =
        Element.el
          "span"
          ["bar"]
          [Element.attr "title" (Text.pack (show n <> " at mana value " <> mv))]
          [Element.el "span" ["fill"] [Element.attr "style" (Text.pack ("height:" <> show (div (n * 100) tallest) <> "%"))] [], Element.el "span" ["mv"] [] [Element.string mv]]
   in Element.el "span" ["curve"] [Element.attr "aria-hidden" (Text.pack "true")] (fmap bar bars)

-- | A deck's list: its curve ('curve') and its cards by kind.
preview :: Text.Text -> [Line] -> Html.Html msg
preview name lines' =
  let total = sum (fmap copies lines')
      section title k =
        let these = List.sortOn (\l -> (value l, CardName.unwrap (card l))) [l | l <- lines', kind l == k]
         in [ Element.el "div" ["deck-section"] [] $
                Element.el "h3" [] [] [Element.string (title <> " \x00b7 " <> show (sum (fmap copies these)))]
                  : [Element.el "p" ["deck-line"] [] ([Element.el "span" ["copies"] [] [Element.string (show (copies l))], Element.el "span" ["deck-card"] [] [Element.text (CardName.unwrap (card l))]] <> foldMap (pure . Mana.cost) (cost l)) | l <- these]
            | not (null these)
            ]
   in Element.el
        "details"
        ["deck-preview"]
        []
        [ Element.el "summary" [] [] [Element.el "span" ["preview-title"] [] [Element.text (label name), Element.string (" \x00b7 " <> show total <> " cards")], curve lines'],
          Element.el "div" ["deck-sections"] [] (section "Creatures" MkCreature <> section "Spells" MkSpell <> section "Lands" MkLand)
        ]

-- | A deck file's name beneath the root, less ".txt"; the path as given
-- where the root does not prefix it.
deckName :: FilePath -> FilePath -> Text.Text
deckName root path =
  let relative = Maybe.fromMaybe path $ List.stripPrefix (root <> "/") path
      name = Text.pack relative
   in Maybe.fromMaybe name $ Text.stripSuffix (Text.pack ".txt") name

-- | The lobby after an event, the value typed being `typed`. Picking your
-- deck moves on to the first bot's still unchosen, and drops each bot's deck
-- under another format; a bot is offered only its 'opponents'.
update :: Event -> Text.Text -> Lobby -> Lobby
update event typed lobby = case event of
  MkPick name
    | Foldable.notElem name (decks lobby) -> lobby
    | Maybe.isJust (refusal lobby) -> update event typed lobby {refusal = Nothing}
    | MkTheirs i <- choosing lobby ->
        if Foldable.notElem name (opponents lobby) then lobby else lobby {theirs = [if j == i then Just name else deck | (j, deck) <- zip [0 ..] (theirs lobby)]}
    | otherwise ->
        let kept = fmap (Monad.mfilter (together lobby name)) (theirs lobby)
         in lobby {mine = Just name, theirs = kept, choosing = maybe MkMine MkTheirs (List.elemIndex Nothing kept)}
  MkChoose slot -> lobby {choosing = slot}
  MkSearch -> lobby {search = typed}
  MkRandom -> lobby
  MkAddBot
    | length (theirs lobby) < mostBots -> lobby {theirs = theirs lobby <> [Nothing]}
    | otherwise -> lobby
  MkDropBot
    | length (theirs lobby) > 1 ->
        let kept = take (length (theirs lobby) - 1) (theirs lobby)
         in lobby {theirs = kept, choosing = if choosing lobby == MkTheirs (length kept) then MkMine else choosing lobby}
    | otherwise -> lobby
  MkStart -> lobby {refusal = either (\problems -> if null problems then Nothing else Just problems) (const Nothing) (startable lobby)}
  MkOpenImport -> lobby {importing = Just (MkImport Text.empty Text.empty [] False)}
  MkCloseImport -> lobby {importing = Nothing}
  MkImportName -> lobby {importing = fmap (\i -> i {importName = typed}) (importing lobby)}
  MkImportText -> lobby {importing = fmap (\i -> i {importText = typed}) (importing lobby)}
  MkSubmitImport -> lobby
  MkDelete _ -> lobby
  MkExport _ -> lobby
  MkCloseExport -> lobby {exporting = Nothing}
  MkOpenBuilder -> lobby
  MkEdit _ -> lobby
  MkHome -> lobby

-- | Your deck, to start a game with; or why it may not: the problems a
-- check found with it, or none while there is no deck.
startable :: Lobby -> Either [Text.Text] Text.Text
startable lobby = case mine lobby of
  Nothing -> Left []
  Just name -> maybe (Right name) Left (Map.lookup name (illegal lobby))

-- | The deck chosen for a slot, if any.
chosen :: Lobby -> Slot -> Maybe Text.Text
chosen lobby slot = case slot of
  MkMine -> mine lobby
  MkTheirs i -> Monad.join (Maybe.listToMaybe (drop i (theirs lobby)))

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

view :: Lobby -> Html.Html Event
view lobby =
  let offered = if choosing lobby == MkMine then decks lobby else opponents lobby
      bots = theirs lobby
      shown = filter (matches (search lobby)) (Foldable.toList offered)
      groups = Map.toList (Map.fromListWith (flip (<>)) [(fst (describe name), [name]) | name <- shown])
      slot which title held fallback =
        Element.button
          (["slot"] <> ["choosing" | choosing lobby == which])
          (MkChoose which)
          [ Element.el "span" ["slot-title"] [] [Element.string title],
            Element.el "span" ["slot-deck"] [] [Element.text (maybe (Text.pack fallback) label held)]
          ]
      deck name =
        let picked = Just name == chosen lobby (choosing lobby)
            pips = Element.el "span" ["pips"] [] [Mana.symbol (Text.pack (Web.Card.letterUpper c)) | c <- Set.toList (Map.findWithDefault Set.empty name (palette lobby))]
            format = [Element.el "span" ["deck-format"] [] [Element.text (titled f)] | f <- Maybe.maybeToList (Map.lookup name (formats lobby))] <> [Element.el "span" ["not-legal"] [] [Element.string "Not legal"] | Map.member name (illegal lobby)]
         in (name, Element.button (["deck"] <> ["picked" | picked]) (MkPick name) [Element.el "span" ["deck-name"] [] (Element.text (snd (describe name)) : format), pips])
   in -- Keyed, so a section coming or going above the panel's fields or the
      -- filter's keeps their nodes, and what was typed into them.
      Element.keyed
        "main"
        ["lobby"]
        []
        $ [ (Text.pack "top", Element.el "div" ["lobby-top"] [] [Element.button ["home-button"] MkHome [Element.string "Home"], Home.hero]),
            ( Text.pack "matchup",
              Element.el
                "section"
                ["matchup"]
                [Element.attr "aria-label" (Text.pack "Match-up")]
                [ slot MkMine "You" (mine lobby) "Choose a deck",
                  Element.el "span" ["versus"] [] [Element.string "vs"],
                  Element.el "div" ["bots"] [] $
                    [slot (MkTheirs i) (if length bots == 1 then "Bot" else "Bot " <> show (i + 1)) held "Random deck" | (i, held) <- zip [0 ..] bots]
                      <> [ Element.el
                             "div"
                             ["bot-count"]
                             []
                             ( [Element.button ["add-bot"] MkAddBot [Element.string "Add a bot"] | length bots < mostBots]
                                 <> [Element.button ["drop-bot"] MkDropBot [Element.string "Remove a bot"] | length bots > 1]
                             )
                         ],
                  Element.el
                    "button"
                    ["primary", "start"]
                    ([Element.attr "type" (Text.pack "button"), Element.on "click" MkStart] <> [Element.attr "disabled" Text.empty | Maybe.isNothing (mine lobby)])
                    [Element.string "Start game"]
                ]
            )
          ]
          <> [(Text.pack "refusal", Element.el "ul" ["problems", "refusal"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- problems]) | problems <- Maybe.maybeToList (refusal lobby)]
          -- The list of the deck being chosen, or else of the player's, with
          -- the way to edit it, and a saved one's to copy it out and delete it.
          <> [ (,) (Text.pack "shown") . Element.el "div" ["deck-shown"] [] $
                 preview name lines'
                   : [ Element.el "div" ["saved-tools"] [] $
                         if isSaved name
                           then [Element.button ["edit"] (MkEdit name) [Element.string "Edit"], Element.button ["export"] (MkExport name) [Element.string "Export"], Element.button ["delete"] (MkDelete name) [Element.string "Delete"]]
                           else [Element.button ["edit"] (MkEdit name) [Element.string "Edit a copy"]]
                     ]
             | name <- Maybe.maybeToList (Monad.mplus (chosen lobby (choosing lobby)) (mine lobby)),
               lines' <- Maybe.maybeToList (Map.lookup name (previews lobby))
             ]
          <> foldMap (pure . (,) (Text.pack "export") . exportView) (exporting lobby)
          <> foldMap (pure . (,) (Text.pack "import") . importView) (importing lobby)
          <> [ (,) (Text.pack "picker") $
                 Element.el
                   "section"
                   ["deck-picker"]
                   []
                   ( [ Element.el
                         "div"
                         ["deck-tools"]
                         []
                         [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-search")] [Element.string "Filter decks"],
                           Element.el "input" ["search"] [Element.attr "id" (Text.pack "deck-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter decks or sets"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] [],
                           Element.button ["random"] MkRandom [Element.string "Random"],
                           Element.button ["open-builder"] MkOpenBuilder [Element.string "Build a deck"],
                           Element.button ["open-import"] MkOpenImport [Element.string "Import a deck"]
                         ],
                       Element.el
                         "p"
                         ["choosing-for"]
                         []
                         [ Element.string
                             ( case choosing lobby of
                                 MkMine -> "Choose your deck."
                                 MkTheirs i -> if length bots == 1 then "Choose the bot's deck." else "Choose bot " <> show (i + 1) <> "'s deck."
                             )
                         ],
                       Element.keyed "div" ["sets"] [] [(group, Element.el "section" ["set"] [] [Element.el "h2" [] [] [Element.text group], Element.keyed "div" ["deck-list"] [] (fmap deck names)]) | (group, names) <- groups]
                     ]
                       <> case problem lobby of
                         Just p -> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p]]
                         Nothing -> [Element.el "p" ["empty"] [] [Element.string (if reading lobby then "Reading the decks\x2026" else "No deck matches.")] | null shown]
                   )
             ]

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

-- | A saved deck's list, to copy.
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
