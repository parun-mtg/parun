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
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana
import qualified Text.Read as Read

data Lobby = MkLobby
  { -- | The card files the registry holds.
    cardFiles :: Natural,
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
    previews :: Map.Map Text.Text [Line]
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
  deriving (Eq, Show)

-- | A lobby holding these decks under these formats, nothing chosen yet.
new :: Natural -> Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Lobby
new files names named = MkLobby files names named Nothing [Nothing] MkMine Text.empty Map.empty Map.empty

-- | The most bots a game seats: a table of four.
mostBots :: Int
mostBots = 3

-- | Whether two decks are under one format, and so may meet: the game's
-- format is the one their headers name (Parun.Play.Command.settle).
together :: Lobby -> Text.Text -> Text.Text -> Bool
together lobby one two = Map.lookup one (formats lobby) == Map.lookup two (formats lobby)

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

-- | A deck's list: a curve of its spells by mana value (the last bar
-- counting six and up), and its cards by kind.
preview :: Text.Text -> [Line] -> Html.Html msg
preview name lines' =
  let total = sum (fmap copies lines')
      spells = [l | l <- lines', kind l /= MkLand]
      bars = [(mv, sum [copies l | l <- spells, inBar l]) | (mv, inBar) <- [(show n, \l -> value l == n) | n <- [0 .. 5]] <> [("6+", \l -> value l >= 6)]]
      tallest = maximum (1 : fmap snd bars)
      bar (mv, n) =
        Element.el
          "span"
          ["bar"]
          [Element.attr "title" (Text.pack (show n <> " at mana value " <> mv))]
          [Element.el "span" ["fill"] [Element.attr "style" (Text.pack ("height:" <> show (div (n * 100) tallest) <> "%"))] [], Element.el "span" ["mv"] [] [Element.string mv]]
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
        [ Element.el "summary" [] [] [Element.el "span" ["preview-title"] [] [Element.text (label name), Element.string (" \x00b7 " <> show total <> " cards")], Element.el "span" ["curve"] [Element.attr "aria-hidden" (Text.pack "true")] (fmap bar bars)],
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
  MkStart -> lobby

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
            format = [Element.el "span" ["deck-format"] [] [Element.text (titled f)] | f <- Maybe.maybeToList (Map.lookup name (formats lobby))]
         in (name, Element.button (["deck"] <> ["picked" | picked]) (MkPick name) [Element.el "span" ["deck-name"] [] (Element.text (snd (describe name)) : format), pips])
   in Element.el
        "main"
        ["lobby"]
        []
        $ [ Element.el
              "header"
              ["hero"]
              []
              [ Element.el "h1" [] [] [Element.el "img" ["logo"] [Element.attr "src" (Text.pack "./icon.svg"), Element.attr "alt" Text.empty, Element.attr "width" (Text.pack "40"), Element.attr "height" (Text.pack "40")] [], Element.string "Parun"],
                Element.el "p" ["tagline"] [] [Element.string ("A Magic rules engine, running in your browser. " <> show (cardFiles lobby) <> " cards loaded.")]
              ],
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
          ]
          -- The list of the deck being chosen, or else of the player's.
          <> [ preview name lines'
             | name <- Maybe.maybeToList (Monad.mplus (chosen lobby (choosing lobby)) (mine lobby)),
               lines' <- Maybe.maybeToList (Map.lookup name (previews lobby))
             ]
          <> [ Element.el
                 "section"
                 ["deck-picker"]
                 []
                 ( [ Element.el
                       "div"
                       ["deck-tools"]
                       []
                       [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-search")] [Element.string "Filter decks"],
                         Element.el "input" ["search"] [Element.attr "id" (Text.pack "deck-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter decks or sets"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] [],
                         Element.button ["random"] MkRandom [Element.string "Random"]
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
                     <> [Element.el "p" ["empty"] [] [Element.string "No deck matches."] | null shown]
                 )
             ]
