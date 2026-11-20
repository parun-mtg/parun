-- | The deck builder: a deck made from a card pool, searched as it is
-- typed, checked as it changes, and saved as a deck list.
module Parun.Web.Builder where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.CardIndex as CardIndex
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Mana as Mana
import qualified Text.Read as Read

data Builder = MkBuilder
  { -- | Every card the deck may draw on, once indexed.
    index :: Maybe (Seq.Seq CardIndex.Entry),
    query :: CardIndex.Query,
    -- | The main deck: how many of each card.
    deck :: Map.Map CardName.CardName Natural,
    -- | A main-deck card's lines in a list opened, in order: what follows its
    -- name on each (its printing and annotations) and how many that line
    -- names; absent for a card on one bare line.
    suffixes :: Map.Map CardName.CardName [(Text.Text, Natural)],
    -- | Every line of a list opened after its main deck, kept as written.
    rest :: [Text.Text],
    -- | The format the list's header names; empty for a casual deck, which
    -- names none and so meets any deck naming none.
    format :: Text.Text,
    -- | Every shipped format, by name.
    formats :: [Text.Text],
    -- | The fewest cards the format allows, and whether a commander counts
    -- toward them, once known.
    least :: Maybe (Natural, Bool),
    -- | Why the list as it stands breaks its format.
    problems :: [Text.Text],
    -- | The cards those problems are about.
    flagged :: Set.Set CardName.CardName,
    -- | The card whose text is shown.
    reading :: Maybe CardName.CardName,
    -- | The name typed to save under.
    saveName :: Text.Text,
    -- | The saved deck this one is, once saved.
    savedAs :: Maybe Text.Text,
    -- | Why the last save was refused.
    saveProblems :: [Text.Text],
    -- | The deck and format as last saved or opened.
    baseline :: (Map.Map CardName.CardName Natural, Text.Text),
    -- | Whether leaving with changes is being asked about.
    leaving :: Bool,
    -- | The cards the deck is built from.
    pool :: Pool,
    -- | A card pool being pasted in, while its panel is open.
    pasting :: Maybe Text.Text,
    -- | The codes of the sets whose cards may be the pool.
    sets :: [Text.Text]
  }
  deriving (Eq, Show)

-- | The cards a deck is built from, basic lands always among them
-- (CR 100.2b).
data Pool
  = -- | Every card.
    MkEveryCard
  | -- | A set's cards, by its code, and their names case-folded; a set's
    -- list holds one of each, so it narrows the search and caps nothing.
    MkSetCards Text.Text (Set.Set Text.Text)
  | -- | CR 100.2b: a limited card pool, which caps each card at its count.
    MkCardPool (Map.Map CardName.CardName Natural)
  deriving (Eq, Show)

-- | What a tap or a keystroke in the builder does.
data Event
  = MkSearch
  | MkToggleColour Color.Color
  | MkToggleKind CardType.CardType
  | MkValue
  | MkAdd CardName.CardName
  | MkRemove CardName.CardName
  | MkRead CardName.CardName
  | MkCloseCard
  | MkFormat
  | MkSaveName
  | MkSave
  | MkBack
  | MkStay
  | MkLeave
  | -- | The card pool chosen: empty for every card, "paste" to paste one,
    -- "set:CODE" for a set's cards, which the shell reads.
    MkPoolChoice
  | MkPoolText
  | MkUsePool
  | MkClosePool
  deriving (Eq, Show)

-- | An empty casual deck, among these formats, its cards not yet indexed.
new :: [Text.Text] -> Builder
new named = MkBuilder Nothing CardIndex.everything Map.empty Map.empty [] Text.empty named Nothing [] Set.empty Nothing Text.empty Nothing [] (Map.empty, Text.empty) False MkEveryCard Nothing []

-- | The builder after an event, the value typed being `typed`. Save, Leave,
-- and Back with nothing changed are the shell's to carry out.
update :: Event -> Text.Text -> Builder -> Builder
update event typed builder = case event of
  MkSearch -> withQuery (\q -> q {CardIndex.text = typed})
  MkToggleColour c -> withQuery (\q -> q {CardIndex.wanted = toggle c (CardIndex.wanted q)})
  MkToggleKind k -> withQuery (\q -> q {CardIndex.kinds = toggle k (CardIndex.kinds q)})
  MkValue -> withQuery (\q -> q {CardIndex.valued = Read.readMaybe (Text.unpack typed)})
  MkAdd card
    | Just most <- capOf builder card, Map.findWithDefault 0 card (deck builder) >= most -> builder
    | otherwise -> builder {deck = Map.insertWith (+) card 1 (deck builder)}
  MkRemove card -> builder {deck = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) card (deck builder)}
  MkRead card -> builder {reading = Just card}
  MkCloseCard -> builder {reading = Nothing}
  MkFormat -> builder {format = typed}
  MkSaveName -> builder {saveName = typed}
  MkSave -> builder
  MkBack -> builder {leaving = dirty builder}
  MkStay -> builder {leaving = False}
  MkLeave -> builder
  MkPoolChoice
    | Text.null typed -> builder {pool = MkEveryCard, query = (query builder) {CardIndex.among = Nothing}, pasting = Nothing}
    | typed == Text.pack "paste" -> builder {pasting = Just (Maybe.fromMaybe Text.empty (pasting builder))}
    | otherwise -> builder
  MkPoolText -> builder {pasting = Just typed}
  MkUsePool -> case pasting builder of
    Just text ->
      let counts = Map.fromListWith (+) (entriesOf (Text.lines text))
       in builder
            { pool = MkCardPool counts,
              format = Text.pack "limited",
              least = Nothing,
              rest = snd (split DeckList.Side (rest builder)),
              query = (query builder) {CardIndex.among = Just (Set.map foldName (Map.keysSet counts))},
              pasting = Nothing
            }
    Nothing -> builder
  MkClosePool -> builder {pasting = Nothing}
  where
    withQuery f = builder {query = f (query builder)}
    toggle :: (Ord a) => a -> Set.Set a -> Set.Set a
    toggle x xs = if Set.member x xs then Set.delete x xs else Set.insert x xs

-- | The deck as a deck list: its format's header, if it names one, the
-- main deck by name, then, after a blank line, the rest of a list opened,
-- as written.
listText :: Builder -> Text.Text
listText builder =
  Text.unlines $
    [Text.pack "Format: " <> format builder | not (Text.null (format builder))]
      <> [ Text.pack (show k) <> Text.singleton ' ' <> CardName.unwrap card <> suffix
         | (card, n) <- List.sortOn (CardName.unwrap . fst) (Map.toList (deck builder)),
           (suffix, k) <- fitted n (Map.findWithDefault [(Text.empty, n)] card (suffixes builder)),
           k > 0
         ]
      <> (if null (unused builder) then [] else Text.empty : DeckList.heading DeckList.Side : [Text.pack (show n) <> Text.singleton ' ' <> CardName.unwrap card | (card, n) <- unused builder])
      <> (if null (rest builder) then [] else Text.empty : rest builder)

-- | The builder with a check's problems, and the cards they name.
checked :: [Text.Text] -> Set.Set CardName.CardName -> Builder -> Builder
checked found named builder = builder {problems = found, flagged = named}

-- | A card's lines holding this many copies: copies added go on the first,
-- copies removed come off the last.
fitted :: Natural -> [(Text.Text, Natural)] -> [(Text.Text, Natural)]
fitted n parts
  | n >= total = case parts of
      (suffix, k) : more -> (suffix, k + n - total) : more
      [] -> [(Text.empty, n)]
  | otherwise = reverse (trimmed (total - n) (reverse parts))
  where
    total = sum (fmap snd parts)
    trimmed d xs = case xs of
      (suffix, k) : more
        | k > d -> (suffix, k - d) : more
        | otherwise -> trimmed (d - k) more
      [] -> []

-- | The builder once saved as this deck.
saved :: Text.Text -> Builder -> Builder
saved name builder = builder {savedAs = Just name, baseline = (deck builder, format builder), saveProblems = []}

-- | The sections of a list opened beside its main deck, each with how many
-- cards its lines name, in the order written.
kept :: Builder -> [(DeckList.Section, Natural)]
kept builder =
  let step acc raw = case (DeckList.sectionNamed line, acc) of
        (Just section, _) -> (section, 0) : acc
        (Nothing, (section, n) : older) | not (Text.null line), Just (more, _, _, _) <- DeckList.entryOf line -> (section, n + more) : older
        _ -> acc
        where
          line = Text.strip raw
   in reverse (List.foldl' step [] (rest builder))

-- | Whether the deck or its format differ from when it was last saved or
-- opened.
dirty :: Builder -> Bool
dirty builder = (deck builder, format builder) /= baseline builder

-- | The saved deck a save writes, given the saved decks' names: the one
-- this deck was saved as, or else the one the typed name makes, unless it
-- may not be used (Lobby.nameProblems).
saveTarget :: Seq.Seq Text.Text -> Builder -> Either [Text.Text] Text.Text
saveTarget decks builder = case savedAs builder of
  Just name -> Right name
  Nothing -> case Lobby.nameProblems decks (saveName builder) of
    [] -> Right (Lobby.savedPrefix <> Lobby.slugOf (saveName builder))
    problems' -> Left problems'

-- | The deck as the lobby's preview lines, each card's cost, mana value and
-- kind from the index.
linesOf :: Builder -> [Lobby.Line]
linesOf builder =
  let byName = Map.fromList [(CardIndex.name e, e) | e <- foldMap Foldable.toList (index builder)]
      line (card, n) = case Map.lookup card byName of
        Just e -> Lobby.MkLine n card (CardIndex.cost e) (CardIndex.value e) (kindOf (CardIndex.types e))
        Nothing -> Lobby.MkLine n card Nothing 0 Lobby.MkSpell
      kindOf types'
        | Set.member CardType.Land types' = Lobby.MkLand
        | Set.member CardType.Creature types' = Lobby.MkCreature
        | otherwise = Lobby.MkSpell
   in fmap line (Map.toList (deck builder))

-- | The basic lands, offered whatever the search.
basics :: [CardName.CardName]
basics = fmap (CardName.MkCardName . Text.pack) ["Plains", "Island", "Swamp", "Mountain", "Forest"]

-- | The card types a search may be narrowed to.
searchKinds :: [CardType.CardType]
searchKinds = [CardType.Creature, CardType.Instant, CardType.Sorcery, CardType.Artifact, CardType.Enchantment, CardType.Planeswalker, CardType.Battle, CardType.Land]

view :: Builder -> Html.Html Event
view builder =
  let lines' = linesOf builder
      total = sum (fmap Lobby.copies lines')
      named = CardName.unwrap
      atCap card = maybe False (<= Map.findWithDefault 0 card (deck builder)) (capOf builder card)
      adding card = Element.el "button" ["add"] ([Element.attr "type" (Text.pack "button"), Element.on "click" (MkAdd card)] <> [Element.attr "disabled" Text.empty | atCap card]) [Element.el "span" ["sr-only"] [] [Element.text (Text.pack "Add " <> named card)]]
      removing card = Element.el "button" ["remove"] [Element.attr "type" (Text.pack "button"), Element.on "click" (MkRemove card)] [Element.el "span" ["sr-only"] [] [Element.text (Text.pack "Remove " <> named card)]]
      reader card = Element.button ["card-name"] (MkRead card) [Element.text (named card)]
      option chosen (value', label') = Element.el "option" [] ([Element.attr "value" value'] <> [Element.attr "selected" Text.empty | chosen == value']) [Element.text label']
      toggled on = ["toggle"] <> ["on" | on]
      q = query builder
      valueChoices = (Text.empty, Text.pack "Any mana value") : [(Text.pack (show n), Text.pack (show n)) | n <- [0 .. 6 :: Int]] <> [(Text.pack "7", Text.pack "7+")]
      chosenValue = maybe Text.empty (Text.pack . show) (CardIndex.valued q)
      results = case index builder of
        Nothing -> [Element.el "p" ["empty"] [] [Element.string "Preparing the cards\x2026"]]
        Just entries ->
          let (count, shown) = CardIndex.search q entries
              row e =
                Element.el "div" (["result"] <> ["in-deck" | Map.member (CardIndex.name e) (deck builder)]) [] $
                  [reader (CardIndex.name e)]
                    <> foldMap (pure . Mana.cost) (CardIndex.cost e)
                    <> [Element.el "span" ["type-line"] [] [Element.text (CardIndex.typeLine e)]]
                    <> [Element.el "span" ["copies"] [] [Element.string (show n)] | n <- Maybe.maybeToList (Map.lookup (CardIndex.name e) (deck builder))]
                    <> [Element.el "span" ["left"] [] [Element.string (show (most - min most (Map.findWithDefault 0 (CardIndex.name e) (deck builder))) <> " left")] | most <- Maybe.maybeToList (capOf builder (CardIndex.name e))]
                    <> [adding (CardIndex.name e)]
           in [Element.el "p" ["empty"] [] [Element.string "No card matches."] | count == 0]
                <> [Element.el "p" ["shown-count"] [] [Element.string ("Showing " <> show (Seq.length shown) <> " of " <> show count)] | count > Seq.length shown]
                <> [Element.el "div" ["results"] [] (fmap row (Foldable.toList shown))]
      section title k =
        let these = List.sortOn (\l -> (Lobby.value l, named (Lobby.card l))) [l | l <- lines', Lobby.kind l == k]
         in [ Element.el "div" ["deck-section"] [] $
                Element.el "h3" [] [] [Element.string (title <> " \x00b7 " <> show (sum (fmap Lobby.copies these)))]
                  : [ Element.el "div" (["deck-line"] <> ["flagged" | Set.member (Lobby.card l) (flagged builder)]) [] $
                        [Element.el "span" ["copies"] [] [Element.string (show (Lobby.copies l))], reader (Lobby.card l)]
                          <> foldMap (pure . Mana.cost) (Lobby.cost l)
                          <> [removing (Lobby.card l), adding (Lobby.card l)]
                    | l <- these
                    ]
            | not (null these)
            ]
      -- CR 903.5a / 903.12d: a commander counts toward the size of a deck whose
      -- format designates one.
      commanders = sum [n | (DeckList.Commander, n) <- kept builder]
      poolValue = case pool builder of
        MkEveryCard -> Text.empty
        MkSetCards code _ -> Text.pack "set:" <> code
        MkCardPool _ -> Text.pack "pool"
      pastePanel =
        [ ( Text.pack "paste",
            Element.el
              "section"
              ["paste-pool"]
              [Element.attr "aria-label" (Text.pack "Paste a pool")]
              [ Element.el "h2" [] [] [Element.string "Paste a card pool"],
                Element.el "p" [] [] [Element.string "One card a line, with its count, as a sealed or draft pool lists it. Each card is capped at its count, basic lands aside, and the deck becomes Limited."],
                Element.el "textarea" ["pool-text"] [Element.attr "rows" (Text.pack "12"), Element.attr "spellcheck" (Text.pack "false"), Element.attr "aria-label" (Text.pack "Card pool"), Element.on "input" MkPoolText] [],
                Element.el "div" ["import-tools"] [] [Element.button ["primary"] MkUsePool [Element.string "Use this pool"], Element.button [] MkClosePool [Element.string "Cancel"]]
              ]
          )
        | Maybe.isJust (pasting builder)
        ]
      counted = case least builder of
        Nothing -> show total <> " cards"
        Just (n, withCommander) -> show (if withCommander then total + commanders else total) <> " / " <> show n
      keptNote = [Element.el "p" ["kept"] [] [Element.string "Also kept: ", Element.text (Text.intercalate (Text.pack ", ") [DeckList.heading part <> Text.pack " \x00b7 " <> Text.pack (show n) | (part, n) <- kept builder])] | not (null (kept builder))]
      reading' = do
        card <- reading builder
        let found = List.find ((== card) . CardIndex.name) (foldMap Foldable.toList (index builder))
        pure
          ( Text.pack "card",
            Element.el "section" ["card-reading"] [Element.attr "aria-label" (named card)] $
              [Element.el "h2" [] [] [Element.text (named card)]]
                <> [Element.el "p" ["type-line"] [] [Element.text (CardIndex.typeLine e)] | e <- Maybe.maybeToList found]
                <> [Element.el "p" ["rules"] [] (Mana.rich l) | e <- Maybe.maybeToList found, l <- CardIndex.rules e]
                <> [Element.button [] MkCloseCard [Element.string "Close"]]
          )
      leave =
        [ ( Text.pack "leave",
            Element.el
              "section"
              ["leave"]
              [Element.attr "role" (Text.pack "alertdialog")]
              [Element.el "p" [] [] [Element.string "Leave without saving your changes?"], Element.button [] MkStay [Element.string "Stay"], Element.button ["danger"] MkLeave [Element.string "Leave"]]
          )
        | leaving builder
        ]
   in Element.keyed "main" ["builder"] [] $
        [ ( Text.pack "top",
            Element.el "div" ["builder-top"] [] $
              [ Element.button ["home-button"] MkBack [Element.string "Back"],
                Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "builder-format")] [Element.string "Format"],
                Element.el "select" ["format"] [Element.attr "id" (Text.pack "builder-format"), Element.on "change" MkFormat] (option (format builder) (Text.empty, Text.pack "Casual (no format)") : [option (format builder) (f, Lobby.titled f) | f <- formats builder]),
                Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "builder-pool")] [Element.string "Card pool"],
                Element.el "select" ["pool"] [Element.attr "id" (Text.pack "builder-pool"), Element.on "change" MkPoolChoice] $
                  [option poolValue (Text.empty, Text.pack "Every card")]
                    <> [option poolValue (Text.pack "set:" <> code, Text.toUpper code) | code <- sets builder]
                    <> [option poolValue (Text.pack "pool", Text.pack "Pasted pool") | MkCardPool _ <- [pool builder]]
                    <> [option poolValue (Text.pack "paste", Text.pack "Paste a pool\x2026")],
                Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "builder-name")] [Element.string "Deck name"],
                Element.el "input" ["builder-name"] ([Element.attr "id" (Text.pack "builder-name"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "placeholder" (Text.pack "Deck name"), Element.attr "value" (saveName builder), Element.on "input" MkSaveName] <> [Element.attr "disabled" Text.empty | Maybe.isJust (savedAs builder)]) [],
                Element.button ["primary"] MkSave [Element.string "Save"]
              ]
                <> [Element.el "span" ["saved"] [] [Element.string "Saved"] | Maybe.isJust (savedAs builder), not (dirty builder)]
                <> [Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- saveProblems builder] | not (null (saveProblems builder))]
          ),
          ( Text.pack "body",
            Element.el
              "div"
              ["builder-body"]
              []
              [ Element.el "section" ["builder-search"] [Element.attr "aria-label" (Text.pack "Cards")] $
                  [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "card-search")] [Element.string "Search cards"],
                    Element.el "input" ["search"] [Element.attr "id" (Text.pack "card-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Search names and rules text"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] [],
                    Element.el "div" ["filters"] [] $
                      [Element.el "button" (toggled (Set.member c (CardIndex.wanted q))) [Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if Set.member c (CardIndex.wanted q) then "true" else "false")), Element.on "click" (MkToggleColour c)] [Mana.symbol (Text.pack (Web.Card.letterUpper c))] | c <- [minBound .. maxBound]]
                        <> [Element.el "button" (toggled (Set.member k (CardIndex.kinds q))) [Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if Set.member k (CardIndex.kinds q) then "true" else "false")), Element.on "click" (MkToggleKind k)] [Element.string (show k)] | k <- searchKinds]
                        <> [Element.el "select" ["mana-value"] [Element.attr "aria-label" (Text.pack "Mana value"), Element.on "change" MkValue] (fmap (option chosenValue) valueChoices)],
                    Element.el "div" ["basics"] [] [Element.button ["basic"] (MkAdd land) [Element.text (named land)] | land <- basics]
                  ]
                    <> results,
                Element.el "section" ["builder-deck"] [Element.attr "aria-label" (Text.pack "Deck")] $
                  [Element.el "h2" [] [] [Element.string counted], Lobby.curve lines']
                    <> keptNote
                    <> [Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- problems builder] | not (null (problems builder))]
                    <> section "Creatures" Lobby.MkCreature
                    <> section "Spells" Lobby.MkSpell
                    <> section "Lands" Lobby.MkLand
                    <> [Element.el "p" ["empty"] [] [Element.string "Add cards from the search."] | null lines']
              ]
          )
        ]
          <> pastePanel
          <> Maybe.maybeToList reading'
          <> leave

-- | The builder on a deck list: the saved deck it is, if one, the name to
-- offer for saving, and its text. The main deck becomes counts, each card's
-- printing and annotations its suffix, and every other section's lines the
-- rest, as written; blank lines in the main deck drop.
open :: Maybe Text.Text -> Text.Text -> Text.Text -> Builder -> Builder
open savedName offered text builder =
  let step (inMain, fmt, counts, after, others) raw =
        let line = Text.strip raw
         in case (DeckList.sectionNamed line, DeckList.formatNamed line) of
              (Just DeckList.Main, _) -> (True, fmt, counts, after, others)
              (Just _, _) -> (False, fmt, counts, after, raw : others)
              (_, Just named) -> (inMain, Maybe.fromMaybe Text.empty named, counts, after, others)
              _
                | not inMain -> (inMain, fmt, counts, after, raw : others)
                | Text.null line -> (inMain, fmt, counts, after, others)
                | Just (n, card, at, notes) <- DeckList.entryOf line ->
                    (inMain, fmt, Map.insertWith (+) card n counts, Map.insertWith (flip (<>)) card [(suffixOf at notes, n)] after, others)
                | otherwise -> (inMain, fmt, counts, after, others)
      suffixOf at notes = foldMap (\(set, number) -> Text.pack " (" <> set <> Text.pack ") " <> number) at <> foldMap (\note -> Text.pack " {" <> note <> Text.singleton '}') notes
      (_, format', deck', suffixes', others') = List.foldl' step (True, Text.empty, Map.empty, Map.empty, []) (Text.lines text)
      (side, unsided) = split DeckList.Side (reverse others')
      -- CR 100.4b: a limited list's main deck and sideboard are its pool.
      held = Map.unionWith (+) deck' (Map.fromListWith (+) (entriesOf side))
      pooled = format' == Text.pack "limited" && not (null (entriesOf side))
   in (if pooled then \b -> b {pool = MkCardPool held, rest = dropWhile (Text.null . Text.strip) (reverse (dropWhile (Text.null . Text.strip) (reverse unsided))), query = (query b) {CardIndex.among = Just (Set.map foldName (Map.keysSet held))}} else id)
        builder
          { format = format',
            deck = deck',
            suffixes = Map.filter (\parts -> length parts > 1 || not (all (Text.null . fst) parts)) suffixes',
            rest = reverse (dropWhile (Text.null . Text.strip) others'),
            savedAs = savedName,
            saveName = offered,
            baseline = (deck', format'),
            problems = [],
            flagged = Set.empty,
            least = Nothing,
            reading = Nothing,
            saveProblems = [],
            leaving = False,
            pool = MkEveryCard,
            query = (query builder) {CardIndex.among = Nothing},
            pasting = Nothing
          }

-- | The builder drawing on a set's cards, by its code.
setPool :: Text.Text -> [CardName.CardName] -> Builder -> Builder
setPool code names builder =
  let folded = Set.fromList (fmap foldName names)
   in builder {pool = MkSetCards code folded, query = (query builder) {CardIndex.among = Just folded}, pasting = Nothing}

-- | A card's name as a pool compares it.
foldName :: CardName.CardName -> Text.Text
foldName = Text.toCaseFold . CardName.unwrap

-- | How many of a card the pool holds, when it caps the card: a limited
-- pool's count, none for a card it lacks; never a basic land's (CR 100.2b).
capOf :: Builder -> CardName.CardName -> Maybe Natural
capOf builder card = case pool builder of
  MkCardPool held
    | not (isBasic builder card) -> Just (sum [n | (named, n) <- Map.toList held, foldName named == foldName card])
  _ -> Nothing

-- | Whether a card is a basic land, by the index, else by name.
isBasic :: Builder -> CardName.CardName -> Bool
isBasic builder card = maybe (card `elem` basics) CardIndex.basic (List.find ((== card) . CardIndex.name) (foldMap Foldable.toList (index builder)))

-- | The cards a list's lines name, whatever section they are in, headers
-- and a format skipped.
entriesOf :: [Text.Text] -> [(CardName.CardName, Natural)]
entriesOf lines' =
  [ (card, n)
  | raw <- lines',
    let line = Text.strip raw,
    not (Text.null line),
    Maybe.isNothing (DeckList.sectionNamed line),
    Maybe.isNothing (DeckList.formatNamed line),
    Just (n, card, _, _) <- [DeckList.entryOf line]
  ]

-- | A list's lines in this section, less its header, and every other line.
split :: DeckList.Section -> [Text.Text] -> ([Text.Text], [Text.Text])
split wanted lines' =
  let step (current, inside, outside) raw = case DeckList.sectionNamed (Text.strip raw) of
        Just section | section == wanted -> (Just section, inside, outside)
        Just section -> (Just section, inside, raw : outside)
        Nothing | current == Just wanted -> (current, raw : inside, outside)
        Nothing -> (current, inside, raw : outside)
      (_, ins, outs) = List.foldl' step (Nothing, [], []) lines'
   in (reverse ins, reverse outs)

-- | The pool's cards the deck leaves out, by name: a limited deck's
-- sideboard (CR 100.4b).
unused :: Builder -> [(CardName.CardName, Natural)]
unused builder = case pool builder of
  MkCardPool held ->
    [ (card, n - used)
    | (card, n) <- Map.toAscList held,
      let used = sum [k | (named, k) <- Map.toList (deck builder), foldName named == foldName card],
      n > used
    ]
  _ -> []
