-- | The deck builder: a deck made from a card pool, searched as it is
-- typed, checked as it changes, and saved as a deck list.
module Parun.Web.Builder where

import qualified Data.Either as Either
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
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana
import qualified Text.Read as Read

data Builder = MkBuilder
  { -- | Every card the deck may draw on, once indexed.
    index :: Maybe (Seq.Seq CardIndex.Entry),
    query :: CardIndex.Query,
    -- | The main deck: how many of each card.
    deck :: Map.Map CardName.CardName Natural,
    -- | CR 100.4a: the sideboard, unless a pool makes it (CR 100.4b).
    side :: Map.Map CardName.CardName Natural,
    -- | CR 903.3: the commander, or commanders.
    command :: Map.Map CardName.CardName Natural,
    -- | Which of those the search's cards go to.
    target :: DeckList.Section,
    -- | A card's lines in a section of a list opened, in order: what follows
    -- its name on each (its printing and annotations other than a choice)
    -- and how many that line names; absent for a card on one bare line.
    suffixes :: Map.Map (DeckList.Section, CardName.CardName) [(Text.Text, Natural)],
    -- | The choice a card in a section makes as its deck is built, and the
    -- colours chosen (CR 207.5, 903.4b).
    chosen :: Map.Map (DeckList.Section, CardName.CardName) (DeckList.Choice, Set.Set Color.Color),
    -- | Every line of a list opened in a section the builder does not edit,
    -- kept as written.
    rest :: [Text.Text],
    -- | The format the list's header names; empty for a casual deck, which
    -- names none and so meets any deck naming none.
    format :: Text.Text,
    -- | Every shipped format a deck is built for ahead of time, by name
    -- (Parun.Legality.builtFormats); the format chosen is offered beside
    -- them.
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
    -- | What the builder could not do with the deck it was given.
    notices :: [Text.Text],
    -- | Why the cards or the rules did not load, if they did not.
    loadProblem :: Maybe Text.Text,
    -- | The deck as last saved or opened.
    baseline :: Snapshot,
    -- | Whether leaving with changes is being asked about.
    leaving :: Bool,
    -- | CR 100.2b: a limited card pool, which caps each card but a basic
    -- land at its count, by the names its list wrote.
    pool :: Maybe (Map.Map CardName.CardName Natural),
    -- | The cards the search draws on.
    scope :: Scope,
    -- | A card pool being pasted in, while its panel is open.
    pasting :: Maybe Text.Text,
    -- | Why the last pool pasted, or set chosen, was refused.
    poolProblems :: [Text.Text],
    -- | The codes of the sets whose cards the search may draw on.
    sets :: [Text.Text],
    -- | Each card of the index by its name, and the card each way of naming
    -- one means, case-folded: its name, each face's, and its faces joined.
    byName :: Map.Map CardName.CardName CardIndex.Entry,
    aliases :: Map.Map Text.Text CardName.CardName
  }
  deriving (Eq, Show)

-- | What a save keeps of a deck: its zones, choices, format and pool.
type Snapshot = ([Map.Map CardName.CardName Natural], Map.Map (DeckList.Section, CardName.CardName) (DeckList.Choice, Set.Set Color.Color), Text.Text, Maybe (Map.Map CardName.CardName Natural))

-- | The deck as a save keeps it.
snapshot :: Builder -> Snapshot
snapshot builder = (fmap (`zoneOf` builder) zones, chosen builder, format builder, pool builder)

-- | The sections the builder edits.
zones :: [DeckList.Section]
zones = [DeckList.Main, DeckList.Side, DeckList.Commander]

-- | A zone's cards.
zoneOf :: DeckList.Section -> Builder -> Map.Map CardName.CardName Natural
zoneOf section builder = case section of
  DeckList.Side -> side builder
  DeckList.Commander -> command builder
  _ -> deck builder

-- | The builder with a zone's cards.
withZone :: DeckList.Section -> Map.Map CardName.CardName Natural -> Builder -> Builder
withZone section cards builder = case section of
  DeckList.Side -> builder {side = cards}
  DeckList.Commander -> builder {command = cards}
  _ -> builder {deck = cards}

-- | Whether a pool makes the sideboard: a Limited deck's unused pool cards.
pooledSide :: Builder -> Bool
pooledSide builder = Maybe.isJust (pool builder) && format builder == Text.pack "limited"

-- | The cards a search draws on, basic lands always among them.
data Scope
  = -- | Every card.
    MkEveryCard
  | -- | A set's cards, by its code, as its list names them; a set's list
    -- holds one of each, so it narrows the search and caps nothing.
    MkSetCards Text.Text [CardName.CardName]
  | -- | The pool's cards.
    MkPoolCards
  deriving (Eq, Show)

-- | What a tap or a keystroke in the builder does.
data Event
  = MkSearch
  | MkToggleColour Color.Color
  | MkToggleKind CardType.CardType
  | MkValue
  | MkAdd DeckList.Section CardName.CardName
  | MkRemove DeckList.Section CardName.CardName
  | -- | Sends the search's cards to this zone.
    MkTarget DeckList.Section
  | -- | Toggles a colour of the choice a card in a zone makes.
    MkChoose DeckList.Section CardName.CardName Color.Color
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
  | -- | Forgets the pool, its caps and its sideboard.
    MkDropPool
  deriving (Eq, Show)

-- | An empty casual deck, among these formats, its cards not yet indexed.
new :: [Text.Text] -> Builder
new named =
  MkBuilder
    { index = Nothing,
      query = CardIndex.everything,
      deck = Map.empty,
      side = Map.empty,
      command = Map.empty,
      target = DeckList.Main,
      suffixes = Map.empty,
      chosen = Map.empty,
      rest = [],
      format = Text.empty,
      formats = named,
      least = Nothing,
      problems = [],
      flagged = Set.empty,
      reading = Nothing,
      saveName = Text.empty,
      savedAs = Nothing,
      saveProblems = [],
      notices = [],
      loadProblem = Nothing,
      baseline = ([Map.empty, Map.empty, Map.empty], Map.empty, Text.empty, Nothing),
      leaving = False,
      pool = Nothing,
      scope = MkEveryCard,
      pasting = Nothing,
      poolProblems = [],
      sets = [],
      byName = Map.empty,
      aliases = Map.empty
    }

-- | The builder after an event, the value typed being `typed`. Save, Leave,
-- and Back with nothing changed are the shell's to carry out.
update :: Event -> Text.Text -> Builder -> Builder
update event typed builder = case event of
  MkSearch -> withQuery (\q -> q {CardIndex.text = typed})
  MkToggleColour c -> withQuery (\q -> q {CardIndex.wanted = toggle c (CardIndex.wanted q)})
  MkToggleKind k -> withQuery (\q -> q {CardIndex.kinds = toggle k (CardIndex.kinds q)})
  MkValue -> withQuery (\q -> q {CardIndex.valued = Read.readMaybe (Text.unpack typed)})
  MkAdd section card
    | section == DeckList.Side, pooledSide builder -> builder
    | section == DeckList.Main, Just most <- capOf builder card, Map.findWithDefault 0 card (deck builder) >= most -> builder
    | otherwise -> withZone section (Map.insertWith (+) card 1 (zoneOf section builder)) builder
  MkRemove section card -> withZone section (Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) card (zoneOf section builder)) builder
  MkTarget section -> builder {target = section}
  MkChoose section card colour -> case choiceOf builder section card of
    Just choice ->
      let before = chosenFor section card builder
          toggled
            | Set.member colour before = Set.delete colour before
            -- CR 903.4b: one colour, which another replaces.
            | DeckList.colorCount choice == 1 = Set.singleton colour
            -- CR 207.5: two colours, and no more.
            | Set.size before >= DeckList.colorCount choice = before
            | otherwise = Set.insert colour before
       in builder {chosen = if Set.null toggled then Map.delete (section, card) (chosen builder) else Map.insert (section, card) (choice, toggled) (chosen builder)}
    Nothing -> builder
  MkRead card -> builder {reading = Just card}
  MkCloseCard -> builder {reading = Nothing}
  MkFormat ->
    let changed = builder {format = typed}
     in -- A held pool makes a Limited deck's sideboard (CR 100.4b), which then
        -- takes the sideboard's cards.
        if pooledSide changed
          then
            changed
              { pool = fmap (Map.unionWith (+) (side builder)) (pool builder),
                side = Map.empty,
                target = if target builder == DeckList.Side then DeckList.Main else target builder
              }
          else changed
  MkSaveName -> builder {saveName = typed, saveProblems = []}
  MkSave -> builder
  MkBack -> builder {leaving = dirty builder}
  MkStay -> builder {leaving = False}
  MkLeave -> builder
  MkPoolChoice
    | Text.null typed -> builder {scope = MkEveryCard, pasting = Nothing, poolProblems = []}
    | typed == Text.pack "pool", Maybe.isJust (pool builder) -> builder {scope = MkPoolCards, pasting = Nothing, poolProblems = []}
    | typed == Text.pack "paste" -> builder {pasting = Just (Maybe.fromMaybe Text.empty (pasting builder)), poolProblems = []}
    | otherwise -> builder
  MkPoolText -> builder {pasting = Just typed}
  MkUsePool -> case pasting builder of
    Just text ->
      let named = entriesOf (Text.lines text)
          unknown = Set.toList (Set.fromList [CardName.unwrap card | (card, _) <- named, Map.notMember (foldName card) (aliases builder)])
       in case () of
            _
              | Map.null (aliases builder) -> builder {poolProblems = [Text.pack "The cards are still loading; try again in a moment."]}
              | null named -> builder {poolProblems = [Text.pack "The pool names no cards."]}
              | not (null unknown) -> builder {poolProblems = [Text.pack "No card is named " <> card <> Text.singleton '.' | card <- unknown]}
              | otherwise ->
                  builder
                    { pool = Just (Map.fromListWith (+) [(canonical builder card, n) | (card, n) <- named]),
                      scope = MkPoolCards,
                      format = Text.pack "limited",
                      least = Nothing,
                      side = Map.empty,
                      suffixes = Map.filterWithKey (\(section, _) _ -> section /= DeckList.Side) (suffixes builder),
                      chosen = Map.filterWithKey (\(section, _) _ -> section /= DeckList.Side) (chosen builder),
                      target = if target builder == DeckList.Side then DeckList.Main else target builder,
                      pasting = Nothing,
                      poolProblems = []
                    }
    Nothing -> builder
  MkClosePool -> builder {pasting = Nothing, poolProblems = []}
  MkDropPool -> builder {pool = Nothing, scope = if scope builder == MkPoolCards then MkEveryCard else scope builder}
  where
    withQuery f = builder {query = f (query builder)}
    toggle :: (Ord a) => a -> Set.Set a -> Set.Set a
    toggle x xs = if Set.member x xs then Set.delete x xs else Set.insert x xs

-- | The deck as a deck list: its format's header, if it names one, its
-- commander under its own header, the main deck by name, its sideboard (a
-- pool's unused cards, if a pool makes it), then the rest of a list opened,
-- as written, each after a blank line.
listText :: Builder -> Text.Text
listText builder =
  let linesIn section cards =
        [ Text.pack (show k) <> Text.singleton ' ' <> CardName.unwrap card <> suffix <> foldMap annotation (Map.lookup (section, card) (chosen builder))
        | (card, n) <- List.sortOn (CardName.unwrap . fst) cards,
          (suffix, k) <- fitted n (Map.findWithDefault [(Text.empty, n)] (section, card) (suffixes builder)),
          k > 0
        ]
      annotation (choice, colours) = Text.pack " {" <> DeckList.keyword choice <> Text.pack ": " <> Text.unwords (fmap DeckList.colorLetter (Set.toList colours)) <> Text.singleton '}'
      sideboard = linesIn DeckList.Side (if pooledSide builder then unused builder else Map.toList (side builder))
      commanders = linesIn DeckList.Commander (Map.toList (command builder))
      block section lines' = if null lines' then [] else Text.empty : DeckList.heading section : lines'
   in Text.unlines $
        [Text.pack "Format: " <> format builder | not (Text.null (format builder))]
          <> (if null commanders then [] else DeckList.heading DeckList.Commander : commanders <> [Text.empty, DeckList.heading DeckList.Main])
          <> linesIn DeckList.Main (Map.toList (deck builder))
          <> block DeckList.Side sideboard
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
saved name builder = builder {savedAs = Just name, baseline = snapshot builder, saveProblems = []}

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

-- | Whether the deck, its choices, format or pool differ from when it was
-- last saved or opened.
dirty :: Builder -> Bool
dirty builder = snapshot builder /= baseline builder

-- | The saved deck a save writes, given the saved decks' names: the one
-- this deck was saved as, or else the one the typed name makes, unless it
-- may not be used (DeckBrowser.nameProblems); none for a deck with no cards.
saveTarget :: Seq.Seq Text.Text -> Builder -> Either [Text.Text] Text.Text
saveTarget decks builder
  | all (Map.null . (`zoneOf` builder)) zones, not (pooledSide builder && not (null (unused builder))) = Left [Text.pack "Add some cards before saving."]
  | otherwise = case savedAs builder of
      Just name -> Right name
      Nothing -> case DeckBrowser.nameProblems decks (saveName builder) of
        [] -> Right (DeckBrowser.savedPrefix <> DeckBrowser.slugOf (saveName builder))
        problems' -> Left problems'

-- | The deck as the lobby's preview lines, each card's cost, mana value and
-- kind from the index.
linesOf :: Builder -> [DeckBrowser.Line]
linesOf builder =
  let line (card, n) = case Map.lookup (canonical builder card) (byName builder) of
        Just e -> DeckBrowser.MkLine n card (CardIndex.cost e) (CardIndex.value e) (kindOf (CardIndex.types e))
        Nothing -> DeckBrowser.MkLine n card Nothing 0 DeckBrowser.MkSpell
      kindOf types'
        | Set.member CardType.Land types' = DeckBrowser.MkLand
        | Set.member CardType.Creature types' = DeckBrowser.MkCreature
        | otherwise = DeckBrowser.MkSpell
   in fmap line (Map.toList (deck builder))

-- | The colours of the deck's cards and its commander's, its sideboard's
-- aside.
deckColours :: Builder -> Set.Set Color.Color
deckColours builder = foldMap (\card -> foldMap CardIndex.colours (Map.lookup (canonical builder card) (byName builder))) (Map.keys (deck builder) <> Map.keys (command builder))

-- | The basic lands, offered whatever the search.
basics :: [CardName.CardName]
basics = fmap (CardName.MkCardName . Text.pack) ["Plains", "Island", "Swamp", "Mountain", "Forest"]

-- | The card types a search may be narrowed to.
searchKinds :: [CardType.CardType]
searchKinds = [CardType.Creature, CardType.Instant, CardType.Sorcery, CardType.Artifact, CardType.Enchantment, CardType.Planeswalker, CardType.Battle, CardType.Land]

view :: Builder -> Html.Html Event
view builder =
  let lines' = linesOf builder
      total = sum (fmap DeckBrowser.copies lines')
      named = CardName.unwrap
      atCap section card = section == DeckList.Main && maybe False (<= Map.findWithDefault 0 card (deck builder)) (capOf builder card)
      adding section card = Element.el "button" ["add"] ([Element.attr "type" (Text.pack "button"), Element.on "click" (MkAdd section card)] <> [Element.attr "disabled" Text.empty | atCap section card]) [Element.el "span" ["sr-only"] [] [Element.text (Text.pack "Add " <> named card)]]
      removing section card = Element.el "button" ["remove"] [Element.attr "type" (Text.pack "button"), Element.on "click" (MkRemove section card)] [Element.el "span" ["sr-only"] [] [Element.text (Text.pack "Remove " <> named card)]]
      -- The colours a card in a zone chooses as its deck is built, to toggle.
      choosing section card = case choiceOf builder section card of
        Nothing -> []
        Just choice ->
          let picked = chosenFor section card builder
              prompt = if DeckList.colorCount choice == 1 then "Choose a colour" else "Circle two colours"
           in [ Element.el "div" ["choice"] [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack prompt <> Text.pack " for " <> named card)] $
                  Element.el "span" ["choice-prompt"] [] [Element.string prompt]
                    : [DeckBrowser.colourToggle (Set.member c picked) (MkChoose section card c) c | c <- [minBound .. maxBound]]
              ]
      deckLine section card n cost' =
        Element.el "div" (["deck-line"] <> ["flagged" | Set.member card (flagged builder)]) [] $
          [Element.el "span" ["copies"] [] [Element.string (show n)], reader card]
            <> foldMap (pure . Mana.cost) cost'
            <> [removing section card, adding section card]
            <> choosing section card
      zoneBlock section title =
        let cards = List.sortOn (named . fst) (Map.toList (zoneOf section builder))
         in [ Element.el "div" ["deck-section", "zone"] [] $
                Element.el "h3" [] [] [Element.string (title <> " \x00b7 " <> show (sum (fmap snd cards)))]
                  : [deckLine section card n (Map.lookup (canonical builder card) (byName builder) >>= CardIndex.cost) | (card, n) <- cards]
            | not (null cards)
            ]
      pooledBlock =
        [ Element.el "div" ["deck-section", "zone"] [] $
            Element.el "h3" [] [] [Element.string ("Sideboard, the rest of the pool \x00b7 " <> show (sum (fmap snd (unused builder))))]
              : [Element.el "div" ["deck-line"] [] ([Element.el "span" ["copies"] [] [Element.string (show n)], reader card] <> choosing DeckList.Side card) | (card, n) <- unused builder]
        | pooledSide builder,
          not (null (unused builder))
        ]
      commanderZone = maybe False snd (least builder) || not (Map.null (command builder))
      targets =
        Element.el "div" ["targets"] [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "Add cards to")] $
          Element.el "span" ["choice-prompt"] [] [Element.string "Add to"]
            : [ Element.el "button" (toggled (target builder == section)) [Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if target builder == section then "true" else "false")), Element.on "click" (MkTarget section)] [Element.string title]
              | (section, title, offered') <- [(DeckList.Main, "Deck", True), (DeckList.Side, "Sideboard", not (pooledSide builder)), (DeckList.Commander, "Commander", commanderZone)],
                offered'
              ]
      reader card = Element.button ["card-name"] (MkRead card) [Element.text (named card)]
      option current (value', label') = Element.el "option" [] ([Element.attr "value" value'] <> [Element.attr "selected" Text.empty | current == value']) [Element.text label']
      toggled on = ["toggle"] <> ["on" | on]
      q = searchQuery builder
      valueChoices = (Text.empty, Text.pack "Any mana value") : [(Text.pack (show n), Text.pack (show n)) | n <- [0 .. 6 :: Int]] <> [(Text.pack "7", Text.pack "7+")]
      chosenValue = maybe Text.empty (Text.pack . show) (CardIndex.valued q)
      results = case index builder of
        Nothing -> [Element.el "p" ["empty"] [] [Element.string "Preparing the cards\x2026"]]
        Just entries ->
          let (count, shown) = CardIndex.search q entries
              row e =
                Element.el "div" (["result"] <> ["in-deck" | Map.member (CardIndex.name e) (zoneOf (target builder) builder)]) [] $
                  [reader (CardIndex.name e)]
                    <> foldMap (pure . Mana.cost) (CardIndex.cost e)
                    <> [Element.el "span" ["type-line"] [] [Element.text (CardIndex.typeLine e)]]
                    <> [Element.el "span" ["copies"] [] [Element.string (show n <> " " <> whereIn (target builder))] | n <- Maybe.maybeToList (Map.lookup (CardIndex.name e) (zoneOf (target builder) builder))]
                    <> [Element.el "span" ["left"] [] [Element.string (show (most - min most (Map.findWithDefault 0 (CardIndex.name e) (deck builder))) <> " left")] | target builder == DeckList.Main, most <- Maybe.maybeToList (capOf builder (CardIndex.name e))]
                    <> [adding (target builder) (CardIndex.name e)]
           in [Element.el "p" ["empty"] [] [Element.string "No card matches."] | count == 0]
                <> [Element.el "p" ["shown-count"] [] [Element.string ("Showing " <> show (Seq.length shown) <> " of " <> show count)] | count > Seq.length shown]
                <> [Element.el "div" ["results", "panel"] [] (fmap row (Foldable.toList shown))]
      kindBlock title k =
        let these = List.sortOn (\l -> (DeckBrowser.value l, named (DeckBrowser.card l))) [l | l <- lines', DeckBrowser.kind l == k]
         in [ Element.el "div" ["deck-section"] [] $
                Element.el "h3" [] [] [Element.string (title <> " \x00b7 " <> show (sum (fmap DeckBrowser.copies these)))]
                  : [deckLine DeckList.Main (DeckBrowser.card l) (DeckBrowser.copies l) (DeckBrowser.cost l) | l <- these]
            | not (null these)
            ]
      -- CR 903.5a / 903.12d: a commander counts toward the size of a deck whose
      -- format designates one.
      commanders = sum (command builder)
      poolValue = case scope builder of
        MkEveryCard -> Text.empty
        MkSetCards code _ -> Text.pack "set:" <> code
        MkPoolCards -> Text.pack "pool"
      pastePanel =
        [ ( Text.pack "paste",
            Element.el
              "section"
              ["paste-pool"]
              [Element.attr "aria-label" (Text.pack "Paste a pool")]
              [ Element.el "h2" [] [] [Element.string "Paste a card pool"],
                Element.el "p" [] [] [Element.string "One card a line, with its count, as a sealed or draft pool lists it. Each card is capped at its count, basic lands aside, and the deck becomes Limited."],
                Element.el "textarea" ["pool-text"] [Element.attr "rows" (Text.pack "12"), Element.attr "spellcheck" (Text.pack "false"), Element.attr "aria-label" (Text.pack "Card pool"), Element.on "input" MkPoolText] [],
                Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- poolProblems builder],
                Element.el "div" ["import-tools"] [] [Element.button ["primary"] MkUsePool [Element.string "Use this pool"], Element.button [] MkClosePool [Element.string "Cancel"]]
              ]
          )
        | Maybe.isJust (pasting builder)
        ]
      -- The main deck's, as the sideboard's and commander's blocks count theirs.
      counted = case least builder of
        Nothing -> show total <> (if total == 1 then " card" else " cards") <> " in the deck"
        Just (n, withCommander) -> show (if withCommander then total + commanders else total) <> " / " <> show n <> " in the deck"
      whereIn section = case section of
        DeckList.Side -> "in sideboard"
        DeckList.Commander -> "as commander"
        _ -> "in deck"
      -- Draft notes and sticker sheets are not cards, so their lines count none.
      countsCards part = part `notElem` [DeckList.DraftNotes, DeckList.Stickers]
      keptNote = [Element.el "p" ["kept"] [] [Element.string "Also kept: ", Element.text (Text.intercalate (Text.pack ", ") [DeckList.heading part <> (if countsCards part then Text.pack " \x00b7 " <> Text.pack (show n) else Text.empty) | (part, n) <- kept builder])] | not (null (kept builder))]
      reading' = do
        card <- reading builder
        let found = Map.lookup (canonical builder card) (byName builder)
        pure
          ( Text.pack "card",
            Element.el "section" ["card-reading"] [Element.attr "aria-label" (named card)] $
              [Element.el "h2" [] [] [Element.text (named card)]]
                <> [Element.el "p" ["type-line"] [] [Element.text (CardIndex.typeLine e)] | e <- Maybe.maybeToList found]
                <> concat
                  [ [Element.el "h3" ["face-name"] [] [Element.text face] | length (CardIndex.rules e) > 1]
                      <> [Element.el "p" ["rules"] [] (Mana.rich l) | l <- said']
                  | e <- Maybe.maybeToList found,
                    (face, said') <- CardIndex.rules e
                  ]
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
              [ Element.el "div" ["builder-head"] [] $
                  [ Element.button ["home-button"] MkBack [Element.string "Back"],
                    Element.el "h1" [] [] [Element.string (if Maybe.isJust (savedAs builder) then "Edit deck" else "New deck")]
                  ]
                    <> [Element.el "span" ["saved", "chip"] [] [Element.string "Saved"] | Maybe.isJust (savedAs builder), not (dirty builder)]
                    <> [Element.button ["primary"] MkSave [Element.string "Save"]],
                Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "builder-name")] [Element.string "Deck name"],
                Element.el "input" ["builder-name"] ([Element.attr "id" (Text.pack "builder-name"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "placeholder" (Text.pack "Deck name"), Element.attr "value" (saveName builder), Element.on "input" MkSaveName] <> [Element.attr "disabled" Text.empty | Maybe.isJust (savedAs builder)]) [],
                Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "builder-format")] [Element.string "Format"],
                Element.el "select" ["format", "deck-menu"] [Element.attr "id" (Text.pack "builder-format"), Element.on "change" MkFormat] (option (format builder) (Text.empty, Text.pack "Casual (no format)") : [option (format builder) (f, DeckBrowser.formatTitle f) | f <- formats builder <> [format builder | not (Text.null (format builder)), format builder `notElem` formats builder]]),
                Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "builder-pool")] [Element.string "Card pool"],
                Element.el "select" ["pool", "deck-menu"] [Element.attr "id" (Text.pack "builder-pool"), Element.on "change" MkPoolChoice] $
                  [option poolValue (Text.empty, Text.pack "Every card")]
                    <> [option poolValue (Text.pack "set:" <> code, Text.toUpper code) | code <- sets builder]
                    <> [option poolValue (Text.pack "pool", Text.pack "The pool") | Maybe.isJust (pool builder)]
                    <> [option poolValue (Text.pack "paste", Text.pack "Paste a pool\x2026")]
              ]
                <> [Element.button ["drop-pool"] MkDropPool [Element.string "Drop pool"] | Maybe.isJust (pool builder)]
                <> [Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- poolProblems builder] | not (null (poolProblems builder)), Maybe.isNothing (pasting builder)]
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
                      [Element.el "div" ["colour-toggles"] [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "Colours")] [DeckBrowser.colourToggle (Set.member c (CardIndex.wanted q)) (MkToggleColour c) c | c <- [minBound .. maxBound]]]
                        <> [Element.el "select" ["mana-value", "deck-menu"] [Element.attr "aria-label" (Text.pack "Mana value"), Element.on "change" MkValue] (fmap (option chosenValue) valueChoices)]
                        <> [Element.el "button" (toggled (Set.member k (CardIndex.kinds q))) [Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if Set.member k (CardIndex.kinds q) then "true" else "false")), Element.on "click" (MkToggleKind k)] [Element.string (show k)] | k <- searchKinds],
                    targets,
                    Element.el "div" ["basics"] [] [Element.button ["basic"] (MkAdd (target builder) land) [Element.text (named land)] | land <- basics]
                  ]
                    <> results,
                Element.el "section" ["builder-deck", "panel"] [Element.attr "aria-label" (Text.pack "Deck")] $
                  [ Element.el "div" ["deck-head"] [] [Element.el "h2" [] [] [Element.string counted], Element.el "span" ["pips"] [Element.attr "aria-label" (Text.pack "Colours")] [Mana.symbol (Text.pack (Web.Card.letterUpper c)) | c <- Set.toList (deckColours builder)]],
                    DeckBrowser.curve [] (DeckBrowser.curveOf lines')
                  ]
                    <> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p] | p <- Maybe.maybeToList (loadProblem builder)]
                    <> [Element.el "ul" ["notices"] [] [Element.el "li" [] [] [Element.text n] | n <- notices builder] | not (null (notices builder))]
                    <> keptNote
                    <> [Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- problems builder] | not (null (problems builder))]
                    <> zoneBlock DeckList.Commander "Commander"
                    <> kindBlock "Creatures" DeckBrowser.MkCreature
                    <> kindBlock "Spells" DeckBrowser.MkSpell
                    <> kindBlock "Lands" DeckBrowser.MkLand
                    <> (if pooledSide builder then [] else zoneBlock DeckList.Side "Sideboard")
                    <> pooledBlock
                    <> [Element.el "p" ["empty"] [] [Element.string "Add cards from the search."] | null lines', Map.null (command builder), Map.null (side builder)]
              ]
          )
        ]
          <> pastePanel
          <> Maybe.maybeToList reading'
          <> leave

-- | The builder on a deck list: the saved deck it is, if one, the name to
-- offer for saving, and its text. The main deck, sideboard and commander
-- become counts, each card's printing and annotations its suffix and its
-- choice its colours; every other section's lines are the rest, as
-- written. A Limited list's sideboard is its pool's unused cards
-- (CR 100.4b). Blank lines and lines of no copies drop; a line no card can
-- be read from is noted.
open :: Maybe Text.Text -> Text.Text -> Text.Text -> Builder -> Builder
open savedName offered text builder =
  let step (current, fmt, counts, after, picked, others, unreadable) raw =
        let line = Text.strip raw
         in case (DeckList.sectionNamed line, DeckList.formatNamed line) of
              (Just section, _)
                | section `elem` zones -> (Just section, fmt, counts, after, picked, others, unreadable)
                | otherwise -> (Nothing, fmt, counts, after, picked, raw : others, unreadable)
              -- The first header naming a format names it, as Parun.DeckList
              -- reads it.
              (_, Just named) -> (current, maybe fmt (\f -> Just (Maybe.fromMaybe f fmt)) named, counts, after, picked, others, unreadable)
              -- The deck's name is `offered`, and saving writes it back.
              _ | DeckList.isNameHeader line -> (current, fmt, counts, after, picked, others, unreadable)
              _ -> case current of
                Nothing -> (current, fmt, counts, after, picked, raw : others, unreadable)
                Just section
                  | Text.null line -> (current, fmt, counts, after, picked, others, unreadable)
                  | Just (n, card, at, notes) <- DeckList.entryOf line ->
                      if n == 0
                        then (current, fmt, counts, after, picked, others, unreadable)
                        else
                          let made = Either.rights (fmap (DeckList.choiceOf 0) notes)
                              plain = [note | note <- notes, Either.isLeft (DeckList.choiceOf 0 note)]
                           in ( current,
                                fmt,
                                Map.insertWith (Map.unionWith (+)) section (Map.singleton card n) counts,
                                Map.insertWith (flip (<>)) (section, card) [(suffixOf at plain, suffixOf at notes, n)] after,
                                Map.insertWith (flip (<>)) (section, card) [made] picked,
                                others,
                                unreadable
                              )
                  | otherwise -> (current, fmt, counts, after, picked, others, line : unreadable)
      suffixOf at notes = foldMap (\(set, number) -> Text.pack " (" <> set <> Text.pack ") " <> number) at <> foldMap (\note -> Text.pack " {" <> note <> Text.singleton '}') notes
      (_, fmt', counts', lined, picks, others', unreadable') = List.foldl' step (Just DeckList.Main, Nothing, Map.empty, Map.empty, Map.empty, [], []) (Text.lines text)
      -- CR 207.5: a card's copies whose lines make one choice share it; lines
      -- choosing differently keep each its own, as written.
      agreed key = case Map.findWithDefault [] key picks of
        made : more | all (== made) more, length made <= 1 -> Just (Maybe.listToMaybe made)
        _ -> Nothing
      suffixes' = Map.mapWithKey (\key parts -> [(if Maybe.isJust (agreed key) then plain else full, n) | (plain, full, n) <- parts]) lined
      picked' = Map.mapMaybe id (Map.mapMaybeWithKey (\key _ -> agreed key) lined)
      split' = [CardName.unwrap card | key@(_, card) <- Map.keys lined, Maybe.isNothing (agreed key)]
      format' = Maybe.fromMaybe Text.empty fmt'
      zone section = Map.findWithDefault Map.empty section counts'
      -- CR 100.4b: a limited list's main deck and sideboard are its pool.
      pooled = format' == Text.pack "limited" && not (Map.null (zone DeckList.Side))
      opened =
        builder
          { format = format',
            deck = zone DeckList.Main,
            side = if pooled then Map.empty else zone DeckList.Side,
            command = zone DeckList.Commander,
            target = DeckList.Main,
            suffixes = Map.filter (\parts -> length parts > 1 || not (all (Text.null . fst) parts)) suffixes',
            chosen = picked',
            rest = reverse (dropWhile (Text.null . Text.strip) others'),
            savedAs = savedName,
            saveName = offered,
            problems = [],
            flagged = Set.empty,
            least = Nothing,
            reading = Nothing,
            saveProblems = [],
            notices =
              [Text.pack "Couldn't read this line, which saving drops: " <> line | line <- reverse unreadable']
                <> [Text.pack "The copies of " <> card <> Text.pack " choose differently, so each line keeps its own choice." | card <- split'],
            leaving = False,
            pool = if pooled then Just (Map.unionWith (+) (zone DeckList.Main) (zone DeckList.Side)) else Nothing,
            scope = if pooled then MkPoolCards else MkEveryCard,
            pasting = Nothing,
            poolProblems = []
          }
   in opened {baseline = snapshot opened}

-- | The builder searching a set's cards, by its code, as its list names them.
setPool :: Text.Text -> [CardName.CardName] -> Builder -> Builder
setPool code names builder = builder {scope = MkSetCards code names, pasting = Nothing, poolProblems = []}

-- | The builder with its cards indexed, and every way of naming one known.
withIndex :: Seq.Seq CardIndex.Entry -> Builder -> Builder
withIndex entries builder =
  let known =
        builder
          { -- By name, as the search wants them (CardIndex.search).
            index = Just (Seq.sortOn CardIndex.name entries),
            byName = Map.fromList [(CardIndex.name e, e) | e <- Foldable.toList entries],
            aliases = Map.fromList [(Text.toCaseFold alias, CardIndex.name e) | e <- Foldable.toList entries, alias <- CardName.unwrap (CardIndex.name e) : Text.intercalate (Text.pack " // ") (CardIndex.faceNames e) : CardIndex.faceNames e]
          }
      -- Every card the deck names, renamed as the index names it.
      rename :: Map.Map CardName.CardName Natural -> Map.Map CardName.CardName Natural
      rename = Map.mapKeysWith (+) (canonical known)
      renamePair :: Map.Map (DeckList.Section, CardName.CardName) a -> (a -> a -> a) -> Map.Map (DeckList.Section, CardName.CardName) a
      renamePair m merge = Map.mapKeysWith merge (fmap (canonical known)) m
      (zones', picked, fmt, held') = baseline builder
      -- Each spelling's lines, a bare one's too, so merging keeps each copy's.
      lined = Map.union (suffixes builder) (Map.fromList [((section, card), [(Text.empty, n)]) | section <- zones, (card, n) <- Map.toList (zoneOf section builder)])
      merged = Map.filter (\parts -> length parts > 1 || not (all (Text.null . fst) parts)) (renamePair lined (flip (<>)))
      renamedChoices = renamePair (chosen builder) const
      -- A choice the index says the card does not make is dropped.
      makes (section, card) (choice, _) = Map.notMember card (byName known) || choiceOf known section card == Just choice
      (keptChoices, dropped) = Map.partitionWithKey makes renamedChoices
   in known
        { deck = rename (deck builder),
          side = rename (side builder),
          command = rename (command builder),
          suffixes = merged,
          chosen = keptChoices,
          notices = notices builder <> [CardName.unwrap card <> Text.pack " makes no choice as its deck is built, so its choice is dropped." | (_, card) <- Map.keys dropped],
          pool = fmap rename (pool builder),
          baseline = (fmap rename zones', renamePair picked const, fmt, fmap rename held')
        }

-- | The index's name for a card however a list named it; the name as
-- written for a card the index lacks.
canonical :: Builder -> CardName.CardName -> CardName.CardName
canonical builder card = Map.findWithDefault card (foldName card) (aliases builder)

-- | The choice a card makes as its deck is built, in a zone, if any.
choiceOf :: Builder -> DeckList.Section -> CardName.CardName -> Maybe DeckList.Choice
choiceOf builder section card = Maybe.listToMaybe [choice | (within, choice) <- foldMap (Set.toList . CardIndex.choices) (Map.lookup (canonical builder card) (byName builder)), within == section]

-- | The colours a card in a zone has chosen.
chosenFor :: DeckList.Section -> CardName.CardName -> Builder -> Set.Set Color.Color
chosenFor section card builder = maybe Set.empty snd (Map.lookup (section, card) (chosen builder))

-- | The search as the scope narrows it.
searchQuery :: Builder -> CardIndex.Query
searchQuery builder =
  let among' = case scope builder of
        MkEveryCard -> Nothing
        MkSetCards _ names -> Just names
        MkPoolCards -> fmap Map.keys (pool builder)
   in (query builder) {CardIndex.among = fmap (Set.fromList . fmap (foldName . canonical builder)) among'}

-- | A pool's counts by the index's names.
held :: Builder -> Map.Map CardName.CardName Natural
held builder = Map.fromListWith (+) [(canonical builder card, n) | (card, n) <- foldMap Map.toList (pool builder)]

-- | A card's name as a pool compares it.
foldName :: CardName.CardName -> Text.Text
foldName = Text.toCaseFold . CardName.unwrap

-- | How many of a card the pool holds, when it caps the card: a limited
-- pool's count, none for a card it lacks; never a basic land's (CR 100.2b).
capOf :: Builder -> CardName.CardName -> Maybe Natural
capOf builder card
  | Maybe.isJust (pool builder), not (isBasic builder card) = Just (Map.findWithDefault 0 (canonical builder card) (held builder))
  | otherwise = Nothing

-- | Whether a card is a basic land, by the index, else by name.
isBasic :: Builder -> CardName.CardName -> Bool
isBasic builder card = maybe (card `elem` basics) CardIndex.basic (Map.lookup (canonical builder card) (byName builder))

-- | The cards a list's lines name, whatever section they are in, headers,
-- a format and a name skipped.
entriesOf :: [Text.Text] -> [(CardName.CardName, Natural)]
entriesOf lines' =
  [ (card, n)
  | raw <- lines',
    let line = Text.strip raw,
    not (Text.null line),
    Maybe.isNothing (DeckList.sectionNamed line),
    Maybe.isNothing (DeckList.formatNamed line),
    not (DeckList.isNameHeader line),
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

-- | The pool's cards a Limited deck leaves out, by name, basic lands aside:
-- its sideboard (CR 100.4b).
unused :: Builder -> [(CardName.CardName, Natural)]
unused builder
  | format builder /= Text.pack "limited" = []
  | otherwise =
      let used = Map.fromListWith (+) [(canonical builder card, n) | (card, n) <- Map.toList (deck builder)]
       in [ (card, n - k)
          | (card, n) <- Map.toAscList (held builder),
            not (isBasic builder card),
            let k = Map.findWithDefault 0 card used,
            n > k
          ]
