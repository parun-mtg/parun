-- Answering one question: given the text a player would actually type, what
-- deck is that?
--
-- A deck list is a count and a card name per line, optionally naming a printing
-- by set code and collector number, with each thing a player brings beside
-- the deck (Parun.Types.Deck's fields) written as a section of its own. Every
-- name, printing and sticker sheet is resolved through a Parun.Registry, which
-- is the only thing that knows what cards and sheets exist; this module decides
-- what a LINE means and never what a name means.
--
-- Here rather than in the test suite or the executable because both of those
-- already build their decks in Haskell and a deck list is what they cannot
-- say. Here rather than in `types` because resolving a name needs a registry.
module Parun.DeckList where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Card as EngineCard
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Ingest as Ingest
import qualified Parun.Registry as Registry
import qualified Parun.Slug as Slug
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingEntry as PrintingEntry
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.StickerSheet as StickerSheet
import qualified Text.Read as Read

-- | Why a deck list's line did not become a deck entry, and which line it was.
-- Every arm carries the one-based line number, since a report that cannot say
-- WHERE is not worth reading.
--
-- A returned value rather than an exception, as a registry's lookup is. Every
-- bad line is reported, not just the first.
data Problem
  = -- | A line that is neither blank, a section header, nor a count and a name.
    Unreadable Natural.Natural Text.Text
  | -- | A name -- or, for a joined name, one of its halves -- that no card has.
    NoSuchCard Natural.Natural CardName.CardName
  | -- | CR 709.4a: a joined name whose halves name two different cards, so the
    -- line names no one card.
    HalvesDisagree Natural.Natural CardName.CardName
  | -- | A format header after the one that already named the list's format.
    FormatRepeated Natural.Natural Text.Text
  | -- | A format header naming one format when another was asked for.
    FormatDisagrees Natural.Natural Text.Text Text.Text
  | -- | A set code and collector number the registry knows no printing at.
    NoSuchPrinting Natural.Natural Text.Text Text.Text
  | -- | A set code and collector number printing a card other than the line's.
    PrintingDisagrees Natural.Natural CardName.CardName Text.Text Text.Text
  | -- | CR 902.1: a vanguard card after the one the list already brings.
    VanguardRepeated Natural.Natural CardName.CardName
  | -- | CR 123.2: a name in the Stickers section that no sticker sheet has.
    NoSuchStickerSheet Natural.Natural Text.Text
  deriving (Eq, Ord, Show)

-- | A problem as a line a person can read.
explain :: Problem -> Text.Text
explain problem =
  let at n = Text.pack ("line " <> show n <> ": ")
   in case problem of
        Unreadable n content -> at n <> Text.pack "not a count and a card name: " <> content
        NoSuchCard n name -> at n <> Text.pack "no card is named " <> CardName.unwrap name
        HalvesDisagree n name -> at n <> Text.pack "the halves of " <> CardName.unwrap name <> Text.pack " are different cards"
        FormatRepeated n name -> at n <> Text.pack "the format is already named, so not " <> name
        FormatDisagrees n named asked -> at n <> Text.pack "the list names the format " <> named <> Text.pack ", not " <> asked
        NoSuchPrinting n set number -> at n <> Text.pack "nothing is printed at " <> suffix (set, number)
        PrintingDisagrees n name set number -> at n <> Text.pack "the printing at " <> suffix (set, number) <> Text.pack " is not " <> CardName.unwrap name
        VanguardRepeated n name -> at n <> Text.pack "the list already brings a vanguard, so not " <> CardName.unwrap name
        NoSuchStickerSheet n name -> at n <> Text.pack "no sticker sheet is named " <> name

-- | Which part of what a player brings the lines that follow belong to: the
-- deck or one of Parun.Types.Deck's fields beside it.
data Section
  = -- | Deck.cards.
    Main
  | -- | CR 100.4: Deck.sideboard.
    Side
  | -- | CR 702.139a: a companion, which is brought in the sideboard, as Arena
    -- exports it.
    Companion
  | -- | CR 903.3: Deck.commander.
    Commander
  | -- | CR 718.2, 901.3, 904.3: one of Deck.sideDecks.
    Supplementary SideDeck.SideDeck
  | -- | CR 315.2: Deck.conspiracies.
    Conspiracies
  | -- | CR 309.2: Deck.dungeons.
    Dungeons
  | -- | CR 902.3: Deck.vanguard.
    Vanguard
  | -- | CR 123.2: Deck.stickerSheets.
    Stickers
  deriving (Eq, Ord, Show)

-- | Every section, in the order `render` writes them.
sections :: [Section]
sections = [Main, Side, Companion, Commander] <> fmap Supplementary [minBound .. maxBound] <> [Conspiracies, Dungeons, Vanguard, Stickers]

-- | The header a section is written under.
heading :: Section -> Text.Text
heading section = Text.pack $ case section of
  Main -> "Deck"
  Side -> "Sideboard"
  Companion -> "Companion"
  Commander -> "Commander"
  Supplementary sideDeck -> show sideDeck <> " Deck"
  Conspiracies -> "Conspiracies"
  Dungeons -> "Dungeons"
  Vanguard -> "Vanguard"
  Stickers -> "Stickers"

-- | What a deck list says: the deck its good lines describe, the format its
-- header names, and every line that did not become part of either. A list with
-- a bad line still has a deck, so it can be checked against its format in the
-- same report (#6396).
data Listed = MkListed
  { -- | The deck the resolved lines add up to.
    deck :: Deck.Deck,
    -- | The line of the list's format header and the format it names.
    format :: Maybe (Natural.Natural, Text.Text),
    -- | Every bad line, in order.
    problems :: [Problem]
  }
  deriving (Eq, Show)

-- | What a deck list says; outside it, the registry's failure to read a card
-- file a lookup reached.
--
-- Each section fills its field. Which commanders or vanguards are legal is the
-- format's business, not the list's.
parse :: (Monad m) => Registry.Registry m -> Text.Text -> m (Either InvalidCorpus.InvalidCorpus Listed)
parse registry text = do
  let first = firstPrintings (Registry.printings registry)
      rows = Text.lines text
      -- The section each line is read in: the one the headers before it name.
      within = List.scanl (\section line -> Maybe.fromMaybe section (sectionNamed (Text.strip line))) Main rows
  looked <- mapM (lineOf registry first) (List.zip3 [1 ..] within rows)
  pure (fmap assemble (sequence looked))

-- What one line of a deck list means.
data Line
  = -- | Nothing.
    Blank
  | -- | The section the lines after it belong to.
    Header Section
  | -- | The format the list is for.
    Named Text.Text
  | -- | A count, the name written and the printing it names.
    Entry Natural.Natural CardName.CardName Printing.Printing
  | -- | A count and the sticker sheet it names.
    Sheet Natural.Natural StickerSheet.StickerSheet

-- The deck the lines' meanings add up to, the format the first header names,
-- and every line that is neither.
assemble :: [(Natural.Natural, Either Problem Line)] -> Listed
assemble results =
  let step (section, entries, sheets, named, bad) (number, result) = case result of
        Left problem -> (section, entries, sheets, named, problem : bad)
        Right Blank -> (section, entries, sheets, named, bad)
        Right (Header next) -> (next, entries, sheets, named, bad)
        Right (Named name) -> case named of
          Nothing -> (section, entries, sheets, Just (number, name), bad)
          Just _ -> (section, entries, sheets, named, FormatRepeated number name : bad)
        Right (Entry count name printing)
          | count == 0 -> (section, entries, sheets, named, bad)
          | section == Vanguard && (count > 1 || any (\(s, _, _) -> s == Vanguard) entries) -> (section, entries, sheets, named, VanguardRepeated number name : bad)
          | otherwise -> (section, (section, printing, count) : entries, sheets, named, bad)
        -- In the order written: a player's sheets are told apart by position
        -- (Parun.Types.StickerRef, CR 123.3a).
        Right (Sheet count sheet) -> (section, entries, sheets <> Seq.replicate (Natural.toIntSaturating count) sheet, named, bad)
      (_, found, brought, header, reported) = List.foldl' step (Main, [], Seq.empty, Nothing, []) results
      gather want = Map.fromListWith (+) [(printing, count) | (section, printing, count) <- found, section `elem` want]
      once want = Map.keysSet (gather [want])
      sideDecks = Map.filter (not . Map.null) (Map.fromList [(sideDeck, gather [Supplementary sideDeck]) | sideDeck <- [minBound .. maxBound]])
   in MkListed
        { deck =
            (Deck.fromCards (gather [Main]))
              { Deck.sideboard = gather [Side, Companion],
                Deck.commander = once Commander,
                Deck.sideDecks = sideDecks,
                Deck.conspiracies = gather [Conspiracies],
                Deck.dungeons = once Dungeons,
                Deck.vanguard = Set.lookupMin (once Vanguard),
                Deck.stickerSheets = brought
              },
          format = header,
          problems = List.reverse reported
        }

-- | The deck of a list with no bad line, or every bad line: for a caller that
-- plays the deck rather than reporting on it.
sound :: Listed -> Either [Problem] Deck.Deck
sound listed = case problems listed of
  [] -> Right (deck listed)
  bad -> Left bad

-- | The format a list is checked against: the one asked for, else the one its
-- header names. A header naming another is a problem rather than silently
-- overruled, and the format asked for is still the one checked.
formatFor :: Maybe Text.Text -> Listed -> (Maybe Text.Text, [Problem])
formatFor asked listed = case (asked, format listed) of
  (Just wanted, Just (number, named)) | wanted /= named -> (asked, [FormatDisagrees number named wanted])
  (Just _, _) -> (asked, [])
  (Nothing, header) -> (fmap snd header, [])

-- What one line, read in a section, means: nothing (blank), the section the
-- lines after it belong to, the format the list is for, or a count and the
-- printing -- or, in the Stickers section, the sticker sheet -- it names.
lineOf :: (Monad m) => Registry.Registry m -> Map.Map Text.Text PrintingEntry.PrintingEntry -> (Natural.Natural, Section, Text.Text) -> m (Either InvalidCorpus.InvalidCorpus (Natural.Natural, Either Problem Line))
lineOf registry first (number, within, raw) =
  let trimmed = Text.strip raw
   in if Text.null trimmed
        then pure (Right (number, Right Blank))
        else case (sectionNamed trimmed, formatNamed trimmed) of
          (Just section, _) -> pure (Right (number, Right (Header section)))
          (_, Just named) -> pure (Right (number, maybe (Left (Unreadable number trimmed)) (Right . Named) named))
          _ | within == Stickers -> pure (Right (number, sheetOf (Registry.stickerSheets registry) number trimmed))
          _ -> case entryOf trimmed of
            Nothing -> pure (Right (number, Left (Unreadable number trimmed)))
            Just (count, name, at) -> fmap (fmap ((,) number . (>>= fmap (Entry count name) . printingOf (Registry.printings registry) first number name at))) (resolve registry number name)

-- What a line of the Stickers section means: a count and the sheet it names,
-- by name or slug, as a card is looked up (Parun.Registry.slugFor). No
-- printing suffix: a sheet has one printing.
sheetOf :: Map.Map Slug.Slug StickerSheet.StickerSheet -> Natural.Natural -> Text.Text -> Either Problem Line
sheetOf sheets number text = case countOf text of
  Nothing -> Left (Unreadable number text)
  Just (count, name) -> maybe (Left (NoSuchStickerSheet number name)) (Right . Sheet count) (Map.lookup (Slug.fromText name) sheets)

-- The format a "Format:" header names, lowercased as data/formats files are
-- named; Just Nothing for a header naming none. No card's name begins
-- "Format:", so a header never hides a card.
formatNamed :: Text.Text -> Maybe (Maybe Text.Text)
formatNamed text =
  let (key, rest) = Text.breakOn (Text.singleton ':') text
      named = Text.toLower (Text.strip (Text.drop 1 rest))
   in if Text.toLower (Text.strip key) == Text.pack "format" && not (Text.null rest)
        then Just (if Text.null named then Nothing else Just named)
        else Nothing

-- The section a header line names, if it is one: a section's `heading`, or
-- another spelling of the deck's. Everything else on a line of its own is a
-- card name with an implicit count of one, so this set stays closed rather
-- than swallowing lines it does not understand. No card is named for a
-- heading, so a header never hides a card.
sectionNamed :: Text.Text -> Maybe Section
sectionNamed text =
  let spelled = Text.toLower (Text.strip (Text.filter (/= ':') text))
   in lookup spelled ([(Text.toLower (heading section), section) | section <- sections] <> [(Text.pack "maindeck", Main), (Text.pack "main deck", Main)])

-- A line's count, the name it precedes and the printing it names, if any. A
-- bare name is one copy, which is how every export writes a singleton deck's
-- list.
entryOf :: Text.Text -> Maybe (Natural.Natural, CardName.CardName, Maybe (Text.Text, Text.Text))
entryOf text = fmap (\(count, (name, at)) -> (count, CardName.MkCardName name, at)) (countOf text >>= traverse (Just . printingNamed))

-- A line's count and the text after it.
countOf :: Text.Text -> Maybe (Natural.Natural, Text.Text)
countOf text =
  let (digits, rest) = Text.span Char.isDigit text
      named = Text.strip
   in if Text.null digits
        then Just (1, named text)
        else do
          count <- Read.readMaybe (Text.unpack digits)
          -- "4Bolt" is not four Bolts: a count is a word of its own, and the
          -- alternative reading would silently accept a name beginning with a
          -- digit as a count plus the rest of itself.
          stripped <- if Text.null rest || Char.isSpace (Text.head rest) then Just (Text.strip rest) else Nothing
          if Text.null stripped then Nothing else Just (count, named stripped)

-- A name and the printing an Arena or MTGO export's suffix names, "(M10) 146":
-- a set code in parentheses and a collector number, each one word. The set
-- code is upper-cased, as MTGJSON writes it.
printingNamed :: Text.Text -> (Text.Text, Maybe (Text.Text, Text.Text))
printingNamed text = case List.reverse (Text.words text) of
  number : code : rest@(_ : _)
    | Just set <- Text.stripPrefix (Text.singleton '(') code >>= Text.stripSuffix (Text.singleton ')'),
      not (Text.null set) ->
        (Text.unwords (List.reverse rest), Just (Text.toUpper set, number))
  _ -> (text, Nothing)

-- The printing a line means of the card its name resolved to: the one at the
-- set code and collector number it names, else the first the table holds of
-- the card (`firstPrintings`), else the card with no lights.
--
-- Not implemented: a line circling Cryptic Spires' colours or choosing a
-- commander's colour (CR 903.4b), which every line leaves with none (#6399).
--
-- The printing at a suffix must be of the line's card, by Parun.Ingest.cardKey,
-- the key MTGJSON names it by.
printingOf :: PrintingTable.PrintingTable -> Map.Map Text.Text PrintingEntry.PrintingEntry -> Natural.Natural -> CardName.CardName -> Maybe (Text.Text, Text.Text) -> Card.Card -> Either Problem Printing.Printing
printingOf table first number name at card =
  let lit entry = (Printing.ofCard card) {Printing.lights = PrintingEntry.lights entry}
   in case at of
        Nothing -> Right (maybe (Printing.ofCard card) lit (Map.lookup (Ingest.cardKey card) first))
        Just (set, collector) -> case Map.lookup set (PrintingTable.printings table) >>= Map.lookup collector of
          Nothing -> Left (NoSuchPrinting number set collector)
          Just entry
            | PrintingEntry.card entry /= Ingest.cardKey card -> Left (PrintingDisagrees number name set collector)
            | otherwise -> Right (lit entry)

-- Each card's first printing in the table, by set code then collector number:
-- the printing a bare name means (docs/decisions.md, "Which printing a deck
-- list's bare name brings").
firstPrintings :: PrintingTable.PrintingTable -> Map.Map Text.Text PrintingEntry.PrintingEntry
firstPrintings table = Map.fromListWith (\_ earlier -> earlier) [(PrintingEntry.card entry, entry) | entry <- foldMap Map.elems (Map.elems (PrintingTable.printings table))]

-- The set code and collector number a printing is written with: the first the
-- table holds of its card with its lights. Nothing for a printing with no
-- lights, which every printing of its card shares, and for one the table
-- does not hold, which is written as its bare name.
suffixFor :: PrintingTable.PrintingTable -> Printing.Printing -> Maybe (Text.Text, Text.Text)
suffixFor table printing
  | Set.null (Printing.lights printing) = Nothing
  | otherwise =
      Maybe.listToMaybe
        [ (set, collector)
        | (set, byNumber) <- Map.toAscList (PrintingTable.printings table),
          (collector, entry) <- Map.toAscList byNumber,
          PrintingEntry.card entry == Ingest.cardKey (Printing.card printing),
          PrintingEntry.lights entry == Printing.lights printing
        ]

-- A set code and collector number as a line's suffix writes them.
suffix :: (Text.Text, Text.Text) -> Text.Text
suffix (set, number) = Text.singleton '(' <> set <> Text.pack ") " <> number

-- The card a deck list's name means.
--
-- CR 709.4a gives a split card two names and no combined one, but every deck
-- list writes both joined ("Wax // Wane"). The joined string is not a name a
-- card has, so the registry rightly refuses it (#649); splitting it here is
-- what lets a deck list say what players write without the lookup learning
-- about "//" again. Both halves are resolved and must agree, so a line naming
-- two cards is an error rather than a silent choice of the first.
--
-- Splitting on "//" and trimming reconciles the two spellings in the tree at
-- once: the printed, spaced "Wax // Wane" and Parun.Types.CardName.join's
-- unspaced "Wax//Wane", which is what data/comprehensive-rules.txt's own
-- examples use.
resolve :: (Monad m) => Registry.Registry m -> Natural.Natural -> CardName.CardName -> m (Either InvalidCorpus.InvalidCorpus (Either Problem Card.Card))
resolve registry number name = do
  looked <- mapM (Registry.fetchCard registry) (halvesOf name)
  pure . flip fmap (sequence looked) $ \found -> case sequence found of
    Nothing -> Left (NoSuchCard number name)
    Just (card NonEmpty.:| rest)
      | all (== card) rest -> Right card
      | otherwise -> Left (HalvesDisagree number name)

-- The names a written name joins, which for every name but a split card's is
-- the one name itself.
halvesOf :: CardName.CardName -> NonEmpty.NonEmpty CardName.CardName
halvesOf name =
  let parts = filter (not . Text.null) (fmap Text.strip (Text.splitOn (Text.pack "//") (CardName.unwrap name)))
   in case NonEmpty.nonEmpty parts of
        Nothing -> NonEmpty.singleton name
        Just halves -> fmap CardName.MkCardName halves

-- | A deck as a deck list, headed by the format it is for if any, each
-- printing with lights named by its set code and collector number in
-- `table`, which `parse` reads back. A companion is written in the sideboard
-- it is brought in.
--
-- Sorted by the line written, so the text is a fact about the deck rather than
-- about the order a Map enumerates its structural keys in. Sticker sheets are
-- the exception, written in their order (`assemble`).
render :: PrintingTable.PrintingTable -> Maybe Text.Text -> Deck.Deck -> Text.Text
render table header listed =
  let entries m = fmap line (List.sortOn snd [(count, writtenAt printing) | (printing, count) <- Map.toList m])
      writtenAt printing = written (Printing.card printing) <> foldMap ((Text.singleton ' ' <>) . suffix) (suffixFor table printing)
      line (count, name) = Text.pack (show count) <> Text.singleton ' ' <> name
      once = Map.fromSet (const (1 :: Natural.Natural))
      contents section = case section of
        Main -> entries (Deck.cards listed)
        Side -> entries (Deck.sideboard listed)
        Companion -> []
        Commander -> entries (once (Deck.commander listed))
        Supplementary sideDeck -> entries (Map.findWithDefault Map.empty sideDeck (Deck.sideDecks listed))
        Conspiracies -> entries (Deck.conspiracies listed)
        Dungeons -> entries (once (Deck.dungeons listed))
        Vanguard -> entries (once (Set.fromList (Maybe.maybeToList (Deck.vanguard listed))))
        Stickers -> fmap (\run -> line (Natural.length run, StickerSheet.name (NonEmpty.head run))) (NonEmpty.group (Deck.stickerSheets listed))
      block section = case contents section of
        [] -> []
        written' -> Text.empty : heading section : written'
      top = foldMap (\named -> [Text.pack "Format: " <> named, Text.empty]) header
      body = top <> contents Main <> foldMap block (filter (/= Main) sections)
   in Text.unlines body

-- A card's name as a deck list writes it: the names of the faces it shows off
-- the stack (Parun.Engine.Card.combinedFaces), joined by a SPACED "//", which is
-- how every printing and every export spells a split card's name (#679).
-- Parun.Types.CardName.join writes the same names unspaced, which is
-- data/comprehensive-rules.txt's spelling; `halvesOf` accepts either. Every
-- other layout shows one face (CR 710.2, 712.6a, 713.1b, 716.4, 721.4, 723.4),
-- so its inset or back face's name, which another card may share, is not
-- written.
written :: Card.Card -> Text.Text
written = Text.intercalate (Text.pack " // ") . NonEmpty.toList . fmap (CardName.unwrap . Face.name) . EngineCard.combinedFaces
