-- Answering one question: given the text a player would actually type, what
-- deck is that?
--
-- A deck list is a count and a card name per line, with CR 100.4's sideboard
-- written as a section of its own. Every name is resolved through a
-- Parun.Registry, which is the only thing that knows what cards exist; this
-- module decides what a LINE means and never what a name means.
--
-- Here rather than in the test suite or the executable because both of those
-- already build their decks in Haskell and a deck list is what they cannot
-- say. Here rather than in `types` because resolving a name needs a registry.
module Parun.DeckList where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Card as EngineCard
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Printing as Printing
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

-- | Which of a deck list's two sections the lines that follow belong to. CR
-- 100.4 makes the sideboard a group of cards beside the deck rather than part
-- of it, which is why Parun.Types.Deck keeps the two apart.
data Section
  = Main
  | Side
  deriving (Eq, Ord, Show)

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
-- The two sections a deck list has are the two fields this fills. CR 903.3's
-- commander, CR 902.3's vanguard card and CR 309.2's dungeon cards are brought
-- alongside a deck rather than written on its list, so a caller in one of those
-- formats designates them itself.
--
-- Not implemented: CR 718.2's Attraction deck and the Unstable FAQ's Contraption
-- deck, and naming which printing of an Attraction a line means, whose lights
-- CR 718.1 lets differ (#4118).
parse :: (Monad m) => Registry.Registry m -> Text.Text -> m (Either InvalidCorpus.InvalidCorpus Listed)
parse registry text = do
  looked <- mapM (lineOf registry) (List.zip [1 ..] (Text.lines text))
  pure (fmap assemble (sequence looked))

-- What one line of a deck list means.
data Line
  = -- | Nothing.
    Blank
  | -- | The section the lines after it belong to.
    Header Section
  | -- | The format the list is for.
    Named Text.Text
  | -- | A count and the card it names.
    Entry Natural.Natural Card.Card

-- The deck the lines' meanings add up to, the format the first header names,
-- and every line that is neither.
assemble :: [(Natural.Natural, Either Problem Line)] -> Listed
assemble results =
  let step (section, entries, named, bad) (number, result) = case result of
        Left problem -> (section, entries, named, problem : bad)
        Right Blank -> (section, entries, named, bad)
        Right (Header next) -> (next, entries, named, bad)
        Right (Named name) -> case named of
          Nothing -> (section, entries, Just (number, name), bad)
          Just _ -> (section, entries, named, FormatRepeated number name : bad)
        Right (Entry count card) -> (section, (section, card, count) : entries, named, bad)
      (_, found, header, reported) = List.foldl' step (Main, [], Nothing, []) results
      gather want = Map.fromListWith (+) [(Printing.ofCard card, count) | (section, card, count) <- found, section == want, count /= 0]
   in MkListed
        { deck = (Deck.fromCards (gather Main)) {Deck.sideboard = gather Side},
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

-- What one line means: nothing (blank), the section the lines after it belong
-- to, the format the list is for, or a count and the card it names.
lineOf :: (Monad m) => Registry.Registry m -> (Natural.Natural, Text.Text) -> m (Either InvalidCorpus.InvalidCorpus (Natural.Natural, Either Problem Line))
lineOf registry (number, raw) =
  let trimmed = Text.strip raw
   in if Text.null trimmed
        then pure (Right (number, Right Blank))
        else case (sectionNamed trimmed, formatNamed trimmed) of
          (Just section, _) -> pure (Right (number, Right (Header section)))
          (_, Just named) -> pure (Right (number, maybe (Left (Unreadable number trimmed)) (Right . Named) named))
          _ -> case entryOf trimmed of
            Nothing -> pure (Right (number, Left (Unreadable number trimmed)))
            Just (count, name) -> fmap (fmap ((,) number . fmap (Entry count))) (resolve registry number name)

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

-- The section a header line names, if it is one. Only the two headers a deck
-- list actually carries: everything else on a line of its own is a card name
-- with an implicit count of one, so this set stays closed rather than
-- swallowing lines it does not understand.
sectionNamed :: Text.Text -> Maybe Section
sectionNamed text = case Text.unpack (Text.toLower (Text.filter (/= ':') text)) of
  "deck" -> Just Main
  "maindeck" -> Just Main
  "main deck" -> Just Main
  "sideboard" -> Just Side
  _ -> Nothing

-- A line's count and the name it precedes. A bare name is one copy, which is
-- how every export writes a singleton deck's list.
--
-- Everything after the count is the name, so an export that appends a set code
-- and a collector number ("4 Lightning Bolt (M10) 146") names no card here and
-- is reported as such rather than guessed at.
entryOf :: Text.Text -> Maybe (Natural.Natural, CardName.CardName)
entryOf text =
  let (digits, rest) = Text.span Char.isDigit text
      named = CardName.MkCardName . Text.strip
   in if Text.null digits
        then Just (1, named text)
        else do
          count <- Read.readMaybe (Text.unpack digits)
          -- "4Bolt" is not four Bolts: a count is a word of its own, and the
          -- alternative reading would silently accept a name beginning with a
          -- digit as a count plus the rest of itself.
          stripped <- if Text.null rest || Char.isSpace (Text.head rest) then Just (Text.strip rest) else Nothing
          if Text.null stripped then Nothing else Just (count, named stripped)

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

-- | A deck as a deck list, headed by the format it is for if any, which
-- `parse` reads back.
--
-- Sorted by the name written, so the text is a fact about the deck rather than
-- about the order a Map enumerates its structural keys in.
render :: Maybe Text.Text -> Deck.Deck -> Text.Text
render header listed =
  let entries m = fmap line (List.sortOn snd [(count, written (Printing.card printing)) | (printing, count) <- Map.toList m])
      line (count, name) = Text.pack (show count) <> Text.singleton ' ' <> name
      side = entries (Deck.sideboard listed)
      heading = foldMap (\named -> [Text.pack "Format: " <> named, Text.empty]) header
      body = heading <> entries (Deck.cards listed) <> if null side then [] else Text.empty : Text.pack "Sideboard" : side
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
