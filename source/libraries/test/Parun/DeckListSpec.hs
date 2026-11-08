-- Covers Parun.DeckList. Every case builds its own corpus in a temporary
-- directory, for Parun.RegistrySpec's reason: what a deck list has to resolve is
-- a name, and a two-faced card plus a one-faced one is the whole vocabulary
-- these cases need.
module Parun.DeckListSpec where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.DeckList as DeckList
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.StickerSheets as StickerSheets
import qualified Parun.Support as S
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingEntry as PrintingEntry
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.StickerSheet as StickerSheet

-- A registry over Goblin Piker and Wax // Wane, which between them are a name
-- with no halves and CR 709.4a's name with two, two preparation cards
-- sharing the prepare spell Seed Suture, whose name is therefore no card's,
-- and Bumper Cars, whose printings light different numbers (CR 718.1), with
-- `printings` for its printing table and the sticker sheets parun ships.
withPool :: String -> (Registry.Registry IO -> IO a) -> IO a
withPool label action = do
  sheets <- StickerSheets.defaultRoot >>= StickerSheets.loadRoot
  bySlug <- either (\(file, problem) -> ioError (userError (file <> ": " <> Text.unpack problem))) pure (Registry.sheetsBySlug sheets)
  piker <- S.pikerJson
  waxWane <- S.waxWaneJson
  angel <- S.committedJson angelFile
  phytomedic <- S.committedJson "emergency-phytomedic-seed-suture.json"
  cars <- S.committedJson "bumper-cars.json"
  S.withCorpusDir label [("goblin-piker.json", piker), ("wax-wane.json", waxWane), (angelFile, angel), ("emergency-phytomedic-seed-suture.json", phytomedic), ("bumper-cars.json", cars)] (Registry.fileRegistry Monad.>=> action . withData bySlug)
  where
    angelFile = "blossom-blessed-angel-seed-suture.json"
    withData bySlug registry = registry {Registry.printings = printings, Registry.stickerSheets = bySlug}

-- Two Bumper Cars printings and a Goblin Piker, as data/printings.json holds
-- them.
printings :: PrintingTable.PrintingTable
printings =
  PrintingTable.MkPrintingTable
    (Text.pack "2026-11-07")
    ( Map.fromList
        [ (Text.pack "M10", Map.singleton (Text.pack "146") (entry "Goblin Piker" [])),
          (Text.pack "UNF", Map.fromList [(Text.pack "202a", entry "Bumper Cars" [2, 3, 6]), (Text.pack "202f", entry "Bumper Cars" [4, 5, 6])])
        ]
    )
  where
    entry name lights = PrintingEntry.MkPrintingEntry (Text.pack name) (Set.fromList lights)

-- The lights of each printing in a deck's Attraction deck, by count.
attractionLights :: Deck.Deck -> [(Set.Set Natural.Natural, Natural.Natural)]
attractionLights deck = [(Printing.lights printing, count) | (printing, count) <- Map.toList (Map.findWithDefault Map.empty SideDeck.Attraction (Deck.sideDecks deck))]

-- A section's contents as sorted (name, count) pairs. Named through
-- Parun.Engine.Card.combined rather than Parun.DeckList.written, so what an
-- assertion reads is not the same function the parser and renderer agree by.
entriesOf :: Map.Map Printing.Printing Natural.Natural -> [(Text.Text, Natural.Natural)]
entriesOf cards = List.sort [(CardName.unwrap (S.nameOf (Printing.card printing)), count) | (printing, count) <- Map.toList cards]

-- The deck a list means, failing the case rather than escaping when a line did
-- not resolve.
deckOf :: Spec.Spec IO n -> String -> Registry.Registry IO -> String -> IO Deck.Deck
deckOf s label registry text = do
  result <- DeckList.parse registry (Text.pack text)
  case fmap DeckList.sound result of
    Right (Right deck) -> pure deck
    Right (Left problems) -> Spec.assertFailure s (label <> ": " <> show (fmap DeckList.explain problems))
    Left corpus -> S.brokenCorpus s corpus

-- What a list says, failing the case rather than escaping when the corpus
-- does not read.
listedOf :: Spec.Spec IO n -> Registry.Registry IO -> String -> IO DeckList.Listed
listedOf s registry text = DeckList.parse registry (Text.pack text) >>= either (S.brokenCorpus s) pure

-- The problems a list raises, failing the case when it unexpectedly parses.
problemsOf :: Spec.Spec IO n -> String -> Registry.Registry IO -> String -> IO [DeckList.Problem]
problemsOf s label registry text = do
  listed <- listedOf s registry text
  case DeckList.problems listed of
    [] -> Spec.assertFailure s (label <> ": expected the list to be rejected")
    problems -> pure problems

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.DeckList" $ do
  Spec.it s "a count and a name become that many copies of one card"
    . withPool "counted"
    $ \registry -> do
      deck <- deckOf s "counted" registry "4 Goblin Piker\n"
      Spec.assertEqWith s "four Pikers" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]

  -- Blank lines separate sections in every export, and the same name on two
  -- lines is one entry: Parun.Types.Deck.cards is a multiset, so a list writing
  -- "2 Piker" twice means four rather than the later line winning.
  Spec.it s "blank lines are skipped and repeated names add up"
    . withPool "repeated"
    $ \registry -> do
      deck <- deckOf s "repeated" registry "2 Goblin Piker\n\n2 Goblin Piker\n"
      Spec.assertEqWith s "two and two" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]

  Spec.it s "a bare name is one copy"
    . withPool "bare"
    $ \registry -> do
      deck <- deckOf s "bare" registry "Goblin Piker\n"
      Spec.assertEqWith s "one Piker" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 1)]

  -- CR 709.4a gives a split card two names and no combined one, and
  -- Parun.RegistrySpec's "a split card is found by either of its names" pins
  -- that the joined string is not one a lookup may ask for. A deck list writes
  -- exactly that joined string, so splitting it is the parser's job (#679).
  -- The two spellings are the printed, spaced one and Parun.Types.CardName.join's
  -- unspaced one, which is what data/comprehensive-rules.txt's examples use;
  -- both are lists a person could hand parun.
  Spec.it s "CR 709.4a a split card's joined name resolves, spaced or not"
    . withPool "joined"
    $ \registry -> do
      -- Either half on its own is already a name the card has (#649), so this
      -- is the deck the two joined spellings have to mean as well. Read
      -- through the parser rather than built by hand so the comparison is
      -- between two deck lists rather than between a list and a fixture.
      half <- deckOf s "half" registry "3 Wane\n"
      Spec.assertEqWith s "one card, three times" (entriesOf (Deck.cards half)) [(Text.pack "Wax//Wane", 3)]
      spaced <- DeckList.parse registry (Text.pack "3 Wax // Wane\n")
      Spec.assertEqWith s "the printed spelling is that same deck" (fmap DeckList.sound spaced) (Right (Right half))
      unspaced <- DeckList.parse registry (Text.pack "3 Wax//Wane\n")
      Spec.assertEqWith s "and so is the unspaced spelling" (fmap DeckList.sound unspaced) (Right (Right half))

  -- A joined name whose halves are two cards names no one card. Without this
  -- the parser could take the first half and silently drop the rest, which
  -- would make "Wax // Goblin Piker" a legal way to write three Waxes.
  Spec.it s "CR 709.4a a joined name whose halves disagree is rejected"
    . withPool "disagree"
    $ \registry -> do
      problems <- problemsOf s "disagree" registry "3 Wax // Goblin Piker\n"
      Spec.assertEqWith s "the one line" problems [DeckList.HalvesDisagree 1 (CardName.MkCardName (Text.pack "Wax // Goblin Piker"))]

  -- CR 100.4: a sideboard is a group of cards beside the deck rather than part
  -- of it, which is why Parun.Types.Deck keeps the two apart. Distinct cards on
  -- the two sides, so a parser that put everything in one field could not pass.
  Spec.it s "CR 100.4 lines after a sideboard header are the sideboard"
    . withPool "sideboard"
    $ \registry -> do
      deck <- deckOf s "sideboard" registry "4 Goblin Piker\n\nSideboard\n2 Wax // Wane\n"
      Spec.assertEqWith s "the deck" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]
      Spec.assertEqWith s "and the sideboard" (entriesOf (Deck.sideboard deck)) [(Text.pack "Wax//Wane", 2)]

  -- A name no file is named for reaches the whole root, and a malformed file
  -- there is the registry's failure rather than a line's problem: the list
  -- is not a deck with one bad line, it is a list the card data cannot read.
  Spec.it s "a name that reaches a malformed card file fails the parse, naming the file" $ do
    piker <- S.pikerJson
    S.withCorpusDir "deck-list-malformed" [("goblin-piker.json", piker), ("bird-maiden.json", Text.pack "{oh no")] $ \dir -> do
      registry <- Registry.fileRegistry dir
      broken <- DeckList.parse registry (Text.pack "4 Goblin Piker\n2 Shivan Dragon\n")
      Spec.assertEqWith s "the registry's failure, naming the file" (fmap (fmap (List.isInfixOf "bird-maiden.json") . InvalidCorpus.problems) (either Just (const Nothing) broken)) (Just [True])
      sound <- DeckList.parse registry (Text.pack "4 Goblin Piker\n")
      Spec.assertBool s (either (const False) (null . DeckList.problems) sound) "a list reaching only the good file reads"

  -- Every bad line is reported rather than the first, for Parun.Registry.index's
  -- reason: a report naming one offender at a time is read once per offender.
  Spec.it s "an unknown name is reported with its line, and every bad line is named"
    . withPool "unknown"
    $ \registry -> do
      problems <- problemsOf s "unknown" registry "4 Goblin Piker\n2 Shivan Dragon\n4Bolt\n"
      Spec.assertEqWith
        s
        "the second line and the third, not the first"
        problems
        [ DeckList.NoSuchCard 2 (CardName.MkCardName (Text.pack "Shivan Dragon")),
          DeckList.Unreadable 3 (Text.pack "4Bolt")
        ]

  -- The rendered list is itself a deck list, which is what makes the output of
  -- `parun deck` something a person can hand back. A split card renders in the
  -- printed, spaced spelling, so the round trip is what pins that the parser
  -- accepts it (#679). A preparation card renders by its front face alone (CR
  -- 723.4): its prepare spell Seed Suture is shared, so a joined name would
  -- name no card.
  Spec.it s "a rendered deck parses back to the same deck"
    . withPool "round-trip"
    $ \registry -> do
      deck <- deckOf s "round-trip" registry "4 Goblin Piker\n1 Blossom-Blessed Angel\n\nSideboard\n2 Wax // Wane\n"
      let text = DeckList.render (Registry.printings registry) Nothing deck
      Spec.assertBool s (Text.isInfixOf (Text.pack "2 Wax // Wane") text) ("the printed spelling: " <> show text)
      again <- deckOf s "round-trip-again" registry (Text.unpack text)
      Spec.assertEqWith s "the same deck" again deck
      Spec.assertBool s (Text.isInfixOf (Text.pack "1 Blossom-Blessed Angel\n") text) ("CR 723.4: the front name alone: " <> show text)

  -- A bad line costs the list its line, not its deck: the good lines still
  -- describe one, so a format can check it in the same report (#6396).
  Spec.it s "a list with a bad line still has the deck its good lines describe"
    . withPool "partial"
    $ \registry -> do
      listed <- listedOf s registry "4 Goblin Piker\n2 Shivan Dragon\n"
      Spec.assertEqWith s "the bad line" (DeckList.problems listed) [DeckList.NoSuchCard 2 (CardName.MkCardName (Text.pack "Shivan Dragon"))]
      Spec.assertEqWith s "and the good one's deck" (entriesOf (Deck.cards (DeckList.deck listed))) [(Text.pack "Goblin Piker", 4)]

  Spec.it s "a format header names the list's format, and a rendered list writes it back"
    . withPool "format"
    $ \registry -> do
      listed <- listedOf s registry "Format: Limited\n4 Goblin Piker\n"
      Spec.assertEqWith s "lowercased, as a format file is named" (DeckList.format listed) (Just (1, Text.pack "limited"))
      Spec.assertEqWith s "and no card is read from it" (DeckList.problems listed) []
      again <- listedOf s registry (Text.unpack (DeckList.render (Registry.printings registry) (Just (Text.pack "limited")) (DeckList.deck listed)))
      Spec.assertEqWith s "the same format" (fmap snd (DeckList.format again)) (Just (Text.pack "limited"))
      Spec.assertEqWith s "and the same deck" (DeckList.deck again) (DeckList.deck listed)

  Spec.it s "a second format header is a problem, and the first names the format"
    . withPool "format-twice"
    $ \registry -> do
      listed <- listedOf s registry "Format: modern\nFormat: limited\nFormat:\n"
      Spec.assertEqWith s "the format" (DeckList.format listed) (Just (1, Text.pack "modern"))
      Spec.assertEqWith s "the problems" (DeckList.problems listed) [DeckList.FormatRepeated 2 (Text.pack "limited"), DeckList.Unreadable 3 (Text.pack "Format:")]

  -- The format asked for is checked, but a header naming another is reported
  -- rather than silently overruled.
  Spec.it s "a format asked for that the header contradicts is a problem"
    . withPool "format-asked"
    $ \registry -> do
      listed <- listedOf s registry "Format: modern\n4 Goblin Piker\n"
      let modern = Text.pack "modern"
          limited = Text.pack "limited"
      Spec.assertEqWith s "the header's own" (DeckList.formatFor Nothing listed) (Just modern, [])
      Spec.assertEqWith s "the same one asked for" (DeckList.formatFor (Just modern) listed) (Just modern, [])
      Spec.assertEqWith s "another asked for" (DeckList.formatFor (Just limited) listed) (Just limited, [DeckList.FormatDisagrees 1 modern limited])

  Spec.it s "every section's heading reads back as that section" $
    Spec.assertEqWith s "each its own" (fmap (DeckList.sectionNamed . DeckList.heading) DeckList.sections) (fmap Just DeckList.sections)

  -- Each of Parun.Types.Deck's fields beside the deck is a section, and a
  -- rendered list writes each back. Distinct counts and cards per section, so
  -- one section read into another's field cannot pass.
  Spec.it s "each section fills its field, and a rendered list writes it back"
    . withPool "sections"
    $ \registry -> do
      deck <-
        deckOf s "sections" registry . unlines $
          [ "Deck",
            "4 Goblin Piker",
            "Sideboard",
            "2 Goblin Piker",
            "Companion",
            "1 Wax // Wane",
            "Commander",
            "1 Blossom-Blessed Angel",
            "Planar Deck",
            "3 Wane",
            "Scheme Deck",
            "5 Goblin Piker",
            "Conspiracies",
            "6 Goblin Piker",
            "Dungeons",
            "1 Goblin Piker",
            "Vanguard",
            "1 Wax"
          ]
      let named = S.nameOf . Printing.card
      Spec.assertEqWith s "the deck" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]
      Spec.assertEqWith s "CR 702.139a: the companion in the sideboard" (entriesOf (Deck.sideboard deck)) [(Text.pack "Goblin Piker", 2), (Text.pack "Wax//Wane", 1)]
      Spec.assertEqWith s "the commander" (fmap named (Set.toList (Deck.commander deck))) [CardName.MkCardName (Text.pack "Blossom-Blessed Angel")]
      Spec.assertEqWith s "the side decks" (Map.map entriesOf (Deck.sideDecks deck)) (Map.fromList [(SideDeck.Planar, [(Text.pack "Wax//Wane", 3)]), (SideDeck.Scheme, [(Text.pack "Goblin Piker", 5)])])
      Spec.assertEqWith s "the conspiracies" (entriesOf (Deck.conspiracies deck)) [(Text.pack "Goblin Piker", 6)]
      Spec.assertEqWith s "the dungeons" (fmap named (Set.toList (Deck.dungeons deck))) [CardName.MkCardName (Text.pack "Goblin Piker")]
      Spec.assertEqWith s "the vanguard" (fmap named (Deck.vanguard deck)) (Just (CardName.MkCardName (Text.pack "Wax//Wane")))
      again <- deckOf s "sections-again" registry (Text.unpack (DeckList.render (Registry.printings registry) Nothing deck))
      Spec.assertEqWith s "the same deck" again deck

  -- CR 902.1: one vanguard card.
  Spec.it s "CR 902.1 a second vanguard is a problem"
    . withPool "vanguard"
    $ \registry -> do
      problems <- problemsOf s "vanguard" registry "Vanguard\n1 Wax\n1 Goblin Piker\n2 Wax\n"
      Spec.assertEqWith s "the second line and the third" problems [DeckList.VanguardRepeated 3 (CardName.MkCardName (Text.pack "Goblin Piker")), DeckList.VanguardRepeated 4 (CardName.MkCardName (Text.pack "Wax"))]

  -- CR 718.1: two printings of one Attraction light different numbers, so a
  -- line names which by set code and collector number, as Arena exports it.
  Spec.it s "CR 718.1 a printing suffix brings that printing's lights, and a rendered list writes it back"
    . withPool "printing"
    $ \registry -> do
      deck <- deckOf s "printing" registry "Attraction Deck\n2 Bumper Cars (UNF) 202a\n1 Bumper Cars (unf) 202f\n4 Goblin Piker (M10) 146\n"
      Spec.assertEqWith s "two printings, each lit as printed" (attractionLights deck) [(Set.fromList [2, 3, 6], 2), (Set.fromList [4, 5, 6], 1), (Set.empty, 4)]
      let text = DeckList.render (Registry.printings registry) Nothing deck
      Spec.assertBool s (Text.isInfixOf (Text.pack "1 Bumper Cars (UNF) 202f\n") text) ("the suffix: " <> show text)
      Spec.assertBool s (Text.isInfixOf (Text.pack "4 Goblin Piker\n") text) ("no suffix where nothing is lit: " <> show text)
      again <- deckOf s "printing-again" registry (Text.unpack text)
      Spec.assertEqWith s "the same deck" again deck

  -- The owner's reading on #4118: a bare name is some printing of the card,
  -- here the table's first by set code and collector number.
  Spec.it s "CR 718.1 a bare name is the card's first printing"
    . withPool "bare-printing"
    $ \registry -> do
      deck <- deckOf s "bare-printing" registry "Attraction Deck\n1 Bumper Cars\n"
      Spec.assertEqWith s "202a's lights" (attractionLights deck) [(Set.fromList [2, 3, 6], 1)]

  Spec.it s "a suffix the table lacks, or printing another card, is a problem"
    . withPool "bad-printing"
    $ \registry -> do
      problems <- problemsOf s "bad-printing" registry "1 Bumper Cars (UNF) 999\n1 Goblin Piker (UNF) 202a\n"
      Spec.assertEqWith s "each line" problems [DeckList.NoSuchPrinting 1 (Text.pack "UNF") (Text.pack "999"), DeckList.PrintingDisagrees 2 (CardName.MkCardName (Text.pack "Goblin Piker")) (Text.pack "UNF") (Text.pack "202a")]

  -- CR 123.2: a sticker sheet is named by name or slug, as a card is, and
  -- kept in the order written, by which a player's sheets are told apart
  -- (CR 123.3a); a rendered list writes that order back, a run of one sheet
  -- as a count.
  Spec.it s "CR 123.2 a Stickers section brings its sheets in order, and a rendered list writes it back"
    . withPool "stickers"
    $ \registry -> do
      deck <- deckOf s "stickers" registry "4 Goblin Piker\n\nStickers\n2 Eldrazi Guacamole Tightrope\nslimy-burrito-illusion\n1 Eldrazi Guacamole Tightrope\n"
      let tightrope = Text.pack "Eldrazi Guacamole Tightrope"
      Spec.assertEqWith s "the sheets, in order" (fmap StickerSheet.name (Foldable.toList (Deck.stickerSheets deck))) [tightrope, tightrope, Text.pack "Slimy Burrito Illusion", tightrope]
      Spec.assertEqWith s "and the deck" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]
      let text = DeckList.render (Registry.printings registry) Nothing deck
      Spec.assertBool s (Text.isInfixOf (Text.pack "\nStickers\n2 Eldrazi Guacamole Tightrope\n1 Slimy Burrito Illusion\n1 Eldrazi Guacamole Tightrope\n") text) ("in order: " <> show text)
      again <- deckOf s "stickers-again" registry (Text.unpack text)
      Spec.assertEqWith s "the same deck" again deck

  -- A sheet is not a card and a card is not a sheet, so each section reads only
  -- its own names; a sheet has one printing, so a suffix is part of the name.
  Spec.it s "CR 123.2 a name no sheet has is a problem, and a sheet is no card"
    . withPool "bad-stickers"
    $ \registry -> do
      problems <- problemsOf s "bad-stickers" registry "1 Slimy Burrito Illusion\nStickers\n1 Goblin Piker\n1 Slimy Burrito Illusion (SUNF) 3\n"
      Spec.assertEqWith
        s
        "each line"
        problems
        [ DeckList.NoSuchCard 1 (CardName.MkCardName (Text.pack "Slimy Burrito Illusion")),
          DeckList.NoSuchStickerSheet 3 (Text.pack "Goblin Piker"),
          DeckList.NoSuchStickerSheet 4 (Text.pack "Slimy Burrito Illusion (SUNF) 3")
        ]
