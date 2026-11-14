-- Covers Parun.Legality, the shipped formats and legality table, and the
-- refusal `parun play` and `parun match` make through Parun.Play.Command,
-- and the format they start the game under.
module Parun.LegalitySpec where

import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Numeric.Natural as Natural
import qualified Parun.DeckList as DeckList
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command as Command
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.StickerSheets as StickerSheets
import qualified Parun.Support as S
import qualified Parun.Trustworthy.Directory as Directory
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.DraftBooster as DraftBooster
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.StickerSheet as StickerSheet
import qualified Parun.Types.Violation as Violation

-- A deck and a sideboard, each as (name, count) pairs.
deckOf :: Spec.Spec IO n -> Registry.Registry IO -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO Deck.Deck
deckOf s registry main side = do
  let section entries = fmap (Map.fromListWith (+)) (traverse (\(name, count) -> fmap (\p -> (p, count)) (S.printingOf s registry name)) entries)
  cards <- section main
  sideboard <- section side
  pure (Deck.fromCards cards) {Deck.sideboard = sideboard}

-- The violations a deck raises under a rule family with no pool, in a game
-- not for ante.
violations :: Spec.Spec IO n -> Registry.Registry IO -> RuleFamily.RuleFamily -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
violations s registry family = violationsIn s registry GameSettings.plain (Format.MkFormat family Nothing Set.empty) noTable

violationsIn :: Spec.Spec IO n -> Registry.Registry IO -> GameSettings.GameSettings -> Format.Format -> LegalityTable.LegalityTable -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
violationsIn s registry settings format table main side = fmap (Legality.check table settings format Nothing) (deckOf s registry main side)

-- The violations a deck raises in a shipped format, by name, in a game not
-- for ante.
violationsNamed :: Spec.Spec IO n -> Registry.Registry IO -> String -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
violationsNamed s registry name main side = do
  checker <- Legality.named (Text.pack name)
  deck <- deckOf s registry main side
  either (Spec.assertFailure s . Text.unpack) (\known -> pure (known GameSettings.plain Nothing deck)) checker

-- A deck and a sideboard led by the commanders named.
ledBy :: Spec.Spec IO n -> Registry.Registry IO -> [String] -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO Deck.Deck
ledBy s registry commanders main side = do
  deck <- deckOf s registry main side
  named <- traverse (S.printingOf s registry) commanders
  pure deck {Deck.commander = Map.fromListWith (+) [(printing, 1) | printing <- named]}

-- The violations a deck led by the commanders named raises under a rule
-- family with no pool, in a game whose settings say nothing of Brawl.
ledViolations :: Spec.Spec IO n -> Registry.Registry IO -> RuleFamily.RuleFamily -> [String] -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
ledViolations s registry family commanders main side = fmap (Legality.check noTable GameSettings.plain (Format.MkFormat family Nothing Set.empty) Nothing) (ledBy s registry commanders main side)

-- The violations a deck led by the commanders named raises in a shipped
-- format, by name, in a game whose settings say nothing of Brawl.
ledViolationsNamed :: Spec.Spec IO n -> Registry.Registry IO -> String -> [String] -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
ledViolationsNamed s registry name commanders main side = do
  checker <- Legality.named (Text.pack name)
  deck <- ledBy s registry commanders main side
  either (Spec.assertFailure s . Text.unpack) (\known -> pure (known GameSettings.plain Nothing deck)) checker

-- A table with no cards, for a format with no pool, which reads none.
noTable :: LegalityTable.LegalityTable
noTable = LegalityTable.MkLegalityTable Text.empty Map.empty

-- The shipped formats' names.
shippedNames :: IO [Text.Text]
shippedNames = do
  root <- Legality.formatsRoot
  fmap (fmap Text.pack . Maybe.mapMaybe (fmap reverse . List.stripPrefix (reverse ".json") . reverse)) (Directory.listDirectory root)

nameOf :: String -> CardName.CardName
nameOf = CardName.MkCardName . Text.pack

-- The committed sticker sheets, in collector-number order.
committedSheets :: Spec.Spec IO n -> IO [StickerSheet.StickerSheet]
committedSheets s = do
  root <- StickerSheets.defaultRoot
  loaded <- StickerSheets.loadRoot root
  sheets <- traverse (either (Spec.assertFailure s . Text.unpack) pure . snd) loaded
  pure (List.sortOn StickerSheet.number sheets)

-- The violations a deck bringing these sheets raises under a rule family with
-- no pool, in a game not for ante.
withSheets :: Spec.Spec IO n -> Registry.Registry IO -> RuleFamily.RuleFamily -> [(String, Natural.Natural)] -> [StickerSheet.StickerSheet] -> IO [Violation.Violation]
withSheets s registry family main sheets = do
  deck <- deckOf s registry main []
  pure (Legality.check noTable GameSettings.plain (Format.MkFormat family Nothing Set.empty) Nothing deck {Deck.stickerSheets = Seq.fromList sheets})

-- The violations a deck bringing these supplementary decks and this vanguard
-- raises under a rule family with no pool, in a game with these settings.
withSideDecks :: Spec.Spec IO n -> Registry.Registry IO -> RuleFamily.RuleFamily -> GameSettings.GameSettings -> Deck.Deck -> [(SideDeck.SideDeck, [(String, Natural.Natural)])] -> Maybe String -> IO [Violation.Violation]
withSideDecks s registry family settings deck piles vanguard = fmap (Legality.check noTable settings (Format.MkFormat family Nothing Set.empty) Nothing) (supplied s registry deck piles vanguard)

-- A deck bringing the supplementary decks and the vanguard named.
supplied :: Spec.Spec IO n -> Registry.Registry IO -> Deck.Deck -> [(SideDeck.SideDeck, [(String, Natural.Natural)])] -> Maybe String -> IO Deck.Deck
supplied s registry deck piles vanguard = do
  let section entries = fmap (Map.fromListWith (+)) (traverse (\(name, count) -> fmap (\p -> (p, count)) (S.printingOf s registry name)) entries)
  held <- traverse (traverse section) piles
  brought <- traverse (S.printingOf s registry) vanguard
  pure deck {Deck.sideDecks = Map.fromList held, Deck.vanguard = brought}

-- Ten planes of different names.
tenPlanes :: [(String, Natural.Natural)]
tenPlanes = fmap (\name -> (name, 1)) ["Academy at Tolaria West", "Agyrem", "Akoum", "Astral Arena", "Celestine Reef", "Eloren Wilds", "Fields of Summer", "Naya", "Orzhova", "Panopticon"]

-- Ten planes of different names, none of tenPlanes'.
otherTen :: [(String, Natural.Natural)]
otherTen = fmap (\name -> (name, 1)) ["Inys Haen", "Prime Minister's Cabinet Room", "Raiders' Allegiance", "Sanctum of Serra", "Strixhaven", "Tazeem", "The Caldaia", "The Dining Car", "The Lux Foundation Library", "The Moonbase"]

copies :: String -> Natural.Natural -> Violation.Violation
copies name count = Violation.TooManyCopies Nothing (CardName.MkCardName (Text.pack name)) count 4

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> Registry.Registry Identity.Identity -> n ()
spec s registry reference = Spec.describe s "Parun.Legality" $ do
  Spec.it s "CR 100.2a: five Grizzly Bears in a constructed deck are one too many" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 5), ("Mountain", 55)] []
    Spec.assertEqWith s "violations" found [copies "Grizzly Bears" 5]

  -- A misspelt name does not keep the rest of the list from being checked:
  -- both are reported together (#6396).
  Spec.it s "CR 100.2a: a list with a bad line still has its five Grizzly Bears counted" $ do
    let text = "5 Grizzly Bears\n55 Mountain\n1 Lightnin Bolt\n"
    listed <- DeckList.parse registry (Text.pack text) >>= either (S.brokenCorpus s) pure
    (format, reported) <- Legality.report (Just (Text.pack "constructed")) Nothing listed
    Spec.assertEqWith s "the format" format (Just (Text.pack "constructed"))
    Spec.assertEqWith s "the bad line, then the violation" reported [DeckList.explain (DeckList.NoSuchCard 3 (nameOf "Lightnin Bolt")), Legality.explain (copies "Grizzly Bears" 5)]
    headed <- DeckList.parse registry (Text.pack ("Format: limited\n" <> text)) >>= either (S.brokenCorpus s) pure
    (headerFormat, headerReported) <- Legality.report Nothing Nothing headed
    Spec.assertEqWith s "the header's format" headerFormat (Just (Text.pack "limited"))
    Spec.assertEqWith s "under which five are legal" headerReported [DeckList.explain (DeckList.NoSuchCard 4 (nameOf "Lightnin Bolt"))]

  Spec.it s "CR 100.2a: a constructed deck of 59 cards is too small" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 4), ("Mountain", 55)] []
    Spec.assertEqWith s "violations" found [Violation.TooFewCards Nothing 59 60]

  Spec.it s "CR 100.4a: the four-card limit counts the sideboard too" $ do
    found <- violations s registry RuleFamily.Constructed [("Lightning Bolt", 3), ("Mountain", 57)] [("Lightning Bolt", 2)]
    Spec.assertEqWith s "violations" found [copies "Lightning Bolt" 5]

  Spec.it s "CR 100.4a: a sixteen-card sideboard is too large" $ do
    found <- violations s registry RuleFamily.Constructed [("Mountain", 60)] [("Mountain", 16)]
    Spec.assertEqWith s "violations" found [Violation.SideboardTooLarge 16 15]

  Spec.it s "CR 100.2a / 100.4a: a deck breaking four rules reports all four" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 5), ("Lightning Bolt", 3), ("Mountain", 50)] [("Lightning Bolt", 2), ("Mountain", 14)]
    Spec.assertEqWith s "violations" found [Violation.TooFewCards Nothing 58 60, copies "Grizzly Bears" 5, copies "Lightning Bolt" 5, Violation.SideboardTooLarge 16 15]

  Spec.it s "CR 100.2a / 205.4c: basic land cards are exempt by supertype, snow basics included" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 4), ("Mountain", 28), ("Snow-Covered Mountain", 28)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2b: a limited deck of forty has no copy limit" $ do
    found <- violations s registry RuleFamily.Limited [("Grizzly Bears", 5), ("Mountain", 35)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2b: a limited deck of 39 cards is too small" $ do
    found <- violations s registry RuleFamily.Limited [("Grizzly Bears", 5), ("Mountain", 34)] []
    Spec.assertEqWith s "violations" found [Violation.TooFewCards Nothing 39 40]

  Spec.it s "CR 100.2a: a deck can have up to nine cards named Nazgul" $ do
    found <- violations s registry RuleFamily.Constructed [("Nazgûl", 9), ("Mountain", 51)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2a: ten cards named Nazgul are one too many" $ do
    found <- violations s registry RuleFamily.Constructed [("Nazgûl", 10), ("Mountain", 50)] []
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies Nothing (CardName.MkCardName (Text.pack "Nazgûl")) 10 9]

  Spec.it s "CR 100.4a: a card's own limit counts the sideboard too" $ do
    found <- violations s registry RuleFamily.Constructed [("Nazgûl", 9), ("Mountain", 51)] [("Nazgûl", 1)]
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies Nothing (CardName.MkCardName (Text.pack "Nazgûl")) 10 9]

  Spec.it s "CR 100.2a: a deck can have any number of cards named Relentless Rats" $ do
    found <- violations s registry RuleFamily.Constructed [("Relentless Rats", 40), ("Mountain", 20)] [("Relentless Rats", 15)]
    Spec.assertEqWith s "violations" found []

  -- Nazgûl's ruling of 2023-06-16: in limited play the rest stay in the
  -- sideboard (CR 100.4b), so the deck alone is counted.
  Spec.it s "CR 100.2b: a limited deck can have nine Nazgul but not ten" $ do
    nine <- violations s registry RuleFamily.Limited [("Nazgûl", 9), ("Mountain", 31)] [("Nazgûl", 3)]
    Spec.assertEqWith s "nine" nine []
    ten <- violations s registry RuleFamily.Limited [("Nazgûl", 10), ("Mountain", 30)] []
    Spec.assertEqWith s "ten" ten [Violation.TooManyCopies Nothing (CardName.MkCardName (Text.pack "Nazgûl")) 10 9]

  Spec.it s "CR 100.2b: a limited deck holds only its card pool and basic lands, no more of a card than the pool" $ do
    let pool = "1 Llanowar Elves\n1 Grizzly Bears\n"
    pooled <- DeckList.parse registry (Text.pack pool) >>= either (S.brokenCorpus s) pure
    let held = Just (Legality.poolOf (DeckList.deck pooled))
        sealed text = DeckList.parse registry (Text.pack text) >>= either (S.brokenCorpus s) pure >>= Legality.report (Just (Text.pack "limited")) held
    (_, outside) <- sealed "2 Llanowar Elves\n1 Lightning Bolt\n37 Forest\n"
    Spec.assertEqWith s "a Lightning Bolt and a second Llanowar Elves" outside (fmap Legality.explain [Violation.NotInCardPool (nameOf "Lightning Bolt"), Violation.TooManyCopies Nothing (nameOf "Llanowar Elves") 2 1])
    (_, legal) <- sealed "1 Llanowar Elves\n39 Forest\n"
    Spec.assertEqWith s "one Llanowar Elves" legal []
    (_, sideboard) <- sealed "1 Llanowar Elves\n39 Forest\n\nSideboard\n1 Llanowar Elves\n"
    Spec.assertEqWith s "CR 100.4b: a second in the sideboard" sideboard (fmap Legality.explain [Violation.TooManyCopies Nothing (nameOf "Llanowar Elves") 2 1])
    (_, constructed) <- DeckList.parse registry (Text.pack "4 Lightning Bolt\n56 Mountain\n") >>= either (S.brokenCorpus s) pure >>= Legality.report (Just (Text.pack "constructed")) held
    Spec.assertEqWith s "a constructed deck ignores the pool" constructed []

  Spec.it s "CR 718.2b: a limited Attraction deck draws on the card pool" $ do
    deck <- deckOf s registry [("Mountain", 40)] []
    attractions <- supplied s registry deck [(SideDeck.Attraction, [("Bumper Cars", 3)])] Nothing
    let limited held = Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.Limited Nothing Set.empty) (Just (Map.singleton (nameOf "Bumper Cars") held)) attractions
    Spec.assertEqWith s "three of three" (limited 3) []
    Spec.assertEqWith s "three of two" (limited 2) [Violation.TooManyCopies Nothing (nameOf "Bumper Cars") 3 2]

  -- Proficient Pyrodancer's player brings sheets for its stickers.
  Spec.it s "CR 123.2a: a constructed player bringing sticker sheets brings ten, each unique" $ do
    sheets <- committedSheets s
    let main = [("Proficient Pyrodancer", 4), ("Mountain", 56 :: Natural.Natural)]
        firstName = foldMap StickerSheet.name (List.take 1 sheets)
    Spec.assertEqWith s "ten sheets are committed" (length sheets) 10
    none <- withSheets s registry RuleFamily.Constructed main []
    Spec.assertEqWith s "none, played without stickers" none []
    two <- withSheets s registry RuleFamily.Constructed main (take 2 sheets)
    Spec.assertEqWith s "two" two [Violation.TooFewStickerSheets 2 10]
    nine <- withSheets s registry RuleFamily.Constructed main (take 9 sheets)
    Spec.assertEqWith s "nine unique" nine [Violation.TooFewStickerSheets 9 10]
    repeated <- withSheets s registry RuleFamily.Constructed main (take 9 sheets <> take 1 sheets)
    Spec.assertEqWith s "ten with a duplicate" repeated [Violation.DuplicateStickerSheet firstName 2]
    ten <- withSheets s registry RuleFamily.Constructed main sheets
    Spec.assertEqWith s "ten unique" ten []
    Spec.assertEqWith s "explained" (fmap Legality.explain (two <> repeated)) (fmap Text.pack ["2 sticker sheets, fewer than 10", "2 copies of the sticker sheet " <> Text.unpack firstName <> ", more than 1"])

  Spec.it s "CR 901.3: a planar deck holds ten planes or phenomena, all names different" $ do
    deck <- deckOf s registry [("Mountain", 60)] []
    let planar cards = withSideDecks s registry RuleFamily.Constructed GameSettings.plain deck [(SideDeck.Planar, cards)] Nothing
        reef = nameOf "Celestine Reef"
    nine <- planar (drop 1 tenPlanes)
    Spec.assertEqWith s "nine" nine [Violation.TooFewCards (Just SideDeck.Planar) 9 10]
    ten <- planar tenPlanes
    Spec.assertEqWith s "ten" ten []
    twoReefs <- planar (("Celestine Reef", 1) : tenPlanes)
    Spec.assertEqWith s "two Celestine Reef" twoReefs [Violation.TooManyCopies (Just SideDeck.Planar) reef 2 1]
    phenomena <- planar (tenPlanes <> [("Fixed Point in Time", 1), ("Mutual Epiphany", 1)])
    Spec.assertEqWith s "two phenomena" phenomena []
    three <- planar (tenPlanes <> [("Fixed Point in Time", 2), ("Mutual Epiphany", 1)])
    Spec.assertEqWith s "three phenomena" three [Violation.TooManyCopies (Just SideDeck.Planar) (nameOf "Fixed Point in Time") 2 1, Violation.TooManyPhenomena 3 2]
    Spec.assertEqWith s "explained" (fmap Legality.explain (nine <> twoReefs <> three)) (fmap Text.pack ["9 cards in the planar deck, fewer than 10", "2 copies of Celestine Reef in the planar deck, more than 1", "2 copies of Fixed Point in Time in the planar deck, more than 1", "3 phenomena in the planar deck, more than 2"])

  -- CR 901.15a's communal deck is every seat's cards together, which
  -- Legality.checkGame sizes.
  Spec.it s "CR 901.15a: a share of a communal planar deck is not sized alone" $ do
    deck <- deckOf s registry [("Mountain", 60)] []
    found <- withSideDecks s registry RuleFamily.Constructed GameSettings.plain {GameSettings.singlePlanarDeck = True} deck [(SideDeck.Planar, [("Celestine Reef", 1), ("All in Good Time", 1)])] Nothing
    Spec.assertEqWith s "only the scheme refused" found [Violation.MisplacedCard SideDeck.Planar (nameOf "All in Good Time")]

  Spec.it s "CR 901.15a: a communal planar deck holds ten a player, at most two phenomena a player, all names different" $ do
    deck <- deckOf s registry [("Mountain", 60)] []
    let communal = GameSettings.plain {GameSettings.singlePlanarDeck = True}
        seats shares = fmap (Legality.checkGame communal) (traverse (\share -> supplied s registry deck [(SideDeck.Planar, share)] Nothing) shares)
    twenty <- seats [tenPlanes, otherTen]
    Spec.assertEqWith s "twenty names" twenty []
    shared <- seats [tenPlanes, ("Celestine Reef", 1) : drop 1 otherTen]
    Spec.assertEqWith s "Celestine Reef in both" shared [Violation.TooManyCopies (Just SideDeck.Planar) (nameOf "Celestine Reef") 2 1]
    nineteen <- seats [tenPlanes, drop 1 otherTen]
    Spec.assertEqWith s "nineteen for two" nineteen [Violation.TooFewCards (Just SideDeck.Planar) 19 20]
    six <- seats [take 5 tenPlanes, drop 5 tenPlanes, take 5 otherTen, drop 5 otherTen, [("Fixed Point in Time", 1), ("Mutual Epiphany", 1)], []]
    Spec.assertEqWith s "forty for six" six [Violation.TooFewCards (Just SideDeck.Planar) 22 40]
    phenomena <- seats [tenPlanes <> [("Fixed Point in Time", 1)], otherTen <> [("Mutual Epiphany", 1)]]
    Spec.assertEqWith s "two phenomena for two" phenomena []
    five <- seats [tenPlanes <> [("Fixed Point in Time", 2), ("Mutual Epiphany", 1)], otherTen <> [("Mutual Epiphany", 2)]]
    Spec.assertEqWith s "five phenomena for two" five [Violation.TooManyCopies (Just SideDeck.Planar) (nameOf "Fixed Point in Time") 2 1, Violation.TooManyCopies (Just SideDeck.Planar) (nameOf "Mutual Epiphany") 3 1, Violation.TooManyPhenomena 5 4]
    separate <- fmap (Legality.checkGame GameSettings.plain) (traverse (\share -> supplied s registry deck [(SideDeck.Planar, share)] Nothing) [tenPlanes, tenPlanes])
    Spec.assertEqWith s "own decks are not pooled" separate []

  -- CR 607.2n with Arcane Savant's ruling of 2016-08-23: each Savant
  -- revealed exiles one instant or sorcery card, so a deck exiles no more
  -- than it has Savants, and nothing else.
  Spec.it s "CR 607.2n: a deck exiles before the game only what its cards of the name exile" $ do
    let exilingIn family lands savants exiled = do
          deck <- deckOf s registry [("Island", lands), ("Arcane Savant", savants)] []
          cards <- fmap (Map.fromListWith (+)) (traverse (\(name, count) -> fmap (\p -> (p, count)) (S.printingOf s registry name)) exiled)
          pure (Legality.check noTable GameSettings.plain (Format.MkFormat family Nothing Set.empty) Nothing deck {Deck.exiledBeforeGame = Map.singleton (nameOf "Arcane Savant") cards})
        exiling = exilingIn RuleFamily.Limited 40
    one <- exiling 1 [("Lightning Bolt", 1)]
    Spec.assertEqWith s "one Savant, one instant" one []
    two <- exiling 2 [("Lightning Bolt", 1), ("Divination", 1)]
    Spec.assertEqWith s "two Savants, an instant and a sorcery" two []
    tooMany <- exiling 1 [("Lightning Bolt", 2)]
    Spec.assertEqWith s "one Savant, two cards" tooMany [Violation.PregameExileCount (nameOf "Arcane Savant") 2]
    none <- exiling 0 [("Lightning Bolt", 1)]
    Spec.assertEqWith s "no Savant" none [Violation.PregameExileCount (nameOf "Arcane Savant") 1]
    creature <- exiling 1 [("Grizzly Bears", 1)]
    Spec.assertEqWith s "a creature card" creature [Violation.PregameExileMismatch (nameOf "Grizzly Bears") (nameOf "Arcane Savant")]
    chimera <- do
      deck <- deckOf s registry [("Mountain", 40), ("Volatile Chimera", 1)] []
      let exiledWith cards = do
            exiled <- fmap (Map.fromListWith (+)) (traverse (\(name, count) -> fmap (\p -> (p, count)) (S.printingOf s registry name)) cards)
            pure (Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.Limited Nothing Set.empty) Nothing deck {Deck.exiledBeforeGame = Map.singleton (nameOf "Volatile Chimera") exiled})
      traverse exiledWith [[("Grizzly Bears", 2)], [("Grizzly Bears", 2), ("Hill Giant", 2)]]
    Spec.assertEqWith s "one Chimera, two creatures, then four" chimera [[Violation.PregameExileCount (nameOf "Volatile Chimera") 2], []]
    constructed <- exilingIn RuleFamily.Constructed 59 1 [("Lightning Bolt", 1)]
    Spec.assertEqWith s "nothing was drafted in constructed play" constructed [Violation.PregameExileCount (nameOf "Arcane Savant") 1]

  Spec.it s "CR 902.3: each player in a Vanguard game brings a vanguard card" $ do
    deck <- deckOf s registry [("Mountain", 60)] []
    let seats vanguards = fmap (Legality.checkGame GameSettings.plain) (traverse (supplied s registry deck []) vanguards)
    one <- seats [Just "Barrin", Nothing]
    Spec.assertEqWith s "Barrin and none" one [Violation.MissingVanguard 1 2]
    both <- seats [Just "Barrin", Just "Selenia"]
    Spec.assertEqWith s "Barrin and Selenia" both []
    neither <- seats [Nothing, Nothing]
    Spec.assertEqWith s "no Vanguard game" neither []
    Spec.assertEqWith s "explained" (fmap Legality.explain one) [Text.pack "1 of 2 players without a vanguard card"]

  Spec.it s "CR 904.3: a scheme deck holds twenty schemes, at most two of a name" $ do
    deck <- deckOf s registry [("Mountain", 60)] []
    found <- withSideDecks s registry RuleFamily.Constructed GameSettings.plain deck [(SideDeck.Scheme, [("All in Good Time", 3)])] Nothing
    Spec.assertEqWith s "three All in Good Time" found [Violation.TooFewCards (Just SideDeck.Scheme) 3 20, Violation.TooManyCopies (Just SideDeck.Scheme) (nameOf "All in Good Time") 3 2]

  Spec.it s "CR 904.13d: a Commander archenemy scheme deck holds ten, all names different" $ do
    deck <- ledBy s registry ["Krenko, Mob Boss"] [("Mountain", 99)] []
    found <- withSideDecks s registry RuleFamily.Commander GameSettings.plain deck [(SideDeck.Scheme, [("All in Good Time", 2)])] Nothing
    Spec.assertEqWith s "two All in Good Time" found [Violation.TooFewCards (Just SideDeck.Scheme) 2 10, Violation.TooManyCopies (Just SideDeck.Scheme) (nameOf "All in Good Time") 2 1]

  Spec.it s "CR 718.2a: a constructed Attraction deck holds ten, all names different" $ do
    deck <- deckOf s registry [("Mountain", 60)] []
    found <- withSideDecks s registry RuleFamily.Constructed GameSettings.plain deck [(SideDeck.Attraction, [("Bumper Cars", 2)])] Nothing
    Spec.assertEqWith s "two Bumper Cars" found [Violation.TooFewCards (Just SideDeck.Attraction) 2 10, Violation.TooManyCopies (Just SideDeck.Attraction) (nameOf "Bumper Cars") 2 1]

  Spec.it s "CR 718.2b: a limited Attraction deck holds three, duplicates allowed" $ do
    deck <- deckOf s registry [("Mountain", 40)] []
    let attractions count = withSideDecks s registry RuleFamily.Limited GameSettings.plain deck [(SideDeck.Attraction, [("Bumper Cars", count)])] Nothing
    three <- attractions 3
    Spec.assertEqWith s "three Bumper Cars" three []
    two <- attractions 2
    Spec.assertEqWith s "two Bumper Cars" two [Violation.TooFewCards (Just SideDeck.Attraction) 2 3]

  -- The Unstable FAQ's rulings of 2018-01-19 (Applied Aeronautics).
  Spec.it s "Unstable FAQ: a constructed Contraption deck holds fifteen, a limited one any number" $ do
    constructed <- deckOf s registry [("Mountain", 60)] []
    two <- withSideDecks s registry RuleFamily.Constructed GameSettings.plain constructed [(SideDeck.Contraption, [("Arms Depot", 2)])] Nothing
    Spec.assertEqWith s "constructed" two [Violation.TooFewCards (Just SideDeck.Contraption) 2 15, Violation.TooManyCopies (Just SideDeck.Contraption) (nameOf "Arms Depot") 2 1]
    limited <- deckOf s registry [("Mountain", 40)] []
    some <- withSideDecks s registry RuleFamily.Limited GameSettings.plain limited [(SideDeck.Contraption, [("Arms Depot", 2)])] Nothing
    Spec.assertEqWith s "limited" some []

  -- A vanguard card in a supplementary deck would be a second one in the
  -- command zone, which Parun.Engine.Vanguard.vanguardOf would break a tie on.
  Spec.it s "CR 902.3: Barrin as a vanguard with Selenia in a scheme deck is refused" $ do
    deck <- deckOf s registry [("Mountain", 60)] []
    found <- withSideDecks s registry RuleFamily.Constructed GameSettings.plain deck [(SideDeck.Scheme, [("Selenia", 1)])] (Just "Barrin")
    Spec.assertEqWith s "Selenia misplaced" found [Violation.MisplacedCard SideDeck.Scheme (nameOf "Selenia"), Violation.TooFewCards (Just SideDeck.Scheme) 1 20]
    alone <- withSideDecks s registry RuleFamily.Constructed GameSettings.plain deck [] (Just "Barrin")
    Spec.assertEqWith s "Barrin alone" alone []
    bears <- withSideDecks s registry RuleFamily.Constructed GameSettings.plain deck [] (Just "Grizzly Bears")
    Spec.assertEqWith s "Grizzly Bears" bears [Violation.NotVanguardCard (nameOf "Grizzly Bears")]
    Spec.assertEqWith s "explained" (fmap Legality.explain (found <> bears)) (fmap Text.pack ["Selenia does not belong in the scheme deck", "1 cards in the scheme deck, fewer than 20", "Grizzly Bears is not a vanguard card"])

  Spec.it s "CR 902.1: a deck list naming Barrin and Selenia as vanguards is refused" $ do
    listed <- DeckList.parse registry (Text.pack "60 Mountain\nVanguard\n1 Barrin\n1 Selenia\n") >>= either (S.brokenCorpus s) pure
    (_, reported) <- Legality.report (Just (Text.pack "constructed")) Nothing listed
    Spec.assertEqWith s "the second refused" reported [DeckList.explain (DeckList.VanguardRepeated 4 (nameOf "Selenia"))]

  Spec.it s "CR 123.2a: a Commander player bringing sticker sheets is in constructed play too" $ do
    sheets <- committedSheets s
    found <- fmap (\deck -> Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.Commander Nothing Set.empty) Nothing deck {Deck.stickerSheets = Seq.fromList (take 2 sheets)}) (ledBy s registry ["Krenko, Mob Boss"] [("Mountain", 99)] [])
    Spec.assertEqWith s "violations" found [Violation.TooFewStickerSheets 2 10]

  Spec.it s "CR 123.2b: a limited player may bring two of one sticker sheet" $ do
    sheets <- committedSheets s
    found <- withSheets s registry RuleFamily.Limited [("Proficient Pyrodancer", 4), ("Mountain", 36)] (take 1 sheets <> take 1 sheets)
    Spec.assertEqWith s "violations" found []

  Spec.it s "every shipped format file decodes, constructed and limited with no pool" $ do
    root <- Legality.formatsRoot
    names <- shippedNames
    loaded <- traverse (Legality.loadFormat root) names
    Spec.assertEqWith s "problems" [(name, problem) | (name, Just (Left problem)) <- zip names loaded] []
    Spec.assertEqWith s "constructed" (lookup (Text.pack "constructed") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Constructed Nothing Set.empty))))
    Spec.assertEqWith s "limited" (lookup (Text.pack "limited") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Limited Nothing Set.empty))))
    Spec.assertEqWith s "commander" (lookup (Text.pack "commander") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Commander (Just (Text.pack "commander")) Set.empty))))
    Spec.assertEqWith s "brawl, from Standard's pool (CR 903.12b)" (lookup (Text.pack "brawl") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Brawl (Just (Text.pack "standard")) Set.empty))))
    Spec.assertEqWith s "commanderdraft, of Commander Legends boosters (CR 903.13a)" (lookup (Text.pack "commanderdraft") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.CommanderDraft Nothing (Set.singleton DraftBooster.CommanderLegends)))))

  Spec.it s "a name with no format file is no format" $ do
    root <- Legality.formatsRoot
    found <- traverse (Legality.loadFormat root . Text.pack) ["nonesuch", "../cards/black-lotus", ""]
    Spec.assertEqWith s "found" found [Nothing, Nothing, Nothing]

  Spec.it s "every shipped format's pool is a pool of the shipped table" $ do
    root <- Legality.formatsRoot
    names <- shippedNames
    pools <- fmap (\loaded -> [pool | Just (Right format) <- loaded, Just pool <- [Format.pool format]]) (traverse (Legality.loadFormat root) names)
    table <- Legality.tablePath >>= Legality.loadTable >>= either (Spec.assertFailure s . Text.unpack) pure
    let known = foldMap Map.keysSet (LegalityTable.cards table)
    Spec.assertEqWith s "unknown pools" (filter (`Set.notMember` known) pools) []

  Spec.it s "the shipped table renders back to its own file" $ do
    path <- Legality.tablePath
    table <- Legality.loadTable path >>= either (Spec.assertFailure s . Text.unpack) pure
    bytes <- ByteString.readFile path
    Spec.assertEqWith s "rendered" (Encoding.encodeUtf8 (Legality.renderTable table)) bytes

  Spec.it s "vintage restricts Black Lotus to one copy" $ do
    one <- violationsNamed s registry "vintage" [("Black Lotus", 1), ("Mountain", 59)] []
    Spec.assertEqWith s "one" one []
    two <- violationsNamed s registry "vintage" [("Black Lotus", 1), ("Mountain", 59)] [("Black Lotus", 1)]
    Spec.assertEqWith s "two" two [Violation.TooManyCopies Nothing (nameOf "Black Lotus") 2 1]

  Spec.it s "legacy bans Black Lotus" $ do
    found <- violationsNamed s registry "legacy" [("Black Lotus", 1), ("Mountain", 59)] []
    Spec.assertEqWith s "violations" found [Violation.BannedCard (nameOf "Black Lotus")]

  Spec.it s "vintage and legacy ban Shahrazad" $ do
    vintage <- violationsNamed s registry "vintage" [("Shahrazad", 1), ("Mountain", 59)] []
    Spec.assertEqWith s "vintage" vintage [Violation.BannedCard (nameOf "Shahrazad")]
    legacy <- violationsNamed s registry "legacy" [("Mountain", 60)] [("Shahrazad", 1)]
    Spec.assertEqWith s "legacy" legacy [Violation.BannedCard (nameOf "Shahrazad")]

  Spec.it s "Black Lotus is not legal in modern" $ do
    found <- violationsNamed s registry "modern" [("Black Lotus", 1), ("Mountain", 59)] []
    Spec.assertEqWith s "violations" found [Violation.NotLegalCard (nameOf "Black Lotus")]

  Spec.it s "a card the table does not have has no legality data, which is not not legal" $ do
    found <- violationsNamed s registry "vintage" [("Synthetic Ability Audit", 1), ("Mountain", 59)] []
    Spec.assertEqWith s "violations" found [Violation.NoLegalityData (nameOf "Synthetic Ability Audit")]

  Spec.it s "CR 407.3: Amulet of Quoz is refused unless the game is for ante" $ do
    let constructed = Format.MkFormat RuleFamily.Constructed Nothing Set.empty
        deck = [("Amulet of Quoz", 1), ("Mountain", 59)] :: [(String, Natural.Natural)]
    notForAnte <- violationsIn s registry GameSettings.plain constructed noTable deck []
    Spec.assertEqWith s "not for ante" notForAnte [Violation.AnteCard (nameOf "Amulet of Quoz")]
    forAnte <- violationsIn s registry GameSettings.plain {GameSettings.ante = True} constructed noTable deck []
    Spec.assertEqWith s "for ante" forAnte []

  Spec.it s "CR 407.3: an ante card in a limited sideboard is refused too" $ do
    found <- violations s registry RuleFamily.Limited [("Mountain", 40)] [("Amulet of Quoz", 1)]
    Spec.assertEqWith s "violations" found [Violation.AnteCard (nameOf "Amulet of Quoz")]

  Spec.it s "CR 315.1: Power Play in a constructed sideboard is refused" $ do
    found <- violations s registry RuleFamily.Constructed [("Mountain", 60)] [("Power Play", 1)]
    Spec.assertEqWith s "violations" found [Violation.ConspiracyCard (nameOf "Power Play")]

  Spec.it s "CR 315.1: Power Play in a limited sideboard is not" $ do
    found <- violations s registry RuleFamily.Limited [("Mountain", 40)] [("Power Play", 1)]
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 207.5: a Cryptic Spires circling blue and black is legal, one circling none or three is not" $ do
    let constructed = Format.MkFormat RuleFamily.Constructed Nothing Set.empty
        circling colors = do
          spires <- S.printingOf s registry "Cryptic Spires"
          deck <- deckOf s registry [("Mountain", 59)] []
          pure (Legality.check noTable GameSettings.plain constructed Nothing deck {Deck.cards = Map.insert spires {Printing.circled = Set.fromList colors} 1 (Deck.cards deck)})
    two <- circling [Color.Blue, Color.Black]
    Spec.assertEqWith s "two circled" two []
    none <- circling []
    Spec.assertEqWith s "none circled" none [Violation.MiscircledCard (nameOf "Cryptic Spires") 0 2]
    three <- circling [Color.White, Color.Blue, Color.Black]
    Spec.assertEqWith s "three circled" three [Violation.MiscircledCard (nameOf "Cryptic Spires") 3 2]

  Spec.it s "CR 207.5: a sideboard Mountain circling two colours is refused" $ do
    mountain <- S.printingOf s registry "Mountain"
    deck <- deckOf s registry [("Mountain", 60)] []
    let found = Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.Limited Nothing Set.empty) Nothing deck {Deck.sideboard = Map.singleton mountain {Printing.circled = Set.fromList [Color.Red, Color.Green]} 1}
    Spec.assertEqWith s "violations" found [Violation.MiscircledCard (nameOf "Mountain") 2 0]

  Spec.it s "CR 903.5: Krenko leading Lightning Bolt and 98 Mountains is a legal Commander deck" $ do
    found <- ledViolationsNamed s registry "commander" ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 98)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 903.5c: Brainstorm is outside Krenko's colour identity" $ do
    found <- ledViolationsNamed s registry "commander" ["Krenko, Mob Boss"] [("Brainstorm", 1), ("Mountain", 98)] []
    Spec.assertEqWith s "violations" found [Violation.OutsideColorIdentity (nameOf "Brainstorm")]

  Spec.it s "CR 903.5c: Transguild Courier is outside Krenko's colour identity by the colours it defines" $ do
    found <- ledViolationsNamed s registry "commander" ["Krenko, Mob Boss"] [("Transguild Courier", 1), ("Mountain", 98)] []
    Spec.assertEqWith s "violations" found [Violation.OutsideColorIdentity (nameOf "Transguild Courier")]

  Spec.it s "CR 903.5: the commander is checked against the pool too, which bans Golos" $ do
    found <- ledViolationsNamed s registry "commander" ["Golos, Tireless Pilgrim"] [("Mountain", 99)] []
    Spec.assertEqWith s "violations" found [Violation.BannedCard (nameOf "Golos, Tireless Pilgrim")]

  Spec.it s "CR 903.5d: a Plains is outside Krenko's colour identity by the mana it makes" $ do
    found <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Plains", 1), ("Mountain", 98)] []
    Spec.assertEqWith s "violations" found [Violation.OutsideColorIdentity (nameOf "Plains")]

  Spec.it s "CR 903.5b: two Lightning Bolt in a Commander deck are one too many" $ do
    found <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 2), ("Mountain", 97)] []
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies Nothing (nameOf "Lightning Bolt") 2 1]

  Spec.it s "CR 903.5b: a second Krenko beside the commander is one too many" $ do
    found <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Krenko, Mob Boss", 1), ("Mountain", 98)] []
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies Nothing (nameOf "Krenko, Mob Boss") 2 1]

  Spec.it s "CR 903.5a: a Commander deck is exactly 100 cards, its commander included" $ do
    small <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 97)] []
    Spec.assertEqWith s "99" small [Violation.TooFewCards Nothing 99 100]
    large <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 99)] []
    Spec.assertEqWith s "101" large [Violation.TooManyCards 101 100]

  Spec.it s "CR 903.5e: Whtz, the Bibliophile lifts CR 903.5a's maximum for the deck it leads" $ do
    whtz <- ledViolationsNamed s registry "commander" ["Whtz, the Bibliophile"] [("Plains", 100)] []
    Spec.assertEqWith s "Whtz leading 101" whtz []
    other <- ledViolationsNamed s registry "commander" ["Cho-Manno, Revolutionary"] [("Plains", 100)] []
    Spec.assertEqWith s "Cho-Manno leading 101" other [Violation.TooManyCards 101 100]
    small <- ledViolationsNamed s registry "commander" ["Whtz, the Bibliophile"] [("Plains", 98)] []
    Spec.assertEqWith s "Whtz leading 99" small [Violation.TooFewCards Nothing 99 100]

  Spec.it s "CR 903.5e: Seluma, Light of Aysen admits Angel and basic land cards of any colour identity" $ do
    seluma <- ledViolationsNamed s registry "commander" ["Seluma, Light of Aysen"] [("Illusory Angel", 1), ("Island", 1), ("Plains", 97)] []
    Spec.assertEqWith s "Seluma leading a blue Angel and an Island" seluma []
    other <- ledViolationsNamed s registry "commander" ["Cho-Manno, Revolutionary"] [("Illusory Angel", 1), ("Island", 1), ("Plains", 97)] []
    Spec.assertEqWith s "Cho-Manno leading both" other [Violation.OutsideColorIdentity (nameOf "Illusory Angel"), Violation.OutsideColorIdentity (nameOf "Island")]
    brainstorm <- ledViolationsNamed s registry "commander" ["Seluma, Light of Aysen"] [("Brainstorm", 1), ("Plains", 98)] []
    Spec.assertEqWith s "Seluma leading a blue non-Angel" brainstorm [Violation.OutsideColorIdentity (nameOf "Brainstorm")]

  Spec.it s "CR 903.5e: Tolabow, Loch Rascal admits instant and sorcery cards of one chosen colour more" $ do
    black <- ledViolationsNamed s registry "commander" ["Tolabow, Loch Rascal"] [("Doom Blade", 1), ("Sign in Blood", 1), ("Swamp", 1), ("Island", 96)] []
    Spec.assertEqWith s "Tolabow leading a black instant, a black sorcery and a Swamp" black []
    other <- ledViolationsNamed s registry "commander" ["Tolabow, Loch Rascal"] [("Doom Blade", 1), ("Day of Judgment", 1), ("Island", 97)] []
    Spec.assertEqWith s "Tolabow leading a black instant and a white sorcery" other [Violation.OutsideColorIdentity (nameOf "Doom Blade")]
    most <- ledViolationsNamed s registry "commander" ["Tolabow, Loch Rascal"] [("Doom Blade", 1), ("Sign in Blood", 1), ("Day of Judgment", 1), ("Island", 96)] []
    Spec.assertEqWith s "Tolabow leading two black spells and a white one" most [Violation.OutsideColorIdentity (nameOf "Day of Judgment")]
    beyond <- ledViolationsNamed s registry "commander" ["Tolabow, Loch Rascal"] [("Crackling Doom", 1), ("Phyrexian Arena", 1), ("Island", 97)] []
    Spec.assertEqWith s "Tolabow leading a three-colour instant and a black enchantment" beyond [Violation.OutsideColorIdentity (nameOf "Crackling Doom"), Violation.OutsideColorIdentity (nameOf "Phyrexian Arena")]
    control <- ledViolationsNamed s registry "commander" ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Doom Blade", 1), ("Mountain", 97)] []
    Spec.assertEqWith s "Krenko leading a black instant" control [Violation.OutsideColorIdentity (nameOf "Doom Blade")]

  Spec.it s "CR 903.3: a Commander deck with no commander is refused" $ do
    found <- ledViolations s registry RuleFamily.Commander [] [("Mountain", 100)] []
    Spec.assertEqWith s "violations" found [Violation.NoCommander]

  Spec.it s "CR 702.124h: Rograkh beside Shimatsu, who has no partner, is refused" $ do
    refused <- ledViolations s registry RuleFamily.Commander ["Rograkh, Son of Rohgahh", "Shimatsu the Bloodcloaked"] [("Mountain", 98)] []
    Spec.assertEqWith s "refused" refused [Violation.IllegalCommander (nameOf "Rograkh, Son of Rohgahh"), Violation.IllegalCommander (nameOf "Shimatsu the Bloodcloaked")]
    partners <- ledViolations s registry RuleFamily.Commander ["Rograkh, Son of Rohgahh", "Akiri, Line-Slinger"] [("Mountain", 97), ("Plains", 1)] []
    Spec.assertEqWith s "control leg: Rograkh beside Akiri, both with partner, and her white" partners []

  Spec.it s "CR 903.5f / 702.139a: a Commander deck has no sideboard but its companion" $ do
    companion <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 98)] [("Zirda, the Dawnwaker", 1)]
    Spec.assertEqWith s "Zirda" companion []
    more <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 98)] [("Zirda, the Dawnwaker", 1), ("Lightning Bolt", 1)]
    Spec.assertEqWith s "Zirda and a Lightning Bolt" more [Violation.SideboardTooLarge 2 1]
    bolt <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 98)] [("Lightning Bolt", 1)]
    Spec.assertEqWith s "a Lightning Bolt" bolt [Violation.SideboardTooLarge 1 0]

  Spec.it s "CR 903.5: a Commander deck breaking four rules reports all four" $ do
    found <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 2), ("Brainstorm", 1), ("Mountain", 95)] [("Lightning Bolt", 1)]
    Spec.assertEqWith s "violations" found [Violation.TooFewCards Nothing 99 100, Violation.TooManyCopies Nothing (nameOf "Lightning Bolt") 2 1, Violation.SideboardTooLarge 1 0, Violation.OutsideColorIdentity (nameOf "Brainstorm")]

  Spec.it s "CR 903.12d: a Brawl deck is exactly 60 cards" $ do
    legal <- ledViolationsNamed s registry "brawl" ["Krenko, Mob Boss"] [("Mountain", 59)] []
    Spec.assertEqWith s "60" legal []
    large <- ledViolationsNamed s registry "brawl" ["Krenko, Mob Boss"] [("Mountain", 99)] []
    Spec.assertEqWith s "100" large [Violation.TooManyCards 100 60]

  Spec.it s "CR 903.12b: Lightning Bolt is not in a Brawl deck's Standard pool" $ do
    found <- ledViolationsNamed s registry "brawl" ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 58)] []
    Spec.assertEqWith s "violations" found [Violation.NotLegalCard (nameOf "Lightning Bolt")]

  -- The settings say nothing of Brawl, so the format alone admits Karn.
  Spec.it s "CR 903.12c / 903.12e: Karn Liberated leads a Brawl deck of Mountains, and no Commander deck" $ do
    brawl <- ledViolations s registry RuleFamily.Brawl ["Karn Liberated"] [("Mountain", 59)] []
    Spec.assertEqWith s "Brawl" brawl []
    commander <- ledViolations s registry RuleFamily.Commander ["Karn Liberated"] [("Mountain", 99)] []
    Spec.assertEqWith s "Commander" commander [Violation.IllegalCommander (nameOf "Karn Liberated"), Violation.OutsideColorIdentity (nameOf "Mountain")]

  Spec.it s "CR 903.12e: a colourless Brawl commander admits basic lands of one type only" $ do
    found <- ledViolations s registry RuleFamily.Brawl ["Karn Liberated"] [("Mountain", 30), ("Plains", 29)] []
    Spec.assertEqWith s "violations" found [Violation.OutsideColorIdentity (nameOf "Plains")]

  -- CR 903.4b: both Pipers chose red, so they are one printing twice.
  Spec.it s "CR 903.13e / 903.13f: two The Prismatic Piper lead a 70-card Commander Draft deck with three Lightning Bolt, and no Commander deck" $ do
    piper <- S.printingOf s registry "The Prismatic Piper"
    deck <- deckOf s registry [("Lightning Bolt", 3), ("Mountain", 65)] []
    let led = deck {Deck.commander = Map.singleton piper {Printing.chosenColor = Just Color.Red} 2}
    checker <- Legality.named (Text.pack "commanderdraft")
    draft <- either (Spec.assertFailure s . Text.unpack) (\known -> pure (known GameSettings.plain Nothing led)) checker
    Spec.assertEqWith s "Commander Draft" draft []
    let commander = Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.Commander Nothing Set.empty) Nothing led
    Spec.assertEqWith s "Commander" commander [Violation.TooFewCards Nothing 70 100, Violation.TooManyCopies Nothing (nameOf "Lightning Bolt") 3 1, Violation.TooManyCopies Nothing (nameOf "The Prismatic Piper") 2 1]

  Spec.it s "CR 903.13e / 903.13f(2): a Commander Draft deck draws on its card pool, but for two The Prismatic Piper leading it" $ do
    piper <- S.printingOf s registry "The Prismatic Piper"
    deck <- deckOf s registry [("Lightning Bolt", 3), ("Mountain", 65)] []
    let draft boosters = Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.CommanderDraft Nothing (Set.fromList boosters)) (Just (Map.singleton (nameOf "Lightning Bolt") 3))
        led = deck {Deck.commander = Map.singleton piper {Printing.chosenColor = Just Color.Red} 2}
    Spec.assertEqWith s "Commander Legends" (draft [DraftBooster.CommanderLegends] led) []
    Spec.assertEqWith s "Battle for Baldur's Gate" (draft [DraftBooster.BattleForBaldursGate] led) [Violation.NotInCardPool (nameOf "The Prismatic Piper")]
    let main = led {Deck.cards = Map.insert piper 1 (Deck.cards led)}
    Spec.assertEqWith s "a third in the deck" (draft [DraftBooster.CommanderLegends] main) [Violation.TooManyCopies Nothing (nameOf "The Prismatic Piper") 3 2]

  Spec.it s "CR 903.13f(1): a Commander Draft deck is at least 60 cards" $ do
    small <- ledViolations s registry RuleFamily.CommanderDraft ["Krenko, Mob Boss"] [("Mountain", 58)] []
    Spec.assertEqWith s "59" small [Violation.TooFewCards Nothing 59 60]

  Spec.it s "CR 903.13f(3): only a Commander Masters draft pairs Krenko with Shimatsu" $ do
    deck <- ledBy s registry ["Krenko, Mob Boss", "Shimatsu the Bloodcloaked"] [("Mountain", 58)] []
    let draft boosters = Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.CommanderDraft Nothing (Set.fromList boosters)) Nothing deck
    Spec.assertEqWith s "Commander Masters" (draft [DraftBooster.CommanderLegends, DraftBooster.CommanderMasters]) []
    Spec.assertEqWith s "Commander Legends" (draft [DraftBooster.CommanderLegends]) [Violation.IllegalCommander (nameOf "Krenko, Mob Boss"), Violation.IllegalCommander (nameOf "Shimatsu the Bloodcloaked")]

  Spec.it s "CR 903.13a / 903.13g: a Commander Draft deck is limited play in a Commander game" $ do
    deck <- ledBy s registry ["Krenko, Mob Boss"] [("Mountain", 59)] []
    sheets <- committedSheets s
    let draft = Legality.check noTable GameSettings.plain (Format.MkFormat RuleFamily.CommanderDraft Nothing Set.empty) Nothing
    Spec.assertEqWith s "CR 123.2b: two sticker sheets" (draft deck {Deck.stickerSheets = Seq.fromList (take 2 sheets)}) []
    attractions <- withSideDecks s registry RuleFamily.CommanderDraft GameSettings.plain deck [(SideDeck.Attraction, [("Bumper Cars", 3)])] Nothing
    Spec.assertEqWith s "CR 718.2b: three Bumper Cars" attractions []
    schemes <- withSideDecks s registry RuleFamily.CommanderDraft GameSettings.plain deck [(SideDeck.Scheme, [("All in Good Time", 2)])] Nothing
    Spec.assertEqWith s "CR 904.13d: two All in Good Time" schemes [Violation.TooFewCards (Just SideDeck.Scheme) 2 10, Violation.TooManyCopies (Just SideDeck.Scheme) (nameOf "All in Good Time") 2 1]

  Spec.it s "parun play --format refuses a name with no format file"
    . S.withCorpusDir "legality-play-nonesuch" [("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- started reference (Just (Text.pack "nonesuch")) [dir <> "/legal.txt", dir <> "/legal.txt"]
      Spec.assertEqWith s "refused" result (Left (fmap Text.pack [dir <> "/legal.txt: not a format: nonesuch", dir <> "/legal.txt: not a format: nonesuch"]))

  Spec.it s "parun play --format constructed refuses five Grizzly Bears before the game starts"
    . S.withCorpusDir "legality-play" [("bears.txt", Text.pack "5 Grizzly Bears\n55 Mountain\n"), ("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- started reference (Just (Text.pack "constructed")) [dir <> "/bears.txt", dir <> "/legal.txt"]
      Spec.assertEqWith s "refused" result (Left (fmap Text.pack [dir <> "/bears.txt: 5 copies of Grizzly Bears, more than 4"]))

  Spec.it s "parun play --format vintage refuses two Black Lotus before the game starts"
    . S.withCorpusDir "legality-play-vintage" [("lotus.txt", Text.pack "1 Black Lotus\n59 Mountain\nSideboard\n1 Black Lotus\n"), ("legal.txt", Text.pack "1 Black Lotus\n59 Mountain\n")]
    $ \dir -> do
      result <- started reference (Just (Text.pack "vintage")) [dir <> "/lotus.txt", dir <> "/legal.txt"]
      Spec.assertEqWith s "refused" result (Left (fmap Text.pack [dir <> "/lotus.txt: 2 copies of Black Lotus, more than 1"]))

  Spec.it s "parun play with no --format refuses a Format: limited list of 39 cards"
    . S.withCorpusDir "legality-play-header" [("small.txt", Text.pack "Format: limited\n4 Grizzly Bears\n35 Mountain\n"), ("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- started reference Nothing [dir <> "/small.txt", dir <> "/legal.txt"]
      Spec.assertEqWith s "refused" result (Left (fmap Text.pack [dir <> "/small.txt: 39 cards in the deck, fewer than 40"]))

  Spec.it s "parun play --format vintage refuses a Format: modern list"
    . S.withCorpusDir "legality-play-disagrees" [("modern.txt", Text.pack "Format: modern\n60 Mountain\n"), ("legal.txt", Text.pack "60 Mountain\n")]
    $ \dir -> do
      result <- started reference (Just (Text.pack "vintage")) [dir <> "/modern.txt", dir <> "/legal.txt"]
      Spec.assertEqWith s "refused" result (Left (fmap Text.pack [dir <> "/modern.txt: line 1: the list names the format modern, not vintage"]))

  -- The format a game starts under when no --format names one. Neither
  -- command's game is played here: RecordSpec's Yorion, Sky Nomad case is a
  -- format reaching the game and being read there (CR 702.139a, #6724).
  Spec.it s "CR 100.2a parun play with no --format settles on the format both headers name"
    . S.withCorpusDir "legality-play-header-game" [("one.txt", yorionList (Just "constructed")), ("two.txt", yorionList (Just "constructed"))]
    $ \dir -> do
      format <- started reference Nothing [dir <> "/one.txt", dir <> "/two.txt"]
      Spec.assertEqWith s "the headers' format" format (Right (Just (Format.MkFormat RuleFamily.Constructed Nothing Set.empty)))

  Spec.it s "CR 100.2a parun match settles on the format one header names"
    . S.withCorpusDir "legality-match-header-game" [("one.txt", yorionList Nothing), ("two.txt", yorionList (Just "constructed"))]
    $ \dir -> do
      format <- started reference Nothing [dir <> "/one.txt", dir <> "/two.txt"]
      Spec.assertEqWith s "the header's format" format (Right (Just (Format.MkFormat RuleFamily.Constructed Nothing Set.empty)))

  Spec.it s "parun play refuses lists whose headers name different formats"
    . S.withCorpusDir "legality-play-headers-disagree" [("constructed.txt", Text.pack "Format: constructed\n60 Mountain\n"), ("limited.txt", Text.pack "Format: limited\n60 Mountain\n")]
    $ \dir -> do
      result <- started reference Nothing [dir <> "/constructed.txt", dir <> "/limited.txt"]
      Spec.assertEqWith s "refused" result (Left (fmap Text.pack [dir <> "/constructed.txt names constructed, " <> dir <> "/limited.txt names limited; name one with --format"]))

  Spec.it s "CR 100.2b parun play checks a list with no header against the format the other names"
    . S.withCorpusDir "legality-play-unheaded" [("small.txt", Text.pack "4 Grizzly Bears\n35 Mountain\n"), ("limited.txt", Text.pack "Format: limited\n4 Grizzly Bears\n36 Mountain\n")]
    $ \dir -> do
      result <- started reference Nothing [dir <> "/small.txt", dir <> "/limited.txt"]
      Spec.assertEqWith s "refused" result (Left (fmap Text.pack [dir <> "/small.txt: 39 cards in the deck, fewer than 40"]))

-- Eighty cards, twenty over constructed's sixty, with Yorion, Sky Nomad as
-- the companion, under a header naming `format` if any.
yorionList :: Maybe String -> Text.Text
yorionList format = Text.pack (foldMap (\name -> "Format: " <> name <> "\n") format <> "4 Grizzly Bears\n76 Forest\nCompanion\n1 Yorion, Sky Nomad\n")

-- The format `parun play` and `parun match` start a game between these lists
-- under, the one `asked` for if any, or why they refuse to (Command.load,
-- Command.settle), the cards named looked up in `reference`.
started :: Registry.Registry Identity.Identity -> Maybe Text.Text -> [FilePath] -> IO (Either [Text.Text] (Maybe Format.Format))
started reference asked paths = do
  lists <- traverse (Command.load reference) paths
  either (pure . Left) (Command.settle asked . zip paths) (sequence lists)
