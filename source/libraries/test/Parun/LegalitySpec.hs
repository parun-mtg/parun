-- Covers Parun.Legality, the shipped formats and legality table, and the
-- refusal `parun play` and `parun match` make through Parun.Play.Command.
module Parun.LegalitySpec where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Numeric.Natural as Natural
import qualified Parun.DeckList as DeckList
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Match as Match
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Types.Violation as Violation
import qualified System.Directory as Directory
import qualified System.Exit as Exit

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
violations s registry family = violationsIn s registry GameSettings.plain (Format.MkFormat family Nothing) noTable

violationsIn :: Spec.Spec IO n -> Registry.Registry IO -> GameSettings.GameSettings -> Format.Format -> LegalityTable.LegalityTable -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
violationsIn s registry settings format table main side = fmap (Legality.check table settings format) (deckOf s registry main side)

-- The violations a deck raises in a shipped format, by name, in a game not
-- for ante.
violationsNamed :: Spec.Spec IO n -> Registry.Registry IO -> String -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
violationsNamed s registry name main side = do
  checker <- Legality.named (Text.pack name)
  deck <- deckOf s registry main side
  either (Spec.assertFailure s . Text.unpack) (\known -> pure (known GameSettings.plain deck)) checker

-- A deck and a sideboard led by the commanders named.
ledBy :: Spec.Spec IO n -> Registry.Registry IO -> [String] -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO Deck.Deck
ledBy s registry commanders main side = do
  deck <- deckOf s registry main side
  named <- traverse (S.printingOf s registry) commanders
  pure deck {Deck.commander = Set.fromList named}

-- The violations a deck led by the commanders named raises under a rule
-- family with no pool, in a game whose settings say nothing of Brawl.
ledViolations :: Spec.Spec IO n -> Registry.Registry IO -> RuleFamily.RuleFamily -> [String] -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
ledViolations s registry family commanders main side = fmap (Legality.check noTable GameSettings.plain (Format.MkFormat family Nothing)) (ledBy s registry commanders main side)

-- The violations a deck led by the commanders named raises in a shipped
-- format, by name, in a game whose settings say nothing of Brawl.
ledViolationsNamed :: Spec.Spec IO n -> Registry.Registry IO -> String -> [String] -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
ledViolationsNamed s registry name commanders main side = do
  checker <- Legality.named (Text.pack name)
  deck <- ledBy s registry commanders main side
  either (Spec.assertFailure s . Text.unpack) (\known -> pure (known GameSettings.plain deck)) checker

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

copies :: String -> Natural.Natural -> Violation.Violation
copies name count = Violation.TooManyCopies (CardName.MkCardName (Text.pack name)) count 4

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Parun.Legality" $ do
  Spec.it s "CR 100.2a: five Grizzly Bears in a constructed deck are one too many" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 5), ("Mountain", 55)] []
    Spec.assertEqWith s "violations" found [copies "Grizzly Bears" 5]

  -- A misspelt name does not keep the rest of the list from being checked:
  -- both are reported together (#6396).
  Spec.it s "CR 100.2a: a list with a bad line still has its five Grizzly Bears counted" $ do
    let text = "5 Grizzly Bears\n55 Mountain\n1 Lightnin Bolt\n"
    listed <- DeckList.parse registry (Text.pack text) >>= either (S.brokenCorpus s) pure
    (format, reported) <- Legality.report (Just (Text.pack "constructed")) listed
    Spec.assertEqWith s "the format" format (Just (Text.pack "constructed"))
    Spec.assertEqWith s "the bad line, then the violation" reported [DeckList.explain (DeckList.NoSuchCard 3 (nameOf "Lightnin Bolt")), Legality.explain (copies "Grizzly Bears" 5)]
    headed <- DeckList.parse registry (Text.pack ("Format: limited\n" <> text)) >>= either (S.brokenCorpus s) pure
    (headerFormat, headerReported) <- Legality.report Nothing headed
    Spec.assertEqWith s "the header's format" headerFormat (Just (Text.pack "limited"))
    Spec.assertEqWith s "under which five are legal" headerReported [DeckList.explain (DeckList.NoSuchCard 4 (nameOf "Lightnin Bolt"))]

  Spec.it s "CR 100.2a: a constructed deck of 59 cards is too small" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 4), ("Mountain", 55)] []
    Spec.assertEqWith s "violations" found [Violation.TooFewCards 59 60]

  Spec.it s "CR 100.4a: the four-card limit counts the sideboard too" $ do
    found <- violations s registry RuleFamily.Constructed [("Lightning Bolt", 3), ("Mountain", 57)] [("Lightning Bolt", 2)]
    Spec.assertEqWith s "violations" found [copies "Lightning Bolt" 5]

  Spec.it s "CR 100.4a: a sixteen-card sideboard is too large" $ do
    found <- violations s registry RuleFamily.Constructed [("Mountain", 60)] [("Mountain", 16)]
    Spec.assertEqWith s "violations" found [Violation.SideboardTooLarge 16 15]

  Spec.it s "CR 100.2a / 100.4a: a deck breaking four rules reports all four" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 5), ("Lightning Bolt", 3), ("Mountain", 50)] [("Lightning Bolt", 2), ("Mountain", 14)]
    Spec.assertEqWith s "violations" found [Violation.TooFewCards 58 60, copies "Grizzly Bears" 5, copies "Lightning Bolt" 5, Violation.SideboardTooLarge 16 15]

  Spec.it s "CR 100.2a / 205.4c: basic land cards are exempt by supertype, snow basics included" $ do
    found <- violations s registry RuleFamily.Constructed [("Grizzly Bears", 4), ("Mountain", 28), ("Snow-Covered Mountain", 28)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2b: a limited deck of forty has no copy limit" $ do
    found <- violations s registry RuleFamily.Limited [("Grizzly Bears", 5), ("Mountain", 35)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2b: a limited deck of 39 cards is too small" $ do
    found <- violations s registry RuleFamily.Limited [("Grizzly Bears", 5), ("Mountain", 34)] []
    Spec.assertEqWith s "violations" found [Violation.TooFewCards 39 40]

  Spec.it s "CR 100.2a: a deck can have up to nine cards named Nazgul" $ do
    found <- violations s registry RuleFamily.Constructed [("Nazgûl", 9), ("Mountain", 51)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2a: ten cards named Nazgul are one too many" $ do
    found <- violations s registry RuleFamily.Constructed [("Nazgûl", 10), ("Mountain", 50)] []
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies (CardName.MkCardName (Text.pack "Nazgûl")) 10 9]

  Spec.it s "CR 100.4a: a card's own limit counts the sideboard too" $ do
    found <- violations s registry RuleFamily.Constructed [("Nazgûl", 9), ("Mountain", 51)] [("Nazgûl", 1)]
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies (CardName.MkCardName (Text.pack "Nazgûl")) 10 9]

  Spec.it s "CR 100.2a: a deck can have any number of cards named Relentless Rats" $ do
    found <- violations s registry RuleFamily.Constructed [("Relentless Rats", 40), ("Mountain", 20)] [("Relentless Rats", 15)]
    Spec.assertEqWith s "violations" found []

  -- Nazgûl's ruling of 2023-06-16: in limited play the rest stay in the
  -- sideboard (CR 100.4b), so the deck alone is counted.
  Spec.it s "CR 100.2b: a limited deck can have nine Nazgul but not ten" $ do
    nine <- violations s registry RuleFamily.Limited [("Nazgûl", 9), ("Mountain", 31)] [("Nazgûl", 3)]
    Spec.assertEqWith s "nine" nine []
    ten <- violations s registry RuleFamily.Limited [("Nazgûl", 10), ("Mountain", 30)] []
    Spec.assertEqWith s "ten" ten [Violation.TooManyCopies (CardName.MkCardName (Text.pack "Nazgûl")) 10 9]

  Spec.it s "every shipped format file decodes, constructed and limited with no pool" $ do
    root <- Legality.formatsRoot
    names <- shippedNames
    loaded <- traverse (Legality.loadFormat root) names
    Spec.assertEqWith s "problems" [(name, problem) | (name, Just (Left problem)) <- zip names loaded] []
    Spec.assertEqWith s "constructed" (lookup (Text.pack "constructed") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Constructed Nothing))))
    Spec.assertEqWith s "limited" (lookup (Text.pack "limited") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Limited Nothing))))
    Spec.assertEqWith s "commander" (lookup (Text.pack "commander") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Commander (Just (Text.pack "commander"))))))
    Spec.assertEqWith s "brawl, from Standard's pool (CR 903.12b)" (lookup (Text.pack "brawl") (zip names loaded)) (Just (Just (Right (Format.MkFormat RuleFamily.Brawl (Just (Text.pack "standard"))))))

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
    Spec.assertEqWith s "two" two [Violation.TooManyCopies (nameOf "Black Lotus") 2 1]

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
    let constructed = Format.MkFormat RuleFamily.Constructed Nothing
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
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies (nameOf "Lightning Bolt") 2 1]

  Spec.it s "CR 903.5b: a second Krenko beside the commander is one too many" $ do
    found <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Krenko, Mob Boss", 1), ("Mountain", 98)] []
    Spec.assertEqWith s "violations" found [Violation.TooManyCopies (nameOf "Krenko, Mob Boss") 2 1]

  Spec.it s "CR 903.5a: a Commander deck is exactly 100 cards, its commander included" $ do
    small <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 97)] []
    Spec.assertEqWith s "99" small [Violation.TooFewCards 99 100]
    large <- ledViolations s registry RuleFamily.Commander ["Krenko, Mob Boss"] [("Lightning Bolt", 1), ("Mountain", 99)] []
    Spec.assertEqWith s "101" large [Violation.TooManyCards 101 100]

  Spec.it s "CR 903.5e: Whtz, the Bibliophile lifts CR 903.5a's maximum for the deck it leads" $ do
    whtz <- ledViolationsNamed s registry "commander" ["Whtz, the Bibliophile"] [("Plains", 100)] []
    Spec.assertEqWith s "Whtz leading 101" whtz []
    other <- ledViolationsNamed s registry "commander" ["Cho-Manno, Revolutionary"] [("Plains", 100)] []
    Spec.assertEqWith s "Cho-Manno leading 101" other [Violation.TooManyCards 101 100]
    small <- ledViolationsNamed s registry "commander" ["Whtz, the Bibliophile"] [("Plains", 98)] []
    Spec.assertEqWith s "Whtz leading 99" small [Violation.TooFewCards 99 100]

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
    Spec.assertEqWith s "violations" found [Violation.TooFewCards 99 100, Violation.TooManyCopies (nameOf "Lightning Bolt") 2 1, Violation.SideboardTooLarge 1 0, Violation.OutsideColorIdentity (nameOf "Brainstorm")]

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

  Spec.it s "parun play --format refuses a name with no format file"
    . S.withCorpusDir "legality-play-nonesuch" [("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- Exception.try (Command.main ["--format", "nonesuch", "--seed", "1", "--first", "1", "--bot", "1", "--bot", "2", dir <> "/legal.txt", dir <> "/legal.txt"])
      Spec.assertEqWith s "exit" result (Left (Exit.ExitFailure 1))

  -- Two bot seats, so a game the refusal let through would play to its end
  -- and return rather than wait on input.
  Spec.it s "parun play --format constructed refuses five Grizzly Bears before the game starts"
    . S.withCorpusDir "legality-play" [("bears.txt", Text.pack "5 Grizzly Bears\n55 Mountain\n"), ("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- Exception.try (Command.main ["--format", "constructed", "--seed", "1", "--first", "1", "--bot", "1", "--bot", "2", dir <> "/bears.txt", dir <> "/legal.txt"])
      Spec.assertEqWith s "exit" result (Left (Exit.ExitFailure 1))

  Spec.it s "parun play --format vintage refuses two Black Lotus before the game starts"
    . S.withCorpusDir "legality-play-vintage" [("lotus.txt", Text.pack "1 Black Lotus\n59 Mountain\nSideboard\n1 Black Lotus\n"), ("legal.txt", Text.pack "1 Black Lotus\n59 Mountain\n")]
    $ \dir -> do
      result <- Exception.try (Command.main ["--format", "vintage", "--seed", "1", "--first", "1", "--bot", "1", "--bot", "2", dir <> "/lotus.txt", dir <> "/legal.txt"])
      Spec.assertEqWith s "exit" result (Left (Exit.ExitFailure 1))

  Spec.it s "parun play with no --format refuses a Format: limited list of 39 cards"
    . S.withCorpusDir "legality-play-header" [("small.txt", Text.pack "Format: limited\n4 Grizzly Bears\n35 Mountain\n"), ("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- Exception.try (Command.main ["--seed", "1", "--first", "1", "--bot", "1", "--bot", "2", dir <> "/small.txt", dir <> "/legal.txt"])
      Spec.assertEqWith s "exit" result (Left (Exit.ExitFailure 1))

  Spec.it s "parun play --format vintage refuses a Format: modern list"
    . S.withCorpusDir "legality-play-disagrees" [("modern.txt", Text.pack "Format: modern\n60 Mountain\n"), ("legal.txt", Text.pack "60 Mountain\n")]
    $ \dir -> do
      result <- Exception.try (Command.main ["--format", "vintage", "--seed", "1", "--first", "1", "--bot", "1", "--bot", "2", dir <> "/modern.txt", dir <> "/legal.txt"])
      Spec.assertEqWith s "exit" result (Left (Exit.ExitFailure 1))

  Spec.it s "parun match refuses a Format: limited list of 39 cards"
    . S.withCorpusDir "legality-match-header" [("small.txt", Text.pack "Format: limited\n4 Grizzly Bears\n35 Mountain\n"), ("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- Exception.try (Match.main ["--games", "1", "--seed", "1", dir <> "/small.txt", dir <> "/legal.txt"])
      Spec.assertEqWith s "exit" result (Left (Exit.ExitFailure 1))
