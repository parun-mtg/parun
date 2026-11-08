-- Covers Parun.Legality, the shipped formats and legality table, and `parun
-- play --format`'s refusal through Parun.Play.Command.
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
