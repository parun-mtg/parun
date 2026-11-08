-- Covers Parun.Legality, and `parun play --format`'s refusal through
-- Parun.Play.Command.
module Parun.LegalitySpec where

import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command as Command
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Format as Format
import qualified Parun.Types.Violation as Violation
import qualified System.Exit as Exit

-- A deck and a sideboard, each as (name, count) pairs.
deckOf :: Spec.Spec IO n -> Registry.Registry IO -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO Deck.Deck
deckOf s registry main side = do
  let section entries = fmap (Map.fromListWith (+)) (traverse (\(name, count) -> fmap (\p -> (p, count)) (S.printingOf s registry name)) entries)
  cards <- section main
  sideboard <- section side
  pure (Deck.fromCards cards) {Deck.sideboard = sideboard}

-- The violations a deck raises in a format.
violations :: Spec.Spec IO n -> Registry.Registry IO -> Format.Format -> [(String, Natural.Natural)] -> [(String, Natural.Natural)] -> IO [Violation.Violation]
violations s registry format main side = fmap (Legality.check format) (deckOf s registry main side)

copies :: String -> Natural.Natural -> Violation.Violation
copies name count = Violation.TooManyCopies (CardName.MkCardName (Text.pack name)) count 4

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Parun.Legality" $ do
  Spec.it s "CR 100.2a: five Grizzly Bears in a constructed deck are one too many" $ do
    found <- violations s registry Format.Constructed [("Grizzly Bears", 5), ("Mountain", 55)] []
    Spec.assertEqWith s "violations" found [copies "Grizzly Bears" 5]

  Spec.it s "CR 100.2a: a constructed deck of 59 cards is too small" $ do
    found <- violations s registry Format.Constructed [("Grizzly Bears", 4), ("Mountain", 55)] []
    Spec.assertEqWith s "violations" found [Violation.TooFewCards 59 60]

  Spec.it s "CR 100.4a: the four-card limit counts the sideboard too" $ do
    found <- violations s registry Format.Constructed [("Lightning Bolt", 3), ("Mountain", 57)] [("Lightning Bolt", 2)]
    Spec.assertEqWith s "violations" found [copies "Lightning Bolt" 5]

  Spec.it s "CR 100.4a: a sixteen-card sideboard is too large" $ do
    found <- violations s registry Format.Constructed [("Mountain", 60)] [("Mountain", 16)]
    Spec.assertEqWith s "violations" found [Violation.SideboardTooLarge 16 15]

  Spec.it s "CR 100.2a / 100.4a: a deck breaking four rules reports all four" $ do
    found <- violations s registry Format.Constructed [("Grizzly Bears", 5), ("Lightning Bolt", 3), ("Mountain", 50)] [("Lightning Bolt", 2), ("Mountain", 14)]
    Spec.assertEqWith s "violations" found [Violation.TooFewCards 58 60, copies "Grizzly Bears" 5, copies "Lightning Bolt" 5, Violation.SideboardTooLarge 16 15]

  Spec.it s "CR 100.2a / 205.4c: basic land cards are exempt by supertype, snow basics included" $ do
    found <- violations s registry Format.Constructed [("Grizzly Bears", 4), ("Mountain", 28), ("Snow-Covered Mountain", 28)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2b: a limited deck of forty has no copy limit" $ do
    found <- violations s registry Format.Limited [("Grizzly Bears", 5), ("Mountain", 35)] []
    Spec.assertEqWith s "violations" found []

  Spec.it s "CR 100.2b: a limited deck of 39 cards is too small" $ do
    found <- violations s registry Format.Limited [("Grizzly Bears", 5), ("Mountain", 34)] []
    Spec.assertEqWith s "violations" found [Violation.TooFewCards 39 40]

  Spec.it s "a format is named on the command line by its name" $
    Spec.assertEqWith s "round trip" (traverse (Legality.named . Legality.name) [minBound .. maxBound]) (Just [minBound .. maxBound :: Format.Format])

  -- Two bot seats, so a game the refusal let through would play to its end
  -- and return rather than wait on input.
  Spec.it s "parun play --format constructed refuses five Grizzly Bears before the game starts"
    . S.withCorpusDir "legality-play" [("bears.txt", Text.pack "5 Grizzly Bears\n55 Mountain\n"), ("legal.txt", Text.pack "4 Grizzly Bears\n56 Mountain\n")]
    $ \dir -> do
      result <- Exception.try (Command.main ["--format", "constructed", "--seed", "1", "--first", "1", "--bot", "1", "--bot", "2", dir <> "/bears.txt", dir <> "/legal.txt"])
      Spec.assertEqWith s "exit" result (Left (Exit.ExitFailure 1))
