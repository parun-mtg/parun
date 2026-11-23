-- Covers Parun.Decks and data/decks: every shipped deck list and set list
-- reads, a list whose header names a format is legal under it, the Portal
-- and Starter sets' starter pairs each play the opening turns of a game that
-- replays, two Commander lists start a Commander game, and
-- data/decks/sets.json names and dates every set.
module Parun.DecksSpec where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Engine.Game as Game
import qualified Parun.Play.Command as Command
import qualified Parun.RecordSpec as RecordSpec
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Player as Player
import qualified Parun.Types.Zone as Zone
import qualified Parun.Web.DeckBrowser as DeckBrowser

spec :: (MonadIO.MonadIO n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = do
  -- Listed, not read: each case reads its own file.
  root <- MonadIO.liftIO Decks.defaultRoot
  decks <- MonadIO.liftIO (Decks.listRoot root)
  Spec.describe s "Decks" $ do
    Spec.it s "the listing reaches a set's own directory" $
      Spec.assertEqWith s "sets/m15/all.txt listed" ((root <> "/sets/m15/all.txt") `elem` decks) True

    -- The web client's deck browser names a precon's set from it.
    Spec.it s "sets.json names and dates every set a directory holds decks for, and no other" $ do
      table <- Decks.setsPath >>= Decks.loadFile
      let held = Set.fromList (Maybe.mapMaybe (fst . DeckBrowser.describe Map.empty . Text.pack . drop (length root + 1)) decks)
      Spec.assertEq s (fmap (Set.fromList . Map.keys) (table >>= DeckBrowser.releasesOf)) (Right held)

    Monad.forM_ decks $ \path -> Spec.it s (drop (length root + 1) path <> " reads") $ do
      contents <- Decks.loadFile path
      parsed <- either (pure . Left . (: []) . Text.unpack) (fmap (either (Left . (: []) . Exception.displayException) (either (Left . fmap (Text.unpack . DeckList.explain (const Nothing))) Right . DeckList.sound)) . DeckList.parse registry) contents
      Spec.assertEqWith s (path <> ": problems") (Either.fromLeft [] parsed) []

    -- The web client's lobby starts a game under the format the lists'
    -- headers name (Parun.Web.Lobby.opponents), so each must be legal there.
    Monad.forM_ decks $ \path -> Spec.it s (drop (length root + 1) path <> " is legal under the format its header names") $ do
      listed <- readListed path
      case listed of
        Right one | Just _ <- DeckList.format one -> Command.settle Nothing [(path, one)] >>= \settled -> Spec.assertEqWith s (path <> ": refusals") (Either.fromLeft [] settled) []
        Right _ -> pure ()
        Left problem -> Spec.assertFailure s problem

    -- CR 903.6, 903.7. Seen as the first turn ends: the first question is the
    -- first seat's shuffle, asked before the second seat is set up.
    Spec.it s "two Commander lists start a Commander game: each seat at 40 life, its commander in the command zone" $ do
      let path name = root <> "/sets/fdc/" <> name <> ".txt"
      lists <- (,) <$> readListed (path "calling-all-angels") <*> readListed (path "wretched-ranks")
      case lists of
        (Right one, Right two) -> do
          settled <- Command.settle Nothing [(path "calling-all-angels", one), (path "wretched-ranks", two)]
          case settled of
            Right format -> do
              let matchup = (S.alice, DeckList.deck one) NonEmpty.:| [(S.bob, DeckList.deck two)]
                  (_, (_, opened)) = RecordSpec.watched (Just 1) 1 format RecordSpec.bots Map.empty matchup
                  seen gs = [(Player.life p, length (Game.zoneMembers Zone.Command pid gs)) | (pid, p) <- Map.toList (GameState.players gs)]
              Spec.assertEqWith s "each seat's life and command zone" (fmap seen opened) (Just [(40, 1), (40, 1)])
            Left problems -> Spec.assertFailure s (show problems)
        _ -> Spec.assertFailure s "the Commander lists did not read"

    let starters = ("starter-1", "starter-2")
    Monad.forM_ [("Portal", "por", starters), ("Portal Second Age", "p02", starters), ("Portal Three Kingdoms", "ptk", starters), ("Starter 1999", "s99", starters), ("Starter 2000", "s00", starters), ("Introductory Two-Player Set", "itp", ("kazzs-first-deck", "zakks-first-deck"))] $ \(set, code, (first, second)) ->
      Spec.it s ("two bots open a game with the " <> set <> " starter decks, and it replays clean") $ do
        let read' name = do
              contents <- Decks.loadFile (root <> "/sets/" <> code <> name)
              either (const (pure (Left []))) (fmap (either (const (Left [])) DeckList.sound) . DeckList.parse registry) contents
        decksRead <- (,) <$> read' ("/" <> first <> ".txt") <*> read' ("/" <> second <> ".txt")
        case decksRead of
          (Right one, Right two) -> played (S.alice, one) (S.bob, two)
          _ -> Spec.assertFailure s "the starter decks did not read"
  where
    readListed path = do
      contents <- Decks.loadFile path
      either (pure . Left . Text.unpack) (fmap (Bifunctor.first Exception.displayException) . DeckList.parse registry) contents
    -- Six turns: each seat's opening, which a list that reads but does not
    -- play would not survive.
    played seatOne seatTwo = do
      let matchup = seatOne NonEmpty.:| [seatTwo]
          (_, (timeline, _)) = RecordSpec.watched (Just 6) 1 Nothing RecordSpec.bots Map.empty matchup
      Spec.assertBool s (not (null timeline)) "played"
      RecordSpec.replaysClean s registry (RecordSpec.saved matchup timeline)
