-- Covers Parun.Play.Match: a match's games, each saved game replayed, and the
-- tally.
module Parun.MatchSpec where

import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Parun.BotSpec as BotSpec
import qualified Parun.Codec.Scenario as Codec.Scenario
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Match as Match
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator
import qualified Parun.Types.Key as Key
import qualified Parun.Types.Label as Label
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.Result as Result
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.Tally as Tally
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.When as When

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Match" $ do
  Spec.it s "the decks take turns playing first, game by game" $ do
    decks <- pair <$> BotSpec.bears s registry
    starts <- mapM (\index -> firstToAct . snd <$> Match.game (S.fixtureRegistry []) Nothing (key 7) index decks) [0, 1, 2, 3]
    Spec.assertEqWith s "first to act on turn 1" starts (fmap (Just . Label.MkLabel . Text.pack) ["player1", "player2", "player1", "player2"])

  Spec.it s "each game of a match replays clean to its result" $ do
    decks <- pair <$> BotSpec.bears s registry
    Foldable.for_ [0, 1, 2, 3] $ \index -> do
      (outcome, timeline) <- Match.game (S.fixtureRegistry []) Nothing (key 7) index decks
      let matchup = Command.seated (Match.firstAt index) decks
      replayed <- Scenario.run registry (saved matchup timeline)
      case (outcome, replayed) of
        (Right (result, _), Right final) -> Spec.assertEqWith s ("game " <> show index) (fmap (renumbered matchup) (GameState.result final)) (Just result)
        (_, Left failure) -> Spec.assertFailure s (Text.unpack (Scenario.render failure))
        (Left reason, _) -> Spec.assertFailure s (Text.unpack reason)

  Spec.it s "a game's key is derived from the match's and its index" $ do
    decks <- pair <$> BotSpec.bears s registry
    once <- Match.game (S.fixtureRegistry []) Nothing (key 7) 2 decks
    again <- Match.game (S.fixtureRegistry []) Nothing (key 7) 2 decks
    early <- Match.game (S.fixtureRegistry []) Nothing (key 7) 0 decks
    other <- Match.game (S.fixtureRegistry []) Nothing (key 9) 2 decks
    Spec.assertEqWith s "the same game again" (snd once) (snd again)
    Spec.assertBool s (snd once /= snd early) "another index, another game"
    Spec.assertBool s (snd once /= snd other) "another match, another game"
    Spec.assertBool s (Match.seedAt (key 7) 2 /= key 7) "not the match's own key"

  Spec.it s "tallies each result, the stops, and the turns of a game with a result" $ do
    let ended n result =
          Right
            ( result,
              S.threePlayerGame
                { GameState.turn =
                    (GameState.turn S.threePlayerGame)
                      { TurnRegisters.turnNumber = n
                      }
                }
            )
        tally = foldr Match.tallied Tally.empty [ended 9 (Result.Won S.alice), ended 12 (Result.Won S.bob), ended 10 (Result.Won S.alice), ended 6 Result.Drawn, Left (Text.pack "end of input")]
    Spec.assertEqWith s "tally" tally Tally.MkTally {Tally.results = Map.fromList [(Result.Won S.alice, 2), (Result.Won S.bob, 1), (Result.Drawn, 1)], Tally.stopped = 1, Tally.turns = 37}
    Spec.assertEqWith
      s
      "summary"
      (Match.summary 5 tally)
      (fmap Text.pack ["Seat 1 won 2 of 5 games.", "Seat 2 won 1 of 5 games.", "1 drawn, 1 stopped.", "A game with a result lasted 9.3 turns on average."])

  Spec.it s "CR 104.2c: a team's wins are told apart from a seat's" $ do
    let tally = Tally.empty {Tally.results = Map.fromList [(Result.TeamWon (TeamId.MkTeamId 1), 1)], Tally.turns = 4}
    Spec.assertEqWith
      s
      "summary"
      (Match.summary 1 tally)
      (fmap Text.pack ["Seat 1 won 0 of 1 games.", "Seat 2 won 0 of 1 games.", "Team 1 won 1 of 1 games.", "0 drawn, 0 stopped.", "A game with a result lasted 4.0 turns on average."])

-- The decks of a two-seat matchup.
-- The key 'S.seeded' starts from.
key :: Word.Word32 -> Key.Key
key = Generator.key . S.seeded

pair :: NonEmpty.NonEmpty (a, d) -> (d, d)
pair matchup = case NonEmpty.toList matchup of
  [(_, one), (_, two)] -> (one, two)
  _ -> error "not two seats"

-- The seat of the first entry on turn 1.
firstToAct :: Seq.Seq Timed.Timed -> Maybe Label.Label
firstToAct = fmap (When.player . Timed.when) . Foldable.find ((== 1) . When.turn . Timed.when)

-- A replayed game's result in the played game's seats: the replay numbers
-- its players in the matchup's order (Scenario.enter).
renumbered :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Result.Result -> Result.Result
renumbered matchup result = case result of
  Result.Won pid -> Result.Won (fst (matchup NonEmpty.!! Natural.toIntSaturating (PlayerId.unwrap pid)))
  _ -> result

-- The scenario a match game saves as, read back.
saved :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Seq.Seq Timed.Timed -> Scenario.Type.Scenario
saved matchup timeline = Either.either (error . Text.unpack) id (Codec.decode Codec.Scenario.codec (Codec.encode Codec.Scenario.codec (Command.saved PrintingTable.empty Text.empty Nothing Map.empty matchup timeline)))
