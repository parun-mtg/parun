-- | `parun match`: many seeded games between two decks, each seat played by
-- the random bot or the search bot, and how often each seat won.
module Parun.Play.Match where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Functor.Identity as Identity
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Record as Record
import qualified Parun.Play.Search as Search
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Trustworthy.Directory as Directory
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Tally as Tally
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Weights as Weights.Type
import qualified Parun.Weights as Weights
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: [String] -> IO ()
main arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, [one, two], []) -> case Monad.foldM (\options update -> update options) MatchOptions.none updates of
    Left problem -> usage [problem]
    Right options -> do
      registry <- Registry.defaultRoot >>= Registry.loadRoot >>= Registry.defaultRegistry
      lists <- (,) <$> Command.orExit (Command.load registry one) <*> Command.orExit (Command.load registry two)
      format <- Command.orExit (Command.settle Nothing [(one, fst lists), (two, snd lists)])
      searchers <- Command.orExit (fmap (Bifunctor.first pure) (searchersOf options))
      prior <- priorOf registry options
      match options searchers prior registry format (Bifunctor.bimap DeckList.deck DeckList.deck lists)
  (_, _, problems) -> usage problems

flags :: [GetOpt.OptDescr (MatchOptions.MatchOptions -> Either String MatchOptions.MatchOptions)]
flags =
  [ GetOpt.Option [] ["seed"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a seed: " <> s)) (\k -> Right o {MatchOptions.seed = Just k}) (Key.fromHex (Text.pack s))) "KEY") "the key each game's is derived from, 64 hexadecimal digits, said when the match ends; drawn from the operating system when absent",
    GetOpt.Option [] ["games"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a number of games: " <> s)) (\n -> Right o {MatchOptions.games = n}) (Read.readMaybe s)) "N") "how many games to play; 100 when absent",
    GetOpt.Option [] ["save"] (GetOpt.ReqArg (\s o -> Right o {MatchOptions.save = Just s}) "DIR") "save each game in this directory as a scenario",
    GetOpt.Option [] ["search"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a seat: " <> s)) (\n -> Right o {MatchOptions.searching = Set.insert n (MatchOptions.searching o)}) (Monad.mfilter (`elem` [1, 2]) (Read.readMaybe s))) "SEAT") "the search bot plays this seat, 1 or 2; the random bot plays every other",
    GetOpt.Option [] ["samples"] (GetOpt.ReqArg (perSeat "a number of samples" (Monad.mfilter (> 0) . Read.readMaybe) (\n b -> b {SearchBudget.samples = n})) "[SEAT=]N") "the search bot's playouts of each option, in every seat or in SEAT alone; 2 when absent",
    GetOpt.Option [] ["turns"] (GetOpt.ReqArg (perSeat "a number of turns" Read.readMaybe (\n b -> b {SearchBudget.turns = n})) "[SEAT=]N") "the turns a playout may begin after the decision's before it is cut off and its position scored, in every seat or in SEAT alone; 0 when absent, ending a playout as the next turn begins",
    GetOpt.Option [] ["cutoff"] (GetOpt.ReqArg (perSeat "a number of prompts" Read.readMaybe (\n b -> b {SearchBudget.prompts = n})) "[SEAT=]N") "the prompts a playout is answered before it is cut off and its position scored, in every seat or in SEAT alone; 300 when absent",
    GetOpt.Option [] ["playout"] (GetOpt.ReqArg (perSeat "a playout policy, random or greedy" playoutNamed (\n b -> b {SearchBudget.playout = n})) "[SEAT=]POLICY") "who answers every seat in the search bot's playouts, random (the random bot) or greedy (playing a land, else casting a permanent spell, where it can), in every seat or in SEAT alone; random when absent",
    GetOpt.Option [] ["candidates"] (GetOpt.ReqArg (perSeat "a number of candidate worlds" Read.readMaybe (\n b -> b {SearchBudget.candidates = n})) "[SEAT=]N") "the worlds the search bot deals once at a decision and weighs by how likely each made the passes its opponents made, every playout's world drawn among them, in every seat or in SEAT alone; 0 when absent, each playout's world dealt fresh",
    GetOpt.Option [] ["weights"] (GetOpt.ReqArg (\s o -> Right $ case seated s of Just (n, path) -> o {MatchOptions.weighing = Map.insert n path (MatchOptions.weighing o)}; Nothing -> o {MatchOptions.weighing = Map.fromList [(n, s) | n <- [1, 2]]}) "[SEAT=]FILE") "the file of the weights the search bot scores a cut-off playout's position by (Parun.Codec.Weights), in every seat or in SEAT alone; every feature weighted 1 on each side when absent",
    GetOpt.Option [] ["meta"] (GetOpt.ReqArg (\s o -> Right o {MatchOptions.meta = Just s}) "META") "the deck lists are not known: the search bot infers each opponent's from this meta file's decks; each list known when absent"
  ]

-- | A flag's value for each seat, or with a `SEAT=` prefix for that seat
-- alone, set in its budget.
perSeat :: String -> (String -> Maybe a) -> (a -> SearchBudget.SearchBudget -> SearchBudget.SearchBudget) -> String -> MatchOptions.MatchOptions -> Either String MatchOptions.MatchOptions
perSeat what parse set s o = case seated s of
  Just (n, value) | Just x <- parse value -> Right o {MatchOptions.budgets = Map.adjust (set x) n (MatchOptions.budgets o)}
  _ | Just x <- parse s -> Right o {MatchOptions.budgets = Map.map (set x) (MatchOptions.budgets o)}
  _ -> Left ("not " <> what <> ": " <> s)

-- | The playout policy a `--playout` value names.
playoutNamed :: String -> Maybe Playout.Playout
playoutNamed s = lookup s [("random", Playout.Random), ("greedy", Playout.Greedy)]

-- | A seat, 1 or 2, and the value it prefixes with `=`.
seated :: String -> Maybe (Natural, String)
seated s = case break (== '=') s of
  (seat, '=' : value) | Just n <- Monad.mfilter (`elem` [1, 2]) (Read.readMaybe seat) -> Just (n, value)
  _ -> Nothing

-- | Each searching seat's budget and the weights its file holds (Parun.Weights).
searchersOf :: MatchOptions.MatchOptions -> IO (Either Text.Text (Map.Map Natural (SearchBudget.SearchBudget, Weights.Type.Weights)))
searchersOf options =
  fmap sequence . sequence $
    Map.fromSet
      ( \n -> do
          weights <- maybe (pure (Right Weights.Type.neutral)) (\path -> fmap (either (\problem -> Left (Text.pack path <> Text.pack ": " <> problem)) Right) (Weights.load path)) (Map.lookup n (MatchOptions.weighing options))
          pure ((,) (Map.findWithDefault MatchOptions.budget n (MatchOptions.budgets options)) <$> weights)
      )
      (MatchOptions.searching options)

-- | How the search bot deals what it cannot see: from the decks of the meta
-- file named (Search.inferred), else from the known lists (Search.known).
priorOf :: Registry.Registry Identity.Identity -> MatchOptions.MatchOptions -> IO Search.Prior
priorOf registry options = case MatchOptions.meta options of
  Nothing -> pure Search.known
  Just path -> Search.inferred . fmap (fmap (DeckList.deck . snd)) <$> Command.loadMeta registry path

usage :: [String] -> IO a
usage problems = do
  mapM_ (IO.hPutStr IO.stderr) problems
  IO.hPutStr IO.stderr (GetOpt.usageInfo "usage: parun match [OPTION...] DECK DECK" flags)
  Exit.exitFailure

-- | Plays the games one after another under `format`, saying how each ended
-- on standard error, then the tally on standard output.
match :: MatchOptions.MatchOptions -> Map.Map Natural (SearchBudget.SearchBudget, Weights.Type.Weights) -> Search.Prior -> Registry.Registry Identity.Identity -> Maybe Format.Format -> (Deck.Deck, Deck.Deck) -> IO ()
match options searchers prior registry format decks = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  seed <- maybe Key.fresh pure (MatchOptions.seed options)
  Monad.forM_ (MatchOptions.save options) (Directory.createDirectoryIfMissing True)
  let count = MatchOptions.games options
  tally <-
    Monad.foldM
      ( \tally index -> do
          let (outcome, timeline) = Identity.runIdentity (game registry format searchers prior seed index (both decks))
          TextIO.hPutStrLn IO.stderr (Text.pack ("Game " <> show (index + 1) <> ", seed ") <> Key.toHex (seedAt seed index) <> Text.pack (", seat " <> show (firstAt index) <> " first: ") <> ended outcome)
          Monad.forM_ (MatchOptions.save options) $ \directory ->
            Command.save (directory <> "/game-" <> show (index + 1) <> ".json") (Command.saved (Registry.printings registry) (Text.pack "Played with parun match, seed " <> Key.toHex (seedAt seed index) <> Text.pack (", seat " <> show (firstAt index) <> " first.")) format Map.empty (Command.seated (firstAt index) (both decks)) timeline)
          pure (tallied outcome tally)
      )
      Tally.empty
      (takeWhile (< count) [0 ..])
  mapM_ TextIO.putStrLn (summary count tally)
  TextIO.hPutStrLn IO.stderr (Text.pack "Seed " <> Key.toHex seed <> Text.pack ".")

-- | Game `index`, from zero, of a match from `seed` under `format` among the
-- decks, seat 1's first: seeded 'seedAt', seat 'firstAt' playing first, the
-- search bot in each seat of `searchers` at its budget and weights
-- (Search.search, dealing what it cannot see by `prior` and weighing it by
-- every pass watched (Search.watching), drawing apart as parun play's does:
-- Game.remembered) and the random bot in every other; with the entry each
-- answer replays as (Record.recording). With every seat searching at
-- PlayOptions.none's budget and weights and Search.known it is the game `parun play --bot 1
-- --bot 2` plays at that seed and `--first`.
game :: (Monad m) => Registry.Registry m -> Maybe Format.Format -> Map.Map Natural (SearchBudget.SearchBudget, Weights.Type.Weights) -> Search.Prior -> Key.Type.Key -> Natural -> NonEmpty.NonEmpty Deck.Deck -> m (Either Text.Text (Result.Result, GameState.GameState), Seq.Seq Timed.Timed)
game registry format searchers prior seed index decks =
  let count = Natural.length decks
      seat n = maybe (Game.bot (lifted registry)) (\(budget, weights) -> Game.remembered (pure Nothing) (const (pure ())) (Search.search (lifted registry) budget weights prior (Game.underneath (State.gets snd)))) (Map.lookup n searchers)
   in fmap (fmap fst) (State.runStateT (Game.run (lifted registry) format Limits.standard [Record.recording (Command.labels count) sink, Search.watching (\f -> State.modify' (\(timeline, passes) -> let kept = f passes in kept `seq` (timeline, kept)))] (Map.fromList [(Command.idOf n, seat n) | n <- [1 .. count]]) (Generator.fromKey (seedAt seed index)) Map.empty (Command.seated (firstAt index) decks)) (Seq.empty, []))

-- | A pair of decks, seat 1's first.
both :: (Deck.Deck, Deck.Deck) -> NonEmpty.NonEmpty Deck.Deck
both (one, two) = one NonEmpty.:| [two]

-- | The registry, beneath the entries and the passes kept so far
-- (Search.watching).
lifted :: (Monad m) => Registry.Registry m -> Registry.Registry (State.StateT (Seq.Seq Timed.Timed, [Search.Passed]) m)
lifted = Registry.hoist Trans.lift

-- | Keeps an answer's entry, after those before it.
sink :: (Monad m) => Timed.Timed -> Game.Play (State.StateT (Seq.Seq Timed.Timed, [Search.Passed]) m) ()
sink entry = Game.underneath (State.modify' (Bifunctor.first (Seq.|> entry)))

-- | Game `index`'s key, derived from the match's (Generator.derive), the
-- index wrapping at 2^64.
seedAt :: Key.Type.Key -> Natural -> Key.Type.Key
seedAt seed index = Generator.derive seed (Maybe.fromMaybe 0 (Natural.toWord64 (mod index (2 ^ (64 :: Natural)))))

-- | The seat playing first in game `index`: seat 1 at an even index, seat 2
-- at an odd one, so each deck plays first in half the games. Each game is a
-- match of its own (CR 103.1), so no loser of a previous game chooses.
firstAt :: Natural -> Natural
firstAt index = if even index then 1 else 2

-- | The tally with one more game counted, a game with a result at the turn it
-- ended in.
tallied :: Either Text.Text (Result.Result, GameState.GameState) -> Tally.Tally -> Tally.Tally
tallied outcome tally = case outcome of
  Right (result, gs) -> tally {Tally.results = Map.insertWith (+) result 1 (Tally.results tally), Tally.turns = Tally.turns tally + TurnRegisters.turnNumber (GameState.turn gs)}
  Left _ -> tally {Tally.stopped = Tally.stopped tally + 1}

-- | How one game ended.
ended :: Either Text.Text (Result.Result, GameState.GameState) -> Text.Text
ended outcome = case outcome of
  Right (result, gs) -> Wording.result result <> Text.pack (" Turn " <> show (TurnRegisters.turnNumber (GameState.turn gs)) <> ".")
  Left reason -> Text.pack "stopped: " <> reason <> Text.pack "."

-- | The tally of `count` games: each seat's wins, any team's, the draws and
-- stops, and the mean turns of a game with a result, to a tenth.
summary :: Natural -> Tally.Tally -> [Text.Text]
summary count tally =
  [Text.pack ("Seat " <> show n <> " won " <> show (won (Result.Won (Command.idOf n))) <> " of " <> show count <> " games.") | n <- [1, 2]]
    <> [Text.pack ("Team " <> show (TeamId.unwrap team) <> " won " <> show k <> " of " <> show count <> " games.") | (Result.TeamWon team, k) <- Map.toList (Tally.results tally)]
    <> [ Text.pack (show (won Result.Drawn) <> " drawn, " <> show (Tally.stopped tally) <> " stopped."),
         Text.pack (mean (sum (Tally.results tally)))
       ]
  where
    won result = Map.findWithDefault 0 result (Tally.results tally)
    mean finished
      | finished == 0 = "No game reached a result."
      | otherwise =
          let tenths = div (Tally.turns tally * 20 + finished) (2 * finished)
           in "A game with a result lasted " <> show (div tenths 10) <> "." <> show (mod tenths 10) <> " turns on average."
