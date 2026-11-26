{-# LANGUAGE GADTs #-}

-- | `parun match`: many seeded games between two decks, each seat played by
-- the random bot or the search bot, and how often each seat won.
module Parun.Play.Match where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Identity
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Numeric.Natural (Natural)
import qualified Parun.CommandLine as CommandLine
import qualified Parun.DeckList as DeckList
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Fit as Fit
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Match.Config as Config
import qualified Parun.Play.Match.Flag as Flag
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Play.Record as Record
import qualified Parun.Play.Search as Search
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Trustworthy.Directory as Directory
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Position as Position
import qualified Parun.Types.Program as Program
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Tally as Tally
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Weights as Weights.Type
import qualified Parun.Weights as Weights
import qualified System.IO as IO

main :: [String] -> IO ()
main arguments = do
  config <- CommandLine.configured Flag.usage Config.help (Flag.fromArguments arguments >>= uncurry (Config.fromFlags ["DECK", "DECK"] (Just 2)))
  Foldable.for_ (pair (Config.files config)) $ \(one, two) -> do
    let options = Config.options config
    registry <- Registry.defaultRoot >>= Registry.loadRoot >>= Registry.defaultRegistry
    lists <- (,) <$> Command.orExit (Command.load registry one) <*> Command.orExit (Command.load registry two)
    format <- Command.orExit (Command.settle GameSettings.plain Nothing [(one, fst lists), (two, snd lists)])
    searchers <- Command.orExit (fmap (Bifunctor.first pure) (searchersOf options))
    prior <- priorOf registry options
    match options searchers prior registry format (Bifunctor.bimap DeckList.deck DeckList.deck lists)
  where
    pair files = case files of
      [one, two] -> Just (one, two)
      _ -> Nothing

-- | Each searching seat's budget and its weights: its persona's, or those
-- its file holds (Parun.Weights).
searchersOf :: MatchOptions.MatchOptions -> IO (Either Text.Text (Map.Map Natural (SearchBudget.SearchBudget, Weights.Type.Weights)))
searchersOf options =
  fmap sequence . sequence $
    Map.fromSet
      ( \n -> do
          weights <- case Map.lookup n (MatchOptions.weighing options) of
            Nothing -> pure (Right Weights.Type.neutral)
            Just (Left persona) -> pure (Right (Opponent.weightsOf persona))
            Just (Right path) -> fmap (either (\problem -> Left (Text.pack path <> Text.pack ": " <> problem)) Right) (Weights.load path)
          pure ((,) (Map.findWithDefault MatchOptions.budget n (MatchOptions.budgets options)) <$> weights)
      )
      (MatchOptions.searching options)

-- | How the search bot deals what it cannot see: as Command.inferring says,
-- else from the known lists (Search.known).
priorOf :: Registry.Registry Identity.Identity -> MatchOptions.MatchOptions -> IO Search.Prior
priorOf registry options = Maybe.fromMaybe (pure Search.known) (Command.inferring registry (MatchOptions.meta options) (MatchOptions.pool options))

-- | Plays the games one after another under `format`, saying how each ended
-- on standard error, then the tally on standard output.
match :: MatchOptions.MatchOptions -> Map.Map Natural (SearchBudget.SearchBudget, Weights.Type.Weights) -> Search.Prior -> Registry.Registry Identity.Identity -> Maybe Format.Format -> (Deck.Deck, Deck.Deck) -> IO ()
match options searchers prior registry format decks = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  seed <- maybe Key.fresh pure (MatchOptions.seed options)
  Monad.forM_ (MatchOptions.save options) (Directory.createDirectoryIfMissing True)
  Monad.forM_ (MatchOptions.positions options) $ \path -> TextIO.writeFile path (Text.unlines [Fit.header])
  let count = MatchOptions.games options
  tally <-
    Monad.foldM
      ( \tally index -> do
          let (outcome, played) = Identity.runIdentity (game registry format searchers prior seed index [positioning | Maybe.isJust (MatchOptions.positions options)] (both decks))
          TextIO.hPutStrLn IO.stderr (Text.pack ("Game " <> show (index + 1) <> ", seed ") <> Key.toHex (seedAt seed index) <> Text.pack (", seat " <> show (firstAt index) <> " first: ") <> ended outcome)
          Monad.forM_ (MatchOptions.save options) $ \directory ->
            Command.save (directory <> "/game-" <> show (index + 1) <> ".json") (Command.saved (Registry.printings registry) (Text.pack "Played with parun match, seed " <> Key.toHex (seedAt seed index) <> Text.pack (", seat " <> show (firstAt index) <> " first.")) GameSettings.plain {GameSettings.format = format} Map.empty (Command.seated (firstAt index) (both decks)) (timeline played))
          Monad.forM_ (MatchOptions.positions options) $ \path -> TextIO.appendFile path (Text.unlines (rows (seedAt seed index) outcome (positions played)))
          -- Forced, so no game's final state outlives its game.
          pure $! tallied outcome tally
      )
      Tally.empty
      (takeWhile (< count) [0 ..])
  mapM_ TextIO.putStrLn (summary count tally)
  TextIO.hPutStrLn IO.stderr (Text.pack "Seed " <> Key.toHex seed <> Text.pack ".")

-- | Game `index`, from zero, of a match from `seed` under `format` among the
-- decks, seat 1's first: seeded 'seedAt', seat 'firstAt' playing first, the
-- search bot in each seat of `searchers` at its budget and weights
-- (Search.search, dealing what it cannot see by `prior` and weighing it by
-- every play at priority watched (Search.watching), remembering what it saw
-- (Search.remembering), keeping the line it chose (Search.keptBy),
-- drawing apart as parun play's does:
-- Game.remembered), never running out of time (Search.unhurried), and the
-- random bot in every other; with the entry each answer replays as
-- (Record.recording), and whatever the `observers` keep. With every seat
-- searching at PlayOptions.none's budget and weights and Search.known it is
-- the game `parun play --bot 1 --bot 2 --think-limit 0` plays at that seed
-- and `--first`.
game :: (Monad m) => Registry.Registry m -> Maybe Format.Format -> Map.Map Natural (SearchBudget.SearchBudget, Weights.Type.Weights) -> Search.Prior -> Key.Type.Key -> Natural -> [Observer.Observer (Game.Play (State.StateT Played m))] -> NonEmpty.NonEmpty Deck.Deck -> m (Either Text.Text (Result.Result, GameState.GameState), Played)
game registry format searchers prior seed index observers decks =
  let count = Natural.length decks
      seat n = maybe (Game.bot (lifted registry)) (\(budget, weights) -> Game.remembered (pure Nothing) (const (pure ())) (Search.search (lifted registry) budget weights prior (Game.underneath (State.gets plays)) (Game.underneath (State.gets (Map.findWithDefault Search.blank (Command.idOf n) . memories))) (Search.keptBy (State.gets aims) (\f -> State.modify' (\played -> played {aims = f (aims played)})) (Command.idOf n)) (pure ()) Search.unhurried)) (Map.lookup n searchers)
   in State.runStateT (Game.run Program.foldProgramM (lifted registry) GameSettings.plain {GameSettings.format = format} Limits.standard ([Record.recording (Command.labels count) sink, Search.watching (\f -> State.modify' (\played -> let kept = f (plays played) in kept `seq` played {plays = kept})), Search.remembering (\f -> State.modify' (\played -> let kept = f (memories played) in kept `seq` played {memories = kept}))] <> observers) (Map.fromList [(Command.idOf n, seat n) | n <- [1 .. count]]) (Generator.fromKey (seedAt seed index)) Map.empty (Command.seated (firstAt index) decks)) unplayed

-- | A game's positions as `parun fit` reads them (Fit.row), keyed by the
-- game's seed; none for a game no seat won.
rows :: Key.Type.Key -> Either Text.Text (Result.Result, GameState.GameState) -> Seq.Seq Position.Position -> [Text.Text]
rows key outcome kept = case outcome of
  Right (Result.Won winner, _) -> [Fit.row (Key.toHex key) winner position | position <- Foldable.toList kept]
  _ -> []

-- | A pair of decks, seat 1's first.
both :: (Deck.Deck, Deck.Deck) -> NonEmpty.NonEmpty Deck.Deck
both (one, two) = one NonEmpty.:| [two]

-- | The registry, beneath what a game keeps ('Played').
lifted :: (Monad m) => Registry.Registry m -> Registry.Registry (State.StateT Played m)
lifted = Registry.hoist Trans.lift

-- | Keeps an answer's entry, after those before it.
sink :: (Monad m) => Timed.Timed -> Game.Play (State.StateT Played m) ()
sink entry = Game.underneath (State.modify' (\played -> played {timeline = timeline played Seq.|> entry}))

-- | Keeps the position of each decision at priority in the main game
-- (Search.featuresOfAll), after those before it.
positioning :: (Monad m) => Observer.Observer (Game.Play (State.StateT Played m))
positioning =
  Observer.MkObserver
    { Observer.before = \asked -> case Asked.prompt asked of
        Prompt.ChooseAction {}
          | null (Asked.enclosing asked) ->
              let gs = Asked.game asked
                  position = Position.MkPosition {Position.turn = TurnRegisters.turnNumber (GameState.turn gs), Position.features = Search.featuresOfAll gs}
               in position `seq` Game.underneath (State.modify' (\played -> played {positions = positions played Seq.|> position}))
        _ -> pure (),
      Observer.after = \_ _ -> pure ()
    }

-- | What a match's game keeps beside its play.
data Played = MkPlayed
  { -- | The entries so far (Record.recording).
    timeline :: Seq.Seq Timed.Timed,
    -- | The plays kept (Search.watching).
    plays :: [Search.Watched],
    -- | The memories kept (Search.remembering).
    memories :: Map.Map PlayerId.PlayerId Search.Memory,
    -- | Each search bot's line kept (Search.keptBy).
    aims :: Map.Map PlayerId.PlayerId Search.Line,
    -- | The positions kept ('positioning').
    positions :: Seq.Seq Position.Position
  }

-- | A game's keeping before its first prompt.
unplayed :: Played
unplayed = MkPlayed {timeline = Seq.empty, plays = [], memories = Map.empty, aims = Map.empty, positions = Seq.empty}

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
  Right (result, gs) -> Wording.result Nothing result <> Text.pack (" Turn " <> show (TurnRegisters.turnNumber (GameState.turn gs)) <> ".")
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
