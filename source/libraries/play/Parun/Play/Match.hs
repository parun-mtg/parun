-- | `parun match`: many seeded games between two decks, the random bot in both
-- seats, and how often each seat won.
module Parun.Play.Match where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Functor.Identity as Identity
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
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
import qualified Parun.Play.Render as Render
import qualified Parun.Registry as Registry
import qualified Parun.Trustworthy.Directory as Directory
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Hotseat as Hotseat
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Result as Result
import qualified Parun.Types.Tally as Tally
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: [String] -> IO ()
main arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, [one, two], []) -> case foldr (=<<) (Right MatchOptions.none) updates of
    Left problem -> usage [problem]
    Right options -> do
      registry <- Registry.defaultRoot >>= Registry.loadRoot >>= Registry.defaultRegistry
      lists <- (,) <$> Command.orExit (Command.load registry one) <*> Command.orExit (Command.load registry two)
      format <- Command.orExit (Command.settle Nothing [(one, fst lists), (two, snd lists)])
      match options registry format (Bifunctor.bimap DeckList.deck DeckList.deck lists)
  (_, _, problems) -> usage problems

flags :: [GetOpt.OptDescr (MatchOptions.MatchOptions -> Either String MatchOptions.MatchOptions)]
flags =
  [ GetOpt.Option [] ["seed"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a seed: " <> s)) (\k -> Right o {MatchOptions.seed = Just k}) (Key.fromHex (Text.pack s))) "KEY") "the key each game's is derived from, 64 hexadecimal digits, said when the match ends; drawn from the operating system when absent",
    GetOpt.Option [] ["games"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a number of games: " <> s)) (\n -> Right o {MatchOptions.games = n}) (Read.readMaybe s)) "N") "how many games to play; 100 when absent",
    GetOpt.Option [] ["save"] (GetOpt.ReqArg (\s o -> Right o {MatchOptions.save = Just s}) "DIR") "save each game in this directory as a scenario"
  ]

usage :: [String] -> IO a
usage problems = do
  mapM_ (IO.hPutStr IO.stderr) problems
  IO.hPutStr IO.stderr (GetOpt.usageInfo "usage: parun match [OPTION...] DECK DECK" flags)
  Exit.exitFailure

-- | Plays the games one after another under `format`, saying how each ended
-- on standard error, then the tally on standard output.
match :: MatchOptions.MatchOptions -> Registry.Registry Identity.Identity -> Maybe Format.Format -> (Deck.Deck, Deck.Deck) -> IO ()
match options registry format decks = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  seed <- maybe Key.fresh pure (MatchOptions.seed options)
  Monad.forM_ (MatchOptions.save options) (Directory.createDirectoryIfMissing True)
  let count = MatchOptions.games options
  tally <-
    Monad.foldM
      ( \tally index -> do
          let (outcome, timeline) = Identity.runIdentity (game registry format seed index decks)
          TextIO.hPutStrLn IO.stderr (Text.pack ("Game " <> show (index + 1) <> ", seed ") <> Key.toHex (seedAt seed index) <> Text.pack (", seat " <> show (firstAt index) <> " first: ") <> ended outcome)
          Monad.forM_ (MatchOptions.save options) $ \directory ->
            Command.save (directory <> "/game-" <> show (index + 1) <> ".json") (Command.saved (Registry.printings registry) (Text.pack "Played with parun match, seed " <> Key.toHex (seedAt seed index) <> Text.pack (", seat " <> show (firstAt index) <> " first.")) format Map.empty (Command.seated (firstAt index) decks) timeline)
          pure (tallied outcome tally)
      )
      Tally.empty
      (takeWhile (< count) [0 ..])
  mapM_ TextIO.putStrLn (summary count tally)
  TextIO.hPutStrLn IO.stderr (Text.pack "Seed " <> Key.toHex seed <> Text.pack ".")

-- | Game `index`, from zero, of a match from `seed` under `format`: seeded
-- 'seedAt', seat 'firstAt' playing first, the random bot in both seats; with
-- the entry each answer replays as (Record.recording). It is the game `parun
-- play --bot 1 --bot 2` plays at that seed and `--first`.
game :: (Monad m) => Registry.Registry m -> Maybe Format.Format -> Key.Type.Key -> Natural -> (Deck.Deck, Deck.Deck) -> m (Either Text.Text (Result.Result, GameState.GameState), Seq.Seq Timed.Timed)
game registry format seed index decks =
  State.runStateT (Game.run (lifted registry) format [Record.recording Command.labels sink] (Map.fromList [(Command.idOf n, Game.bot (lifted registry)) | n <- [1, 2]]) (const (pure ())) Hotseat.fresh (Generator.fromKey (seedAt seed index)) Map.empty (Command.seated (firstAt index) decks)) Seq.empty

-- | The registry, beneath the entries kept so far.
lifted :: (Monad m) => Registry.Registry m -> Registry.Registry (State.StateT (Seq.Seq Timed.Timed) m)
lifted = Registry.hoist Trans.lift

-- | Keeps an answer's entry, after those before it.
sink :: (Monad m) => Timed.Timed -> Game.Play (State.StateT (Seq.Seq Timed.Timed) m) ()
sink entry = Game.underneath (State.modify' (Seq.|> entry))

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
  Right (result, gs) -> Render.result result <> Text.pack (" Turn " <> show (TurnRegisters.turnNumber (GameState.turn gs)) <> ".")
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
