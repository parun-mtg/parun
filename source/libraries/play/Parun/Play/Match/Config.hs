-- | What `parun match`'s or `parun meta`'s command line asks for, each
-- flag's argument checked.
module Parun.Play.Match.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.CommandLine as CommandLine
import qualified Parun.CommandLine.Completion.Command as Command
import qualified Parun.CommandLine.Completion.Value as Value
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Match.Flag as Flag
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.SearchMethod as SearchMethod
import qualified Text.Read as Read

-- | What the match is played under, the files its positional arguments name
-- (none only when the usage is asked for), and whether to print the usage
-- instead.
data Config = MkConfig
  { options :: MatchOptions.MatchOptions,
    files :: [FilePath],
    help :: Bool
  }
  deriving (Eq, Show)

-- | The config of a command line with no flags and no file.
initial :: Config
initial = MkConfig {options = MatchOptions.none, files = [], help = False}

-- | The config the flags ask for, each in turn over 'initial', reading the
-- files from the positional arguments, checked against the names of those
-- required and the most allowed ('CommandLine.positionals'); throws at the
-- first flag whose argument is no good, a missing file or an extra argument.
fromFlags :: (Catch.MonadThrow m) => [String] -> Maybe Natural -> [Flag.Flag] -> [String] -> m Config
fromFlags required most fs positional = do
  config <- Monad.foldM applyFlag initial fs
  Monad.unless (help config) $ CommandLine.positionals required most positional
  pure config {files = positional}

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Candidates s -> perSeat "a number of candidate worlds" Read.readMaybe (\n b -> b {SearchBudget.candidates = n}) s
  Flag.Cutoff s -> perSeat "a number of prompts" Read.readMaybe (\n b -> b {SearchBudget.prompts = n}) s
  Flag.Games s -> CommandLine.orInvalid ("not a number of games: " <> s) (Read.readMaybe s) >>= \n -> set o {MatchOptions.games = n}
  Flag.Help b -> pure config {help = b}
  Flag.Level s -> perSeat "a level, easy, medium or hard" (Opponent.named Opponent.difficultyName) Opponent.budgetAt s
  Flag.Meta s -> set o {MatchOptions.meta = Just s}
  Flag.Method s -> perSeat "a search method, flat or tree" methodNamed (\n b -> b {SearchBudget.method = n}) s
  Flag.Mistakes s -> perSeat "a percent of decisions" (Monad.mfilter (<= 100) . Read.readMaybe) (\n b -> b {SearchBudget.mistakes = n}) s
  Flag.Persona s -> weighed (fmap Left . Opponent.named Opponent.personaName) s
  Flag.Playout s -> perSeat "a playout policy, random, greedy, polar or classified" playoutNamed (\n b -> b {SearchBudget.playout = n}) s
  Flag.Pool s -> set o {MatchOptions.pool = Just (Text.pack s)}
  Flag.Samples s -> perSeat "a number of samples" (Monad.mfilter (> 0) . Read.readMaybe) (\n b -> b {SearchBudget.samples = n}) s
  Flag.Save s -> set o {MatchOptions.save = Just s}
  Flag.Search s -> CommandLine.orInvalid ("not a seat: " <> s) (Monad.mfilter (`elem` seats) (Read.readMaybe s)) >>= \n -> set o {MatchOptions.searching = Set.insert n (MatchOptions.searching o)}
  Flag.Seed s -> CommandLine.orInvalid ("not a seed: " <> s) (Key.fromHex (Text.pack s)) >>= \k -> set o {MatchOptions.seed = Just k}
  Flag.Turns s -> perSeat "a number of turns" Read.readMaybe (\n b -> b {SearchBudget.turns = n}) s
  Flag.Weights s -> weighed (Just . Right) s
  where
    o = options config
    set x = pure config {options = x}
    -- A flag's value for each seat, or with a `SEAT=` prefix for that seat
    -- alone, set in its budget.
    perSeat what parse update s = CommandLine.orInvalid ("not " <> what <> ": " <> s) (perSeatOf parse update s (MatchOptions.budgets o)) >>= \budgets -> set o {MatchOptions.budgets = budgets}
    -- A flag's weights for each seat, or with a `SEAT=` prefix for that seat
    -- alone, the last flag for a seat winning.
    weighed parse s = CommandLine.orInvalid ("not a persona: " <> s) (weighedOf parse s (MatchOptions.weighing o)) >>= \weighing -> set o {MatchOptions.weighing = weighing}

-- | The budgets with a value set in each seat's, or with a `SEAT=` prefix in
-- that seat's alone; Nothing for a value that does not parse.
perSeatOf :: (String -> Maybe a) -> (a -> SearchBudget.SearchBudget -> SearchBudget.SearchBudget) -> String -> Map.Map Natural SearchBudget.SearchBudget -> Maybe (Map.Map Natural SearchBudget.SearchBudget)
perSeatOf parse update s budgets = case seated s of
  Just (n, value) | Just x <- parse value -> Just (Map.adjust (update x) n budgets)
  _ -> fmap (\x -> Map.map (update x) budgets) (parse s)

-- | The weighing with weights set for each seat, or with a `SEAT=` prefix for
-- that seat alone; Nothing for weights that do not parse.
weighedOf :: (String -> Maybe (Either Persona.Persona FilePath)) -> String -> Map.Map Natural (Either Persona.Persona FilePath) -> Maybe (Map.Map Natural (Either Persona.Persona FilePath))
weighedOf parse s weighing = case seated s of
  Just (n, value) | Just x <- parse value -> Just (Map.insert n x weighing)
  _ -> fmap (\x -> Map.fromList [(n, x) | n <- seats]) (parse s)

-- | The playout policy a `--playout` value names.
playoutNamed :: String -> Maybe Playout.Playout
playoutNamed s = List.find ((== s) . playoutName) [minBound .. maxBound]

-- | The `--playout` value naming a playout policy.
playoutName :: Playout.Playout -> String
playoutName playout = case playout of
  Playout.Random -> "random"
  Playout.Greedy -> "greedy"
  Playout.Polar -> "polar"
  Playout.Classified -> "classified"

-- | The search method a `--method` value names.
methodNamed :: String -> Maybe SearchMethod.SearchMethod
methodNamed s = List.find ((== s) . methodName) [minBound .. maxBound]

-- | The `--method` value naming a search method.
methodName :: SearchMethod.SearchMethod -> String
methodName method = case method of
  SearchMethod.Flat -> "flat"
  SearchMethod.InformationSet -> "tree"

-- | The seats a match has.
seats :: [Natural]
seats = [1, 2]

-- | A seat, 1 or 2, and the value it prefixes with `=`.
seated :: String -> Maybe (Natural, String)
seated s = case break (== '=') s of
  (seat, '=' : value) | Just n <- Monad.mfilter (`elem` seats) (Read.readMaybe seat) -> Just (n, value)
  _ -> Nothing

-- | `parun match` as tab completion sees it.
command :: Command.Command
command =
  (Command.new "match" Flag.flags)
    { Command.values =
        [ ("save", Value.MkFiles),
          ("search", Value.MkChoices (fmap show seats)),
          ("playout", perSeat (fmap playoutName [minBound .. maxBound])),
          ("method", perSeat (fmap methodName [minBound .. maxBound])),
          ("level", perSeat (Opponent.names Opponent.difficultyName)),
          ("weights", Value.MkSeated (fmap show seats) Value.MkFiles),
          ("persona", perSeat (Opponent.names Opponent.personaName)),
          ("meta", Value.MkFiles),
          ("pool", Value.MkFormats)
        ],
      Command.positionals = [Value.MkFiles, Value.MkFiles]
    }
  where
    perSeat = Value.MkSeated (fmap show seats) . Value.MkChoices
