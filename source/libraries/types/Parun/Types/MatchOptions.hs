module Parun.Types.MatchOptions where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Key as Key
import qualified Parun.Types.SearchBudget as SearchBudget

-- | What `parun match` was asked for.
data MatchOptions = MkMatchOptions
  { -- | The key each game's is derived from; drawn from the operating system
    -- when absent.
    seed :: Maybe Key.Key,
    games :: Natural.Natural,
    -- | The directory to save each game in as a scenario.
    save :: Maybe FilePath,
    -- | The seats the search bot plays (Parun.Play.Search), the random bot
    -- playing the rest.
    searching :: Set.Set Natural.Natural,
    -- | How hard the search bot looks in each seat it may play.
    budgets :: Map.Map Natural.Natural SearchBudget.SearchBudget,
    -- | The file of the search bot's weights in a seat (Parun.Weights),
    -- Weights.neutral in a seat absent.
    weighing :: Map.Map Natural.Natural FilePath,
    -- | The meta file whose decks the search bot infers an opponent's unseen
    -- cards from, the lists not being known; each list known when absent.
    meta :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | A hundred games keyed from the operating system's entropy, none saved,
-- the random bot in every seat, every list known.
none :: MatchOptions
none = MkMatchOptions {seed = Nothing, games = 100, save = Nothing, searching = Set.empty, budgets = Map.fromList [(1, budget), (2, budget)], weighing = Map.empty, meta = Nothing}

-- | The search bot's budget unless told otherwise.
budget :: SearchBudget.SearchBudget
budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 0, SearchBudget.prompts = 300}
