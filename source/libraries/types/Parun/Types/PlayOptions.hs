module Parun.Types.PlayOptions where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Key as Key
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Weights as Weights

-- | What `parun play` was asked for, seats numbered from one.
data PlayOptions = MkPlayOptions
  { -- | The generator's key; drawn from the operating system when absent.
    seed :: Maybe Key.Key,
    first :: Maybe Natural.Natural,
    -- | The seats the search bot plays (Parun.Play.Search).
    bots :: Set.Set Natural.Natural,
    -- | Whether every seat not the bot's speaks the line protocol.
    json :: Bool,
    -- | Where to save the game as a scenario.
    save :: Maybe FilePath,
    -- | CR 404.3: the seats that arrange cards put into their graveyard at the
    -- same time.
    arrangesGraveyard :: Set.Set Natural.Natural,
    -- | The name of the format both decks must be legal in, a file under
    -- data/formats; any deck at all when absent.
    format :: Maybe Text.Text,
    -- | How large the game may grow.
    limits :: Limits.Limits,
    -- | How hard the search bot looks.
    budget :: SearchBudget.SearchBudget,
    -- | What the search bot values in a position it cuts a playout off at.
    weights :: Weights.Weights
  }
  deriving (Eq, Show)

-- | A game keyed from the operating system's entropy, no seat the bot's and
-- none over the line protocol, the seat to play first asked for, under the
-- standard limits, the search bot looking at 2 samples cut off as the next
-- turn begins or after 40 prompts, a budget a browser turn stays responsive
-- at, weighing the neutral weights, played out by the random bot.
none :: PlayOptions
none = MkPlayOptions {seed = Nothing, first = Nothing, bots = Set.empty, json = False, save = Nothing, arrangesGraveyard = Set.empty, format = Nothing, limits = Limits.standard, budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 0, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random}, weights = Weights.neutral}
