module Parun.Types.PlayOptions where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Key as Key
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.SearchMethod as SearchMethod
import qualified Parun.Types.Weights as Weights

-- | What `parun play` was asked for, seats numbered from one.
data PlayOptions = MkPlayOptions
  { -- | The generator's key; drawn from the operating system when absent.
    seed :: Maybe Key.Key,
    first :: Maybe Natural.Natural,
    -- | The seats the search bot plays (Parun.Play.Search), each with how
    -- hard it looks there and what it values in a position it cuts a
    -- playout off at.
    bots :: Map.Map Natural.Natural (SearchBudget.SearchBudget, Weights.Weights),
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
    -- | The meta file whose decks the search bot infers an opponent's unseen
    -- cards from.
    meta :: Maybe FilePath,
    -- | The name of the format whose pool the search bot deals an opponent's
    -- unseen cards from, absent a meta; the game's format's when absent.
    pool :: Maybe Text.Text,
    -- | The milliseconds the search bot may think over a decision before it
    -- takes the best option its playouts found so far; 0, no limit.
    thinkLimit :: Natural.Natural
  }
  deriving (Eq, Show)

-- | A game keyed from the operating system's entropy, no seat the bot's and
-- none over the line protocol, the seat to play first asked for, under the
-- standard limits, the search bot dealing an opponent's unseen cards from
-- the game's format's pool and thinking 5 seconds at most over a decision.
none :: PlayOptions
none = MkPlayOptions {seed = Nothing, first = Nothing, bots = Map.empty, json = False, save = Nothing, arrangesGraveyard = Set.empty, format = Nothing, limits = Limits.standard, meta = Nothing, pool = Nothing, thinkLimit = 5000}

-- | A seat the search bot plays unless told otherwise: its 'budget', weighing
-- the neutral weights.
bot :: (SearchBudget.SearchBudget, Weights.Weights)
bot = (budget, Weights.neutral)

-- | The search bot's budget unless told otherwise: 2 samples cut off as the
-- next turn begins or after 40 prompts, a budget a browser turn stays
-- responsive at, making no mistakes, played out by the random bot.
budget :: SearchBudget.SearchBudget
budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 0, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = SearchMethod.Flat}
