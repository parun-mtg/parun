module Parun.Types.MatchOptions where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Key as Key

-- | What `parun match` was asked for.
data MatchOptions = MkMatchOptions
  { -- | The key each game's is derived from; drawn from the operating system
    -- when absent.
    seed :: Maybe Key.Key,
    games :: Natural.Natural,
    -- | The directory to save each game in as a scenario.
    save :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | A hundred games keyed from the operating system's entropy, none saved.
none :: MatchOptions
none = MkMatchOptions {seed = Nothing, games = 100, save = Nothing}
