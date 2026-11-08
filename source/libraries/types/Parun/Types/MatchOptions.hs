module Parun.Types.MatchOptions where

import qualified Data.Word as Word
import qualified Numeric.Natural as Natural

-- | What `parun match` was asked for.
data MatchOptions = MkMatchOptions
  { -- | The first game's seed; the clock's when absent.
    seed :: Maybe Word.Word64,
    games :: Natural.Natural,
    -- | The directory to save each game in as a scenario.
    save :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | A hundred games seeded from the clock, none saved.
none :: MatchOptions
none = MkMatchOptions {seed = Nothing, games = 100, save = Nothing}
