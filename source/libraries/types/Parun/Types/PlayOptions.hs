module Parun.Types.PlayOptions where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Key as Key

-- | What `parun play` was asked for, seats numbered from one.
data PlayOptions = MkPlayOptions
  { -- | The generator's key; drawn from the operating system when absent.
    seed :: Maybe Key.Key,
    first :: Maybe Natural.Natural,
    bots :: Set.Set Natural.Natural,
    json :: Bool,
    -- | Where to save the game as a scenario.
    save :: Maybe FilePath,
    -- | Whether a person is passed for when passing and mana abilities are all
    -- they could do.
    autoPass :: Bool,
    -- | CR 404.3: the seats that arrange cards put into their graveyard at the
    -- same time.
    arrangesGraveyard :: Set.Set Natural.Natural,
    -- | The name of the format both decks must be legal in, a file under
    -- data/formats; any deck at all when absent.
    format :: Maybe Text.Text
  }
  deriving (Eq, Show)

-- | A game keyed from the operating system's entropy between two people, the first asked for.
none :: PlayOptions
none = MkPlayOptions {seed = Nothing, first = Nothing, bots = Set.empty, json = False, save = Nothing, autoPass = True, arrangesGraveyard = Set.empty, format = Nothing}
