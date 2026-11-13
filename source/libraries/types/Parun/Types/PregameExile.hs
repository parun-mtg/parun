module Parun.Types.PregameExile where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | CR 607.2n: a static ability letting its card's owner, before shuffling
-- their deck to start the game, reveal the card from that deck and exile cards
-- they brought that are not in it. `filter` shadows the Prelude's,
-- Parun.Types.Count's naming.
data PregameExile = MkPregameExile
  { -- | What each card exiled must be (Arcane Savant's "instant or sorcery").
    filter :: Filter.Filter Keyword.Keyword,
    -- | How many cards each card revealed exiles at least (Volatile Chimera's
    -- three).
    atLeast :: Natural.Natural,
    -- | How many at most; Nothing is Volatile Chimera's "or more".
    atMost :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)
