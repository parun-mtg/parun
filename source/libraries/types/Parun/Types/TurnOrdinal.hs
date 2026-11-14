module Parun.Types.TurnOrdinal where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.TurnScope as TurnScope

-- | Shadow in the Warp's "the FIRST creature spell you cast each turn" and
-- Uthros Psionicist's "the SECOND spell you cast each turn": in a turn the
-- scope admits, the nth matching cast or activation the player begins that
-- turn. Plain card text with no rule of its own, as
-- Parun.Types.SpellCast.ordinal's haddock says of the trigger's.
data TurnOrdinal = MkTurnOrdinal
  { -- | Whose turns count.
    scope :: TurnScope.TurnScope,
    -- | One for "first", two for "second"; counts the matching ones begun
    -- earlier this turn plus this one, so zero matches none.
    ordinal :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
