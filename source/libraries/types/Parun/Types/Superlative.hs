module Parun.Types.Superlative where

import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.PlayerMeasure as PlayerMeasure
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | The players whose measure stands in @comparison@ to EVERY other compared
-- player's. GreaterThan is Damping Engine's strict "more permanents than each
-- other player", so a tie names nobody; AtLeast is Raiders' Allegiance's "the
-- most point counters or tied for the most".
--
-- Read by Parun.Engine.Players.superlative over the players @among@ names.
data Superlative = MkSuperlative
  { measure :: PlayerMeasure.PlayerMeasure,
    comparison :: Comparison.Comparison,
    -- | Who is compared, against CR 109.5's "you": AnyPlayer is the whole
    -- table, perspective-free (Damping Engine); Opponent is The Mighty Will
    -- Fall's "among your opponents" (CR 102.3).
    among :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)
