module Parun.Types.When where

import Numeric.Natural (Natural)
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Phase as Phase

-- | The moment a timeline entry waits for: a turn, a step, and the seat that
-- DECIDES, which CR 724.5 makes the controller of a controlled player.
data When = MkWhen
  { -- | The turn and step of the game at `depth`, not of the main game.
    turn :: Natural,
    phase :: Phase.Phase,
    player :: Label.Label,
    -- | How many games enclose the one asking (CR 730.1a): 0 for the main
    -- game, 1 for its subgame.
    depth :: Natural
  }
  deriving (Eq, Ord, Show)
