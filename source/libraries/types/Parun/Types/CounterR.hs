module Parun.Types.CounterR where

import qualified Parun.Types.CounterPattern as CounterPattern
import qualified Parun.Types.Scaling as Scaling

-- | The payload of Parun.Types.ReplacementEffect's CounterR arm (#1305): which
-- counter placements are intercepted, and how the number placed is scaled.
data CounterR = MkCounterR
  { matching :: CounterPattern.CounterPattern,
    scaling :: Scaling.Scaling,
    -- | CR 614.1a / 101.2: "and you can't get additional poison counters this
    -- turn" -- applying the row also bars the receiving player from getting
    -- more of the settled kind until cleanup (Melira, the Living Cure).
    -- Installed as the row applies, so a later placement in the same batch
    -- meets it. False, and elided, for every other row.
    --
    -- Unprinted: a bar on a permanent receiving counters; Parun.CardSpec's
    -- "barsAdditional only on a player pattern" keeps it out of card data.
    barsAdditional :: Bool
  }
  deriving (Eq, Ord, Show)
