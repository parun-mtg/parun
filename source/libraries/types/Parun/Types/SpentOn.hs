module Parun.Types.SpentOn where

import qualified Parun.Types.ClauseIndex as ClauseIndex

-- | What spends a per-turn trigger limit (Parun.Types.TriggerLimit's
-- OncePerTurn): the ability triggering, or its controller taking the
-- instruction the limit qualifies.
data SpentOn
  = -- | "This ability triggers only once each turn": spent as it triggers (CR 702.179d).
    Triggering
  | -- | "Do this only once each turn": spent as the clause it qualifies is taken (CR 603.2h).
    TakingInstruction ClauseIndex.ClauseIndex
  deriving (Eq, Ord, Show)
