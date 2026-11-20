module Parun.Types.ReplacedDestruction where

import qualified Parun.Types.DestructionCause as DestructionCause

-- | CR 614.6 / 608.2f / 704.3: a destruction event a replacement modified,
-- while the effects it queued in the destruction's place run as part of that
-- one event (Parun.Engine.Event.asPartOf). `state` is the whole game, as in
-- Parun.Types.Snapshots.
data ReplacedDestruction state = MkReplacedDestruction
  { -- | The board the event began on, which its replacements are gathered
    -- from (CR 614.4).
    board :: !state,
    -- | Whether an effect or a rule performs the event (CR 122.1c), which a
    -- destruction among the queued effects inherits.
    cause :: !DestructionCause.DestructionCause
  }
  deriving (Eq, Ord, Show)
