module Parun.Types.EventTeamTally where

import qualified Parun.Types.EventTeam as EventTeam
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Quantity's OnEventTeam arm: whether a player is
-- on an event team. A leaf, Parun.Types.PlayerDesignationTally's shape.
data EventTeamTally = MkEventTeamTally
  { player :: PlayerRef.PlayerRef,
    team :: EventTeam.EventTeam
  }
  deriving (Eq, Ord, Show)
