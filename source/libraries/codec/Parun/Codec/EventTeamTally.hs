{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EventTeamTally where

import qualified Parun.Codec.EventTeam as EventTeam
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.EventTeamTally as EventTeamTally

-- | A bare object keyed by the record's field names. The tag that picks it is
-- Parun.Codec.Quantity's "OnEventTeam".
codec :: Codec.Codec EventTeamTally.EventTeamTally
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec EventTeamTally.player
  team <- Fields.required "team" EventTeam.codec EventTeamTally.team
  pure EventTeamTally.MkEventTeamTally {EventTeamTally.player = player, EventTeamTally.team = team}
