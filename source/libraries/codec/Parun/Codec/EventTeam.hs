module Parun.Codec.EventTeam where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.EventTeam as EventTeam

codec :: Codec.Codec EventTeam.EventTeam
codec = Arm.enum
