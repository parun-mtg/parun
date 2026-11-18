module Parun.Codec.EventTeamTallySpec where

import qualified Parun.Codec.EventTeamTally as EventTeamTally
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EventTeam as EventTeam
import qualified Parun.Types.EventTeamTally as EventTeamTally
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.EventTeamTally" $ do
  Spec.it s "MkEventTeamTally" $
    Common.assertCodec
      s
      EventTeamTally.codec
      ( EventTeamTally.MkEventTeamTally
          { EventTeamTally.player = PlayerRef.Relative PlayerRelation.You,
            EventTeamTally.team = EventTeam.SpeedDemons
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"team\":\"SpeedDemons\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EventTeamTally.codec
