module Parun.Codec.EventTeamSpec where

import qualified Parun.Codec.EventTeam as EventTeam
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EventTeam as EventTeam

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.EventTeam" $ do
  Spec.it s "Cloudspire" $ Common.assertCodec s EventTeam.codec EventTeam.Cloudspire " \"Cloudspire\" "
  Spec.it s "SpeedDemons" $ Common.assertCodec s EventTeam.codec EventTeam.SpeedDemons " \"SpeedDemons\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s EventTeam.codec
