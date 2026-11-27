module Parun.Codec.PlaneswalkAwaySpec where

import qualified Parun.Codec.PlaneswalkAway as PlaneswalkAway
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlaneswalkAway as PlaneswalkAway

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlaneswalkAway" $ do
  Spec.it s "FromEveryCard" $
    Common.assertCodec s PlaneswalkAway.codec PlaneswalkAway.FromEveryCard " \"FromEveryCard\" "
  Spec.it s "FromPhenomenaOnly" $
    Common.assertCodec s PlaneswalkAway.codec PlaneswalkAway.FromPhenomenaOnly " \"FromPhenomenaOnly\" "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlaneswalkAway.codec
