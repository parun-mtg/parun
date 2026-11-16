module Parun.Codec.CastOriginSpec where

import qualified Parun.Codec.CastOrigin as CastOrigin
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CastOrigin as CastOrigin
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CastOrigin" $ do
  Spec.it s "FromZone" $
    Common.assertCodec s CastOrigin.codec (CastOrigin.FromZone Zone.Graveyard) " {\"type\":\"FromZone\",\"value\":\"Graveyard\"} "
  Spec.it s "OutsideTheGame" $
    Common.assertCodec s CastOrigin.codec CastOrigin.OutsideTheGame " {\"type\":\"OutsideTheGame\"} "
