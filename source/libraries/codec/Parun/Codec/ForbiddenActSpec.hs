module Parun.Codec.ForbiddenActSpec where

import qualified Parun.Codec.ForbiddenAct as ForbiddenAct
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ForbiddenAct as ForbiddenAct

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ForbiddenAct" $ do
  Spec.it s "GainLife" $
    Common.assertCodec s ForbiddenAct.codec ForbiddenAct.GainLife " {\"type\":\"GainLife\"} "
  Spec.it s "LoseLife" $
    Common.assertCodec s ForbiddenAct.codec ForbiddenAct.LoseLife " {\"type\":\"LoseLife\"} "
  Spec.it s "DrawMoreThan" $
    Common.assertCodec s ForbiddenAct.codec (ForbiddenAct.DrawMoreThan 1) " {\"type\":\"DrawMoreThan\",\"value\":1} "
  Spec.it s "BecomeMonarch" $
    Common.assertCodec s ForbiddenAct.codec ForbiddenAct.BecomeMonarch " {\"type\":\"BecomeMonarch\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ForbiddenAct.codec
