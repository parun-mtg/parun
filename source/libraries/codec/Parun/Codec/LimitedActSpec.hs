module Parun.Codec.LimitedActSpec where

import qualified Parun.Codec.LimitedAct as LimitedAct
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.LimitedAct as LimitedAct

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LimitedAct" $ do
  Spec.it s "Venture" $
    Common.assertCodec s LimitedAct.codec LimitedAct.Venture " \"Venture\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s LimitedAct.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s LimitedAct.codec
