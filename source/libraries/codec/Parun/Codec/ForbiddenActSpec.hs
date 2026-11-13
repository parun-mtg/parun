module Parun.Codec.ForbiddenActSpec where

import qualified Parun.Codec.ForbiddenAct as ForbiddenAct
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ForbiddenAct as ForbiddenAct

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ForbiddenAct" $ do
  Spec.it s "DrawCards" $
    Common.assertCodec
      s
      ForbiddenAct.codec
      ForbiddenAct.DrawCards
      " \"DrawCards\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ForbiddenAct.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ForbiddenAct.codec
