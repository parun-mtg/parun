module Parun.Codec.SpentOnSpec where

import qualified Parun.Codec.SpentOn as SpentOn
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.SpentOn as SpentOn

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SpentOn" $ do
  Spec.it s "Triggering" $
    Common.assertCodec
      s
      SpentOn.codec
      SpentOn.Triggering
      " \"Triggering\" "
  Spec.it s "TakingInstruction" $
    Common.assertCodec
      s
      SpentOn.codec
      SpentOn.TakingInstruction
      " \"TakingInstruction\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s SpentOn.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s SpentOn.codec
