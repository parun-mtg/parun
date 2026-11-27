module Parun.Codec.ExileMarkSpec where

import qualified Parun.Codec.ExileMark as ExileMark
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ExileMark as ExileMark

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ExileMark" $ do
  Spec.it s "Foretold" $
    Common.assertCodec
      s
      ExileMark.codec
      ExileMark.Foretold
      " \"Foretold\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ExileMark.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ExileMark.codec
