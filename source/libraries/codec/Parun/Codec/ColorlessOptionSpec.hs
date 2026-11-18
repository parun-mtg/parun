module Parun.Codec.ColorlessOptionSpec where

import qualified Parun.Codec.ColorlessOption as ColorlessOption
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ColorlessOption as ColorlessOption

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ColorlessOption" $ do
  Spec.it s "ColorsOnly" $
    Common.assertCodec
      s
      ColorlessOption.codec
      ColorlessOption.ColorsOnly
      " \"ColorsOnly\" "
  Spec.it s "OrColorless" $
    Common.assertCodec
      s
      ColorlessOption.codec
      ColorlessOption.OrColorless
      " \"OrColorless\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ColorlessOption.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ColorlessOption.codec
