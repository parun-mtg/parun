module Parun.Codec.ParitySpec where

import qualified Parun.Codec.Parity as Parity
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Parity as Parity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Parity" $ do
  Spec.it s "Even" $
    Common.assertCodec
      s
      Parity.codec
      Parity.Even
      " \"Even\" "
  Spec.it s "Odd" $
    Common.assertCodec
      s
      Parity.codec
      Parity.Odd
      " \"Odd\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Parity.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Parity.codec
