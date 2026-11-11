module Parun.Codec.CopyabilitySpec where

import qualified Parun.Codec.Copyability as Copyability
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Copyability as Copyability

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Copyability" $ do
  Spec.it s "CantBeCopied" $
    Common.assertCodec
      s
      Copyability.codec
      Copyability.CantBeCopied
      " \"CantBeCopied\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Copyability.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s Copyability.codec
