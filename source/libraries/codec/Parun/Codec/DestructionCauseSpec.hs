module Parun.Codec.DestructionCauseSpec where

import qualified Parun.Codec.DestructionCause as DestructionCause
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DestructionCause as DestructionCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DestructionCause" $ do
  Spec.it s "ByRule" $
    Common.assertCodec
      s
      DestructionCause.codec
      DestructionCause.ByRule
      " \"ByRule\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s DestructionCause.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s DestructionCause.codec
