module Parun.Codec.UntapScopeSpec where

import qualified Parun.Codec.UntapScope as UntapScope
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.UntapScope as UntapScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.UntapScope" $ do
  Spec.it s "AnyUntap" $
    Common.assertCodec
      s
      UntapScope.codec
      UntapScope.AnyUntap
      " \"AnyUntap\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s UntapScope.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s UntapScope.codec
