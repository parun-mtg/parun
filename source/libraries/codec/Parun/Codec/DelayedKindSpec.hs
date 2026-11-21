module Parun.Codec.DelayedKindSpec where

import qualified Parun.Codec.DelayedKind as DelayedKind
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DelayedKind as DelayedKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DelayedKind" $ do
  Spec.it s "OneTimeBoon" $
    Common.assertCodec
      s
      DelayedKind.codec
      DelayedKind.OneTimeBoon
      " \"OneTimeBoon\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s DelayedKind.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s DelayedKind.codec
