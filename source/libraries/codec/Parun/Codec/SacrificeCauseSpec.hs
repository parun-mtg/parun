module Parun.Codec.SacrificeCauseSpec where

import qualified Parun.Codec.SacrificeCause as SacrificeCause
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.SacrificeCause as SacrificeCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SacrificeCause" $ do
  Spec.it s "Devour" $
    Common.assertCodec
      s
      SacrificeCause.codec
      SacrificeCause.Devour
      " \"Devour\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s SacrificeCause.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s SacrificeCause.codec
