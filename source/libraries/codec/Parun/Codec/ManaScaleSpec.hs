module Parun.Codec.ManaScaleSpec where

import qualified Parun.Codec.ManaScale as ManaScale
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ManaScale as ManaScale

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaScale" $ do
  Spec.it s "Keeps" $
    Common.assertCodec s ManaScale.codec ManaScale.Keeps " {\"type\":\"Keeps\"} "
  Spec.it s "Exactly" $
    Common.assertCodec s ManaScale.codec (ManaScale.Exactly 1) " {\"type\":\"Exactly\",\"value\":1} "
  Spec.it s "Times" $
    Common.assertCodec s ManaScale.codec (ManaScale.Times 2) " {\"type\":\"Times\",\"value\":2} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaScale.codec
