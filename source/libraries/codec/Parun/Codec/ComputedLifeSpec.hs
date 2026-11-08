module Parun.Codec.ComputedLifeSpec where

import qualified Parun.Codec.ComputedLife as ComputedLife
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ComputedLife as ComputedLife
import qualified Parun.Types.Rounding as Rounding

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ComputedLife" $ do
  Spec.it s "HalfLife" $
    Common.assertCodec
      s
      ComputedLife.codec
      (ComputedLife.HalfLife Rounding.Up)
      " {\"type\":\"HalfLife\",\"value\":\"Up\"} "
  Spec.it s "CommanderColorCount" $
    Common.assertCodec
      s
      ComputedLife.codec
      ComputedLife.CommanderColorCount
      " {\"type\":\"CommanderColorCount\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ComputedLife.codec
