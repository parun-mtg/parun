module Parun.Codec.ManaInsteadSpec where

import qualified Parun.Codec.ManaInstead as ManaInstead
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ManaInstead as ManaInstead
import qualified Parun.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaInstead" $ do
  Spec.it s "OfType" $
    Common.assertCodec
      s
      ManaInstead.codec
      (ManaInstead.OfType (ManaType.Colored Color.Red))
      " {\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":\"Red\"}} "
  Spec.it s "AnyColor" $
    Common.assertCodec
      s
      ManaInstead.codec
      ManaInstead.AnyColor
      " {\"type\":\"AnyColor\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaInstead.codec
