module Parun.Codec.CopyPowerToughnessSpec where

import qualified Parun.Codec.CopyPowerToughness as CopyPowerToughness
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.CopyPowerToughness as CopyPowerToughness
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SetPowerToughness as SetPowerToughness

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CopyPowerToughness" $ do
  Spec.it s "Numbers round-trips" $
    Common.assertCodec
      s
      CopyPowerToughness.codec
      (CopyPowerToughness.Numbers (SetPowerToughness.MkSetPowerToughness {SetPowerToughness.power = 7, SetPowerToughness.toughness = 8}))
      " {\"type\":\"Numbers\",\"value\":{\"power\":7,\"toughness\":8}} "
  Spec.it s "Defined round-trips" $
    Common.assertCodec
      s
      CopyPowerToughness.codec
      (CopyPowerToughness.Defined (CharacteristicPT.MkCharacteristicPT {CharacteristicPT.power = Quantity.Literal 1, CharacteristicPT.toughness = Quantity.Literal 2}))
      " {\"type\":\"Defined\",\"value\":{\"power\":{\"type\":\"Literal\",\"value\":1},\"toughness\":{\"type\":\"Literal\",\"value\":2}}} "
  Spec.it s "Own round-trips" $
    Common.assertCodec
      s
      CopyPowerToughness.codec
      CopyPowerToughness.Own
      " {\"type\":\"Own\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CopyPowerToughness.codec
