module Parun.Codec.CopyPowerToughness where

import qualified Parun.Codec.CharacteristicPT as CharacteristicPT
import qualified Parun.Codec.SetPowerToughness as SetPowerToughness
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CopyPowerToughness as CopyPowerToughness

codec :: Codec.Codec CopyPowerToughness.CopyPowerToughness
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Numbers" SetPowerToughness.codec CopyPowerToughness.Numbers $ \x -> case x of
        CopyPowerToughness.Numbers y -> Just y
        _ -> Nothing,
      Arm.payload "Defined" CharacteristicPT.codec CopyPowerToughness.Defined $ \x -> case x of
        CopyPowerToughness.Defined y -> Just y
        _ -> Nothing
    ]

tagOf :: CopyPowerToughness.CopyPowerToughness -> String
tagOf x = case x of
  CopyPowerToughness.Numbers {} -> "Numbers"
  CopyPowerToughness.Defined {} -> "Defined"
