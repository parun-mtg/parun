module Parun.Codec.TapCount where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.TapCount as TapCount

codec :: Codec.Codec TapCount.TapCount
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "UsingToughness" TapCount.UsingToughness,
      Arm.payload "GreaterBy" Common.natural TapCount.GreaterBy (\x -> case x of TapCount.GreaterBy n -> Just n; _ -> Nothing)
    ]

tagOf :: TapCount.TapCount -> String
tagOf x = case x of
  TapCount.UsingToughness -> "UsingToughness"
  TapCount.GreaterBy {} -> "GreaterBy"
