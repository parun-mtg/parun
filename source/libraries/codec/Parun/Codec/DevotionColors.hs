module Parun.Codec.DevotionColors where

import qualified Parun.Codec.Color as Color
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.DevotionColors as DevotionColors

codec :: Codec.Codec DevotionColors.DevotionColors
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Listed" (Common.set Color.codec) DevotionColors.Listed (\x -> case x of DevotionColors.Listed y -> Just y; _ -> Nothing),
      Arm.nullary "SourceChosen" DevotionColors.SourceChosen
    ]

tagOf :: DevotionColors.DevotionColors -> String
tagOf x = case x of
  DevotionColors.Listed {} -> "Listed"
  DevotionColors.SourceChosen {} -> "SourceChosen"
