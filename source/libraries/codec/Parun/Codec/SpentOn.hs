module Parun.Codec.SpentOn where

import qualified Parun.Codec.ClauseIndex as ClauseIndex
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.SpentOn as SpentOn

codec :: Codec.Codec SpentOn.SpentOn
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Triggering" SpentOn.Triggering,
      Arm.payload "TakingInstruction" ClauseIndex.codec SpentOn.TakingInstruction (\x -> case x of SpentOn.TakingInstruction y -> Just y; _ -> Nothing)
    ]

tagOf :: SpentOn.SpentOn -> String
tagOf x = case x of
  SpentOn.Triggering -> "Triggering"
  SpentOn.TakingInstruction {} -> "TakingInstruction"
