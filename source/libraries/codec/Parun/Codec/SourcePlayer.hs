module Parun.Codec.SourcePlayer where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.SourcePlayer as SourcePlayer

codec :: Codec.Codec SourcePlayer.SourcePlayer
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Enchanted" SourcePlayer.Enchanted,
      Arm.nullary "Chosen" SourcePlayer.Chosen,
      Arm.payload "LastChoseWord" Common.natural SourcePlayer.LastChoseWord (\x -> case x of SourcePlayer.LastChoseWord n -> Just n; _ -> Nothing)
    ]

tagOf :: SourcePlayer.SourcePlayer -> String
tagOf x = case x of
  SourcePlayer.Enchanted -> "Enchanted"
  SourcePlayer.Chosen -> "Chosen"
  SourcePlayer.LastChoseWord _ -> "LastChoseWord"
