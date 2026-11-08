module Parun.Codec.ComputedLife where

import qualified Parun.Codec.Rounding as Rounding
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ComputedLife as ComputedLife

codec :: Codec.Codec ComputedLife.ComputedLife
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "HalfLife" Rounding.codec ComputedLife.HalfLife (\x -> case x of ComputedLife.HalfLife y -> Just y; _ -> Nothing),
      Arm.nullary "CommanderColorCount" ComputedLife.CommanderColorCount
    ]

tagOf :: ComputedLife.ComputedLife -> String
tagOf x = case x of
  ComputedLife.HalfLife {} -> "HalfLife"
  ComputedLife.CommanderColorCount {} -> "CommanderColorCount"
