module Parun.Codec.InstructionRewrite where

import qualified Parun.Codec.PlayerInstruction as PlayerInstruction
import qualified Parun.Codec.Scaling as Scaling
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.InstructionRewrite as InstructionRewrite

codec :: Codec.Codec InstructionRewrite.InstructionRewrite
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Scaled" Scaling.codec InstructionRewrite.Scaled (\x -> case x of InstructionRewrite.Scaled y -> Just y; _ -> Nothing),
      Arm.nullary "DrawInstead" InstructionRewrite.DrawInstead,
      Arm.nullary "RollD20Instead" InstructionRewrite.RollD20Instead,
      Arm.payload "PerformFirst" PlayerInstruction.codec InstructionRewrite.PerformFirst (\x -> case x of InstructionRewrite.PerformFirst y -> Just y; _ -> Nothing),
      Arm.payload "SetTo" Common.natural InstructionRewrite.SetTo (\x -> case x of InstructionRewrite.SetTo y -> Just y; _ -> Nothing),
      Arm.nullary "ReturnSourceInstead" InstructionRewrite.ReturnSourceInstead
    ]

tagOf :: InstructionRewrite.InstructionRewrite -> String
tagOf x = case x of
  InstructionRewrite.Scaled {} -> "Scaled"
  InstructionRewrite.DrawInstead -> "DrawInstead"
  InstructionRewrite.RollD20Instead -> "RollD20Instead"
  InstructionRewrite.PerformFirst {} -> "PerformFirst"
  InstructionRewrite.SetTo {} -> "SetTo"
  InstructionRewrite.ReturnSourceInstead -> "ReturnSourceInstead"
