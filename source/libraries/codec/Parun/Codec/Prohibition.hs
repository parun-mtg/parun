module Parun.Codec.Prohibition where

import qualified Parun.Codec.UntapScope as UntapScope
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Prohibition as Prohibition

codec :: Codec.Codec Prohibition.Prohibition
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Activate" Prohibition.Activate,
      Arm.nullary "Block" Prohibition.Block,
      Arm.nullary "Counters" Prohibition.Counters,
      Arm.nullary "PhaseIn" Prohibition.PhaseIn,
      Arm.nullary "Regenerate" Prohibition.Regenerate,
      Arm.payload "Untap" UntapScope.codec Prohibition.Untap (\x -> case x of Prohibition.Untap y -> Just y; _ -> Nothing)
    ]

tagOf :: Prohibition.Prohibition -> String
tagOf x = case x of
  Prohibition.Activate -> "Activate"
  Prohibition.Block -> "Block"
  Prohibition.Counters -> "Counters"
  Prohibition.PhaseIn -> "PhaseIn"
  Prohibition.Regenerate -> "Regenerate"
  Prohibition.Untap {} -> "Untap"
