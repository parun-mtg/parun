module Parun.Codec.Prohibition where

import qualified Parun.Codec.UntapScope as UntapScope
import qualified Parun.Codec.Zone as Zone
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
      Arm.payload "EnterFrom" Zone.codec Prohibition.EnterFrom (\x -> case x of Prohibition.EnterFrom y -> Just y; _ -> Nothing),
      Arm.nullary "PhaseIn" Prohibition.PhaseIn,
      Arm.nullary "Regenerate" Prohibition.Regenerate,
      Arm.nullary "TapExceptAttacking" Prohibition.TapExceptAttacking,
      Arm.payload "Untap" UntapScope.codec Prohibition.Untap (\x -> case x of Prohibition.Untap y -> Just y; _ -> Nothing)
    ]

tagOf :: Prohibition.Prohibition -> String
tagOf x = case x of
  Prohibition.Activate -> "Activate"
  Prohibition.Block -> "Block"
  Prohibition.Counters -> "Counters"
  Prohibition.EnterFrom {} -> "EnterFrom"
  Prohibition.PhaseIn -> "PhaseIn"
  Prohibition.Regenerate -> "Regenerate"
  Prohibition.TapExceptAttacking -> "TapExceptAttacking"
  Prohibition.Untap {} -> "Untap"
