module Parun.Codec.CombatDamageObligation where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CombatDamageObligation as CombatDamageObligation

codec :: Codec.Codec CombatDamageObligation.CombatDamageObligation
codec = Arm.enum
