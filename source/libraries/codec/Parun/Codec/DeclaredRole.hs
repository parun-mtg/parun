module Parun.Codec.DeclaredRole where

import qualified Parun.Codec.AttackTargetKind as AttackTargetKind
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.DeclaredRole as DeclaredRole

codec :: Codec.Codec DeclaredRole.DeclaredRole
codec =
  Arm.tagged
    tagOf
    [ Arm.optionalPayload "Attacked" AttackTargetKind.codec DeclaredRole.Attacked (\x -> case x of DeclaredRole.Attacked y -> Just y; _ -> Nothing),
      Arm.nullary "Blocked" DeclaredRole.Blocked
    ]

tagOf :: DeclaredRole.DeclaredRole -> String
tagOf x = case x of
  DeclaredRole.Attacked {} -> "Attacked"
  DeclaredRole.Blocked -> "Blocked"
