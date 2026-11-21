module Parun.Codec.AttackChoice where

import qualified Parun.Codec.AttackTarget as AttackTarget
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.AttackChoice as AttackChoice

-- | CR 508.4's readings, as a deferred lot carries them
-- (Parun.Codec.DeferredLot).
codec :: Codec.Codec AttackChoice.AttackChoice
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Any" AttackChoice.Any,
      Arm.payload "Specified" AttackTarget.codec AttackChoice.Specified (\x -> case x of AttackChoice.Specified y -> Just y; _ -> Nothing),
      Arm.payload "Under" PlayerId.codec AttackChoice.Under (\x -> case x of AttackChoice.Under y -> Just y; _ -> Nothing)
    ]

tagOf :: AttackChoice.AttackChoice -> String
tagOf x = case x of
  AttackChoice.Any {} -> "Any"
  AttackChoice.Specified {} -> "Specified"
  AttackChoice.Under {} -> "Under"
