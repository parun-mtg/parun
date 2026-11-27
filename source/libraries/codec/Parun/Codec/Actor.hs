module Parun.Codec.Actor where

import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Actor as Actor

codec :: Codec.Codec Actor.Actor
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Candidate" Actor.Candidate,
      Arm.nullary "Source" Actor.Source,
      Arm.payload "InSlot" SlotName.codec Actor.InSlot (\x -> case x of Actor.InSlot y -> Just y; _ -> Nothing)
    ]

tagOf :: Actor.Actor -> String
tagOf x = case x of
  Actor.Candidate -> "Candidate"
  Actor.Source -> "Source"
  Actor.InSlot {} -> "InSlot"
