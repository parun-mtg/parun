module Parun.Codec.Again where

import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Again as Again

codec :: Codec.Codec Again.Again
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "AskPlayer" PlayerRef.codec Again.AskPlayer (\x -> case x of Again.AskPlayer y -> Just y; _ -> Nothing),
      Arm.payload "MoreTimes" Quantity.codec Again.MoreTimes (\x -> case x of Again.MoreTimes y -> Just y; _ -> Nothing)
    ]

tagOf :: Again.Again -> String
tagOf x = case x of
  Again.AskPlayer {} -> "AskPlayer"
  Again.MoreTimes {} -> "MoreTimes"
