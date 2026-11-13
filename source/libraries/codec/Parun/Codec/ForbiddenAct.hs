module Parun.Codec.ForbiddenAct where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.ForbiddenAct as ForbiddenAct

codec :: Codec.Codec ForbiddenAct.ForbiddenAct
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "GainLife" ForbiddenAct.GainLife,
      Arm.nullary "LoseLife" ForbiddenAct.LoseLife,
      Arm.payload "DrawMoreThan" Common.natural ForbiddenAct.DrawMoreThan (\x -> case x of ForbiddenAct.DrawMoreThan y -> Just y; _ -> Nothing),
      Arm.nullary "BecomeMonarch" ForbiddenAct.BecomeMonarch
    ]

tagOf :: ForbiddenAct.ForbiddenAct -> String
tagOf x = case x of
  ForbiddenAct.GainLife -> "GainLife"
  ForbiddenAct.LoseLife -> "LoseLife"
  ForbiddenAct.DrawMoreThan {} -> "DrawMoreThan"
  ForbiddenAct.BecomeMonarch -> "BecomeMonarch"
