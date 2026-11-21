module Parun.Codec.Noted where

import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.Noted as Noted

codec :: Codec.Codec Noted.Noted
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "NotedMana" (Common.seq ManaType.codec) Noted.NotedMana (\x -> case x of Noted.NotedMana y -> Just y; _ -> Nothing),
      Arm.payload "NotedLifeTotal" Common.integer Noted.NotedLifeTotal (\x -> case x of Noted.NotedLifeTotal y -> Just y; _ -> Nothing)
    ]

tagOf :: Noted.Noted -> String
tagOf x = case x of
  Noted.NotedMana {} -> "NotedMana"
  Noted.NotedLifeTotal {} -> "NotedLifeTotal"
