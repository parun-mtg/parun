module Parun.Codec.PlayAllowance where

import qualified Parun.Codec.CardType as CardType
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.PlayAllowance as PlayAllowance

codec :: Codec.Codec PlayAllowance.PlayAllowance
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Plays" Common.natural PlayAllowance.Plays (\x -> case x of PlayAllowance.Plays y -> Just y; _ -> Nothing),
      Arm.payload "EachCardTypeBut" (Common.set CardType.codec) PlayAllowance.EachCardTypeBut (\x -> case x of PlayAllowance.EachCardTypeBut y -> Just y; _ -> Nothing)
    ]

tagOf :: PlayAllowance.PlayAllowance -> String
tagOf x = case x of
  PlayAllowance.Plays {} -> "Plays"
  PlayAllowance.EachCardTypeBut {} -> "EachCardTypeBut"
