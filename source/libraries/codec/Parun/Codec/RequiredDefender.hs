module Parun.Codec.RequiredDefender where

import qualified Parun.Codec.CardName as CardName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RequiredDefender as RequiredDefender

codec :: Codec.Codec RequiredDefender.RequiredDefender
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "ControllerOfAttached" RequiredDefender.ControllerOfAttached,
      Arm.nullary "OpponentWithMostLife" RequiredDefender.OpponentWithMostLife,
      Arm.nullary "ControllerOrTheirPlaneswalkers" RequiredDefender.ControllerOrTheirPlaneswalkers,
      Arm.payload "Noted" CardName.codec RequiredDefender.Noted (\x -> case x of RequiredDefender.Noted y -> Just y; _ -> Nothing)
    ]

tagOf :: RequiredDefender.RequiredDefender -> String
tagOf x = case x of
  RequiredDefender.ControllerOfAttached {} -> "ControllerOfAttached"
  RequiredDefender.OpponentWithMostLife {} -> "OpponentWithMostLife"
  RequiredDefender.ControllerOrTheirPlaneswalkers {} -> "ControllerOrTheirPlaneswalkers"
  RequiredDefender.Noted {} -> "Noted"
