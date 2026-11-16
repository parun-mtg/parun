module Parun.Codec.CostedSpecialAction where

import qualified Parun.Codec.TurnUpProcedure as TurnUpProcedure
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CostedSpecialAction as CostedSpecialAction

codec :: Codec.Codec CostedSpecialAction.CostedSpecialAction
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Unlock" CostedSpecialAction.Unlock,
      Arm.nullary "Plot" CostedSpecialAction.Plot,
      Arm.payload "TurnFaceUp" TurnUpProcedure.codec CostedSpecialAction.TurnFaceUp (\x -> case x of CostedSpecialAction.TurnFaceUp y -> Just y; _ -> Nothing)
    ]

tagOf :: CostedSpecialAction.CostedSpecialAction -> String
tagOf x = case x of
  CostedSpecialAction.Unlock -> "Unlock"
  CostedSpecialAction.Plot -> "Plot"
  CostedSpecialAction.TurnFaceUp {} -> "TurnFaceUp"
