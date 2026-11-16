module Parun.Codec.RestrictedAction where

import qualified Parun.Codec.Designation as Designation
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RestrictedAction as RestrictedAction

codec :: Codec.Codec RestrictedAction.RestrictedAction
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Sacrifice" RestrictedAction.Sacrifice,
      Arm.nullary "TurnFaceUp" RestrictedAction.TurnFaceUp,
      Arm.payload "BecomeDesignated" Designation.codec RestrictedAction.BecomeDesignated (\x -> case x of RestrictedAction.BecomeDesignated y -> Just y; _ -> Nothing)
    ]

tagOf :: RestrictedAction.RestrictedAction -> String
tagOf x = case x of
  RestrictedAction.Sacrifice {} -> "Sacrifice"
  RestrictedAction.TurnFaceUp {} -> "TurnFaceUp"
  RestrictedAction.BecomeDesignated {} -> "BecomeDesignated"
