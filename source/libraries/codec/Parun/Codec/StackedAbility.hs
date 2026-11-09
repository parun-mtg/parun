module Parun.Codec.StackedAbility where

import qualified Parun.Codec.ActivatedAbility as ActivatedAbility
import qualified Parun.Codec.Card as Card
import qualified Parun.Codec.GrantedAbility as GrantedAbility
import qualified Parun.Codec.TriggeredAbility as TriggeredAbility
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.StackedAbility as StackedAbility

codec :: Codec.Codec StackedAbility.StackedAbility
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Activated" (ActivatedAbility.codec Card.codec (GrantedAbility.codec Card.codec)) StackedAbility.Activated (\x -> case x of StackedAbility.Activated y -> Just y; _ -> Nothing),
      Arm.payload "Triggered" (TriggeredAbility.codec Card.codec (GrantedAbility.codec Card.codec)) StackedAbility.Triggered (\x -> case x of StackedAbility.Triggered y -> Just y; _ -> Nothing)
    ]

tagOf :: StackedAbility.StackedAbility -> String
tagOf x = case x of
  StackedAbility.Activated {} -> "Activated"
  StackedAbility.Triggered {} -> "Triggered"
