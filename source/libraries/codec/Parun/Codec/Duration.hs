module Parun.Codec.Duration where

import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.DurationEvent as DurationEvent
import qualified Parun.Codec.NextStepOf as NextStepOf
import qualified Parun.Codec.NextWindowOf as NextWindowOf
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.Codec.UntilPaid as UntilPaid
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Duration as Duration

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Duration.Duration
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "UntilEndOfTurn" Duration.UntilEndOfTurn,
      Arm.nullary "Indefinite" Duration.Indefinite,
      Arm.nullary "Perpetual" Duration.Perpetual,
      Arm.payload "UntilNextTurnOf" PlayerRef.codec Duration.UntilNextTurnOf (\x -> case x of Duration.UntilNextTurnOf y -> Just y; _ -> Nothing),
      Arm.payload "UntilNextStepOf" NextStepOf.codec Duration.UntilNextStepOf (\x -> case x of Duration.UntilNextStepOf y -> Just y; _ -> Nothing),
      Arm.nullary "UntilEndOfYourNextTurn" Duration.UntilEndOfYourNextTurn,
      Arm.payload "UntilEndOfNextTurnOf" PlayerRef.codec Duration.UntilEndOfNextTurnOf (\x -> case x of Duration.UntilEndOfNextTurnOf y -> Just y; _ -> Nothing),
      Arm.payload "DuringNextOf" NextWindowOf.codec Duration.DuringNextOf (\x -> case x of Duration.DuringNextOf y -> Just y; _ -> Nothing),
      Arm.nullary "DuringYourNextTurn" Duration.DuringYourNextTurn,
      Arm.nullary "DuringThatExtraTurn" Duration.DuringThatExtraTurn,
      Arm.nullary "DuringThatCombatPhase" Duration.DuringThatCombatPhase,
      Arm.payload "ForAsLongAs" Condition.codec Duration.ForAsLongAs (\x -> case x of Duration.ForAsLongAs y -> Just y; _ -> Nothing),
      Arm.nullary "UntilEndOfCombat" Duration.UntilEndOfCombat,
      Arm.nullary "UntilEndOfCombatOnYourNextTurn" Duration.UntilEndOfCombatOnYourNextTurn,
      Arm.payload "UntilPaid" UntilPaid.codec Duration.UntilPaid (\x -> case x of Duration.UntilPaid y -> Just y; _ -> Nothing),
      Arm.optionalPayload "UntilUsed" SlotName.codec Duration.UntilUsed (\x -> case x of Duration.UntilUsed y -> Just y; _ -> Nothing),
      Arm.payload "UntilEvent" DurationEvent.codec Duration.UntilEvent (\x -> case x of Duration.UntilEvent y -> Just y; _ -> Nothing)
    ]

tagOf :: Duration.Duration -> String
tagOf x = case x of
  Duration.UntilEndOfTurn {} -> "UntilEndOfTurn"
  Duration.Indefinite {} -> "Indefinite"
  Duration.Perpetual {} -> "Perpetual"
  Duration.UntilNextTurnOf {} -> "UntilNextTurnOf"
  Duration.UntilNextStepOf {} -> "UntilNextStepOf"
  Duration.UntilEndOfYourNextTurn {} -> "UntilEndOfYourNextTurn"
  Duration.UntilEndOfNextTurnOf {} -> "UntilEndOfNextTurnOf"
  Duration.DuringNextOf {} -> "DuringNextOf"
  Duration.DuringYourNextTurn {} -> "DuringYourNextTurn"
  Duration.DuringThatExtraTurn {} -> "DuringThatExtraTurn"
  Duration.DuringThatCombatPhase {} -> "DuringThatCombatPhase"
  Duration.ForAsLongAs {} -> "ForAsLongAs"
  Duration.UntilEndOfCombat {} -> "UntilEndOfCombat"
  Duration.UntilEndOfCombatOnYourNextTurn {} -> "UntilEndOfCombatOnYourNextTurn"
  Duration.UntilPaid {} -> "UntilPaid"
  Duration.UntilUsed {} -> "UntilUsed"
  Duration.UntilEvent {} -> "UntilEvent"
