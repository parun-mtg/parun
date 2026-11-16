module Parun.Codec.EventShape where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.AttackWindow as AttackWindow
import qualified Parun.Codec.CardArrivedIn as CardArrivedIn
import qualified Parun.Codec.CastWindow as CastWindow
import qualified Parun.Codec.MovedBetween as MovedBetween
import qualified Parun.Codec.SourceDealtDamage as SourceDealtDamage
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.EventShape as EventShape

-- | Every arm is tagged, and each payload's own codec says what its value looks
-- like: CardArrivedIn's is an object rather than the bare Zone it once was,
-- since the shape now carries an origin exclusion beside the destination.
codec :: (Typeable.Typeable keyword, Ord keyword) => Codec.Codec keyword -> Codec.Codec (EventShape.EventShape keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "MovedBetween" MovedBetween.codec EventShape.MovedBetween (\x -> case x of EventShape.MovedBetween y -> Just y; _ -> Nothing),
      Arm.payload "MovedFrom" Zone.codec EventShape.MovedFrom (\x -> case x of EventShape.MovedFrom y -> Just y; _ -> Nothing),
      Arm.payload "CardArrivedIn" CardArrivedIn.codec EventShape.CardArrivedIn (\x -> case x of EventShape.CardArrivedIn y -> Just y; _ -> Nothing),
      Arm.payload "SpellCast" CastWindow.codec EventShape.SpellCast (\x -> case x of EventShape.SpellCast y -> Just y; _ -> Nothing),
      Arm.payload "AttackerDeclared" AttackWindow.codec EventShape.AttackerDeclared (\x -> case x of EventShape.AttackerDeclared y -> Just y; _ -> Nothing),
      Arm.nullary "CountersPut" EventShape.CountersPut,
      Arm.nullary "Sacrificed" EventShape.Sacrificed,
      Arm.nullary "TurnedFaceUp" EventShape.TurnedFaceUp,
      Arm.payload "SourceDealtDamage" (SourceDealtDamage.codec keywordCodec) EventShape.SourceDealtDamage (\x -> case x of EventShape.SourceDealtDamage y -> Just y; _ -> Nothing),
      Arm.nullary "PermanentDealtDamage" EventShape.PermanentDealtDamage
    ]

tagOf :: EventShape.EventShape keyword -> String
tagOf x = case x of
  EventShape.MovedBetween {} -> "MovedBetween"
  EventShape.MovedFrom {} -> "MovedFrom"
  EventShape.CardArrivedIn {} -> "CardArrivedIn"
  EventShape.SpellCast {} -> "SpellCast"
  EventShape.AttackerDeclared {} -> "AttackerDeclared"
  EventShape.CountersPut -> "CountersPut"
  EventShape.Sacrificed -> "Sacrificed"
  EventShape.TurnedFaceUp -> "TurnedFaceUp"
  EventShape.SourceDealtDamage {} -> "SourceDealtDamage"
  EventShape.PermanentDealtDamage -> "PermanentDealtDamage"
