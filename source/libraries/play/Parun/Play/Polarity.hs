-- | Which way an effect bears on what a target slot holds: the
-- classification the polar playout policy (Parun.Play.Search.polar) chooses
-- targets by. A heuristic, not a rule: nothing in the rules core reads it.
module Parun.Play.Polarity where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Parun.Types.CountedDiscard as CountedDiscard
import qualified Parun.Types.Counter as Counter
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.DamagePart as DamagePart
import qualified Parun.Types.DealDamage as DealDamage
import qualified Parun.Types.Destroy as Destroy
import qualified Parun.Types.Discard as Discard
import qualified Parun.Types.Draw as Draw
import Parun.Types.Effect (Effect)
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.GainControl as GainControl
import qualified Parun.Types.LifeLoss as LifeLoss
import qualified Parun.Types.Mill as Mill
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.ModifyTarget as ModifyTarget
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerQuantity as PlayerQuantity
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.Polarity as Polarity
import qualified Parun.Types.PutCounters as PutCounters
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Tap as Tap
import qualified Parun.Types.Zone as Zone

-- | The polarity of the first of `effects` that names the slot and has one,
-- Nothing where none does.
slotPolarity :: (Foldable t) => SlotName.SlotName -> t (Effect card ability) -> Maybe Polarity.Polarity
slotPolarity slot = Maybe.listToMaybe . Maybe.mapMaybe (polarity slot) . Foldable.toList

-- | Whether `effect` harms or helps what `slot` holds, Nothing where it names
-- the slot otherwise or not at all. It reads the effect's kind and its
-- direction (a sign, a counter kind, a destination); every effect left out is
-- unclassified, which leaves its targets to the random pick. A move to a hand
-- reads as a bounce: a return from a graveyard targets its caster's own cards
-- alone (Raise Dead). Parun.SearchSpec's table names the card behind each arm.
polarity :: SlotName.SlotName -> Effect card ability -> Maybe Polarity.Polarity
polarity slot effect = case effect of
  Effect.DealDamage d -> harms (any (names . DamagePart.ref) (DealDamage.parts d))
  Effect.Destroy d -> harms (names (Destroy.ref d))
  Effect.Counter c -> harms (names (Counter.ref c))
  Effect.Tap t -> harms (names (Tap.ref t))
  Effect.GainControl g -> harms (names (GainControl.ref g))
  Effect.MoveToZone m
    | names (MoveToZone.ref m) -> Just (if MoveToZone.zone m == Zone.Battlefield then Polarity.Helps else Polarity.Harms)
  Effect.ModifyTarget m
    | names (ModifyTarget.ref m) -> modification (ModifyTarget.modification m)
  Effect.PutCounters p
    | names (PutCounters.ref p) -> counterKind (PutCounters.kind p)
  Effect.Draw d -> helps (player (Draw.player d))
  Effect.GainLife q -> helps (player (PlayerQuantity.player q))
  Effect.LoseLife l -> harms (player (LifeLoss.player l))
  Effect.Mill m -> harms (player (Mill.player m))
  Effect.Discard (Discard.Counted d) -> harms (player (CountedDiscard.player d))
  _ -> Nothing
  where
    harms named = if named then Just Polarity.Harms else Nothing
    helps named = if named then Just Polarity.Helps else Nothing
    names ref = case ref of
      ObjectRef.InSlot s -> s == slot
      ObjectRef.AllOf refs -> any names refs
      _ -> False
    player ref = case ref of
      PlayerRef.InSlot s -> s == slot
      PlayerRef.EachInSlot s -> s == slot
      _ -> False

-- | A continuous effect's polarity: a printed change of power and toughness
-- by its sign, a keyword gained helping.
modification :: Modification.Modification ability -> Maybe Polarity.Polarity
modification m = case m of
  Modification.ModifyPowerToughness change -> case (ModifyPowerToughness.power change, ModifyPowerToughness.toughness change) of
    (Quantity.Literal p, Quantity.Literal t)
      | p + t > 0 -> Just Polarity.Helps
      | p + t < 0 -> Just Polarity.Harms
    _ -> Nothing
  Modification.GainKeyword _ -> Just Polarity.Helps
  _ -> Nothing

-- | A counter's polarity: +1/+1 counters help, -1/-1 counters harm (CR
-- 122.1a).
counterKind :: CounterKind.CounterKind keyword -> Maybe Polarity.Polarity
counterKind kind = case kind of
  CounterKind.PlusOnePlusOne -> Just Polarity.Helps
  CounterKind.MinusOneMinusOne -> Just Polarity.Harms
  _ -> Nothing
