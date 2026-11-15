-- | Builders for the abilities the engine writes itself rather than reads off a
-- card: a keyword's, a designation's, the rulebook's own. Its only imports are
-- types, so Parun.Engine.Keyword and the modules beneath it can use it.
module Parun.Engine.Mint where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Activator as Activator
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Clause as Clause
import Parun.Types.Condition (Condition)
import qualified Parun.Types.Copyability as Copyability
import Parun.Types.Cost (Cost)
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.DamageTrigger as DamageTrigger
import Parun.Types.Effect (Effect)
import qualified Parun.Types.Effect as Effect
import Parun.Types.Filter (Filter)
import Parun.Types.Keyword (Keyword)
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaProduction as ManaProduction
import qualified Parun.Types.ManaRetention as ManaRetention
import Parun.Types.ManaType (ManaType)
import qualified Parun.Types.MayPass as MayPass
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeMemory as ModeMemory
import qualified Parun.Types.ModeSelection as ModeSelection
import Parun.Types.Optionality (Optionality)
import qualified Parun.Types.Optionality as Optionality
import Parun.Types.PayGate (PayGate)
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import Parun.Types.SlotName (SlotName)
import qualified Parun.Types.SpellCast as SpellCast
import qualified Parun.Types.StepBegins as StepBegins
import Parun.Types.TargetSlot (TargetSlot)
import Parun.Types.TriggerCondition (TriggerCondition)
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerLimit as TriggerLimit
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnScope as TurnScope

-- | One clause (CR 608.2e) of these effects in order (CR 608.2c), with this
-- optionality and nothing else: no "if you do", no "if", no "otherwise", no
-- "unless".
clause :: Optionality -> Seq.Seq (Effect card ability) -> Clause.Clause card ability
clause optionality = Clause.MkClause Nothing Nothing Nothing optionality Nothing

-- | A `clause` its controller must follow.
mandatory :: Seq.Seq (Effect card ability) -> Clause.Clause card ability
mandatory = clause Optionality.Mandatory

-- | A `mandatory` clause that runs only if this condition holds as it resolves:
-- a printed "if" scoped to one clause (CR 608.2c).
mandatoryIf :: Condition -> Seq.Seq (Effect card ability) -> Clause.Clause card ability
mandatoryIf condition = Clause.MkClause Nothing (Just condition) Nothing Optionality.Mandatory Nothing

-- | A `mandatory` clause hung on this CR 118.12 payment offer: "unless", or
-- "you may pay ... if you do", by the gate's branch.
mandatoryPaying :: PayGate -> Seq.Seq (Effect card ability) -> Clause.Clause card ability
mandatoryPaying gate = Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory (Just gate)

-- | A `clause` its controller may decline as it resolves: "you may" (CR 603.5).
youMay :: Seq.Seq (Effect card ability) -> Clause.Clause card ability
youMay = clause (Optionality.Optional (PlayerRef.Relative PlayerRelation.You) MayPass.AllSeatsFirst)

-- | One mode, selected outright, of one mandatory clause holding these effects
-- in order (CR 608.2c): an ability that is not modal (CR 700.2) and chooses
-- nothing but its targets.
oneMode :: Seq.Seq (Effect card ability) -> Modal.Modal card ability
oneMode = oneModeTargeting Map.empty

-- | `oneMode`, with these target slots (CR 115.1).
oneModeTargeting :: Map.Map SlotName TargetSlot -> Seq.Seq (Effect card ability) -> Modal.Modal card ability
oneModeTargeting slots = oneModeOf slots . Seq.singleton . mandatory

-- | One mode, selected outright, of these clauses and target slots: the
-- general form of `oneModeTargeting`, for a clause that is optional, gated or
-- one of several.
oneModeOf :: Map.Map SlotName TargetSlot -> Seq.Seq (Clause.Clause card ability) -> Modal.Modal card ability
oneModeOf slots clauses = Modal.MkModal (Seq.singleton (Mode.MkMode clauses slots)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty

-- | A triggered ability (CR 603.1) of one `oneMode`, with no intervening "if"
-- (CR 603.4), no name and no "only once" rider.
trigger :: TriggerCondition -> Seq.Seq (Effect card ability) -> TriggeredAbility.TriggeredAbility card ability
trigger condition = triggerOf condition Nothing . oneMode

-- | `trigger`, with this intervening "if" (CR 603.4).
triggerIf :: TriggerCondition -> Condition -> Seq.Seq (Effect card ability) -> TriggeredAbility.TriggeredAbility card ability
triggerIf condition intervening = triggerOf condition (Just intervening) . oneMode

-- | A triggered ability (CR 603.1) of this modal and intervening "if" (CR
-- 603.4), with no name and no "only once" rider: the general form of
-- `trigger` and `triggerIf`.
triggerOf :: TriggerCondition -> Maybe Condition -> Modal.Modal card ability -> TriggeredAbility.TriggeredAbility card ability
triggerOf condition intervening modal =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal = modal,
      TriggeredAbility.intervening = intervening,
      TriggeredAbility.name = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited,
      TriggeredAbility.functionsFrom = Set.empty,
      TriggeredAbility.keyword = Nothing
    }

-- | "At the beginning of your upkeep" (CR 503.1): @Relative You@ with the
-- ability's controller as "you".
yourUpkeep :: TriggerCondition
yourUpkeep = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) Nothing (TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.You)))

-- | "Whenever ... casts a spell" (CR 601.2i), the filter naming whose and
-- which: from any zone, on every occurrence rather than one counted one, and
-- never for a copy.
spellCast :: Filter Keyword -> TriggerCondition
spellCast = TriggerCondition.SpellCast . SpellCast.matching

-- | "When you cast this spell" (CR 601.2i): cascade, storm and the other
-- keywords whose trigger is the bearer's own cast.
selfCast :: TriggerCondition
selfCast = TriggerCondition.SpellCast SpellCast.self

-- | "Whenever this creature deals combat damage to a player". A PLAYER and not
-- an opponent: rules 702.70a, 702.99a, 702.112a and 702.115a all word it so,
-- where Akki Lavarunner and Questing Beast print "to an opponent".
dealsCombatDamageToAPlayer :: TriggerCondition
dealsCombatDamageToAPlayer =
  TriggerCondition.DealsDamage
    DamageTrigger.MkDamageTrigger
      { DamageTrigger.actor = ActingPermanent.Self,
        DamageTrigger.kind = Just DamageKind.Combat,
        DamageTrigger.recipients = Set.singleton (DamageRecipient.Player PlayerRelation.AnyPlayer),
        DamageTrigger.atLeast = 0
      }

-- | CR 305.6's intrinsic mana ability is "{T}: Add [the type]", printed on no card
-- and so minted rather than read off one: {T} and nothing else. What makes
-- the cost worth carrying at all is that a PRINTED mana ability's need not be
-- (Mana Confluence's is "{T}, Pay 1 life"), and CR 602.2b makes an activation
-- pay whichever it has.
intrinsicManaCost :: Cost Keyword
intrinsicManaCost =
  Cost.MkCost
    { Cost.mana = Just (ManaCost.MkManaCost []),
      Cost.components = [CostComponent.TapThis]
    }

-- | The ability above's one clause, carrying the instruction below and no rider.
intrinsicManaClause :: ManaType -> Clause.Clause card ability
intrinsicManaClause manaType =
  Clause.MkClause
    { Clause.ifTaken = Nothing,
      Clause.condition = Nothing,
      Clause.orElse = Nothing,
      Clause.optionality = Optionality.Mandatory,
      Clause.payGate = Nothing,
      Clause.effects = Seq.singleton (Effect.AddMana (intrinsicManaAddition manaType))
    }

-- | The instruction the ability gives: CR 305.6 writes it out as "{T}: Add
-- [mana symbol]" and nothing more, so every field beyond the type takes the
-- value an unwritten clause means -- CR 109.5's "you", CR 106.4's ordinary loss
-- as the step ends, and neither of CR 106.6's two clauses.
intrinsicManaAddition :: ManaType -> ManaAddition.ManaAddition
intrinsicManaAddition manaType =
  ManaAddition.MkManaAddition
    { ManaAddition.player = PlayerRef.Relative PlayerRelation.You,
      ManaAddition.production = ManaProduction.OfType manaType,
      ManaAddition.count = Quantity.Literal 1,
      ManaAddition.retention = ManaRetention.Ordinary,
      ManaAddition.restriction = Nothing,
      ManaAddition.rider = Nothing,
      ManaAddition.whenSpent = Nothing,
      ManaAddition.choosesColor = False
    }

-- | The ability `intrinsicManaCost` and `intrinsicManaClause` make, whole, for
-- an object that has it without being a land with a basic land type
-- (Manascape Refractor, CR 613.1f).
intrinsicManaAbility :: ManaType -> ActivatedAbility.ActivatedAbility card ability
intrinsicManaAbility manaType =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = intrinsicManaCost,
      ActivatedAbility.costChoices = [],
      ActivatedAbility.maximumX = [],
      ActivatedAbility.definedX = Nothing,
      ActivatedAbility.modal = oneModeOf Map.empty (Seq.singleton (intrinsicManaClause manaType)),
      ActivatedAbility.restrictions = [],
      ActivatedAbility.activator = Activator.Related PlayerRelation.You,
      ActivatedAbility.condition = Nothing,
      ActivatedAbility.name = Nothing,
      ActivatedAbility.keyword = Nothing,
      ActivatedAbility.copyability = Copyability.Copyable,
      ActivatedAbility.chosenPlayer = Nothing
    }
