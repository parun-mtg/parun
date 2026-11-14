-- | The effect DSL's one walk, answering four questions of a tree -- an effect,
-- an ability, a replacement, a cost, a duration, a card -- through the leaves
-- an Asking names: whether it might name one object, which
-- Parun.Engine.Interchangeable asks of every stored row; which of the turn's
-- history it reads (CR 733.3), which Parun.Engine.Fragmented asks of every
-- printing; whether it reads what its source's prevention effect stopped
-- (CR 615.13), which Parun.Engine.Replacement.choose asks of a waiting ability;
-- and which combined back faces it carries (CR 713.1a), which
-- Parun.Interpreter.chosenFace asks of the reference (CR 201.4e).
--
-- One function per type, each matching every constructor POSITIONALLY, so a
-- new constructor is a missing-pattern error here and a new field an arity
-- error, rather than an ObjectId or a history read this silently stops
-- reading. A field whose type can hold no ObjectId, slot name, Filter or
-- history read is a named discard, and that is the one site nothing forces: a
-- type that later gains one owes every discard of it here a function. A type
-- with a traversal in Parun.Engine.Children (Filter, Quantity, Effect,
-- TriggerCondition, Keyword, Cost, PlayerEffect, ReplacementEffect, Condition,
-- Duration, and the ability records: ActivatedAbility, TriggeredAbility,
-- StaticAbility, PlayerStaticAbility, PrintedReplacement, CostReduction,
-- AlternativeCost, GrantedAbility, Modal with its Mode, Clause and
-- ModeSelection, Face and Card, and the stored rows: ActiveReplacement,
-- DelayedTrigger, ActiveLaterPayment, Prevention, PreventionRider, the
-- Pending* effects and Expiry with its While, PaidExpiry and EventExpiry,
-- ObjectRef with ChosenPermanents, the rule-affecting rows: RuleAbilities
-- with its ActivationProhibition, AttachRestriction, RulePermission,
-- AttackRequirement, BlockPermission, BlockRequirement, CounterRestriction, CounterLimit,
-- KeywordCostRestriction, EntryRestriction, SacrificeRestriction, TargetRestriction
-- and UntapRestriction, the combat rules CombatRestriction, AttackCost,
-- BlockCost and PerCreature, Affected, the mana payloads: ManaAddition with
-- its ManaProduction, ManaCount, ManaRestriction, ManaRider and WhenSpent, and
-- SpendManaAsThough, DamagePattern, the copy and entry payloads: AsCopy with
-- its CopyClause, CopyException, WithCounters, EntryRiders with its
-- EntryAttack, EntryBlock, EntryController and EntryHost, FaceDownState and
-- FaceDownCharacteristics, CharacteristicPT, and TargetSlot with its Pool,
-- SlotCount and SlotPerPlayer, CastingPermission, ActivationRestriction, and
-- PayGate with its CostChoice, MovedKinds, RemovalCount, LibraryPlacement
-- with its ArrangedArrival, and Modification with its CardAbilities,
-- FullText, SetBasePowerToughness and ModifyPowerToughness) is matched there
-- instead, its leaf types discarded there.
--
-- A tree names an object in three ways: an ObjectId it holds outright, a slot
-- it reads (slotNames), and a Filter atom (filterNames). A Card it holds is
-- printed data and names nothing -- Parun.FilterPositionLintSpec's "CR
-- 702.119c no card writes IsObject" keeps the one runtime atom out of it --
-- so Parun.Engine.Interchangeable passes `const mempty` for it, and
-- Parun.Engine.Fragmented cardNames. A type parameter is answered by the
-- function the caller passes for it.
--
-- A tree reads the history through the arm of the construct whose engine read
-- site consults TurnRegisters.events or TurnRegisters.activationsThisTurn -- and a
-- keyword's keywordLooksBack in Parun.Engine.Children, where the engine
-- EXPANDS the keyword into one or gates its cost on one (Parun.Engine.Keyword,
-- Parun.Engine.Cost.candidateCostsGiven), and a cost component's costLooksBack
-- there likewise.
module Parun.Engine.Interchangeable.Mentions where

import qualified Data.Functor.Const as Const
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Parun.Engine.Binding as Binding.Engine
import qualified Parun.Engine.Children as Children
import qualified Parun.Types.AbilityAct as AbilityAct
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.ActivationProhibition as ActivationProhibition
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.ActiveLaterPayment as ActiveLaterPayment
import qualified Parun.Types.ActiveReplacement as ActiveReplacement
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.AffectedPlayers as AffectedPlayers
import qualified Parun.Types.AimedAt as AimedAt
import qualified Parun.Types.AimedPlayers as AimedPlayers
import qualified Parun.Types.AlternativeCost as AlternativeCost
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttachRestriction as AttachRestriction
import qualified Parun.Types.AttackCost as AttackCost
import qualified Parun.Types.AttackRequirement as AttackRequirement
import qualified Parun.Types.AttackingPlayers as AttackingPlayers
import qualified Parun.Types.Binding as Binding
import qualified Parun.Types.BlockCost as BlockCost
import qualified Parun.Types.BlockPermission as BlockPermission
import qualified Parun.Types.BlockRequirement as BlockRequirement
import qualified Parun.Types.BlockRole as BlockRole
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.CandidateId as CandidateId
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.CastingRestriction as CastingRestriction
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.Chooser as Chooser
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostBasis as CostBasis
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CostReduction as CostReduction
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLimit as CounterLimit
import qualified Parun.Types.CounterRestriction as CounterRestriction
import qualified Parun.Types.Counterability as Counterability
import qualified Parun.Types.DamagePattern as DamagePattern
import qualified Parun.Types.DamageTally as DamageTally
import qualified Parun.Types.DelayedTrigger as DelayedTrigger
import qualified Parun.Types.DungeonRoom as DungeonRoom
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.DurationEvent as DurationEvent
import qualified Parun.Types.DuringPhase as DuringPhase
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EnteredZone as EnteredZone
import qualified Parun.Types.EntryRestriction as EntryRestriction
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.EventShape as EventShape
import qualified Parun.Types.Expiry as Expiry
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FloatingCandidate as FloatingCandidate
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.HandAction as HandAction
import qualified Parun.Types.HistoryRead as HistoryRead
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordCostRestriction as KeywordCostRestriction
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.KeywordInstance as KeywordInstance
import qualified Parun.Types.LentKeyword as LentKeyword
import qualified Parun.Types.LibraryDeparture as LibraryDeparture
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LifeDirection as LifeDirection
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.MovedKinds as MovedKinds
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.Onset as Onset
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PendingEntryEffect as PendingEntryEffect
import qualified Parun.Types.PendingReplacementEffect as PendingReplacementEffect
import qualified Parun.Types.PermanentCandidate as PermanentCandidate
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.Power as Power
import qualified Parun.Types.PregameExile as PregameExile
import qualified Parun.Types.Prevention as Prevention
import qualified Parun.Types.PreventionRider as PreventionRider
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RemovalCount as RemovalCount
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.SacrificeRestriction as SacrificeRestriction
import qualified Parun.Types.Scope as Scope
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SpecialAction as SpecialAction
import qualified Parun.Types.SpellTally as SpellTally
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.SpentOn as SpentOn
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.TargetRestriction as TargetRestriction
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.Toughness as Toughness
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.TriggerLimit as TriggerLimit
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnScope as TurnScope
import qualified Parun.Types.TurnTally as TurnTally
import qualified Parun.Types.UntapRestriction as UntapRestriction
import qualified Parun.Types.WhichCounters as WhichCounters
import qualified Parun.Types.WhichTurn as WhichTurn
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneScope as ZoneScope

-- | What a traversal answers at each kind of leaf, combined over the tree by
-- r's Monoid: whether a tree might name one object (Parun.Engine.Interchangeable,
-- over Data.Monoid.Any), or which history it reads (Parun.Engine.Fragmented).
data Asking r = MkAsking
  { -- | An ObjectId the tree holds, by the test it passes.
    names :: (ObjectId -> Bool) -> r,
    -- | A slot read, or an atom reading the resolution context.
    context :: r,
    -- | A Filter atom reading a record kept per object id, which whole-object
    -- Eq does not compare.
    keyed :: r,
    -- | A read of the turn's history (CR 733.3).
    history :: HistoryRead.HistoryRead -> r,
    -- | A read of what the source's own prevention effect stopped (CR 615.13).
    prevention :: r,
    -- | A read of the counters the source's own effects moved (CR 607.1).
    counterLinks :: r,
    -- | What an inline card answers as a meld pair's combined back face (CR
    -- 713.1a), given what it answers as any inline card.
    melded :: r -> r
  }

-- | CR 201.4e / 713.1a: the combined back faces a card's meld instructions
-- carry inline, each the whole card the melded permanent shows.
combinedBackFaces :: Card.Card -> [Card.Card]
combinedBackFaces = snd . foldMap (faceNames asking (\card -> ([card], []))) . Card.faces
  where
    asking =
      MkAsking
        { names = const mempty,
          context = mempty,
          keyed = mempty,
          history = const mempty,
          prevention = mempty,
          counterLinks = mempty,
          melded = \(inline, backs) -> ([], inline <> backs)
        }

-- | Does this ability read what its source's prevention effect stopped?
readsPrevention :: TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Bool
readsPrevention =
  Monoid.getAny
    . triggeredAbilityNames
      asking
      (const mempty)
      (grantedAbilityNames asking (const mempty))
  where
    asking =
      MkAsking
        { names = const mempty,
          context = mempty,
          keyed = mempty,
          history = const mempty,
          prevention = Monoid.Any True,
          counterLinks = mempty,
          melded = id
        }

-- | The question whose answer is whether a tree reads the counters its
-- source's own effects moved (Parun.Engine.Fragmented.digest).
readsCounterLinks :: Asking Monoid.Any
readsCounterLinks =
  MkAsking
    { names = const mempty,
      context = mempty,
      keyed = mempty,
      history = const mempty,
      prevention = mempty,
      counterLinks = Monoid.Any True,
      melded = id
    }

-- Whether reading this slot might name the object.
slotNames :: Asking r -> SlotName.SlotName -> r
slotNames asking _slot = contextNames asking

-- A history read of the turn a tally names: the turn in progress is read
-- (CR 733.3), and last turn's log, which no loop of this turn can grow, is not.
turnHistory :: (Monoid r) => Asking r -> WhichTurn.WhichTurn -> HistoryRead.HistoryRead -> r
turnHistory asking which reading = case which of
  WhichTurn.ThisTurn -> history asking reading
  WhichTurn.LastTurn -> mempty

-- Whether an atom reading the resolution context -- an enclosing target slot's
-- amount, an attach's subject, a baked crew or attachment set -- might name the
-- object. The same answer as a slot read, since a stored row's context is its
-- environment.
contextNames :: Asking r -> r
contextNames = context

-- Whether a Filter might single this object out from one alike in every other
-- respect.
--
-- Two objects equal whole bar the timestamp, projecting alike, named by no
-- other object, relation, stored row or combat assignment, present every atom
-- the same Parun.Engine.Filter.View in all but three respects, so an atom reading
-- none of them answers the same of both, now and after either is chosen:
--
--   * IDENTITY: IsObject names an id outright.
--   * The turn's EVENT LOG: the look-back atoms (DeclaredThisTurn,
--     LeftLibraryThisTurn, DealtDamageThisTurn, ActivatedThisTurn,
--     EnteredThisTurn, TappedForManaThisTurn, the
--     crewed/convoked/saddled-the-source atoms), EnteredWithSource
--     (GameState.enteredWith), CounterLinkedToSource
--     (GameState.counteredWith) and CouldAttackThisTurn
--     (TurnRegisters.couldAttackThisTurn) read a record kept per object id,
--     answered by Asking's keyed -- and the look-back atoms by its history too.
--   * A RESOLUTION CONTEXT: the slot atoms, the slot and amount operands, the
--     attach and search subject atoms, CantPayKeywordCost and
--     AttachedNoLaterThanSource's baked set, answered by slotNames and
--     contextNames.
--
-- No atom reads a timestamp. The source comparisons read the row's source,
-- which Parun.Engine.Interchangeable checks is neither object; the combat atoms
-- read the combat it searches; the attachment atoms read the candidate's own
-- attachedTo, which whole-object Eq compares, or another object's, which
-- namedByAnother searches. A nested Filter is asked of some OTHER object -- a
-- host, a target, a source, a represented card, an attacher, a permanent a
-- player counts -- which may be one of the two, so it is recursed into.
filterNames :: (Monoid r) => Asking r -> Filter.Filter keyword -> r
filterNames asking criterion = case criterion of
  -- The operand may read the enclosing amount as well as a slot.
  Filter.Measures measures -> measuresNames asking measures
  Filter.IsTarget -> slotNames asking Binding.Engine.announcedTargets
  Filter.DeclaredThisTurn role -> keyed asking <> history asking (case role of BlockRole.Attacker -> HistoryRead.Attacks; BlockRole.Blocker -> HistoryRead.Blockers)
  Filter.CouldAttackThisTurn -> keyed asking
  Filter.LeftLibraryThisTurn how -> keyed asking <> case how of LibraryDeparture.Drawn -> mempty; LibraryDeparture.LastDrawn -> mempty; LibraryDeparture.Milled -> history asking HistoryRead.Mills
  Filter.CantPayKeywordCost _ -> contextNames asking
  Filter.CrewedSourceThisTurn -> keyed asking <> history asking HistoryRead.Crews
  Filter.ConvokedSourceThisTurn -> keyed asking <> history asking HistoryRead.Convokes <> history asking HistoryRead.Moves
  Filter.SaddledSourceThisTurn -> keyed asking <> history asking HistoryRead.Saddles
  Filter.DealtDamageThisTurn nested -> keyed asking <> history asking HistoryRead.Damage <> foldMap (filterNames asking) nested
  Filter.ActivatedThisTurn -> keyed asking <> history asking HistoryRead.Activations
  Filter.DealtDamageTo _ -> keyed asking <> history asking HistoryRead.Damage
  Filter.EnteredThisTurn (EnteredZone.MkEnteredZone to _from _window) -> keyed asking <> history asking (if to == Zone.Battlefield then HistoryRead.Entries else HistoryRead.Moves)
  Filter.BeforeMove nested -> keyed asking <> history asking HistoryRead.Moves <> filterNames asking nested
  Filter.TappedForManaThisTurn nested -> keyed asking <> history asking HistoryRead.TapsForMana <> filterNames asking nested
  Filter.CountersPutThisTurn _ _ -> keyed asking <> history asking HistoryRead.CounterPlacements
  Filter.EnteredWithSource -> keyed asking
  Filter.CounterLinkedToSource _moved -> keyed asking <> counterLinks asking
  Filter.AttachedNoLaterThanSource -> contextNames asking
  Filter.CanHostSubject -> contextNames asking
  Filter.CanAttachToSubject -> contextNames asking
  Filter.HostOfSubjectHasCardType _cardType -> contextNames asking
  _ ->
    Const.getConst $
      Children.filterChildren
        (Children.descending (Const.Const . filterNames asking))
          { Children.slot = \_arity slot -> Const.Const (slotNames asking slot),
            Children.object = \oid -> Const.Const (names asking (== oid))
          }
        criterion

activatedAbilityNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> ActivatedAbility.ActivatedAbility card ability -> r
activatedAbilityNames asking onCard onAbility = Const.getConst . Children.activatedAbilityChildren (abilityVisitors asking onCard onAbility (const mempty))

activationProhibitionNames :: (Monoid r) => Asking r -> ActivationProhibition.ActivationProhibition -> r
activationProhibitionNames asking = Const.getConst . Children.activationProhibitionChildren (ruleVisitors asking)

activationRestrictionNames :: (Monoid r) => Asking r -> ActivationRestriction.ActivationRestriction -> r
activationRestrictionNames asking = Const.getConst . Children.activationRestrictionChildren (permissionVisitors asking)

activeReplacementNames :: (Monoid r) => Asking r -> ActiveReplacement.ActiveReplacement -> r
activeReplacementNames asking = Const.getConst . Children.activeReplacementChildren (storedVisitors asking)

affectedNames :: (Monoid r) => Asking r -> Affected.Affected -> r
affectedNames asking =
  Const.getConst
    . Children.affectedChildren
      Children.MkAffectedChildren
        { Children.affectedObjects = \objects -> Const.Const (names asking (`elem` objects)),
          Children.affectedFilter = Const.Const . filterNames asking,
          Children.affectedDuration = Const.Const . durationNames asking
        }

affectedPlayersNames :: (Monoid r) => Asking r -> (player -> r) -> AffectedPlayers.AffectedPlayers player -> r
affectedPlayersNames _asking onPlayer x = case x of
  AffectedPlayers.Scoped _playerScope -> mempty
  AffectedPlayers.Named player -> onPlayer player

aimedAtNames :: (Monoid r) => Asking r -> AimedAt.AimedAt -> r
aimedAtNames asking x = case x of
  AimedAt.MkAimedAt defenders _kinds -> aimedPlayersNames asking defenders

aimedPlayersNames :: (Monoid r) => Asking r -> AimedPlayers.AimedPlayers -> r
aimedPlayersNames asking x = case x of
  AimedPlayers.Scoped _playerScope -> mempty
  AimedPlayers.EachInSlot slotName -> slotNames asking slotName
  AimedPlayers.BoundPlayer _playerId -> mempty

alternativeCostNames :: (Monoid r) => Asking r -> AlternativeCost.AlternativeCost -> r
alternativeCostNames asking = Const.getConst . Children.alternativeCostChildren (abilityVisitors asking (const mempty) (const mempty) (const mempty))

onsetNames :: (Monoid r) => Asking r -> Onset.Onset -> r
onsetNames asking x = case x of
  Onset.Immediately -> mempty
  Onset.FromNextTurnOf playerRef -> foldMap (playerRefNames asking) playerRef
  Onset.FromThatExtraTurn -> mempty

asCopyNames :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> AsCopy.AsCopy ability effect -> r
asCopyNames asking onAbility onEffect = Const.getConst . Children.asCopyChildren (copyVisitors asking onAbility onEffect)

attachRestrictionNames :: (Monoid r) => Asking r -> AttachRestriction.AttachRestriction -> r
attachRestrictionNames asking = Const.getConst . Children.attachRestrictionChildren (ruleVisitors asking)

attackCostNames :: (Monoid r) => Asking r -> AttackCost.AttackCost -> r
attackCostNames asking = Const.getConst . Children.attackCostChildren (ruleVisitors asking)

rulePermissionNames :: (Monoid r) => Asking r -> RulePermission.RulePermission -> r
rulePermissionNames asking = Const.getConst . Children.rulePermissionChildren (ruleVisitors asking)

attackRequirementNames :: (Monoid r) => Asking r -> AttackRequirement.AttackRequirement -> r
attackRequirementNames asking = Const.getConst . Children.attackRequirementChildren (ruleVisitors asking)

attackingPlayersNames :: Asking r -> AttackingPlayers.AttackingPlayers -> r
attackingPlayersNames asking x = case x of
  AttackingPlayers.MkAttackingPlayers _relation attacked -> slotNames asking attacked

bindingNames :: (Monoid r) => Asking r -> Binding.Binding -> r
bindingNames asking x = case x of
  Binding.MkBinding targets _amount _modes copy objects _sticker _votes division perMember -> foldMap (foldMap (recipientNames asking)) targets <> foldMap (projectedCharacteristicsNames asking) copy <> names asking (\oid -> any (elem oid) objects) <> foldMap (foldMap (recipientNames asking) . Map.keys) division <> foldMap (foldMap (recipientNames asking) . Map.keys) perMember

blockCostNames :: (Monoid r) => Asking r -> BlockCost.BlockCost -> r
blockCostNames asking = Const.getConst . Children.blockCostChildren (ruleVisitors asking)

blockPermissionNames :: (Monoid r) => Asking r -> BlockPermission.BlockPermission -> r
blockPermissionNames asking = Const.getConst . Children.blockPermissionChildren (ruleVisitors asking)

blockRequirementNames :: (Monoid r) => Asking r -> BlockRequirement.BlockRequirement -> r
blockRequirementNames asking = Const.getConst . Children.blockRequirementChildren (ruleVisitors asking)

boundMeasureNames :: Asking r -> BoundMeasure.BoundMeasure -> r
boundMeasureNames asking x = case x of
  BoundMeasure.MkBoundMeasure slot _measure -> slotNames asking slot

candidateIdNames :: (Monoid r) => Asking r -> CandidateId.CandidateId -> r
candidateIdNames asking x = case x of
  CandidateId.OfPermanent permanentCandidate -> permanentCandidateNames asking permanentCandidate
  CandidateId.OfFloating floatingCandidate -> floatingCandidateNames asking floatingCandidate

-- A printed card, every face of it, and the cards its faces create.
cardNames :: (Monoid r) => Asking r -> Card.Card -> r
cardNames asking = Const.getConst . Children.cardChildren (Const.Const . faceNames asking (cardNames asking))

spendManaAsThoughNames :: (Monoid r) => Asking r -> SpendManaAsThough.SpendManaAsThough -> r
spendManaAsThoughNames asking = Const.getConst . Children.spendManaAsThoughChildren (manaVisitors asking)

castingPermissionNames :: (Monoid r) => Asking r -> CastingPermission.CastingPermission -> r
castingPermissionNames asking = Const.getConst . Children.castingPermissionChildren (permissionVisitors asking)

counterabilityNames :: (Monoid r) => Asking r -> Counterability.Counterability -> r
counterabilityNames asking x = case x of
  Counterability.Counterable -> mempty
  Counterability.CantBeCountered condition -> foldMap (conditionNames asking) condition

castingRestrictionNames :: (Monoid r) => Asking r -> CastingRestriction.CastingRestriction -> r
castingRestrictionNames asking x = case x of
  CastingRestriction.DuringPhase duringPhase -> duringPhaseNames asking duringPhase
  CastingRestriction.AttackedThisStep -> mempty
  CastingRestriction.AfterBlockersDeclared -> mempty
  CastingRestriction.BeforeStep _phase -> history asking HistoryRead.StepBegins
  CastingRestriction.DuringCombatBefore _step -> mempty
  CastingRestriction.DuringTurn turnScope -> turnScopeNames asking turnScope
  CastingRestriction.NotDuringFirstTurns _turns -> mempty
  CastingRestriction.OnlyIf condition -> conditionNames asking condition
  CastingRestriction.SpendOnlyOnX _color -> mempty

characteristicPTNames :: (Monoid r) => Asking r -> CharacteristicPT.CharacteristicPT -> r
characteristicPTNames asking = Const.getConst . Children.characteristicPTChildren (Const.Const . quantityNames asking)

chooserNames :: (Monoid r) => Asking r -> Chooser.Chooser -> r
chooserNames asking x = case x of
  Chooser.TheController -> mempty
  Chooser.EachInScope -> mempty
  Chooser.BoundInSlot slotName -> slotNames asking slotName

chosenPermanentsNames :: (Monoid r) => Asking r -> ChosenPermanents.ChosenPermanents -> r
chosenPermanentsNames asking = Const.getConst . Children.chosenPermanentsChildren (objectRefVisitors asking)

-- Parun.Engine.Children's traversal at Const. FromAnywhere's read of the zone
-- moves (CR 400.7j) is refLooksBack.
objectRefVisitors :: (Monoid r) => Asking r -> Children.ObjectRefChildren (Const.Const r)
objectRefVisitors asking =
  Children.MkObjectRefChildren
    { Children.refFilter = Const.Const . filterNames asking,
      Children.refQuantity = Const.Const . quantityNames asking,
      Children.refSlot = Const.Const . slotNames asking,
      Children.refPlayer = Const.Const . playerRefNames asking,
      Children.refZones = Const.Const . zoneScopeNames asking,
      Children.refChooser = Const.Const . chooserNames asking,
      Children.refNested = Const.Const . objectRefNames asking,
      Children.refLooksBack = Const.Const . history asking
    }

combatRestrictionNames :: (Monoid r) => Asking r -> CombatRestriction.CombatRestriction -> r
combatRestrictionNames asking = Const.getConst . Children.combatRestrictionChildren (ruleVisitors asking)

-- Parun.Engine.Children's traversal at Const. No rule-affecting row reads the
-- history itself; those reads sit in its Conditions and Quantities.
ruleVisitors :: (Monoid r) => Asking r -> Children.RuleChildren (Const.Const r)
ruleVisitors asking =
  Children.MkRuleChildren
    { Children.ruleAffected = Const.Const . affectedNames asking,
      Children.ruleFilter = Const.Const . filterNames asking,
      Children.ruleCondition = Const.Const . conditionNames asking,
      Children.ruleQuantity = Const.Const . quantityNames asking,
      Children.ruleCost = Const.Const . costNames asking (keywordNames asking),
      Children.ruleCounterKind = Const.Const . counterKindNames asking (keywordNames asking)
    }

-- Parun.Engine.Children's traversal at Const. No condition reads the history
-- itself; those reads sit in its Quantities.
conditionNames :: (Monoid r) => Asking r -> Condition.Condition -> r
conditionNames asking =
  Const.getConst
    . Children.conditionChildren
      Children.MkConditionChildren
        { Children.conditionQuantity = Const.Const . quantityNames asking,
          Children.conditionNested = Const.Const . conditionNames asking,
          Children.conditionTurn = Const.Const . turnScopeNames asking
        }

controllerRelationNames :: (Monoid r) => Asking r -> ControllerRelation.ControllerRelation -> r
controllerRelationNames asking x = case x of
  ControllerRelation.Related _ -> mempty
  ControllerRelation.EnchantedPlayers -> mempty
  ControllerRelation.InSlot slotName -> slotNames asking slotName
  ControllerRelation.Among _playerId -> mempty

copyExceptionNames :: (Monoid r) => Asking r -> (ability -> r) -> CopyException.CopyException ability -> r
copyExceptionNames asking onAbility = Const.getConst . Children.copyExceptionChildren (copyVisitors asking onAbility (const mempty))

-- Parun.Engine.Children's traversal at Const. A subtype, a colour and a type
-- line name no object and read no history.
copyVisitors :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> Children.CopyChildren ability effect (Const.Const r)
copyVisitors asking onAbility onEffect =
  Children.MkCopyChildren
    { Children.copyFilter = Const.Const . filterNames asking,
      Children.copyQuantity = Const.Const . quantityNames asking,
      Children.copyCounterKind = Const.Const . counterKindNames asking (keywordNames asking),
      Children.copyKeyword = Const.Const . keywordNames asking,
      Children.copySubtype = pure,
      Children.copyColor = pure,
      Children.copyAbility = Const.Const . onAbility,
      Children.copyEffect = Const.Const . onEffect,
      Children.copyCondition = Const.Const . conditionNames asking,
      Children.copyDuration = Const.Const . durationNames asking,
      Children.copySlot = Const.Const . slotNames asking,
      Children.copyPlayer = Const.Const . playerRefNames asking,
      Children.copyModification = Const.Const . modificationNames asking onAbility,
      Children.copyTypeLine = pure,
      Children.copyPower = Const.Const . powerNames asking,
      Children.copyToughness = Const.Const . toughnessNames asking,
      Children.copyListedKeyword = Const.Const . keywordNames asking
    }

costNames :: (Monoid r) => Asking r -> (keyword -> r) -> Cost.Cost keyword -> r
costNames asking onKeyword = Const.getConst . Children.costChildren (costVisitors asking onKeyword)

costBasisNames :: Asking r -> CostBasis.CostBasis -> r
costBasisNames asking x = case x of
  CostBasis.MkCostBasis slot _reducedBy -> slotNames asking slot

costChoiceNames :: (Monoid r) => Asking r -> CostChoice.CostChoice -> r
costChoiceNames asking x = case x of
  CostChoice.MkCostChoice unwrap -> foldMap (costNames asking (keywordNames asking)) unwrap

costComponentNames :: (Monoid r) => Asking r -> (keyword -> r) -> CostComponent.CostComponent keyword -> r
costComponentNames asking onKeyword = Const.getConst . Children.costComponentChildren (costVisitors asking onKeyword)

-- The visitors costNames and costComponentNames share.
costVisitors :: (Monoid r) => Asking r -> (keyword -> r) -> Children.CostChildren keyword (Const.Const r)
costVisitors asking onKeyword =
  Children.MkCostChildren
    { Children.costFilter = Const.Const . filterNames asking,
      Children.costCounterKind = Const.Const . counterKindNames asking onKeyword,
      Children.costSlot = Const.Const . slotNames asking,
      -- A colour of mana symbols names no object and reads no history.
      Children.costColor = const (Const.Const mempty),
      Children.costLooksBack = Const.Const . history asking
    }

costReductionNames :: (Monoid r) => Asking r -> CostReduction.CostReduction -> r
costReductionNames asking = Const.getConst . Children.costReductionChildren (abilityVisitors asking (const mempty) (const mempty) (const mempty))

counterKindNames :: (Monoid r) => Asking r -> (keyword -> r) -> CounterKind.CounterKind keyword -> r
counterKindNames _asking onKeyword x = case x of
  CounterKind.PlusOnePlusOne -> mempty
  CounterKind.MinusOneMinusOne -> mempty
  CounterKind.Keyword keyword -> onKeyword keyword
  CounterKind.Loyalty -> mempty
  CounterKind.Lore -> mempty
  CounterKind.Defense -> mempty
  CounterKind.Time -> mempty
  CounterKind.Fade -> mempty
  CounterKind.Age -> mempty
  CounterKind.Shield -> mempty
  CounterKind.Finality -> mempty
  CounterKind.Stun -> mempty
  CounterKind.Level -> mempty
  CounterKind.Hone -> mempty
  CounterKind.UnsafePowerToughness _ _ -> mempty
  CounterKind.Named _counterName -> mempty

counterRestrictionNames :: (Monoid r) => Asking r -> CounterRestriction.CounterRestriction -> r
counterRestrictionNames asking = Const.getConst . Children.counterRestrictionChildren (ruleVisitors asking)

counterLimitNames :: (Monoid r) => Asking r -> CounterLimit.CounterLimit -> r
counterLimitNames asking = Const.getConst . Children.counterLimitChildren (ruleVisitors asking)

commanderDeckRuleNames :: (Monoid r) => Asking r -> CommanderDeckRule.CommanderDeckRule -> r
commanderDeckRuleNames asking x = case x of
  CommanderDeckRule.NoMaximumDeckSize -> mempty
  CommanderDeckRule.AnyColorIdentity matching -> filterNames asking matching
  CommanderDeckRule.ChosenExtraColor matching -> filterNames asking matching

keywordCostRestrictionNames :: (Monoid r) => Asking r -> KeywordCostRestriction.KeywordCostRestriction -> r
keywordCostRestrictionNames asking = Const.getConst . Children.keywordCostRestrictionChildren (ruleVisitors asking)

targetRestrictionNames :: (Monoid r) => Asking r -> TargetRestriction.TargetRestriction Affected.Affected -> r
targetRestrictionNames asking = Const.getConst . Children.targetRestrictionChildren (ruleVisitors asking)

-- Parun.Engine.Children's traversal at Const.
damagePatternNames :: (Monoid r) => Asking r -> DamagePattern.DamagePattern -> r
damagePatternNames asking =
  Const.getConst
    . Children.damagePatternChildren
      Children.MkDamagePatternChildren
        { Children.damageFilter = Const.Const . filterNames asking,
          Children.damageRecipient = Const.Const . recipientNames asking,
          Children.damageObject = \oid -> Const.Const (names asking (== oid)),
          Children.damageSlot = Const.Const . slotNames asking
        }

activeLaterPaymentNames :: (Monoid r) => Asking r -> ActiveLaterPayment.ActiveLaterPayment -> r
activeLaterPaymentNames asking = Const.getConst . Children.activeLaterPaymentChildren (storedVisitors asking)

delayedTriggerNames :: (Monoid r) => Asking r -> DelayedTrigger.DelayedTrigger -> r
delayedTriggerNames asking = Const.getConst . Children.delayedTriggerChildren (storedVisitors asking)

dungeonRoomNames :: (Monoid r) => Asking r -> (card -> r) -> DungeonRoom.DungeonRoom card -> r
dungeonRoomNames asking onCard x = case x of
  DungeonRoom.MkDungeonRoom _name ability _exits -> modalNames asking onCard (grantedAbilityNames asking onCard) ability

-- Parun.Engine.Children's traversal at Const.
durationNames :: (Monoid r) => Asking r -> Duration.Duration -> r
durationNames asking = Const.getConst . Children.durationChildren (durationVisitors asking)

-- The event that ends a duration, through the same visitors: Expiry.OnEvent
-- holds one too.
durationEventNames :: (Monoid r) => Asking r -> DurationEvent.DurationEvent -> r
durationEventNames asking = Const.getConst . Children.durationEventChildren (durationVisitors asking)

durationVisitors :: (Monoid r) => Asking r -> Children.DurationChildren (Const.Const r)
durationVisitors asking =
  Children.MkDurationChildren
    { Children.durationPlayer = Const.Const . playerRefNames asking,
      Children.durationCondition = Const.Const . conditionNames asking,
      Children.durationCost = Const.Const . costNames asking (keywordNames asking),
      Children.durationSlot = Const.Const . slotNames asking,
      Children.durationFilter = Const.Const . filterNames asking,
      Children.durationObject = \oid -> Const.Const (names asking (== oid))
    }

-- Parun.Engine.Children's traversal at Const: every child an effect holds,
-- each through its own walk. No effect constructor reads the history or
-- prevention itself; those reads sit in the children (a Quantity, an
-- ObjectRef, a keyword).
effectNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> Effect.Effect card ability -> r
effectNames asking onCard onAbility =
  Const.getConst
    . Children.effectChildren
      Children.MkEffectChildren
        { Children.onFilter = Const.Const . filterNames asking,
          Children.onQuantity = Const.Const . quantityNames asking,
          Children.onEffect = Const.Const . effectNames asking onCard onAbility,
          Children.onObjectRef = Const.Const . objectRefNames asking,
          Children.onPlayer = Const.Const . playerRefNames asking,
          Children.onSlot = Const.Const . slotNames asking,
          Children.onDuration = Const.Const . durationNames asking,
          Children.onCondition = Const.Const . conditionNames asking,
          Children.onCard = Const.Const . onCard,
          Children.onConjured = Const.Const . onCard,
          Children.onMelded = Const.Const . melded asking . onCard,
          Children.onAbility = Const.Const . onAbility,
          Children.onCost = Const.Const . costNames asking (keywordNames asking),
          Children.onKeyword = Const.Const . keywordNames asking,
          Children.onCounterKind = Const.Const . counterKindNames asking (keywordNames asking),
          Children.onWhichCounters = Const.Const . whichCountersNames asking (keywordNames asking),
          Children.onMovedKinds = Const.Const . movedKindsNames asking,
          Children.onRemovalCount = Const.Const . removalCountNames asking,
          Children.onModification = Const.Const . modificationNames asking onAbility,
          Children.onPlayerEffect = Const.Const . playerEffectNames asking,
          Children.onReplacement = Const.Const . replacementEffectNames asking onCard onAbility (effectNames asking onCard onAbility),
          Children.onPayGate = Const.Const . payGateNames asking,
          Children.onEntryRiders = Const.Const . entryRidersNames asking onAbility,
          Children.onPlacement = Const.Const . libraryPlacementNames asking,
          Children.onCopyException = Const.Const . copyExceptionNames asking onAbility,
          Children.onFaceDown = Const.Const . faceDownCharacteristicsNames asking onAbility,
          Children.onManaAddition = Const.Const . manaAdditionNames asking,
          Children.onAffectedPlayers = Const.Const . affectedPlayersNames asking (slotNames asking),
          Children.onOnset = Const.Const . onsetNames asking,
          Children.onMoveDuration = Const.Const . moveDurationNames asking,
          Children.onAimedAt = Const.Const . aimedAtNames asking,
          Children.onChosenPermanents = Const.Const . chosenPermanentsNames asking,
          -- A subtype names no object and reads no history.
          Children.onSubtype = const (Const.Const mempty)
        }

entryRestrictionNames :: (Monoid r) => Asking r -> EntryRestriction.EntryRestriction -> r
entryRestrictionNames asking = Const.getConst . Children.entryRestrictionChildren (ruleVisitors asking)

entryRidersNames :: (Monoid r) => Asking r -> (ability -> r) -> EntryRiders.EntryRiders Quantity.Quantity ability -> r
entryRidersNames asking onAbility = Const.getConst . Children.entryRidersChildren (copyVisitors asking onAbility (const mempty))

expiryNames :: (Monoid r) => Asking r -> Expiry.Expiry -> r
expiryNames asking = Const.getConst . Children.expiryChildren (storedVisitors asking)

-- What a stored row's children answer: its effects and ability through their
-- own walks, a slot through slotNames, an object id outright.
storedVisitors :: (Monoid r) => Asking r -> Children.StoredChildren (Const.Const r)
storedVisitors asking =
  Children.MkStoredChildren
    { Children.storedReplacement = Const.Const . replacementEffectNames asking (const mempty) (grantedAbilityNames asking (const mempty)) (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty))),
      Children.storedTriggered = Const.Const . triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty)),
      Children.storedEffect = Const.Const . effectNames asking (const mempty) (grantedAbilityNames asking (const mempty)),
      Children.storedCost = Const.Const . costNames asking (keywordNames asking),
      Children.storedCondition = Const.Const . conditionNames asking,
      Children.storedDurationEvent = Const.Const . durationEventNames asking,
      Children.storedBinding = Const.Const . bindingNames asking,
      Children.storedRecipient = Const.Const . recipientNames asking,
      Children.storedCandidate = Const.Const . candidateIdNames asking,
      Children.storedSlot = Const.Const . slotNames asking,
      Children.storedObject = \oid -> Const.Const (names asking (== oid))
    }

eventShapeNames :: Asking r -> EventShape.EventShape -> r
eventShapeNames asking x = case x of
  EventShape.MovedBetween _movedBetween -> history asking HistoryRead.Moves
  EventShape.MovedFrom _zone -> history asking HistoryRead.Moves
  EventShape.CardArrivedIn _cardArrivedIn -> history asking HistoryRead.Moves
  EventShape.SpellCast _castWindow -> history asking HistoryRead.Casts
  EventShape.AttackerDeclared -> history asking HistoryRead.Attacks
  EventShape.CountersPut -> history asking HistoryRead.CounterPlacements
  EventShape.Sacrificed -> history asking HistoryRead.Sacrifices

faceNames :: (Monoid r) => Asking r -> (card -> r) -> Face.Face card -> r
faceNames asking onCard =
  Const.getConst
    . Children.faceChildren
      Children.MkFaceChildren
        { Children.facePower = Const.Const . powerNames asking,
          Children.faceToughness = Const.Const . toughnessNames asking,
          Children.faceCommanderDeckRule = Const.Const . commanderDeckRuleNames asking,
          Children.facePregameExile = Const.Const . filterNames asking . PregameExile.filter,
          Children.faceKeyword = Const.Const . keywordNames asking,
          Children.faceColor = const (Const.Const mempty),
          Children.faceCharacteristicPT = Const.Const . characteristicPTNames asking,
          Children.faceStatic = Const.Const . staticAbilityNames asking (grantedAbilityNames asking onCard),
          Children.faceModal = Const.Const . modalNames asking onCard (grantedAbilityNames asking onCard),
          Children.faceActivated = Const.Const . activatedAbilityNames asking onCard (grantedAbilityNames asking onCard),
          Children.faceReplacement = Const.Const . printedReplacementNames asking onCard (grantedAbilityNames asking onCard) (effectNames asking onCard (grantedAbilityNames asking onCard)),
          Children.faceTriggered = Const.Const . triggeredAbilityNames asking onCard (grantedAbilityNames asking onCard),
          Children.faceRoom = Const.Const . dungeonRoomNames asking onCard,
          Children.faceCastingPermission = Const.Const . castingPermissionNames asking,
          Children.faceCastingRestriction = Const.Const . castingRestrictionNames asking,
          Children.faceTargetSlot = Const.Const . targetSlotNames asking,
          Children.faceCounterability = Const.Const . counterabilityNames asking,
          Children.faceCostComponent = Const.Const . costComponentNames asking (keywordNames asking),
          Children.faceCostChoice = Const.Const . costChoiceNames asking,
          Children.faceCost = Const.Const . costNames asking (keywordNames asking),
          Children.faceQuantity = Const.Const . quantityNames asking,
          Children.faceAlternativeCost = Const.Const . alternativeCostNames asking,
          Children.faceCostReduction = Const.Const . costReductionNames asking,
          Children.facePlayerStatic = Const.Const . playerStaticAbilityNames asking,
          Children.faceBlockRequirement = Const.Const . blockRequirementNames asking,
          Children.faceBlockPermission = Const.Const . blockPermissionNames asking,
          Children.faceAttackRequirement = Const.Const . attackRequirementNames asking,
          Children.faceCombatRestriction = Const.Const . combatRestrictionNames asking,
          Children.faceSacrificeRestriction = Const.Const . sacrificeRestrictionNames asking,
          Children.faceUntapRestriction = Const.Const . untapRestrictionNames asking,
          Children.faceKeywordCostRestriction = Const.Const . keywordCostRestrictionNames asking,
          Children.faceTargetRestriction = Const.Const . targetRestrictionNames asking,
          Children.faceRulePermission = Const.Const . rulePermissionNames asking,
          Children.faceAttachRestriction = Const.Const . attachRestrictionNames asking,
          Children.faceEntryRestriction = Const.Const . entryRestrictionNames asking,
          Children.faceCounterRestriction = Const.Const . counterRestrictionNames asking,
          Children.faceActivationProhibition = Const.Const . activationProhibitionNames asking,
          Children.faceAttackCost = Const.Const . attackCostNames asking,
          Children.faceBlockCost = Const.Const . blockCostNames asking,
          Children.faceCounterLimit = Const.Const . counterLimitNames asking,
          Children.faceHandAction = Const.Const . handActionNames asking onCard,
          Children.faceSpecialAction = Const.Const . specialActionNames asking
        }

faceDownCharacteristicsNames :: (Monoid r) => Asking r -> (ability -> r) -> FaceDownCharacteristics.FaceDownCharacteristics ability -> r
faceDownCharacteristicsNames asking onAbility = Const.getConst . Children.faceDownChildren (copyVisitors asking onAbility (const mempty))

floatingCandidateNames :: Asking r -> FloatingCandidate.FloatingCandidate -> r
floatingCandidateNames asking x = case x of
  FloatingCandidate.MkFloatingCandidate source _timestamp -> names asking (== source)

-- A granted ability's slots are its OWN: each read resolves against the
-- bindings of the stack object it becomes or of the object it is granted to,
-- which Parun.Engine.Interchangeable.namedByAnother searches and whole-object
-- Eq compares. So inside one, every slot read is Searched, whatever the row
-- granting it stores.
grantedAbilityNames :: (Monoid r) => Asking r -> (card -> r) -> GrantedAbility.GrantedAbility card -> r
grantedAbilityNames asking onCard =
  let own = asking {context = mempty}
      onAbility = grantedAbilityNames own onCard
   in Const.getConst . Children.grantedAbilityChildren (abilityVisitors own onCard onAbility (effectNames own onCard onAbility))

-- An ability record's children, each through its own walk; card, ability and
-- effect are answered by the functions the caller passes for them. The history
-- reads sit in the children (a trigger's "only once" rider, a restriction's
-- step), so no record answers one itself.
abilityVisitors :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> (effect -> r) -> Children.AbilityChildren card ability effect (Const.Const r)
abilityVisitors asking onCard onAbility onEffect =
  Children.MkAbilityChildren
    { Children.abilityCost = Const.Const . costNames asking (keywordNames asking),
      Children.abilityQuantity = Const.Const . quantityNames asking,
      Children.abilityModal = Const.Const . modalNames asking onCard onAbility,
      Children.abilityRestriction = Const.Const . activationRestrictionNames asking,
      Children.abilityCondition = Const.Const . conditionNames asking,
      Children.abilityKeyword = Const.Const . keywordNames asking,
      Children.abilityTrigger = Const.Const . triggerConditionNames asking,
      Children.abilityLimit = Const.Const . triggerLimitNames asking,
      Children.abilityAffected = Const.Const . affectedNames asking,
      Children.abilityDuration = Const.Const . durationNames asking,
      Children.abilityModification = Const.Const . modificationNames asking onAbility,
      Children.abilityPlayerEffect = Const.Const . playerEffectNames asking,
      Children.abilityReplacement = Const.Const . replacementEffectNames asking onCard onAbility onEffect,
      Children.abilityFilter = Const.Const . filterNames asking,
      Children.abilityRules = Const.Const . ruleAbilitiesNames asking,
      Children.abilitySpendMana = Const.Const . spendManaAsThoughNames asking
    }

handActionNames :: (Monoid r) => Asking r -> (card -> r) -> HandAction.HandAction card -> r
handActionNames asking onCard x = case x of
  HandAction.MkHandAction condition effects -> foldMap (conditionNames asking) condition <> foldMap (effectNames asking onCard (grantedAbilityNames asking onCard)) effects

inZoneNames :: (Monoid r) => Asking r -> InZone.InZone -> r
inZoneNames asking x = case x of
  InZone.MkInZone _zone player -> playerRefNames asking player

-- A keyword through its children, each through its own walk. Its history
-- reads are the events Children.keywordChildren says the engine looks back
-- over to expand or pay it.
keywordNames :: (Monoid r) => Asking r -> Keyword.Keyword -> r
keywordNames asking =
  Const.getConst
    . Children.keywordChildren
      Children.MkKeywordChildren
        { Children.keywordFilter = Const.Const . filterNames asking,
          Children.keywordCost = Const.Const . costNames asking (keywordNames asking),
          Children.keywordNested = Const.Const . keywordNames asking,
          -- A gift token's creature type names no object and reads no history.
          Children.keywordSubtype = const (Const.Const mempty),
          Children.keywordScope = Const.Const . scopeNames asking,
          Children.keywordPlayer = Const.Const . playerRefNames asking,
          Children.keywordLooksBack = Const.Const . history asking
        }

keywordDesignatorNames :: (Monoid r) => Asking r -> KeywordDesignator.KeywordDesignator Keyword.Keyword -> r
keywordDesignatorNames asking x = case x of
  KeywordDesignator.OfFamily _keywordFamily -> mempty
  KeywordDesignator.OfKeyword keyword -> keywordNames asking keyword
  KeywordDesignator.Printed _keywordFamily -> mempty
  KeywordDesignator.OfInstance keyword _ordinal -> keywordNames asking keyword

libraryPlacementNames :: (Monoid r) => Asking r -> LibraryPlacement.LibraryPlacement -> r
libraryPlacementNames asking = Const.getConst . Children.libraryPlacementChildren (countVisitors asking)

manaAdditionNames :: (Monoid r) => Asking r -> ManaAddition.ManaAddition -> r
manaAdditionNames asking = Const.getConst . Children.manaAdditionChildren (manaVisitors asking)

manaRestrictionNames :: (Monoid r) => Asking r -> ManaRestriction.ManaRestriction -> r
manaRestrictionNames asking = Const.getConst . Children.manaRestrictionChildren (manaVisitors asking)

-- Parun.Engine.Children's traversal at Const. Mana's type names no object and
-- reads no history.
manaVisitors :: (Monoid r) => Asking r -> Children.ManaChildren (Const.Const r)
manaVisitors asking =
  Children.manaLeaves
    { Children.manaCriterion = Const.Const . filterNames asking,
      Children.manaQuantity = Const.Const . quantityNames asking,
      Children.manaPlayer = Const.Const . playerRefNames asking,
      Children.manaSlot = Const.Const . slotNames asking,
      Children.manaDesignator = Const.Const . keywordDesignatorNames asking,
      Children.manaKeyword = Const.Const . keywordNames asking,
      Children.manaDuration = Const.Const . durationNames asking
    }

measuresNames :: (Monoid r) => Asking r -> Measures.Measures -> r
measuresNames asking x = case x of
  Measures.MkMeasures _measure _comparison operand -> operandNames asking operand

-- Parun.Engine.Children's traversal at Const.
modalNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> Modal.Modal card ability -> r
modalNames asking onCard onAbility =
  Const.getConst
    . Children.modalChildren
      Children.MkModalChildren
        { Children.modalInstead = Const.Const . conditionNames asking,
          Children.modalQuantity = Const.Const . quantityNames asking,
          Children.modalCondition = Const.Const . conditionNames asking,
          Children.modalPlayer = Const.Const . playerRefNames asking,
          Children.modalPayGate = Const.Const . payGateNames asking,
          Children.modalEffect = Const.Const . effectNames asking onCard onAbility,
          Children.modalSlot = Const.Const . slotNames asking,
          Children.modalTargetSlot = Const.Const . targetSlotNames asking
        }

-- Parun.Engine.Children's traversal at Const. A lost ability's name names the
-- objects that acquired it; a subtype or colour names none.
modificationNames :: (Monoid r) => Asking r -> (ability -> r) -> Modification.Modification ability -> r
modificationNames asking onAbility =
  Const.getConst
    . Children.modificationChildren
      Children.MkModificationChildren
        { Children.modificationKeyword = Const.Const . keywordNames asking,
          Children.modificationStampedKeyword = Const.Const . keywordNames asking,
          Children.modificationTargetSlot = Const.Const . targetSlotNames asking,
          Children.modificationCastingPermission = Const.Const . castingPermissionNames asking,
          Children.modificationAbility = Const.Const . onAbility,
          Children.modificationFilter = Const.Const . filterNames asking,
          Children.modificationRestriction = Const.Const . activationRestrictionNames asking,
          Children.modificationAbilityName = \name -> Const.Const (names asking (`Binding.Engine.isAcquiredFrom` name)),
          Children.modificationQuantity = Const.Const . quantityNames asking,
          Children.modificationPlayer = Const.Const . playerRefNames asking,
          Children.modificationLandSubtype = pure,
          Children.modificationCreatureSubtype = pure,
          Children.modificationColors = pure
        }

moveDurationNames :: (Monoid r) => Asking r -> MoveDuration.MoveDuration -> r
moveDurationNames asking x = case x of
  MoveDuration.UntilSourceLeavesTheBattlefield -> mempty
  MoveDuration.UntilAnOpponentBecomesTheMonarch -> history asking HistoryRead.MonarchChanges

movedKindsNames :: (Monoid r) => Asking r -> MovedKinds.MovedKinds -> r
movedKindsNames asking = Const.getConst . Children.movedKindsChildren (countVisitors asking)

objectRefNames :: (Monoid r) => Asking r -> ObjectRef.ObjectRef -> r
objectRefNames asking = Const.getConst . Children.objectRefChildren (objectRefVisitors asking)

operandNames :: (Monoid r) => Asking r -> Operand.Operand -> r
operandNames asking x = case x of
  Operand.Literal _integer -> mempty
  Operand.OfSource _measure -> mempty
  Operand.Own _measure -> mempty
  Operand.OfBound bound -> boundMeasureNames asking bound
  Operand.AmountInSlot slot -> slotNames asking slot
  Operand.EnclosingAmount -> contextNames asking
  -- The other candidates' numbers: two objects alike in every respect read
  -- each other's, so the answer is the same for both.
  Operand.OfEachOtherCandidate _measure -> mempty
  -- A player's counters, the same for every candidate.
  Operand.YourCounters _kind -> mempty

payGateNames :: (Monoid r) => Asking r -> PayGate.PayGate -> r
payGateNames asking = Const.getConst . Children.payGateChildren (countVisitors asking)

-- Parun.Engine.Children's pay gate, counter move, counter removal and library
-- placement traversal at Const.
countVisitors :: (Monoid r) => Asking r -> Children.CountChildren (Const.Const r)
countVisitors asking =
  Children.MkCountChildren
    { Children.countQuantity = Const.Const . quantityNames asking,
      Children.countPlayer = Const.Const . playerRefNames asking,
      Children.countKind = Const.Const . counterKindNames asking (keywordNames asking),
      Children.countCost = Const.Const . costNames asking (keywordNames asking),
      Children.countBasis = Const.Const . costBasisNames asking
    }

pendingDamageEffectNames :: (Monoid r) => Asking r -> PendingReplacementEffect.PendingReplacementEffect -> r
pendingDamageEffectNames asking = Const.getConst . Children.pendingReplacementEffectChildren (storedVisitors asking)

pendingEntryEffectNames :: (Monoid r) => Asking r -> PendingEntryEffect.PendingEntryEffect -> r
pendingEntryEffectNames asking = Const.getConst . Children.pendingEntryEffectChildren (storedVisitors asking)

permanentCandidateNames :: (Monoid r) => Asking r -> PermanentCandidate.PermanentCandidate -> r
permanentCandidateNames asking x = case x of
  PermanentCandidate.MkPermanentCandidate source _provenance effect _ordinal -> names asking (== source) <> replacementEffectNames asking (const mempty) (grantedAbilityNames asking (const mempty)) (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty))) effect

activationCriteriaNames :: (Monoid r) => Asking r -> ActivationCriteria.ActivationCriteria Keyword.Keyword -> r
activationCriteriaNames asking x = case x of
  ActivationCriteria.MkActivationCriteria grantedBy _whichKind _whichLoyalty _whichAbility -> foldMap (keywordDesignatorNames asking) grantedBy

playerEffectNames :: (Monoid r) => Asking r -> PlayerEffect.PlayerEffect -> r
playerEffectNames asking =
  Const.getConst
    . Children.playerEffectChildren
      Children.MkPlayerEffectChildren
        { Children.playerFilter = Const.Const . filterNames asking,
          Children.playerQuantity = Const.Const . quantityNames asking,
          Children.playerCost = Const.Const . costNames asking (keywordNames asking),
          Children.playerCostComponent = Const.Const . costComponentNames asking (keywordNames asking),
          -- A colour of mana symbols names no object and reads no history.
          Children.playerColor = const (Const.Const mempty),
          Children.playerDesignator = Const.Const . keywordDesignatorNames asking,
          Children.playerActivations = Const.Const . activationCriteriaNames asking,
          Children.playerZone = Const.Const . inZoneNames asking,
          Children.playerTurn = Const.Const . turnScopeNames asking,
          -- Mana's type names no object and reads no history.
          Children.playerManaFilter = const (Const.Const mempty),
          Children.playerSpending = Const.Const . spendManaAsThoughNames asking,
          Children.playerManaRestriction = Const.Const . manaRestrictionNames asking,
          Children.playerDamage = Const.Const . damagePatternNames asking,
          Children.playerKeyword = Const.Const . keywordNames asking,
          Children.playerLooksBack = Const.Const . history asking
        }

playerChoiceNames :: (Monoid r) => Asking r -> PlayerChoice.PlayerChoice -> r
playerChoiceNames asking x = case x of
  PlayerChoice.AsEnters -> mempty
  PlayerChoice.Promised _gift instance_ -> keywordInstanceNames asking instance_

keywordInstanceNames :: (Monoid r) => Asking r -> KeywordInstance.KeywordInstance -> r
keywordInstanceNames asking x = case x of
  KeywordInstance.Own _ordinal -> mempty
  KeywordInstance.Granted objectId _timestamp _ordinal -> names asking (== objectId)

playerRefNames :: (Monoid r) => Asking r -> PlayerRef.PlayerRef -> r
playerRefNames asking x = case x of
  PlayerRef.EachPlayerExcept slotName -> slotNames asking slotName
  PlayerRef.EachOpponentExcept slotName -> slotNames asking slotName
  PlayerRef.Relative _playerRelation -> mempty
  PlayerRef.InSlot slotName -> slotNames asking slotName
  PlayerRef.EachInSlot slotName -> slotNames asking slotName
  PlayerRef.Specific _playerId -> mempty
  PlayerRef.Candidate -> mempty
  PlayerRef.HolderOfBound _holder _hop slotName -> slotNames asking slotName
  PlayerRef.HolderOfObject _holder objectId -> names asking (== objectId)
  PlayerRef.ChosenPlayerOfBound slotName choice -> slotNames asking slotName <> playerChoiceNames asking choice
  PlayerRef.Attacking attackingPlayers -> attackingPlayersNames asking attackingPlayers
  PlayerRef.Superlative _ -> mempty
  PlayerRef.InRole _ -> mempty

playerStaticAbilityNames :: (Monoid r) => Asking r -> PlayerStaticAbility.PlayerStaticAbility -> r
playerStaticAbilityNames asking = Const.getConst . Children.playerStaticAbilityChildren (abilityVisitors asking (const mempty) (const mempty) (const mempty))

powerNames :: (Monoid r) => Asking r -> Power.Power -> r
powerNames asking x = case x of
  Power.MkPower unwrap -> quantityNames asking unwrap

preventionNames :: (Monoid r) => Asking r -> Prevention.Prevention -> r
preventionNames asking = Const.getConst . Children.preventionChildren (storedVisitors asking)

preventionRiderNames :: (Monoid r) => Asking r -> PreventionRider.PreventionRider -> r
preventionRiderNames asking = Const.getConst . Children.preventionRiderChildren (storedVisitors asking)

printedReplacementNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> (effect -> r) -> PrintedReplacement.PrintedReplacement card ability effect -> r
printedReplacementNames asking onCard onAbility onEffect = Const.getConst . Children.printedReplacementChildren (abilityVisitors asking onCard onAbility onEffect)

-- The lender is a name drawn from an object's id (Binding.acquiredFrom), as
-- the lent abilities' own names are, and is read as they are: not at all.
lentKeywordNames :: (Monoid r) => Asking r -> LentKeyword.LentKeyword -> r
lentKeywordNames asking x = case x of
  LentKeyword.MkLentKeyword keyword _instances _lender _triggered restrictions -> keywordNames asking keyword <> foldMap (activationRestrictionNames asking) restrictions

projectedCharacteristicsNames :: (Monoid r) => Asking r -> ProjectedCharacteristics.ProjectedCharacteristics -> r
projectedCharacteristicsNames asking x = case x of
  ProjectedCharacteristics.MkProjectedCharacteristics _names _supertypes keywords _colors _manaCost _manaValue _power _toughness _basePower _baseToughness _loyalty _defense _intensity characteristicPT _cardTypes _subtypes staticAbilities playerAbilities grantedPlayerAbilities grantedStaticAbilities grantedRuleAbilities specialActions activatedAbilities replacementEffects triggeredAbilities delayedAbilities enchant castingPermissions ruleAbilities _lostAllAbilities barredKeywords lentKeywords _hasFullText _wordChanges textChangedKeywords _combatDamageAmount _combatDamageOptions _grantsStationToughness _loyaltyIsToughness additionalCosts additionalCostChoices alternativeCosts costReductions grantedCostReductions grantedAlternativeCosts _grantedSpendManaAsThough _halves exceptions mergedDonors prepare alternativeSpell _prototyped spell counterability _copyability _circled _definesChosenColor flipped -> foldMap (keywordNames asking) (Map.keys keywords) <> foldMap (keywordDesignatorNames asking) barredKeywords <> foldMap (lentKeywordNames asking) lentKeywords <> foldMap (characteristicPTNames asking) characteristicPT <> foldMap (staticAbilityNames asking (grantedAbilityNames asking (const mempty))) staticAbilities <> foldMap (playerStaticAbilityNames asking) playerAbilities <> foldMap (playerStaticAbilityNames asking . snd) grantedPlayerAbilities <> foldMap (staticAbilityNames asking (grantedAbilityNames asking (const mempty)) . snd) grantedStaticAbilities <> ruleAbilitiesNames asking grantedRuleAbilities <> foldMap (specialActionNames asking) specialActions <> foldMap (activatedAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) activatedAbilities <> foldMap (printedReplacementNames asking (const mempty) (grantedAbilityNames asking (const mempty)) (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty)))) replacementEffects <> foldMap (triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) triggeredAbilities <> foldMap (triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) delayedAbilities <> foldMap (targetSlotNames asking) enchant <> foldMap (castingPermissionNames asking) castingPermissions <> ruleAbilitiesNames asking ruleAbilities <> foldMap (keywordNames asking) (Map.keys textChangedKeywords) <> foldMap (costComponentNames asking (keywordNames asking)) additionalCosts <> foldMap (costChoiceNames asking) additionalCostChoices <> foldMap (alternativeCostNames asking) alternativeCosts <> foldMap (costReductionNames asking) costReductions <> foldMap (costReductionNames asking) grantedCostReductions <> foldMap (alternativeCostNames asking) grantedAlternativeCosts <> foldMap (copyExceptionNames asking (grantedAbilityNames asking (const mempty))) exceptions <> foldMap (projectedCharacteristicsNames asking) mergedDonors <> foldMap (faceNames asking (const mempty)) prepare <> foldMap (faceNames asking (const mempty)) alternativeSpell <> modalNames asking (const mempty) (grantedAbilityNames asking (const mempty)) spell <> counterabilityNames asking counterability <> foldMap (projectedCharacteristicsNames asking) flipped

quantityNames :: (Monoid r) => Asking r -> Quantity.Quantity -> r
quantityNames asking x =
  let own = case x of
        Quantity.WasBlockedThisTurn -> history asking HistoryRead.Blocks
        Quantity.TimesTappedThisTurn -> history asking HistoryRead.Taps
        Quantity.Convokers -> history asking HistoryRead.Convokes <> history asking HistoryRead.Moves
        Quantity.AttackersDeclaredThisTurn _ -> history asking HistoryRead.Attacks
        Quantity.AttackersInTheirLastTurn _ -> history asking HistoryRead.Attacks
        -- Draws and land plays from anywhere read player-keyed tallies naming no
        -- object (TurnRegisters.drawsThisTurn, GameState.landsPlayed), so no
        -- history; land plays excluding an origin read the log's.
        Quantity.TalliedThisTurn tally _ -> case tally of
          TurnTally.CardsDiscarded -> history asking HistoryRead.Discards
          TurnTally.CardsDrawn -> mempty
          TurnTally.ActionsDone acts -> history asking (HistoryRead.Acts acts)
          TurnTally.DilemmasFaced -> history asking HistoryRead.Dilemmas
          TurnTally.CrimesCommitted -> history asking HistoryRead.Crimes
          TurnTally.LandsPlayed excluding
            | Set.null excluding -> mempty
            | otherwise -> history asking HistoryRead.LandPlays
        Quantity.LifeChanged tally -> turnHistory asking (LifeTally.turn tally) (case LifeTally.direction tally of LifeDirection.Gained -> HistoryRead.LifeGains; LifeDirection.Lost -> HistoryRead.LifeLosses)
        Quantity.PlayersDealtDamage tally -> turnHistory asking (DamageTally.turn tally) HistoryRead.Damage
        Quantity.DamageDealtToPlayersThisTurn _ -> history asking HistoryRead.Damage
        Quantity.DamageDealtThisTurn _ -> history asking HistoryRead.Damage
        Quantity.SpellsCast tally -> turnHistory asking (SpellTally.turn tally) HistoryRead.Casts
        Quantity.TimesThisTurn act -> history asking $ case act of
          AbilityAct.Activated -> HistoryRead.Activations
          AbilityAct.Resolved -> HistoryRead.Resolutions
        Quantity.ActivationsThisTurn _ -> history asking HistoryRead.Activations
        Quantity.PermanentsDiedThisTurn -> history asking HistoryRead.Deaths
        Quantity.DamagePreventedThisWay -> prevention asking
        Quantity.SpellsCastUsingThisTurn _ -> history asking HistoryRead.Casts
        Quantity.PhasesBegunThisTurn _ -> history asking HistoryRead.StepBegins
        Quantity.EnteredThisTurn -> history asking HistoryRead.Entries
        Quantity.EnteredFrom _ -> history asking HistoryRead.Entries
        Quantity.WasCastFrom _ -> history asking HistoryRead.Entries <> history asking HistoryRead.Casts
        _ -> mempty
   in own
        <> Const.getConst
          ( Children.quantityChildren
              (Children.descendingQuantities (Const.Const . quantityNames asking))
                { Children.nest = Const.Const . filterNames asking,
                  Children.keyword = Const.Const . keywordNames asking,
                  Children.slot = \_arity slot -> Const.Const (slotNames asking slot),
                  Children.scope = Const.Const . scopeNames asking,
                  Children.player = Const.Const . playerRefNames asking
                }
              x
          )

recipientNames :: (Monoid r) => Asking r -> Recipient.Recipient -> r
recipientNames asking x = case x of
  Recipient.ToCreature objectId -> names asking (== objectId)
  Recipient.ToPlaneswalker objectId -> names asking (== objectId)
  Recipient.ToBattle objectId -> names asking (== objectId)
  Recipient.ToPlayer _playerId -> mempty
  Recipient.ToObject objectId -> names asking (== objectId)
  Recipient.ToPile _pile -> mempty

removalCountNames :: (Monoid r) => Asking r -> RemovalCount.RemovalCount -> r
removalCountNames asking = Const.getConst . Children.removalCountChildren (countVisitors asking)

-- A replacement effect through its children, each through its own walk. The
-- history reads sit in the children (a draw-step exception, bloodthirst).
replacementEffectNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> (effect -> r) -> ReplacementEffect.ReplacementEffect card ability effect -> r
replacementEffectNames asking onCard onAbility onEffect =
  Const.getConst
    . Children.replacementEffectChildren
      Children.MkReplacementChildren
        { Children.replacementFilter = Const.Const . filterNames asking,
          Children.replacementQuantity = Const.Const . quantityNames asking,
          Children.replacementEffect = Const.Const . onEffect,
          Children.replacementAbility = Const.Const . onAbility,
          Children.replacementCard = Const.Const . onCard,
          Children.replacementKeyword = Const.Const . keywordNames asking,
          -- A subtype names no object and reads no history.
          Children.replacementSubtype = const (Const.Const mempty),
          Children.replacementCounterKind = Const.Const . counterKindNames asking (keywordNames asking),
          Children.replacementProcedureCost = Const.Const . costNames asking (keywordNames asking),
          Children.replacementWhose = Const.Const . controllerRelationNames asking,
          Children.replacementRecipient = Const.Const . recipientNames asking,
          Children.replacementDamage = Const.Const . damagePatternNames asking,
          Children.replacementCounters = Const.Const . withCountersNames asking,
          Children.replacementCopy = Const.Const . asCopyNames asking onAbility onEffect,
          Children.replacementLooksBack = Const.Const . history asking
        }

ruleAbilitiesNames :: (Monoid r) => Asking r -> RuleAbilities.RuleAbilities -> r
ruleAbilitiesNames asking = Const.getConst . Children.ruleAbilitiesChildren (ruleVisitors asking)

sacrificeRestrictionNames :: (Monoid r) => Asking r -> SacrificeRestriction.SacrificeRestriction -> r
sacrificeRestrictionNames asking = Const.getConst . Children.sacrificeRestrictionChildren (ruleVisitors asking)

scopeNames :: (Monoid r) => Asking r -> Scope.Scope -> r
scopeNames asking x = case x of
  Scope.InZone inZone -> inZoneNames asking inZone
  Scope.InHistory eventShape -> eventShapeNames asking eventShape
  Scope.OverPlayers playerRef -> playerRefNames asking playerRef
  Scope.OverBound _ slotName -> slotNames asking slotName
  Scope.InPosition _position inZone -> inZoneNames asking inZone
  Scope.AsTurnBegan -> mempty

specialActionNames :: (Monoid r) => Asking r -> SpecialAction.SpecialAction -> r
specialActionNames asking x = case x of
  SpecialAction.DiscardThisAnyTime -> mempty
  SpecialAction.IgnoreThisUntilEndOfTurn _abilityName cost -> costNames asking (keywordNames asking) cost

-- CR 102.1: a named player's turn reads whatever its reference reads.
turnScopeNames :: (Monoid r) => Asking r -> TurnScope.TurnScope -> r
turnScopeNames asking x = case x of
  TurnScope.PlayersTurn playerRef -> playerRefNames asking playerRef

duringPhaseNames :: (Monoid r) => Asking r -> DuringPhase.DuringPhase -> r
duringPhaseNames asking x = case x of
  DuringPhase.MkDuringPhase _window scope -> turnScopeNames asking scope

staticAbilityNames :: (Monoid r) => Asking r -> (ability -> r) -> StaticAbility.StaticAbility ability -> r
staticAbilityNames asking onAbility = Const.getConst . Children.staticAbilityChildren (abilityVisitors asking (const mempty) onAbility (const mempty))

targetSlotNames :: (Monoid r) => Asking r -> TargetSlot.TargetSlot -> r
targetSlotNames asking = Const.getConst . Children.targetSlotChildren (permissionVisitors asking)

-- Parun.Engine.Children's traversal at Const. "Before this step"'s read of
-- when steps began is permissionLooksBack; a printed colour of mana names
-- nothing.
permissionVisitors :: (Monoid r) => Asking r -> Children.PermissionChildren (Const.Const r)
permissionVisitors asking =
  Children.MkPermissionChildren
    { Children.permissionFilter = Const.Const . filterNames asking,
      Children.permissionQuantity = Const.Const . quantityNames asking,
      Children.permissionPlayer = Const.Const . playerRefNames asking,
      Children.permissionZones = Const.Const . zoneScopeNames asking,
      Children.permissionSlot = Const.Const . slotNames asking,
      Children.permissionCondition = Const.Const . conditionNames asking,
      Children.permissionCost = Const.Const . costComponentNames asking (keywordNames asking),
      Children.permissionTurn = Const.Const . turnScopeNames asking,
      Children.permissionMana = const (Const.Const mempty),
      Children.permissionLooksBack = Const.Const . history asking
    }

toughnessNames :: (Monoid r) => Asking r -> Toughness.Toughness -> r
toughnessNames asking x = case x of
  Toughness.MkToughness unwrap -> quantityNames asking unwrap

-- A trigger condition through its children, each through its own walk. The
-- one condition answering itself reads what its source's prevention stopped
-- (CR 615.13); the history reads sit in the children (an ordinal, a
-- frequency, a turn scope's player).
triggerConditionNames :: (Monoid r) => Asking r -> TriggerCondition.TriggerCondition -> r
triggerConditionNames asking x = case x of
  TriggerCondition.SelfPreventsDamage filter_ -> prevention asking <> filterNames asking filter_
  _ ->
    Const.getConst
      ( Children.triggerConditionChildren
          Children.MkTriggerChildren
            { Children.triggerFilter = Const.Const . filterNames asking,
              Children.triggerNested = Const.Const . triggerConditionNames asking,
              Children.triggerState = Const.Const . conditionNames asking,
              Children.triggerSlot = Const.Const . slotNames asking,
              Children.triggerTurn = Const.Const . turnScopeNames asking,
              Children.triggerOrdinal = \events _ -> Const.Const (history asking events),
              Children.triggerFrequency = \events -> Const.Const . triggerFrequencyNames asking events,
              Children.triggerCounters = Const.Const . whichCountersNames asking (keywordNames asking),
              -- A colour of mana names no object and reads no history.
              Children.triggerMana = const (Const.Const mempty)
            }
          x
      )

-- The look-back a first-time-each-turn trigger makes, over the events its
-- condition names.
triggerFrequencyNames :: (Monoid r) => Asking r -> HistoryRead.HistoryRead -> TriggerFrequency.TriggerFrequency -> r
triggerFrequencyNames asking events x = case x of
  TriggerFrequency.EveryTime -> mempty
  TriggerFrequency.FirstTimeEachTurn -> history asking events

triggerLimitNames :: (Monoid r) => Asking r -> TriggerLimit.TriggerLimit -> r
triggerLimitNames asking x = case x of
  TriggerLimit.Unlimited -> mempty
  TriggerLimit.OncePerTurn SpentOn.Triggering -> history asking HistoryRead.Triggers
  -- TurnRegisters.instructionsTakenThisTurn, which Parun.Engine.Fragmented.digest keeps.
  TriggerLimit.OncePerTurn (SpentOn.TakingInstruction _) -> mempty
  -- History.triggeredThisGame, which Parun.Engine.Fragmented.digest keeps.
  TriggerLimit.OncePerGame -> mempty

triggeredAbilityNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> TriggeredAbility.TriggeredAbility card ability -> r
triggeredAbilityNames asking onCard onAbility = Const.getConst . Children.triggeredAbilityChildren (abilityVisitors asking onCard onAbility (const mempty))

untapRestrictionNames :: (Monoid r) => Asking r -> UntapRestriction.UntapRestriction -> r
untapRestrictionNames asking = Const.getConst . Children.untapRestrictionChildren (ruleVisitors asking)

whichCountersNames :: (Monoid r) => Asking r -> (keyword -> r) -> WhichCounters.WhichCounters keyword -> r
whichCountersNames asking onKeyword x = case x of
  WhichCounters.OfKind counterKind -> counterKindNames asking onKeyword counterKind
  WhichCounters.OfAnyKind -> mempty

withCountersNames :: (Monoid r) => Asking r -> WithCounters.WithCounters -> r
withCountersNames asking = Const.getConst . Children.withCountersChildren (copyVisitors asking (const mempty) (const mempty))

zoneScopeNames :: (Monoid r) => Asking r -> ZoneScope.ZoneScope -> r
zoneScopeNames asking x = case x of
  ZoneScope.Scoped _playerScope -> mempty
  ZoneScope.InSlot slotName -> slotNames asking slotName
  ZoneScope.ControllerOfBound slotName -> slotNames asking slotName
  ZoneScope.BoundPlayer _playerId -> mempty
