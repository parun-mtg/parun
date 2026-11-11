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
-- AlternativeCost, GrantedAbility, and Modal with its Mode, Clause and
-- ModeSelection) is matched there instead, its leaf types discarded there.
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
-- site consults GameState.events or GameState.activationsThisTurn -- and a
-- keyword's keywordLooksBack in Parun.Engine.Children, where the engine
-- EXPANDS the keyword into one or gates its cost on one (Parun.Engine.Keyword,
-- Parun.Engine.Cost.candidateCostsGiven), and a cost component's costLooksBack
-- there likewise.
module Parun.Engine.Interchangeable.Mentions where

import qualified Data.Functor.Const as Const
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
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
import qualified Parun.Types.AffectedUnless as AffectedUnless
import qualified Parun.Types.AfterObjectTurn as AfterObjectTurn
import qualified Parun.Types.AimedAt as AimedAt
import qualified Parun.Types.AimedPlayers as AimedPlayers
import qualified Parun.Types.AloneUnless as AloneUnless
import qualified Parun.Types.AlternativeCost as AlternativeCost
import qualified Parun.Types.ArrangedArrival as ArrangedArrival
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttachRestriction as AttachRestriction
import qualified Parun.Types.AttachedToBound as AttachedToBound
import qualified Parun.Types.AttackCost as AttackCost
import qualified Parun.Types.AttackLimitUnless as AttackLimitUnless
import qualified Parun.Types.AttackRequirement as AttackRequirement
import qualified Parun.Types.AttackingPlayers as AttackingPlayers
import qualified Parun.Types.Binding as Binding
import qualified Parun.Types.BlockCost as BlockCost
import qualified Parun.Types.BlockPermission as BlockPermission
import qualified Parun.Types.BlockRequirement as BlockRequirement
import qualified Parun.Types.BlockRole as BlockRole
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.CandidateId as CandidateId
import qualified Parun.Types.CantAttackPlayer as CantAttackPlayer
import qualified Parun.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Parun.Types.CantBlockCreatures as CantBlockCreatures
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardAbilities as CardAbilities
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.CastingRestriction as CastingRestriction
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.Chooser as Chooser
import qualified Parun.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Parun.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.Types.ChosenCardInHand as ChosenCardInHand
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CopyClause as CopyClause
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostBasis as CostBasis
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CostReduction as CostReduction
import qualified Parun.Types.CouldProduce as CouldProduce
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterRestriction as CounterRestriction
import qualified Parun.Types.Counterability as Counterability
import qualified Parun.Types.CrewRestriction as CrewRestriction
import qualified Parun.Types.DamagePattern as DamagePattern
import qualified Parun.Types.DamageTally as DamageTally
import qualified Parun.Types.DelayedTrigger as DelayedTrigger
import qualified Parun.Types.DungeonRoom as DungeonRoom
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.DurationEvent as DurationEvent
import qualified Parun.Types.DuringPhase as DuringPhase
import qualified Parun.Types.EachCardFromAmong as EachCardFromAmong
import qualified Parun.Types.EachCardInExiledPile as EachCardInExiledPile
import qualified Parun.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Parun.Types.EachCardInHand as EachCardInHand
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EnteredZone as EnteredZone
import qualified Parun.Types.EntryAttack as EntryAttack
import qualified Parun.Types.EntryBlock as EntryBlock
import qualified Parun.Types.EntryController as EntryController
import qualified Parun.Types.EntryHost as EntryHost
import qualified Parun.Types.EntryRestriction as EntryRestriction
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.EventExpiry as EventExpiry
import qualified Parun.Types.EventShape as EventShape
import qualified Parun.Types.Expiry as Expiry
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FloatingCandidate as FloatingCandidate
import qualified Parun.Types.FromAnywhere as FromAnywhere
import qualified Parun.Types.FullText as FullText
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.HandAction as HandAction
import qualified Parun.Types.HistoryRead as HistoryRead
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.KeywordInstance as KeywordInstance
import qualified Parun.Types.LibraryDeparture as LibraryDeparture
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LifeDirection as LifeDirection
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.LimitUnless as LimitUnless
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.ManaProduction as ManaProduction
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.ManaRider as ManaRider
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.MovedKinds as MovedKinds
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.Onset as Onset
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.PaidExpiry as PaidExpiry
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PendingEntryEffect as PendingEntryEffect
import qualified Parun.Types.PendingReplacementEffect as PendingReplacementEffect
import qualified Parun.Types.PerCreature as PerCreature
import qualified Parun.Types.PermanentCandidate as PermanentCandidate
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.PlayersWho as PlayersWho
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.Power as Power
import qualified Parun.Types.Prevention as Prevention
import qualified Parun.Types.PreventionRider as PreventionRider
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.RandomCard as RandomCard
import qualified Parun.Types.RandomCardInGraveyard as RandomCardInGraveyard
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RemovalCount as RemovalCount
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.SacrificeRestriction as SacrificeRestriction
import qualified Parun.Types.Scope as Scope
import qualified Parun.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Parun.Types.SlotCount as SlotCount
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SlotPerPlayer as SlotPerPlayer
import qualified Parun.Types.SpecialAction as SpecialAction
import qualified Parun.Types.SpellTally as SpellTally
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.SpentOn as SpentOn
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.StepOfControllerOf as StepOfControllerOf
import qualified Parun.Types.TargetRestriction as TargetRestriction
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.TopOfLibrary as TopOfLibrary
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.Types.Toughness as Toughness
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.TriggerLimit as TriggerLimit
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnScope as TurnScope
import qualified Parun.Types.TurnTally as TurnTally
import qualified Parun.Types.UntapRestriction as UntapRestriction
import qualified Parun.Types.WhenSpent as WhenSpent
import qualified Parun.Types.WhichCounters as WhichCounters
import qualified Parun.Types.WhichTurn as WhichTurn
import qualified Parun.Types.While as While
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
--     LeftLibraryThisTurn, DealtDamageThisTurn, EnteredThisTurn, the
--     crewed/convoked/saddled-the-source atoms), EnteredWithSource
--     (GameState.enteredWith), CounterLinkedToSource
--     (GameState.counteredWith) and CouldAttackThisTurn
--     (GameState.couldAttackThisTurn) read a record kept per object id,
--     answered by Asking's keyed -- and the look-back atoms by its history too.
--   * A RESOLUTION CONTEXT: the slot atoms, the slot and amount operands, the
--     attach and search subject atoms, CantCrewVehicles and
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
  Filter.CantCrewVehicles -> contextNames asking
  Filter.CrewedSourceThisTurn -> keyed asking <> history asking HistoryRead.Crews
  Filter.ConvokedSourceThisTurn -> keyed asking <> history asking HistoryRead.Convokes <> history asking HistoryRead.Moves
  Filter.SaddledSourceThisTurn -> keyed asking <> history asking HistoryRead.Saddles
  Filter.DealtDamageThisTurn -> keyed asking <> history asking HistoryRead.Damage
  Filter.DealtDamageTo _ -> keyed asking <> history asking HistoryRead.Damage
  Filter.EnteredThisTurn (EnteredZone.MkEnteredZone to _from) -> keyed asking <> history asking (if to == Zone.Battlefield then HistoryRead.Entries else HistoryRead.Moves)
  Filter.BeforeMove nested -> keyed asking <> history asking HistoryRead.Moves <> filterNames asking nested
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
activationProhibitionNames asking x = case x of
  ActivationProhibition.MkActivationProhibition affected _kind _costs _name -> affectedNames asking affected

activationRestrictionNames :: (Monoid r) => Asking r -> ActivationRestriction.ActivationRestriction -> r
activationRestrictionNames asking x = case x of
  ActivationRestriction.SorcerySpeed -> mempty
  ActivationRestriction.DuringPhase duringPhase -> duringPhaseNames asking duringPhase
  ActivationRestriction.DuringTurn turnScope -> turnScopeNames asking turnScope
  ActivationRestriction.AttackedThisStep -> mempty
  ActivationRestriction.AfterBlockersDeclared -> mempty
  ActivationRestriction.DuringCombatBefore _step -> mempty
  ActivationRestriction.BeforeStep _phase -> history asking HistoryRead.StepBegins
  ActivationRestriction.OnlyIf condition -> conditionNames asking condition
  ActivationRestriction.TimesEver _ -> mempty
  ActivationRestriction.TimesEachTurn _ -> mempty
  ActivationRestriction.DuringThrow -> mempty
  ActivationRestriction.InstantSpeed -> mempty
  ActivationRestriction.SpendOnly _ -> mempty

activeReplacementNames :: (Monoid r) => Asking r -> ActiveReplacement.ActiveReplacement -> r
activeReplacementNames asking x = case x of
  ActiveReplacement.MkActiveReplacement effect source _controller _timestamp expiry _uses _origin condition rider slots_ _choices -> replacementEffectNames asking (const mempty) (grantedAbilityNames asking (const mempty)) (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty))) effect <> names asking (== source) <> expiryNames asking expiry <> foldMap (conditionNames asking) condition <> foldMap (preventionRiderNames asking) rider <> foldMap (slotNames asking) (Map.keys slots_) <> names asking (\oid -> any (elem oid) slots_)

affectedNames :: (Monoid r) => Asking r -> Affected.Affected -> r
affectedNames asking x = case x of
  Affected.TheseObjects objectId -> names asking (`elem` objectId)
  Affected.Matching filter_ -> filterNames asking filter_
  Affected.MatchingAnywhere filter_ -> filterNames asking filter_
  Affected.MatchingOffBattlefield filter_ -> filterNames asking filter_
  Affected.Attached -> mempty
  Affected.AttachedPlayerControls filter_ -> filterNames asking filter_
  Affected.PlayedThisWay duration -> durationNames asking duration

affectedPlayersNames :: (Monoid r) => Asking r -> (player -> r) -> AffectedPlayers.AffectedPlayers player -> r
affectedPlayersNames _asking onPlayer x = case x of
  AffectedPlayers.Scoped _playerScope -> mempty
  AffectedPlayers.Named player -> onPlayer player

affectedUnlessNames :: (Monoid r) => Asking r -> AffectedUnless.AffectedUnless -> r
affectedUnlessNames asking x = case x of
  AffectedUnless.MkAffectedUnless affected unless _name amount -> affectedNames asking affected <> foldMap (conditionNames asking) unless <> foldMap (quantityNames asking) amount

aloneUnlessNames :: (Monoid r) => Asking r -> AloneUnless.AloneUnless -> r
aloneUnlessNames asking x = case x of
  AloneUnless.MkAloneUnless affected alongside _others unless _name -> affectedNames asking affected <> foldMap (filterNames asking) alongside <> foldMap (conditionNames asking) unless

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
asCopyNames asking onAbility onEffect x = case x of
  AsCopy.MkAsCopy eligible clauses _tapped whenYouDo _among duration _recipient _optional -> filterNames asking eligible <> foldMap (copyClauseNames asking onAbility) clauses <> foldMap onEffect whenYouDo <> foldMap (durationNames asking) duration

attachRestrictionNames :: (Monoid r) => Asking r -> AttachRestriction.AttachRestriction -> r
attachRestrictionNames asking x = case x of
  AttachRestriction.MkAttachRestriction affected attachers -> affectedNames asking affected <> filterNames asking attachers

attachedToBoundNames :: (Monoid r) => Asking r -> AttachedToBound.AttachedToBound -> r
attachedToBoundNames asking x = case x of
  AttachedToBound.MkAttachedToBound slot filter_ -> slotNames asking slot <> filterNames asking filter_

attackCostNames :: (Monoid r) => Asking r -> AttackCost.AttackCost -> r
attackCostNames asking x = case x of
  AttackCost.MkAttackCost subject perAttacker _scope while -> affectedNames asking subject <> perCreatureNames asking perAttacker <> foldMap (conditionNames asking) while

attackLimitUnlessNames :: (Monoid r) => Asking r -> AttackLimitUnless.AttackLimitUnless -> r
attackLimitUnlessNames asking x = case x of
  AttackLimitUnless.MkAttackLimitUnless _limit _defenders unless -> foldMap (conditionNames asking) unless

rulePermissionNames :: (Monoid r) => Asking r -> RulePermission.RulePermission -> r
rulePermissionNames asking x = case x of
  RulePermission.MkRulePermission affected while _waives -> affectedNames asking affected <> foldMap (conditionNames asking) while

attackRequirementNames :: (Monoid r) => Asking r -> AttackRequirement.AttackRequirement -> r
attackRequirementNames asking x = case x of
  AttackRequirement.MkAttackRequirement subject _object_ while _arity -> affectedNames asking subject <> foldMap (conditionNames asking) while

attackingPlayersNames :: Asking r -> AttackingPlayers.AttackingPlayers -> r
attackingPlayersNames asking x = case x of
  AttackingPlayers.MkAttackingPlayers _relation attacked -> slotNames asking attacked

bindingNames :: (Monoid r) => Asking r -> Binding.Binding -> r
bindingNames asking x = case x of
  Binding.MkBinding targets _amount _modes copy objects _sticker _votes division -> foldMap (foldMap (recipientNames asking)) targets <> foldMap (projectedCharacteristicsNames asking) copy <> names asking (\oid -> any (elem oid) objects) <> foldMap (foldMap (recipientNames asking) . Map.keys) division

blockCostNames :: (Monoid r) => Asking r -> BlockCost.BlockCost -> r
blockCostNames asking x = case x of
  BlockCost.MkBlockCost subject perBlocker attackers while -> affectedNames asking subject <> perCreatureNames asking perBlocker <> foldMap (filterNames asking) attackers <> foldMap (conditionNames asking) while

blockPermissionNames :: (Monoid r) => Asking r -> BlockPermission.BlockPermission -> r
blockPermissionNames asking x = case x of
  BlockPermission.MkBlockPermission affected additional while -> affectedNames asking affected <> foldMap (quantityNames asking) additional <> foldMap (conditionNames asking) while

blockRequirementNames :: (Monoid r) => Asking r -> BlockRequirement.BlockRequirement -> r
blockRequirementNames asking x = case x of
  BlockRequirement.MkBlockRequirement subject attacker while _arity -> foldMap (affectedNames asking) subject <> foldMap (affectedNames asking) attacker <> foldMap (conditionNames asking) while

boundMeasureNames :: Asking r -> BoundMeasure.BoundMeasure -> r
boundMeasureNames asking x = case x of
  BoundMeasure.MkBoundMeasure slot _measure -> slotNames asking slot

candidateIdNames :: (Monoid r) => Asking r -> CandidateId.CandidateId -> r
candidateIdNames asking x = case x of
  CandidateId.OfPermanent permanentCandidate -> permanentCandidateNames asking permanentCandidate
  CandidateId.OfFloating floatingCandidate -> floatingCandidateNames asking floatingCandidate

cantAttackPlayerNames :: (Monoid r) => Asking r -> CantAttackPlayer.CantAttackPlayer -> r
cantAttackPlayerNames asking x = case x of
  CantAttackPlayer.MkCantAttackPlayer affected _defenders _kinds unless _name -> affectedNames asking affected <> foldMap (conditionNames asking) unless

cantBeBlockedByNames :: (Monoid r) => Asking r -> CantBeBlockedBy.CantBeBlockedBy -> r
cantBeBlockedByNames asking x = case x of
  CantBeBlockedBy.MkCantBeBlockedBy affected blockers unless _name -> affectedNames asking affected <> filterNames asking blockers <> foldMap (conditionNames asking) unless

cantBlockCreaturesNames :: (Monoid r) => Asking r -> CantBlockCreatures.CantBlockCreatures -> r
cantBlockCreaturesNames asking x = case x of
  CantBlockCreatures.MkCantBlockCreatures affected attackers unless _name -> affectedNames asking affected <> filterNames asking attackers <> foldMap (conditionNames asking) unless

-- A printed card, every face of it, and the cards its faces create.
cardNames :: (Monoid r) => Asking r -> Card.Card -> r
cardNames asking x = case x of
  Card.MkCard _layout faces -> foldMap (faceNames asking (cardNames asking)) faces

spendManaAsThoughNames :: (Monoid r) => Asking r -> SpendManaAsThough.SpendManaAsThough -> r
spendManaAsThoughNames asking x = case x of
  SpendManaAsThough.MkSpendManaAsThough _which _asThough _only casting -> foldMap (filterNames asking) casting

castingPermissionNames :: (Monoid r) => Asking r -> CastingPermission.CastingPermission -> r
castingPermissionNames asking x = case x of
  CastingPermission.CastFromLibraryWhileSearching -> mempty
  CastingPermission.CastFromGraveyard _family additionalCosts -> foldMap (costComponentNames asking (keywordNames asking)) additionalCosts
  CastingPermission.PlayFromGraveyard -> mempty
  CastingPermission.CastAsThoughItHadFlash condition _additionalCost -> foldMap (conditionNames asking) condition

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
characteristicPTNames asking x = case x of
  CharacteristicPT.MkCharacteristicPT power toughness -> quantityNames asking power <> quantityNames asking toughness

chooserNames :: (Monoid r) => Asking r -> Chooser.Chooser -> r
chooserNames asking x = case x of
  Chooser.TheController -> mempty
  Chooser.EachInScope -> mempty
  Chooser.BoundInSlot slotName -> slotNames asking slotName

chosenCardFromAmongNames :: (Monoid r) => Asking r -> ChosenCardFromAmong.ChosenCardFromAmong -> r
chosenCardFromAmongNames asking x = case x of
  ChosenCardFromAmong.MkChosenCardFromAmong slot filter_ count chooser pileChooser _ _ -> slotNames asking slot <> filterNames asking filter_ <> foldMap (quantityNames asking) (HowMany.quantity count) <> playerRefNames asking chooser <> foldMap (playerRefNames asking) pileChooser

chosenCardInGraveyardNames :: (Monoid r) => Asking r -> ChosenCardInGraveyard.ChosenCardInGraveyard -> r
chosenCardInGraveyardNames asking x = case x of
  ChosenCardInGraveyard.MkChosenCardInGraveyard chooser players filter_ count -> chooserNames asking chooser <> zoneScopeNames asking players <> filterNames asking filter_ <> foldMap (quantityNames asking) (HowMany.quantity count)

chosenCardInHandNames :: (Monoid r) => Asking r -> ChosenCardInHand.ChosenCardInHand -> r
chosenCardInHandNames asking x = case x of
  ChosenCardInHand.MkChosenCardInHand player filter_ count -> playerRefNames asking player <> filterNames asking filter_ <> foldMap (quantityNames asking) (HowMany.quantity count)

chosenPermanentsNames :: (Monoid r) => Asking r -> ChosenPermanents.ChosenPermanents -> r
chosenPermanentsNames asking x = case x of
  ChosenPermanents.MkChosenPermanents filter_ chooser count _zone -> filterNames asking filter_ <> playerRefNames asking chooser <> foldMap (quantityNames asking) (HowMany.quantity count)

combatRestrictionNames :: (Monoid r) => Asking r -> CombatRestriction.CombatRestriction -> r
combatRestrictionNames asking x = case x of
  CombatRestriction.CantAttack affectedUnless -> affectedUnlessNames asking affectedUnless
  CombatRestriction.CantBlock affectedUnless -> affectedUnlessNames asking affectedUnless
  CombatRestriction.CantBeBlockedBy cantBeBlockedBy -> cantBeBlockedByNames asking cantBeBlockedBy
  CombatRestriction.CantBlockCreatures cantBlockCreatures -> cantBlockCreaturesNames asking cantBlockCreatures
  CombatRestriction.CantAttackPlayer cantAttackPlayer -> cantAttackPlayerNames asking cantAttackPlayer
  CombatRestriction.CantAttackAlone aloneUnless -> aloneUnlessNames asking aloneUnless
  CombatRestriction.CantBlockAlone aloneUnless -> aloneUnlessNames asking aloneUnless
  CombatRestriction.CantAttackMoreThan attackLimitUnless -> attackLimitUnlessNames asking attackLimitUnless
  CombatRestriction.CantBlockMoreThan limitUnless -> limitUnlessNames asking limitUnless

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

copyClauseNames :: (Monoid r) => Asking r -> (ability -> r) -> CopyClause.CopyClause ability -> r
copyClauseNames asking onAbility x = case x of
  CopyClause.MkCopyClause condition exceptions counters -> foldMap (conditionNames asking) condition <> foldMap (copyExceptionNames asking onAbility) exceptions <> foldMap (withCountersNames asking) counters

copyExceptionNames :: (Monoid r) => Asking r -> (ability -> r) -> CopyException.CopyException ability -> r
copyExceptionNames asking onAbility x = case x of
  CopyException.SetPowerToughness _setPowerToughness -> mempty
  CopyException.GainKeywords keyword -> foldMap (keywordNames asking) keyword
  CopyException.GainThisAbility -> mempty
  CopyException.CardTypes _mode _cardType -> mempty
  CopyException.AddSubtypes _subtype -> mempty
  CopyException.AddSupertypes _supertype -> mempty
  CopyException.SetCreatureSubtypes _subtype -> mempty
  CopyException.RemoveSupertypes _supertype -> mempty
  CopyException.SetName _cardName -> mempty
  CopyException.SetColors _color -> mempty
  CopyException.NoManaCost -> mempty
  CopyException.GainAbility ability -> onAbility ability
  CopyException.DontCopyColors -> mempty

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
counterRestrictionNames asking x = case x of
  CounterRestriction.MkCounterRestriction affected kind -> affectedNames asking affected <> foldMap (counterKindNames asking (keywordNames asking)) kind

commanderDeckRuleNames :: (Monoid r) => Asking r -> CommanderDeckRule.CommanderDeckRule -> r
commanderDeckRuleNames asking x = case x of
  CommanderDeckRule.NoMaximumDeckSize -> mempty
  CommanderDeckRule.AnyColorIdentity matching -> filterNames asking matching
  CommanderDeckRule.ChosenExtraColor matching -> filterNames asking matching

crewRestrictionNames :: (Monoid r) => Asking r -> CrewRestriction.CrewRestriction -> r
crewRestrictionNames asking x = case x of
  CrewRestriction.MkCrewRestriction affected -> affectedNames asking affected

targetRestrictionNames :: (Monoid r) => Asking r -> (affected -> r) -> TargetRestriction.TargetRestriction affected -> r
targetRestrictionNames asking onAffected x = case x of
  TargetRestriction.MkTargetRestriction affected _kind aimers -> onAffected affected <> filterNames asking aimers

damagePatternNames :: (Monoid r) => Asking r -> DamagePattern.DamagePattern -> r
damagePatternNames asking x = case x of
  DamagePattern.MkDamagePattern _whichKind whatSource whatRecipient _whoRecipient whichRecipient whichSource boundRecipient -> filterNames asking whatSource <> foldMap (filterNames asking) whatRecipient <> foldMap (recipientNames asking) whichRecipient <> names asking (`elem` whichSource) <> foldMap (slotNames asking) boundRecipient

activeLaterPaymentNames :: (Monoid r) => Asking r -> ActiveLaterPayment.ActiveLaterPayment -> r
activeLaterPaymentNames asking x = case x of
  ActiveLaterPayment.MkActiveLaterPayment cost payoff _createdAt expiry -> costNames asking (keywordNames asking) cost <> preventionRiderNames asking payoff <> expiryNames asking expiry

delayedTriggerNames :: (Monoid r) => Asking r -> DelayedTrigger.DelayedTrigger -> r
delayedTriggerNames asking x = case x of
  DelayedTrigger.MkDelayedTrigger ability source _controller bindings _window expiry _createdAt -> triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty)) ability <> names asking (== source) <> foldMap (slotNames asking) (Map.keys bindings) <> foldMap (bindingNames asking) bindings <> foldMap (expiryNames asking) expiry

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

eachCardFromAmongNames :: (Monoid r) => Asking r -> EachCardFromAmong.EachCardFromAmong -> r
eachCardFromAmongNames asking x = case x of
  EachCardFromAmong.MkEachCardFromAmong slot filter_ -> slotNames asking slot <> filterNames asking filter_

fromAnywhereNames :: (Monoid r) => Asking r -> FromAnywhere.FromAnywhere -> r
fromAnywhereNames asking x = case x of
  FromAnywhere.MkFromAnywhere slot among -> slotNames asking slot <> foldMap (slotNames asking) among

eachCardInGraveyardNames :: (Monoid r) => Asking r -> EachCardInGraveyard.EachCardInGraveyard -> r
eachCardInGraveyardNames asking x = case x of
  EachCardInGraveyard.MkEachCardInGraveyard graveyards filter_ -> zoneScopeNames asking graveyards <> filterNames asking filter_

eachCardInHandNames :: (Monoid r) => Asking r -> EachCardInHand.EachCardInHand -> r
eachCardInHandNames asking x = case x of
  EachCardInHand.MkEachCardInHand hands filter_ -> zoneScopeNames asking hands <> foldMap (filterNames asking) filter_

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
          Children.onEntryRiders = Const.Const . entryRidersNames asking (quantityNames asking) onAbility,
          Children.onPlacement = Const.Const . libraryPlacementNames asking,
          Children.onCopyException = Const.Const . copyExceptionNames asking onAbility,
          Children.onFaceDown = Const.Const . faceDownCharacteristicsNames asking onAbility,
          Children.onManaAddition = Const.Const . manaAdditionNames asking,
          Children.onAffectedPlayers = Const.Const . affectedPlayersNames asking (slotNames asking),
          Children.onOnset = Const.Const . onsetNames asking,
          Children.onMoveDuration = Const.Const . moveDurationNames asking,
          Children.onAimedAt = Const.Const . aimedAtNames asking,
          Children.onChosenPermanents = Const.Const . chosenPermanentsNames asking
        }

entryAttackNames :: (Monoid r) => Asking r -> EntryAttack.EntryAttack -> r
entryAttackNames asking x = case x of
  EntryAttack.Chosen -> mempty
  EntryAttack.SameAs slotName -> slotNames asking slotName
  EntryAttack.UnderPlayer slotName -> slotNames asking slotName

entryBlockNames :: (Monoid r) => Asking r -> EntryBlock.EntryBlock -> r
entryBlockNames asking x = case x of
  EntryBlock.Chosen -> mempty
  EntryBlock.Specified slotName -> slotNames asking slotName

entryHostNames :: (Monoid r) => Asking r -> EntryHost.EntryHost -> r
entryHostNames asking x = case x of
  EntryHost.Specified slotName -> slotNames asking slotName
  EntryHost.Chosen filter_ -> filterNames asking filter_

entryRestrictionNames :: (Monoid r) => Asking r -> EntryRestriction.EntryRestriction -> r
entryRestrictionNames asking x = case x of
  EntryRestriction.MkEntryRestriction affected _origins -> affectedNames asking affected

entryRidersNames :: (Monoid r) => Asking r -> (count -> r) -> (ability -> r) -> EntryRiders.EntryRiders count ability -> r
entryRidersNames asking onCount onAbility x = case x of
  EntryRiders.MkEntryRiders _tapped attacking blocking _transformed counters under _exiledFaceDown attachedTo faceDown _noted characteristics -> entryControllerNames asking under <> foldMap (entryAttackNames asking) attacking <> foldMap (entryBlockNames asking) blocking <> foldMap (counterKindNames asking (keywordNames asking)) (Map.keys counters) <> foldMap onCount counters <> foldMap (entryHostNames asking) attachedTo <> foldMap (faceDownStateNames asking onAbility) faceDown <> foldMap (modificationNames asking onAbility) characteristics

entryControllerNames :: (Monoid r) => Asking r -> EntryController.EntryController -> r
entryControllerNames asking x = case x of
  EntryController.Instructed player -> playerRefNames asking player
  EntryController.Owner -> mempty

expiryNames :: (Monoid r) => Asking r -> Expiry.Expiry -> r
expiryNames asking x = case x of
  Expiry.AtCleanup -> mempty
  Expiry.Never -> mempty
  Expiry.Perpetual -> mempty
  Expiry.While while -> whileNames asking while
  Expiry.AtTurnOf _playerId -> mempty
  Expiry.AtStepOf _stepBegan -> mempty
  Expiry.AtStepOfControllerOf stepOf -> names asking (== StepOfControllerOf.object stepOf)
  Expiry.AtEndOfTurnOf _afterTurn -> mempty
  Expiry.DuringTurnOf _afterTurn -> mempty
  Expiry.DuringUntapStepOf _playerId -> mempty
  Expiry.DuringTurnOfControllerOf afterObjectTurn -> names asking (== AfterObjectTurn.object afterObjectTurn)
  Expiry.DuringExtraTurn _timestamp -> mempty
  Expiry.AtEndOf _phaseSelector -> mempty
  Expiry.AtEndOfCombatOn _afterTurn -> mempty
  Expiry.WhenPaid paidExpiry -> paidExpiryNames asking paidExpiry
  Expiry.WhenUsed slot -> foldMap (slotNames asking) slot
  Expiry.OnEvent eventExpiry -> eventExpiryNames asking eventExpiry

eventShapeNames :: Asking r -> EventShape.EventShape -> r
eventShapeNames asking x = case x of
  EventShape.MovedBetween _movedBetween -> history asking HistoryRead.Moves
  EventShape.MovedFrom _zone -> history asking HistoryRead.Moves
  EventShape.CardArrivedIn _cardArrivedIn -> history asking HistoryRead.Moves
  EventShape.SpellCast _castWindow -> history asking HistoryRead.Casts
  EventShape.AttackerDeclared -> history asking HistoryRead.Attacks

eventExpiryNames :: (Monoid r) => Asking r -> EventExpiry.EventExpiry -> r
eventExpiryNames asking x = case x of
  EventExpiry.MkEventExpiry _controller durationEvent -> durationEventNames asking durationEvent

faceNames :: (Monoid r) => Asking r -> (card -> r) -> Face.Face card -> r
faceNames asking onCard x = case x of
  Face.MkFace _name _oracleText _manaCost _typeLine power toughness _loyalty _defense _startingIntensity _vanguard _canBeYourCommander _claimsStartingPlayer _anteOnly _circlesTwo _deckLimit commanderDeckRules keywords _colorIndicator characteristicPT _characteristicColors staticAbilities spell activatedAbilities replacementEffects triggeredAbilities delayedAbilities rooms _dungeonEntryQuality castingPermissions castingRestrictions enchant counterability additionalCosts additionalCostChoices modeCosts _modeChooser maximumX _minimumX alternativeCosts costReductions playerAbilities blockRequirements blockPermissions attackRequirements combatRestrictions sacrificeRestrictions untapRestrictions crewRestrictions targetRestrictions rulePermissions attachRestrictions entryRestrictions counterRestrictions activationProhibitions attackCosts blockCosts mulliganActions openingHandActions specialActions -> foldMap (powerNames asking) power <> foldMap (toughnessNames asking) toughness <> foldMap (commanderDeckRuleNames asking) commanderDeckRules <> foldMap (keywordNames asking) (Map.keys keywords) <> foldMap (characteristicPTNames asking) characteristicPT <> foldMap (staticAbilityNames asking (grantedAbilityNames asking onCard)) staticAbilities <> modalNames asking onCard (grantedAbilityNames asking onCard) spell <> foldMap (activatedAbilityNames asking onCard (grantedAbilityNames asking onCard)) activatedAbilities <> foldMap (printedReplacementNames asking onCard (grantedAbilityNames asking onCard) (effectNames asking onCard (grantedAbilityNames asking onCard))) replacementEffects <> foldMap (triggeredAbilityNames asking onCard (grantedAbilityNames asking onCard)) triggeredAbilities <> foldMap (triggeredAbilityNames asking onCard (grantedAbilityNames asking onCard)) delayedAbilities <> foldMap (dungeonRoomNames asking onCard) rooms <> foldMap (castingPermissionNames asking) castingPermissions <> foldMap (castingRestrictionNames asking) castingRestrictions <> foldMap (targetSlotNames asking) enchant <> counterabilityNames asking counterability <> foldMap (costComponentNames asking (keywordNames asking)) additionalCosts <> foldMap (costChoiceNames asking) additionalCostChoices <> foldMap (costNames asking (keywordNames asking)) modeCosts <> foldMap (quantityNames asking) maximumX <> foldMap (alternativeCostNames asking) alternativeCosts <> foldMap (costReductionNames asking) costReductions <> foldMap (playerStaticAbilityNames asking) playerAbilities <> foldMap (blockRequirementNames asking) blockRequirements <> foldMap (blockPermissionNames asking) blockPermissions <> foldMap (attackRequirementNames asking) attackRequirements <> foldMap (combatRestrictionNames asking) combatRestrictions <> foldMap (sacrificeRestrictionNames asking) sacrificeRestrictions <> foldMap (untapRestrictionNames asking) untapRestrictions <> foldMap (crewRestrictionNames asking) crewRestrictions <> foldMap (targetRestrictionNames asking (affectedNames asking)) targetRestrictions <> foldMap (rulePermissionNames asking) rulePermissions <> foldMap (attachRestrictionNames asking) attachRestrictions <> foldMap (entryRestrictionNames asking) entryRestrictions <> foldMap (counterRestrictionNames asking) counterRestrictions <> foldMap (activationProhibitionNames asking) activationProhibitions <> foldMap (attackCostNames asking) attackCosts <> foldMap (blockCostNames asking) blockCosts <> foldMap (handActionNames asking onCard) mulliganActions <> foldMap (handActionNames asking onCard) openingHandActions <> foldMap (specialActionNames asking) specialActions

faceDownCharacteristicsNames :: (Monoid r) => Asking r -> (ability -> r) -> FaceDownCharacteristics.FaceDownCharacteristics ability -> r
faceDownCharacteristicsNames asking onAbility x = case x of
  FaceDownCharacteristics.MkFaceDownCharacteristics _typeLine power toughness keywords abilities -> foldMap (powerNames asking) power <> foldMap (toughnessNames asking) toughness <> foldMap (keywordNames asking) keywords <> foldMap onAbility abilities

faceDownStateNames :: (Monoid r) => Asking r -> (ability -> r) -> FaceDownState.FaceDownState ability -> r
faceDownStateNames asking onAbility x = case x of
  FaceDownState.MkFaceDownState _reason listed _turnsUpBefore -> faceDownCharacteristicsNames asking onAbility listed

floatingCandidateNames :: Asking r -> FloatingCandidate.FloatingCandidate -> r
floatingCandidateNames asking x = case x of
  FloatingCandidate.MkFloatingCandidate source _timestamp -> names asking (== source)

fullTextNames :: (Monoid r) => Asking r -> (ability -> r) -> FullText.FullText ability -> r
fullTextNames asking onAbility x = case x of
  FullText.MkFullText graveyard alsoHas -> playerRefNames asking graveyard <> foldMap onAbility alsoHas

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
libraryPlacementNames asking x = case x of
  LibraryPlacement.Stated _libraryPosition -> mempty
  LibraryPlacement.OwnerChooses -> mempty
  LibraryPlacement.RandomOrder _libraryPosition -> mempty
  LibraryPlacement.ArrangedBy (ArrangedArrival.MkArrangedArrival _libraryPosition chooser) -> playerRefNames asking chooser
  LibraryPlacement.Beneath quantity -> quantityNames asking quantity
  LibraryPlacement.BeneathOrBottom quantity -> quantityNames asking quantity

limitUnlessNames :: (Monoid r) => Asking r -> LimitUnless.LimitUnless -> r
limitUnlessNames asking x = case x of
  LimitUnless.MkLimitUnless _limit _comparison attackers unless -> foldMap (affectedNames asking) attackers <> foldMap (conditionNames asking) unless

manaAdditionNames :: (Monoid r) => Asking r -> ManaAddition.ManaAddition -> r
manaAdditionNames asking x = case x of
  ManaAddition.MkManaAddition player production count _retention restriction rider whenSpent -> playerRefNames asking player <> manaProductionNames asking production <> quantityNames asking count <> foldMap (manaRestrictionNames asking) restriction <> foldMap (manaRiderNames asking) rider <> foldMap (whenSpentNames asking) whenSpent

manaProductionNames :: (Monoid r) => Asking r -> ManaProduction.ManaProduction -> r
manaProductionNames asking x = case x of
  ManaProduction.OfType _manaType -> mempty
  ManaProduction.AnyColor _ -> mempty
  ManaProduction.Chosen -> mempty
  ManaProduction.SnowSymbol -> mempty
  ManaProduction.Noted -> mempty
  ManaProduction.CouldProduce (CouldProduce.MkCouldProduce among _types) -> filterNames asking among
  ManaProduction.Pooled manaCount -> manaCountNames asking manaCount
  ManaProduction.ProducedByTapped -> slotNames asking Binding.Engine.manaSource
  ManaProduction.InCommanderIdentity -> mempty
  ManaProduction.Circled -> mempty

manaCountNames :: (Monoid r) => Asking r -> ManaCount.ManaCount -> r
manaCountNames asking x = case x of
  ManaCount.MkManaCount player _filter_ -> playerRefNames asking player

manaRestrictionNames :: (Monoid r) => Asking r -> ManaRestriction.ManaRestriction -> r
manaRestrictionNames asking x = case x of
  ManaRestriction.MkManaRestriction casts activations keywordActivations unlocks turnsFaceUp _prohibits _paysGeneric -> foldMap (filterNames asking) casts <> foldMap (filterNames asking) activations <> foldMap (keywordDesignatorNames asking) keywordActivations <> foldMap (filterNames asking) unlocks <> foldMap (filterNames asking) turnsFaceUp

manaRiderNames :: (Monoid r) => Asking r -> ManaRider.ManaRider -> r
manaRiderNames asking x = case x of
  ManaRider.MkManaRider condition _effect -> filterNames asking condition

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

modificationNames :: (Monoid r) => Asking r -> (ability -> r) -> Modification.Modification ability -> r
modificationNames asking onAbility x = case x of
  Modification.GainKeyword keyword -> keywordNames asking keyword
  Modification.GainKeywordAtManaCost _costKeyword -> mempty
  Modification.GainEnchant targetSlot -> targetSlotNames asking targetSlot
  Modification.LoseEnchant targetSlot -> targetSlotNames asking targetSlot
  Modification.GainCastingPermission castingPermission -> castingPermissionNames asking castingPermission
  Modification.GainAbility ability -> onAbility ability
  Modification.GainAbilitiesOfSource keyword -> foldMap (keywordNames asking) keyword
  Modification.GainCardAbilities (CardAbilities.MkCardAbilities _ cards items) -> filterNames asking cards <> foldMap (activationRestrictionNames asking) items
  Modification.GainAbilitiesOfStickers -> mempty
  Modification.LoseAllAbilities -> mempty
  Modification.LoseNamedAbility abilityName -> names asking (`Binding.Engine.isAcquiredFrom` abilityName)
  Modification.LoseKeyword keyword -> keywordNames asking keyword
  Modification.LoseKeywordFamily _keywordFamily -> mempty
  Modification.SetBasePowerToughness setBasePowerToughness -> setBasePowerToughnessNames asking setBasePowerToughness
  Modification.ModifyPowerToughness modifyPowerToughness -> modifyPowerToughnessNames asking modifyPowerToughness
  Modification.LandSubtype _changeMode _subtype -> mempty
  Modification.ChosenSubtype {} -> mempty
  Modification.CreatureSubtype _changeMode _subtype -> mempty
  Modification.AddEveryCreatureSubtype -> mempty
  Modification.LoseEverySubtype _ -> mempty
  Modification.SetCreatureSubtypesOfLastCardExiledWith filter_ -> filterNames asking filter_
  Modification.AddSubtype _subtype -> mempty
  Modification.RemoveSubtype _subtype -> mempty
  Modification.CardType {} -> mempty
  Modification.LoseCardType _cardType -> mempty
  Modification.AddSupertype _supertype -> mempty
  Modification.RemoveSupertype _supertype -> mempty
  Modification.ChangeWord _changeSubtypeWord -> mempty
  Modification.ExchangeTextBoxes -> mempty
  Modification.AddNamesMatching filter_ -> filterNames asking filter_
  Modification.SetName _cardName -> mempty
  Modification.InsertNameWords _nameInsertion -> mempty
  Modification.HasFullText fullText -> fullTextNames asking onAbility fullText
  Modification.SetController _playerId -> mempty
  Modification.SetControllerToSource -> mempty
  Modification.Colors _colorMode _colors -> mempty
  Modification.ChosenColor _ -> mempty
  Modification.SwitchPowerToughness -> mempty
  Modification.AssignCombatDamage _ -> mempty
  Modification.MayAssignCombatDamage _option -> mempty
  Modification.GrantsStationToughness -> mempty
  Modification.Intensify quantity -> quantityNames asking quantity

modifyPowerToughnessNames :: (Monoid r) => Asking r -> ModifyPowerToughness.ModifyPowerToughness -> r
modifyPowerToughnessNames asking x = case x of
  ModifyPowerToughness.MkModifyPowerToughness power toughness -> quantityNames asking power <> quantityNames asking toughness

moveDurationNames :: (Monoid r) => Asking r -> MoveDuration.MoveDuration -> r
moveDurationNames asking x = case x of
  MoveDuration.UntilSourceLeavesTheBattlefield -> mempty
  MoveDuration.UntilAnOpponentBecomesTheMonarch -> history asking HistoryRead.MonarchChanges

movedKindsNames :: (Monoid r) => Asking r -> MovedKinds.MovedKinds -> r
movedKindsNames asking x = case x of
  MovedKinds.Every -> mempty
  MovedKinds.Named counterKind quantity -> counterKindNames asking (keywordNames asking) counterKind <> quantityNames asking quantity
  MovedKinds.EveryOfKind counterKind -> counterKindNames asking (keywordNames asking) counterKind
  MovedKinds.Chosen quantity -> quantityNames asking quantity
  MovedKinds.AnyNumber -> mempty
  MovedKinds.AtLeastOne -> mempty
  MovedKinds.AnyNumberOfKind counterKind -> counterKindNames asking (keywordNames asking) counterKind
  MovedKinds.EachAbsentKind -> mempty
  MovedKinds.UpToOneChosen -> mempty

objectRefNames :: (Monoid r) => Asking r -> ObjectRef.ObjectRef -> r
objectRefNames asking x = case x of
  ObjectRef.InSlot slotName -> slotNames asking slotName
  ObjectRef.EachMatching filter_ -> filterNames asking filter_
  ObjectRef.EachCardInGraveyard eachCardInGraveyard -> eachCardInGraveyardNames asking eachCardInGraveyard
  ObjectRef.EachCardInYourHand -> mempty
  ObjectRef.EachCardInHand eachCardInHand -> eachCardInHandNames asking eachCardInHand
  ObjectRef.EachCardInYourLibrary filter_ -> foldMap (filterNames asking) filter_
  ObjectRef.EachCardYouOwn filter_ -> filterNames asking filter_
  ObjectRef.EachCardInExiledPile (EachCardInExiledPile.MkEachCardInExiledPile _pile filter_) -> foldMap (filterNames asking) filter_
  ObjectRef.EachCardEncodedOnSource filter_ -> foldMap (filterNames asking) filter_
  ObjectRef.EachSpell filter_ -> filterNames asking filter_
  ObjectRef.EachAbility filter_ -> filterNames asking filter_
  ObjectRef.EachOnStack filter_ -> filterNames asking filter_
  ObjectRef.Players (PlayersWho.MkPlayersWho playerRef who) -> playerRefNames asking playerRef <> foldMap (filterNames asking) who
  ObjectRef.TopOfLibrary topOfLibrary -> topOfLibraryNames asking topOfLibrary
  ObjectRef.TopOfLibraryUntil topOfLibraryUntil -> topOfLibraryUntilNames asking topOfLibraryUntil
  ObjectRef.TopOfGraveyard playerRef -> playerRefNames asking playerRef
  ObjectRef.ChosenCardInGraveyard chosenCardInGraveyard -> chosenCardInGraveyardNames asking chosenCardInGraveyard
  ObjectRef.ChosenCardInHand chosenCardInHand -> chosenCardInHandNames asking chosenCardInHand
  ObjectRef.ChosenCardFromAmong chosenCardFromAmong -> chosenCardFromAmongNames asking chosenCardFromAmong
  ObjectRef.EachCardFromAmong eachCardFromAmong -> eachCardFromAmongNames asking eachCardFromAmong
  ObjectRef.RandomCard randomCard -> randomCardNames asking randomCard
  ObjectRef.RandomCardInGraveyard randomCardInGraveyard -> randomCardInGraveyardNames asking randomCardInGraveyard
  ObjectRef.AtRandom inner -> objectRefNames asking inner
  ObjectRef.ChosenPermanents chosenPermanents -> chosenPermanentsNames asking chosenPermanents
  ObjectRef.AttachedToBound attachedToBound -> attachedToBoundNames asking attachedToBound
  ObjectRef.AttackedBy slot -> slotNames asking slot
  ObjectRef.FromAnywhere fromAnywhere -> history asking HistoryRead.Moves <> fromAnywhereNames asking fromAnywhere
  ObjectRef.AllOf refs -> foldMap (objectRefNames asking) refs

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

paidExpiryNames :: (Monoid r) => Asking r -> PaidExpiry.PaidExpiry -> r
paidExpiryNames asking x = case x of
  PaidExpiry.MkPaidExpiry _player cost resolution -> costNames asking (keywordNames asking) cost <> names asking (== resolution)

payGateNames :: (Monoid r) => Asking r -> PayGate.PayGate -> r
payGateNames asking x = case x of
  PayGate.MkPayGate payer cost basis _branch _obligation perEach chosenX _offeredAt -> playerRefNames asking payer <> costChoiceNames asking cost <> foldMap (costBasisNames asking) basis <> foldMap (quantityNames asking) perEach <> foldMap (quantityNames asking) chosenX

pendingDamageEffectNames :: (Monoid r) => Asking r -> PendingReplacementEffect.PendingReplacementEffect -> r
pendingDamageEffectNames asking x = case x of
  PendingReplacementEffect.MkPendingReplacementEffect effects targets _controller source -> foldMap (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty))) effects <> foldMap (slotNames asking) (Map.keys targets) <> foldMap (foldMap (recipientNames asking)) targets <> names asking (== source)

pendingEntryEffectNames :: (Monoid r) => Asking r -> PendingEntryEffect.PendingEntryEffect -> r
pendingEntryEffectNames asking x = case x of
  PendingEntryEffect.MkPendingEntryEffect object_ _controller effects -> names asking (== object_) <> foldMap (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty))) effects

perCreatureNames :: (Monoid r) => Asking r -> PerCreature.PerCreature -> r
perCreatureNames asking x = case x of
  PerCreature.Fixed cost -> costNames asking (keywordNames asking) cost
  PerCreature.Counted quantity -> quantityNames asking quantity

permanentCandidateNames :: (Monoid r) => Asking r -> PermanentCandidate.PermanentCandidate -> r
permanentCandidateNames asking x = case x of
  PermanentCandidate.MkPermanentCandidate source _provenance effect _ordinal -> names asking (== source) <> replacementEffectNames asking (const mempty) (grantedAbilityNames asking (const mempty)) (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty))) effect

activationCriteriaNames :: (Monoid r) => Asking r -> ActivationCriteria.ActivationCriteria -> r
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

poolNames :: (Monoid r) => Asking r -> Pool.Pool -> r
poolNames asking x = case x of
  Pool.Creatures -> mempty
  Pool.Players -> mempty
  Pool.AnyTarget -> mempty
  Pool.Permanents -> mempty
  Pool.Spells -> mempty
  Pool.Abilities -> mempty
  Pool.SpellsAndPermanents -> mempty
  Pool.SpellsAndAbilities -> mempty
  Pool.PlayersAndPlaneswalkers -> mempty
  Pool.PermanentsAndPlayers -> mempty
  Pool.CardsInGraveyard zoneScope -> zoneScopeNames asking zoneScope
  Pool.CardsInExile -> mempty
  Pool.CardsInAnte -> mempty
  Pool.CreaturesAndCardsInGraveyard zoneScope -> zoneScopeNames asking zoneScope

powerNames :: (Monoid r) => Asking r -> Power.Power -> r
powerNames asking x = case x of
  Power.MkPower unwrap -> quantityNames asking unwrap

preventionNames :: (Monoid r) => Asking r -> Prevention.Prevention -> r
preventionNames asking x = case x of
  Prevention.MkPrevention by amounts rider -> candidateIdNames asking by <> names asking (`Map.member` amounts) <> foldMap (foldMap (recipientNames asking) . Map.keys) amounts <> foldMap (preventionRiderNames asking) rider

preventionRiderNames :: (Monoid r) => Asking r -> PreventionRider.PreventionRider -> r
preventionRiderNames asking x = case x of
  PreventionRider.MkPreventionRider effects targets _controller source -> foldMap (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty))) effects <> foldMap (slotNames asking) (Map.keys targets) <> foldMap (foldMap (recipientNames asking)) targets <> names asking (== source)

printedReplacementNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> (effect -> r) -> PrintedReplacement.PrintedReplacement card ability effect -> r
printedReplacementNames asking onCard onAbility onEffect = Const.getConst . Children.printedReplacementChildren (abilityVisitors asking onCard onAbility onEffect)

projectedCharacteristicsNames :: (Monoid r) => Asking r -> ProjectedCharacteristics.ProjectedCharacteristics -> r
projectedCharacteristicsNames asking x = case x of
  ProjectedCharacteristics.MkProjectedCharacteristics _names _supertypes keywords _colors _manaCost _manaValue _power _toughness _loyalty _defense _intensity characteristicPT _cardTypes _subtypes staticAbilities playerAbilities grantedPlayerAbilities grantedStaticAbilities grantedRuleAbilities specialActions activatedAbilities replacementEffects triggeredAbilities delayedAbilities enchant castingPermissions ruleAbilities _lostAllAbilities _hasFullText _wordChanges textChangedKeywords _combatDamageAmount _combatDamageOptions _grantsStationToughness additionalCosts additionalCostChoices alternativeCosts costReductions grantedCostReductions grantedAlternativeCosts _grantedSpendManaAsThough _halves exceptions mergedDonors prepare alternativeSpell _prototyped spell _circled _definesChosenColor flipped -> foldMap (keywordNames asking) (Map.keys keywords) <> foldMap (characteristicPTNames asking) characteristicPT <> foldMap (staticAbilityNames asking (grantedAbilityNames asking (const mempty))) staticAbilities <> foldMap (playerStaticAbilityNames asking) playerAbilities <> foldMap (playerStaticAbilityNames asking . snd) grantedPlayerAbilities <> foldMap (staticAbilityNames asking (grantedAbilityNames asking (const mempty)) . snd) grantedStaticAbilities <> ruleAbilitiesNames asking grantedRuleAbilities <> foldMap (specialActionNames asking) specialActions <> foldMap (activatedAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) activatedAbilities <> foldMap (printedReplacementNames asking (const mempty) (grantedAbilityNames asking (const mempty)) (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty)))) replacementEffects <> foldMap (triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) triggeredAbilities <> foldMap (triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) delayedAbilities <> foldMap (targetSlotNames asking) enchant <> foldMap (castingPermissionNames asking) castingPermissions <> ruleAbilitiesNames asking ruleAbilities <> foldMap (keywordNames asking) (Map.keys textChangedKeywords) <> foldMap (costComponentNames asking (keywordNames asking)) additionalCosts <> foldMap (costChoiceNames asking) additionalCostChoices <> foldMap (alternativeCostNames asking) alternativeCosts <> foldMap (costReductionNames asking) costReductions <> foldMap (costReductionNames asking) grantedCostReductions <> foldMap (alternativeCostNames asking) grantedAlternativeCosts <> foldMap (copyExceptionNames asking (grantedAbilityNames asking (const mempty))) exceptions <> foldMap (projectedCharacteristicsNames asking) mergedDonors <> foldMap (faceNames asking (const mempty)) prepare <> foldMap (faceNames asking (const mempty)) alternativeSpell <> modalNames asking (const mempty) (grantedAbilityNames asking (const mempty)) spell <> foldMap (projectedCharacteristicsNames asking) flipped

quantityNames :: (Monoid r) => Asking r -> Quantity.Quantity -> r
quantityNames asking x =
  let own = case x of
        Quantity.WasBlockedThisTurn -> history asking HistoryRead.Blocks
        Quantity.TimesTappedThisTurn -> history asking HistoryRead.Taps
        Quantity.AttackersDeclaredThisTurn _ -> history asking HistoryRead.Attacks
        Quantity.AttackersInTheirLastTurn _ -> history asking HistoryRead.Attacks
        -- Draws and land plays read player-keyed tallies naming no object
        -- (GameState.drawsThisTurn, GameState.landsPlayed), so no history.
        Quantity.TalliedThisTurn tally _ -> case tally of
          TurnTally.CardsDiscarded -> history asking HistoryRead.Discards
          TurnTally.CardsDrawn -> mempty
          TurnTally.ActionsDone acts -> history asking (HistoryRead.Acts acts)
          TurnTally.LandsPlayed -> mempty
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

randomCardInGraveyardNames :: (Monoid r) => Asking r -> RandomCardInGraveyard.RandomCardInGraveyard -> r
randomCardInGraveyardNames asking x = case x of
  RandomCardInGraveyard.MkRandomCardInGraveyard players filter_ count -> zoneScopeNames asking players <> filterNames asking filter_ <> quantityNames asking count

randomCardNames :: (Monoid r) => Asking r -> RandomCard.RandomCard -> r
randomCardNames asking x = case x of
  RandomCard.MkRandomCard _ player filter_ count -> playerRefNames asking player <> filterNames asking filter_ <> quantityNames asking count

recipientNames :: (Monoid r) => Asking r -> Recipient.Recipient -> r
recipientNames asking x = case x of
  Recipient.ToCreature objectId -> names asking (== objectId)
  Recipient.ToPlaneswalker objectId -> names asking (== objectId)
  Recipient.ToBattle objectId -> names asking (== objectId)
  Recipient.ToPlayer _playerId -> mempty
  Recipient.ToObject objectId -> names asking (== objectId)
  Recipient.ToPile _pile -> mempty

removalCountNames :: (Monoid r) => Asking r -> RemovalCount.RemovalCount -> r
removalCountNames asking x = case x of
  RemovalCount.Exactly quantity -> quantityNames asking quantity
  RemovalCount.UpTo quantity -> quantityNames asking quantity
  RemovalCount.AnyNumber -> mempty
  RemovalCount.All -> mempty

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
ruleAbilitiesNames asking x = case x of
  RuleAbilities.MkRuleAbilities activationProhibitions attachRestrictions attackCosts rulePermissions attackRequirements blockCosts blockPermissions blockRequirements combatRestrictions counterRestrictions crewRestrictions entryRestrictions sacrificeRestrictions targetRestrictions untapRestrictions -> foldMap (activationProhibitionNames asking) activationProhibitions <> foldMap (attachRestrictionNames asking) attachRestrictions <> foldMap (attackCostNames asking) attackCosts <> foldMap (rulePermissionNames asking) rulePermissions <> foldMap (attackRequirementNames asking) attackRequirements <> foldMap (blockCostNames asking) blockCosts <> foldMap (blockPermissionNames asking) blockPermissions <> foldMap (blockRequirementNames asking) blockRequirements <> foldMap (combatRestrictionNames asking) combatRestrictions <> foldMap (counterRestrictionNames asking) counterRestrictions <> foldMap (crewRestrictionNames asking) crewRestrictions <> foldMap (entryRestrictionNames asking) entryRestrictions <> foldMap (sacrificeRestrictionNames asking) sacrificeRestrictions <> foldMap (targetRestrictionNames asking (affectedNames asking)) targetRestrictions <> foldMap (untapRestrictionNames asking) untapRestrictions

sacrificeRestrictionNames :: (Monoid r) => Asking r -> SacrificeRestriction.SacrificeRestriction -> r
sacrificeRestrictionNames asking x = case x of
  SacrificeRestriction.MkSacrificeRestriction affected -> affectedNames asking affected

scopeNames :: (Monoid r) => Asking r -> Scope.Scope -> r
scopeNames asking x = case x of
  Scope.InZone inZone -> inZoneNames asking inZone
  Scope.InHistory eventShape -> eventShapeNames asking eventShape
  Scope.OverPlayers playerRef -> playerRefNames asking playerRef
  Scope.OverBound _ slotName -> slotNames asking slotName
  Scope.InPosition _position inZone -> inZoneNames asking inZone
  Scope.AsTurnBegan -> mempty

setBasePowerToughnessNames :: (Monoid r) => Asking r -> SetBasePowerToughness.SetBasePowerToughness -> r
setBasePowerToughnessNames asking x = case x of
  SetBasePowerToughness.MkSetBasePowerToughness power toughness -> foldMap (quantityNames asking) power <> foldMap (quantityNames asking) toughness

slotCountNames :: (Monoid r) => Asking r -> SlotCount.SlotCount -> r
slotCountNames asking x = case x of
  SlotCount.Printed _targetCount -> mempty
  SlotCount.AnnouncedX -> mempty
  SlotCount.UpToAnnouncedX -> mempty
  SlotCount.UpToComputed quantity -> quantityNames asking quantity

slotPerPlayerNames :: Asking r -> SlotPerPlayer.SlotPerPlayer -> r
slotPerPlayerNames asking x = case x of
  SlotPerPlayer.MkSlotPerPlayer _players slot -> slotNames asking slot

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
targetSlotNames asking x = case x of
  TargetSlot.MkTargetSlot pool filter_ count amount chooser perPlayer divided _together asCast -> poolNames asking pool <> foldMap (filterNames asking) filter_ <> foldMap (filterNames asking) asCast <> slotCountNames asking count <> foldMap (quantityNames asking) amount <> foldMap (playerRefNames asking) chooser <> foldMap (slotPerPlayerNames asking) perPlayer <> foldMap (quantityNames asking) divided

topOfLibraryNames :: (Monoid r) => Asking r -> TopOfLibrary.TopOfLibrary -> r
topOfLibraryNames asking x = case x of
  TopOfLibrary.MkTopOfLibrary player count -> playerRefNames asking player <> quantityNames asking count

topOfLibraryUntilNames :: (Monoid r) => Asking r -> TopOfLibraryUntil.TopOfLibraryUntil -> r
topOfLibraryUntilNames asking x = case x of
  TopOfLibraryUntil.MkTopOfLibraryUntil player filter_ count -> playerRefNames asking player <> filterNames asking filter_ <> quantityNames asking count

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
  -- GameState.instructionsTakenThisTurn, which Parun.Engine.Fragmented.digest keeps.
  TriggerLimit.OncePerTurn SpentOn.TakingInstruction -> mempty
  -- GameState.triggeredThisGame, which Parun.Engine.Fragmented.digest keeps.
  TriggerLimit.OncePerGame -> mempty

triggeredAbilityNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> TriggeredAbility.TriggeredAbility card ability -> r
triggeredAbilityNames asking onCard onAbility = Const.getConst . Children.triggeredAbilityChildren (abilityVisitors asking onCard onAbility (const mempty))

untapRestrictionNames :: (Monoid r) => Asking r -> UntapRestriction.UntapRestriction -> r
untapRestrictionNames asking x = case x of
  UntapRestriction.MkUntapRestriction affected _hold while -> affectedNames asking affected <> foldMap (conditionNames asking) while

whenSpentNames :: (Monoid r) => Asking r -> WhenSpent.WhenSpent -> r
whenSpentNames asking x = case x of
  WhenSpent.MkWhenSpent casts _ability -> filterNames asking casts

whichCountersNames :: (Monoid r) => Asking r -> (keyword -> r) -> WhichCounters.WhichCounters keyword -> r
whichCountersNames asking onKeyword x = case x of
  WhichCounters.OfKind counterKind -> counterKindNames asking onKeyword counterKind
  WhichCounters.OfAnyKind -> mempty

whileNames :: (Monoid r) => Asking r -> While.While -> r
whileNames asking x = case x of
  While.MkWhile _player condition -> conditionNames asking condition

withCountersNames :: (Monoid r) => Asking r -> WithCounters.WithCounters -> r
withCountersNames asking x = case x of
  WithCounters.MkWithCounters counters -> foldMap (counterKindNames asking (keywordNames asking)) (Map.keys counters) <> foldMap (quantityNames asking) counters

zoneScopeNames :: (Monoid r) => Asking r -> ZoneScope.ZoneScope -> r
zoneScopeNames asking x = case x of
  ZoneScope.Scoped _playerScope -> mempty
  ZoneScope.InSlot slotName -> slotNames asking slotName
  ZoneScope.ControllerOfBound slotName -> slotNames asking slotName
  ZoneScope.BoundPlayer _playerId -> mempty
