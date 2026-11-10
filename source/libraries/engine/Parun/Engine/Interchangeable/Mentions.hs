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
-- TriggerCondition) is matched there instead, its leaf types discarded there.
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
-- keyword's arm, where the engine EXPANDS the keyword into one or gates its
-- cost on one (Parun.Engine.Keyword, Parun.Engine.Cost.candidateCostsGiven).
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
import qualified Parun.Types.AlternativeActivationCost as AlternativeActivationCost
import qualified Parun.Types.AlternativeCost as AlternativeCost
import qualified Parun.Types.ArrangedArrival as ArrangedArrival
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttachR as AttachR
import qualified Parun.Types.AttachRestriction as AttachRestriction
import qualified Parun.Types.AttachRewrite as AttachRewrite
import qualified Parun.Types.AttachedToBound as AttachedToBound
import qualified Parun.Types.AttackCost as AttackCost
import qualified Parun.Types.AttackLimitUnless as AttackLimitUnless
import qualified Parun.Types.AttackRequirement as AttackRequirement
import qualified Parun.Types.AttackingPlayers as AttackingPlayers
import qualified Parun.Types.Backup as Backup
import qualified Parun.Types.Behold as Behold
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
import qualified Parun.Types.CastFromZone as CastFromZone
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.CastingRestriction as CastingRestriction
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.ChooseInstead as ChooseInstead
import qualified Parun.Types.Chooser as Chooser
import qualified Parun.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Parun.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.Types.ChosenCardInHand as ChosenCardInHand
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule
import qualified Parun.Types.CompanionCondition as CompanionCondition
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CopyClause as CopyClause
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostAddition as CostAddition
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostBasis as CostBasis
import qualified Parun.Types.CostChange as CostChange
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CostModifier as CostModifier
import qualified Parun.Types.CostReduction as CostReduction
import qualified Parun.Types.CostSubject as CostSubject
import qualified Parun.Types.CouldProduce as CouldProduce
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterPattern as CounterPattern
import qualified Parun.Types.CounterProhibition as CounterProhibition
import qualified Parun.Types.CounterR as CounterR
import qualified Parun.Types.CounterRestriction as CounterRestriction
import qualified Parun.Types.CounterSubject as CounterSubject
import qualified Parun.Types.Counterability as Counterability
import qualified Parun.Types.CountersFromPermanents as CountersFromPermanents
import qualified Parun.Types.CountersOnThis as CountersOnThis
import qualified Parun.Types.Craft as Craft
import qualified Parun.Types.CrewRestriction as CrewRestriction
import qualified Parun.Types.Cycling as Cycling
import qualified Parun.Types.DamagePattern as DamagePattern
import qualified Parun.Types.DamageR as DamageR
import qualified Parun.Types.DamageRewrite as DamageRewrite
import qualified Parun.Types.DamageTally as DamageTally
import qualified Parun.Types.DelayedTrigger as DelayedTrigger
import qualified Parun.Types.DestructionR as DestructionR
import qualified Parun.Types.Devour as Devour
import qualified Parun.Types.DieRollR as DieRollR
import qualified Parun.Types.DiscardCards as DiscardCards
import qualified Parun.Types.DrawCountR as DrawCountR
import qualified Parun.Types.DrawR as DrawR
import qualified Parun.Types.DrawRewrite as DrawRewrite
import qualified Parun.Types.DungeonRoom as DungeonRoom
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.DurationEvent as DurationEvent
import qualified Parun.Types.DuringPhase as DuringPhase
import qualified Parun.Types.EachCardFromAmong as EachCardFromAmong
import qualified Parun.Types.EachCardInExiledPile as EachCardInExiledPile
import qualified Parun.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Parun.Types.EachCardInHand as EachCardInHand
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.Emerge as Emerge
import qualified Parun.Types.EnteredZone as EnteredZone
import qualified Parun.Types.EntersWith as EntersWith
import qualified Parun.Types.EntriesTrigger as EntriesTrigger
import qualified Parun.Types.EntryAttack as EntryAttack
import qualified Parun.Types.EntryBlock as EntryBlock
import qualified Parun.Types.EntryController as EntryController
import qualified Parun.Types.EntryFlip as EntryFlip
import qualified Parun.Types.EntryHost as EntryHost
import qualified Parun.Types.EntryOption as EntryOption
import qualified Parun.Types.EntryPrice as EntryPrice
import qualified Parun.Types.EntryR as EntryR
import qualified Parun.Types.EntryRestriction as EntryRestriction
import qualified Parun.Types.EntryRewrite as EntryRewrite
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Equip as Equip
import qualified Parun.Types.EventExpiry as EventExpiry
import qualified Parun.Types.EventShape as EventShape
import qualified Parun.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Parun.Types.ExileCardsFromHand as ExileCardsFromHand
import qualified Parun.Types.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Types.ExileFromGraveyard as ExileFromGraveyard
import qualified Parun.Types.ExileMaterials as ExileMaterials
import qualified Parun.Types.ExilePermanents as ExilePermanents
import qualified Parun.Types.Expiry as Expiry
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FloatingCandidate as FloatingCandidate
import qualified Parun.Types.ForetellCost as ForetellCost
import qualified Parun.Types.FreeCastFromHand as FreeCastFromHand
import qualified Parun.Types.FromAnywhere as FromAnywhere
import qualified Parun.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Parun.Types.FullText as FullText
import qualified Parun.Types.GameLossR as GameLossR
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.HandAction as HandAction
import qualified Parun.Types.HexproofExemption as HexproofExemption
import qualified Parun.Types.HistoryRead as HistoryRead
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.Impending as Impending
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.InstructionR as InstructionR
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordCount as KeywordCount
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.KeywordInstance as KeywordInstance
import qualified Parun.Types.KeywordRiderGrant as KeywordRiderGrant
import qualified Parun.Types.KeywordTally as KeywordTally
import qualified Parun.Types.LibraryDeparture as LibraryDeparture
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LifeDirection as LifeDirection
import qualified Parun.Types.LifeGainR as LifeGainR
import qualified Parun.Types.LifeLossPattern as LifeLossPattern
import qualified Parun.Types.LifeLossR as LifeLossR
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.LimitUnless as LimitUnless
import qualified Parun.Types.LoyaltyClause as LoyaltyClause
import qualified Parun.Types.LoyaltyGrant as LoyaltyGrant
import qualified Parun.Types.MadnessCost as MadnessCost
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.ManaProduction as ManaProduction
import qualified Parun.Types.ManaR as ManaR
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.ManaRider as ManaRider
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.Morph as Morph
import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.NextStepOf as NextStepOf
import qualified Parun.Types.Ninjutsu as Ninjutsu
import qualified Parun.Types.ObjectCount as ObjectCount
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.Onset as Onset
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.Optionality as Optionality
import qualified Parun.Types.OrElse as OrElse
import qualified Parun.Types.PaidExpiry as PaidExpiry
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PendingEntryEffect as PendingEntryEffect
import qualified Parun.Types.PendingReplacementEffect as PendingReplacementEffect
import qualified Parun.Types.PerCreature as PerCreature
import qualified Parun.Types.PerTarget as PerTarget
import qualified Parun.Types.PerformerR as PerformerR
import qualified Parun.Types.PermanentCandidate as PermanentCandidate
import qualified Parun.Types.PhasePattern as PhasePattern
import qualified Parun.Types.PhaseR as PhaseR
import qualified Parun.Types.PhaseRewrite as PhaseRewrite
import qualified Parun.Types.PlaneswalkR as PlaneswalkR
import qualified Parun.Types.PlaneswalkRewrite as PlaneswalkRewrite
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerCounterTally as PlayerCounterTally
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.PlayersWho as PlayersWho
import qualified Parun.Types.PlotFromZone as PlotFromZone
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.Power as Power
import qualified Parun.Types.Prevention as Prevention
import qualified Parun.Types.PreventionRider as PreventionRider
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Protection as Protection
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.RandomCard as RandomCard
import qualified Parun.Types.RandomCardInGraveyard as RandomCardInGraveyard
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Reinforce as Reinforce
import qualified Parun.Types.RemovalCount as RemovalCount
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.ReturnPermanents as ReturnPermanents
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.Sacrifice as Sacrifice
import qualified Parun.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Parun.Types.SacrificeRestriction as SacrificeRestriction
import qualified Parun.Types.SacrificeToEnter as SacrificeToEnter
import qualified Parun.Types.Scope as Scope
import qualified Parun.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Parun.Types.SlotCount as SlotCount
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SlotPerPlayer as SlotPerPlayer
import qualified Parun.Types.SpecialAction as SpecialAction
import qualified Parun.Types.SpellTally as SpellTally
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.SpentOn as SpentOn
import qualified Parun.Types.Splice as Splice
import qualified Parun.Types.StatedFlip as StatedFlip
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.StepOfControllerOf as StepOfControllerOf
import qualified Parun.Types.Suspend as Suspend
import qualified Parun.Types.TapForMana as TapForMana
import qualified Parun.Types.TapForTotalPower as TapForTotalPower
import qualified Parun.Types.TapPermanents as TapPermanents
import qualified Parun.Types.TargetRestriction as TargetRestriction
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.TokenPattern as TokenPattern
import qualified Parun.Types.TokenPlus as TokenPlus
import qualified Parun.Types.TokenR as TokenR
import qualified Parun.Types.TopOfLibrary as TopOfLibrary
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.Types.Toughness as Toughness
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.TriggerLimit as TriggerLimit
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnScope as TurnScope
import qualified Parun.Types.TurnTally as TurnTally
import qualified Parun.Types.TurnUpR as TurnUpR
import qualified Parun.Types.TurnUpRewrite as TurnUpRewrite
import qualified Parun.Types.UntapR as UntapR
import qualified Parun.Types.UntapRestriction as UntapRestriction
import qualified Parun.Types.UntapRewrite as UntapRewrite
import qualified Parun.Types.UntilPaid as UntilPaid
import qualified Parun.Types.Ward as Ward
import qualified Parun.Types.WhenSpent as WhenSpent
import qualified Parun.Types.WhichCounters as WhichCounters
import qualified Parun.Types.WhichTurn as WhichTurn
import qualified Parun.Types.While as While
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChangePattern as ZoneChangePattern
import qualified Parun.Types.ZoneChangeR as ZoneChangeR
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
activatedAbilityNames asking onCard onAbility x = case x of
  ActivatedAbility.MkActivatedAbility cost maximumX _minimumX definedX modal restrictions _activator condition _name keyword -> costNames asking (keywordNames asking) cost <> foldMap (quantityNames asking) maximumX <> foldMap (quantityNames asking) definedX <> modalNames asking onCard onAbility modal <> foldMap (activationRestrictionNames asking) restrictions <> foldMap (conditionNames asking) condition <> foldMap (keywordNames asking) keyword

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

alternativeActivationCostNames :: (Monoid r) => Asking r -> AlternativeActivationCost.AlternativeActivationCost -> r
alternativeActivationCostNames asking x = case x of
  AlternativeActivationCost.MkAlternativeActivationCost grantedBy onlyFirst _cost -> keywordDesignatorNames asking grantedBy <> foldMap (\scope -> history asking HistoryRead.Activations <> turnScopeNames asking scope) onlyFirst

alternativeCostNames :: (Monoid r) => Asking r -> AlternativeCost.AlternativeCost -> r
alternativeCostNames asking x = case x of
  AlternativeCost.MkAlternativeCost condition cost -> foldMap (conditionNames asking) condition <> costNames asking (keywordNames asking) cost

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

backupNames :: (Monoid r) => Asking r -> (keyword -> r) -> Backup.Backup keyword -> r
backupNames _asking onKeyword x = case x of
  Backup.MkBackup _count printedAbove -> foldMap onKeyword printedAbove

beholdNames :: (Monoid r) => Asking r -> Behold.Behold keyword -> r
beholdNames asking x = case x of
  Behold.MkBehold _count whichObjects -> filterNames asking whichObjects

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

castFromZoneNames :: (Monoid r) => Asking r -> CastFromZone.CastFromZone -> r
castFromZoneNames asking x = case x of
  CastFromZone.MkCastFromZone from matching _limit _verb _pool additionalCosts _reduction _spending -> inZoneNames asking from <> filterNames asking matching <> foldMap (costComponentNames asking (keywordNames asking)) additionalCosts

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

clauseNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> Clause.Clause card ability -> r
clauseNames asking onCard onAbility x = case x of
  Clause.MkClause _ifTaken condition orElse optionality payGate effects -> foldMap (conditionNames asking) condition <> foldMap (orElseNames asking) orElse <> optionalityNames asking optionality <> foldMap (payGateNames asking) payGate <> foldMap (effectNames asking onCard onAbility) effects

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

comparesNames :: (Monoid r) => Asking r -> Compares.Compares -> r
comparesNames asking x = case x of
  Compares.MkCompares measured _comparison threshold -> quantityNames asking measured <> quantityNames asking threshold

conditionNames :: (Monoid r) => Asking r -> Condition.Condition -> r
conditionNames asking x = case x of
  Condition.Compares compares -> comparesNames asking compares
  Condition.Any items -> foldMap (conditionNames asking) items
  Condition.All items -> foldMap (conditionNames asking) items
  Condition.During duringPhase -> duringPhaseNames asking duringPhase

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
costNames asking onKeyword x = case x of
  Cost.MkCost _mana components -> foldMap (costComponentNames asking onKeyword) components

costBasisNames :: Asking r -> CostBasis.CostBasis -> r
costBasisNames asking x = case x of
  CostBasis.MkCostBasis slot _reducedBy -> slotNames asking slot

costChoiceNames :: (Monoid r) => Asking r -> CostChoice.CostChoice -> r
costChoiceNames asking x = case x of
  CostChoice.MkCostChoice unwrap -> foldMap (costNames asking (keywordNames asking)) unwrap

costComponentNames :: (Monoid r) => Asking r -> (keyword -> r) -> CostComponent.CostComponent keyword -> r
costComponentNames asking onKeyword x = case x of
  CostComponent.TapThis -> mempty
  CostComponent.UntapThis -> mempty
  CostComponent.SacrificeThis -> mempty
  CostComponent.ReturnThis -> mempty
  CostComponent.PayLife amount -> costAmountNames asking amount
  CostComponent.PayComputedLife _computed -> mempty
  CostComponent.Sacrifice sacrifice -> sacrificeNames asking sacrifice
  CostComponent.TapForTotalPower tapForTotalPower -> tapForTotalPowerNames asking tapForTotalPower
  CostComponent.TapPermanents tapPermanents -> tapPermanentsNames asking tapPermanents
  CostComponent.ReturnPermanents returnPermanents -> returnPermanentsNames asking returnPermanents
  CostComponent.ExilePermanents exilePermanents -> exilePermanentsNames asking exilePermanents
  CostComponent.DiscardCards discardCards -> discardCardsNames asking discardCards
  CostComponent.DiscardThis _discardCause -> mempty
  CostComponent.PutCardFromHandOntoBattlefield filter_ -> filterNames asking filter_
  CostComponent.PayEnergy amount -> costAmountNames asking amount
  CostComponent.AddLoyaltyToThis _natural -> history asking HistoryRead.LoyaltyActivations
  CostComponent.RemoveLoyaltyFromThis amount -> history asking HistoryRead.LoyaltyActivations <> costAmountNames asking amount
  CostComponent.RemoveCountersFromThis countersOnThis -> countersOnThisNames asking onKeyword countersOnThis
  CostComponent.RemoveCounters countersFromPermanents -> countersFromPermanentsNames asking onKeyword countersFromPermanents
  CostComponent.PutCountersOnThis countersOnThis -> countersOnThisNames asking onKeyword countersOnThis
  CostComponent.PutCountersOnPayer _ -> mempty
  CostComponent.Blight amount -> costAmountNames asking amount
  CostComponent.Forage -> mempty
  CostComponent.FlipCoin -> mempty
  CostComponent.ExileThisFrom _ -> mempty
  CostComponent.ExileThis -> mempty
  CostComponent.ExileCardsFromGraveyard exileCardsFromGraveyard -> exileCardsFromGraveyardNames asking exileCardsFromGraveyard
  CostComponent.ExileMaterials exileMaterials -> exileMaterialsNames asking exileMaterials
  CostComponent.ExileTopFromGraveyard filter_ -> filterNames asking filter_
  CostComponent.CollectEvidence _natural -> mempty
  CostComponent.CollectEvidenceOfTargets -> mempty
  CostComponent.ExileCardsTotalling exileCardsTotalling -> exileCardsTotallingNames asking exileCardsTotalling
  CostComponent.ExileCardsFromHand exileCardsFromHand -> exileCardsFromHandNames asking exileCardsFromHand
  CostComponent.RevealCardFromHand filter_ -> filterNames asking filter_
  CostComponent.Behold behold -> beholdNames asking behold
  CostComponent.BeholdAndExile filter_ -> filterNames asking filter_
  CostComponent.MillCards _natural -> mempty
  CostComponent.RevealTopOfLibrary _natural -> mempty
  CostComponent.ChooseOpponent _ -> mempty
  CostComponent.Waterbend amount -> costAmountNames asking amount
  CostComponent.WaterbendInstead _natural -> mempty

objectCountNames :: (Monoid r) => Asking r -> ObjectCount.ObjectCount -> r
objectCountNames asking x = case x of
  ObjectCount.Exactly amount -> costAmountNames asking amount
  ObjectCount.Every -> mempty

costAmountNames :: (Monoid r) => Asking r -> CostAmount.CostAmount -> r
costAmountNames asking x = case x of
  CostAmount.Fixed _natural -> mempty
  CostAmount.Announced slot -> slotNames asking slot

costReductionNames :: (Monoid r) => Asking r -> CostReduction.CostReduction -> r
costReductionNames asking x = case x of
  CostReduction.MkCostReduction _amount perEach condition whichTargets _direction -> quantityNames asking perEach <> foldMap (conditionNames asking) condition <> foldMap (filterNames asking) whichTargets

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

counterPatternNames :: (Monoid r) => Asking r -> CounterPattern.CounterPattern -> r
counterPatternNames asking x = case x of
  CounterPattern.MkCounterPattern whichKind subject whose onWhat onWho -> foldMap (counterKindNames asking (keywordNames asking)) whichKind <> counterSubjectNames asking subject <> controllerRelationNames asking whose <> filterNames asking onWhat <> foldMap (controllerRelationNames asking) onWho

counterRNames :: (Monoid r) => Asking r -> CounterR.CounterR -> r
counterRNames asking x = case x of
  CounterR.MkCounterR matching _scaling -> counterPatternNames asking matching

counterRestrictionNames :: (Monoid r) => Asking r -> CounterRestriction.CounterRestriction -> r
counterRestrictionNames asking x = case x of
  CounterRestriction.MkCounterRestriction affected kind -> affectedNames asking affected <> foldMap (counterKindNames asking (keywordNames asking)) kind

counterSubjectNames :: (Monoid r) => Asking r -> CounterSubject.CounterSubject -> r
counterSubjectNames asking x = case x of
  CounterSubject.ByEffect -> mempty
  CounterSubject.ByPlayer controllerRelation -> controllerRelationNames asking controllerRelation
  CounterSubject.ByAnything -> mempty

countersFromPermanentsNames :: (Monoid r) => Asking r -> (keyword -> r) -> CountersFromPermanents.CountersFromPermanents keyword -> r
countersFromPermanentsNames asking onKeyword x = case x of
  CountersFromPermanents.MkCountersFromPermanents count kind whichPermanent _spread -> costAmountNames asking count <> whichCountersNames asking onKeyword kind <> filterNames asking whichPermanent

countersOnThisNames :: (Monoid r) => Asking r -> (keyword -> r) -> CountersOnThis.CountersOnThis keyword -> r
countersOnThisNames asking onKeyword x = case x of
  CountersOnThis.MkCountersOnThis kind _count -> counterKindNames asking onKeyword kind

craftNames :: (Monoid r) => Asking r -> (keyword -> r) -> Craft.Craft keyword -> r
craftNames asking onKeyword x = case x of
  Craft.MkCraft cost materials -> costNames asking onKeyword cost <> exileMaterialsNames asking materials

counterProhibitionNames :: (Monoid r) => Asking r -> CounterProhibition.CounterProhibition -> r
counterProhibitionNames asking x = case x of
  CounterProhibition.MkCounterProhibition countered counterers -> filterNames asking countered <> filterNames asking counterers

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

cyclingNames :: (Monoid r) => Asking r -> (keyword -> r) -> Cycling.Cycling keyword -> r
cyclingNames asking onKeyword x = case x of
  Cycling.MkCycling cost searchFor -> costNames asking onKeyword cost <> foldMap (filterNames asking) searchFor

damagePatternNames :: (Monoid r) => Asking r -> DamagePattern.DamagePattern -> r
damagePatternNames asking x = case x of
  DamagePattern.MkDamagePattern _whichKind whatSource whatRecipient _whoRecipient whichRecipient whichSource boundRecipient -> filterNames asking whatSource <> foldMap (filterNames asking) whatRecipient <> foldMap (recipientNames asking) whichRecipient <> names asking (`elem` whichSource) <> foldMap (slotNames asking) boundRecipient

damageRNames :: (Monoid r) => Asking r -> (effect -> r) -> DamageR.DamageR effect -> r
damageRNames asking onEffect x = case x of
  DamageR.MkDamageR matching rewrite riders -> damagePatternNames asking matching <> damageRewriteNames asking onEffect rewrite <> foldMap onEffect riders

damageRewriteNames :: (Monoid r) => Asking r -> (effect -> r) -> DamageRewrite.DamageRewrite effect -> r
damageRewriteNames asking onEffect x = case x of
  DamageRewrite.PreventAll -> mempty
  DamageRewrite.PreventRemovingShieldCounter -> mempty
  DamageRewrite.PreventRemovingCounters _kind -> mempty
  DamageRewrite.PreventNext _natural -> mempty
  DamageRewrite.PreventAllBut _natural -> mempty
  DamageRewrite.PreventUpTo quantity -> quantityNames asking quantity
  DamageRewrite.SetAmount _natural -> mempty
  DamageRewrite.Scale _scaling -> mempty
  DamageRewrite.Redirect recipient -> recipientNames asking recipient
  DamageRewrite.RedirectNext _natural recipient -> recipientNames asking recipient
  DamageRewrite.RedirectMatching filter_ -> filterNames asking filter_
  DamageRewrite.RunEffects seq_ -> foldMap onEffect seq_
  DamageRewrite.TurnFaceUp -> mempty
  DamageRewrite.AlsoToSourceController -> mempty
  DamageRewrite.HealSourceFirst -> mempty

activeLaterPaymentNames :: (Monoid r) => Asking r -> ActiveLaterPayment.ActiveLaterPayment -> r
activeLaterPaymentNames asking x = case x of
  ActiveLaterPayment.MkActiveLaterPayment cost payoff _createdAt expiry -> costNames asking (keywordNames asking) cost <> preventionRiderNames asking payoff <> expiryNames asking expiry

delayedTriggerNames :: (Monoid r) => Asking r -> DelayedTrigger.DelayedTrigger -> r
delayedTriggerNames asking x = case x of
  DelayedTrigger.MkDelayedTrigger ability source _controller bindings _window expiry _createdAt -> triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty)) ability <> names asking (== source) <> foldMap (slotNames asking) (Map.keys bindings) <> foldMap (bindingNames asking) bindings <> foldMap (expiryNames asking) expiry

destructionRNames :: (Monoid r) => Asking r -> DestructionR.DestructionR -> r
destructionRNames asking x = case x of
  DestructionR.MkDestructionR matching _rewrite -> foldMap (filterNames asking) matching

devourNames :: (Monoid r) => Asking r -> Devour.Devour keyword -> r
devourNames asking x = case x of
  Devour.MkDevour quality _count -> foldMap (filterNames asking) quality

dieRollRNames :: (Monoid r) => Asking r -> DieRollR.DieRollR -> r
dieRollRNames asking x = case x of
  DieRollR.MkDieRollR whose _rewrite -> controllerRelationNames asking whose

discardCardsNames :: (Monoid r) => Asking r -> DiscardCards.DiscardCards keyword -> r
discardCardsNames asking x = case x of
  DiscardCards.MkDiscardCards _count whichCards _selection -> filterNames asking whichCards

drawCountRNames :: (Monoid r) => Asking r -> DrawCountR.DrawCountR -> r
drawCountRNames asking x = case x of
  DrawCountR.MkDrawCountR whose _atLeast _rewrite -> controllerRelationNames asking whose

drawRNames :: (Monoid r) => Asking r -> (effect -> r) -> DrawR.DrawR effect -> r
drawRNames asking onEffect x = case x of
  -- CR 504.1: the exception reads the turn's log back to the step's beginning.
  DrawR.MkDrawR whose exceptFirst rewrite -> controllerRelationNames asking whose <> (if exceptFirst then history asking HistoryRead.StepBegins else mempty) <> drawRewriteNames asking onEffect rewrite

phaseRewriteNames :: (Monoid r) => (effect -> r) -> PhaseRewrite.PhaseRewrite effect -> r
phaseRewriteNames onEffect x = case x of
  PhaseRewrite.Skip -> mempty
  PhaseRewrite.MaySkipFor effects -> foldMap onEffect effects

drawRewriteNames :: (Monoid r) => Asking r -> (effect -> r) -> DrawRewrite.DrawRewrite effect -> r
drawRewriteNames asking onEffect x = case x of
  DrawRewrite.GainLife _natural -> mempty
  DrawRewrite.FromOutsideTheGame fromOutsideTheGame -> fromOutsideTheGameNames asking fromOutsideTheGame
  DrawRewrite.Dredge _natural -> mempty
  DrawRewrite.YouDraw -> mempty
  DrawRewrite.OthersMayPayLife _natural -> mempty
  DrawRewrite.FromGraveyardOrLose -> mempty
  DrawRewrite.RevealUntilChosenKind -> mempty
  DrawRewrite.DrawInstead _natural -> mempty
  DrawRewrite.DiscardThenDrawOrMill -> mempty
  DrawRewrite.KeepOneOfTopThenDraw quantity -> quantityNames asking quantity
  DrawRewrite.MaySkipFor effects -> foldMap onEffect effects

dungeonRoomNames :: (Monoid r) => Asking r -> (card -> r) -> DungeonRoom.DungeonRoom card -> r
dungeonRoomNames asking onCard x = case x of
  DungeonRoom.MkDungeonRoom _name ability _exits -> modalNames asking onCard (grantedAbilityNames asking onCard) ability

durationNames :: (Monoid r) => Asking r -> Duration.Duration -> r
durationNames asking x = case x of
  Duration.UntilEndOfTurn -> mempty
  Duration.Indefinite -> mempty
  Duration.Perpetual -> mempty
  Duration.UntilYourNextTurn -> mempty
  Duration.UntilNextStepOf next -> playerRefNames asking (NextStepOf.player next)
  Duration.UntilEndOfYourNextTurn -> mempty
  Duration.UntilEndOfNextTurnOf playerRef -> playerRefNames asking playerRef
  Duration.DuringNextTurnOf playerRef -> playerRefNames asking playerRef
  Duration.DuringNextUntapStepOf playerRef -> playerRefNames asking playerRef
  Duration.DuringYourNextTurn -> mempty
  Duration.DuringThatExtraTurn -> mempty
  Duration.ForAsLongAs condition -> conditionNames asking condition
  Duration.UntilEndOfCombat -> mempty
  Duration.UntilEndOfCombatOnYourNextTurn -> mempty
  Duration.UntilPaid (UntilPaid.MkUntilPaid payer cost) -> playerRefNames asking payer <> costNames asking (keywordNames asking) cost
  Duration.UntilUsed slot -> foldMap (slotNames asking) slot
  Duration.UntilEvent durationEvent -> durationEventNames asking durationEvent

durationEventNames :: (Monoid r) => Asking r -> DurationEvent.DurationEvent -> r
durationEventNames asking x = case x of
  DurationEvent.SourceTurnedFaceDown -> mempty
  DurationEvent.SpellCast filter_ -> filterNames asking filter_
  DurationEvent.Planeswalked -> mempty
  DurationEvent.SourceExiledCard -> mempty
  DurationEvent.SourceLeftBattlefield -> mempty
  DurationEvent.BoundCardCast slot -> slotNames asking slot
  DurationEvent.CardCast objectId -> names asking (== objectId)

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

emergeNames :: (Monoid r) => Asking r -> (keyword -> r) -> Emerge.Emerge keyword -> r
emergeNames asking onKeyword x = case x of
  Emerge.MkEmerge cost quality -> costNames asking onKeyword cost <> foldMap (filterNames asking) quality

entersWithNames :: (Monoid r) => Asking r -> (ability -> r) -> EntersWith.EntersWith ability -> r
entersWithNames asking onAbility x = case x of
  EntersWith.MkEntersWith counters keywords abilities -> foldMap (withCountersNames asking) counters <> foldMap (keywordNames asking) keywords <> foldMap onAbility abilities

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

entryFlipNames :: (Monoid r) => Asking r -> EntryFlip.EntryFlip -> r
entryFlipNames asking x = case x of
  EntryFlip.MkEntryFlip heads tails -> entryOptionNames asking heads <> entryOptionNames asking tails

entryOptionNames :: (Monoid r) => Asking r -> EntryOption.EntryOption -> r
entryOptionNames asking x = case x of
  EntryOption.MkEntryOption _power _toughness keywords _subtypes -> foldMap (keywordNames asking) keywords

entryPriceNames :: (Monoid r) => Asking r -> EntryPrice.EntryPrice -> r
entryPriceNames asking x = case x of
  EntryPrice.PayLife _natural -> mempty
  EntryPrice.Reveal filter_ -> filterNames asking filter_
  EntryPrice.Behold filter_ -> filterNames asking filter_

entryRNames :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> EntryR.EntryR ability effect -> r
entryRNames asking onAbility onEffect x = case x of
  EntryR.MkEntryR matching rewrite -> filterNames asking matching <> entryRewriteNames asking onAbility onEffect rewrite

entryRestrictionNames :: (Monoid r) => Asking r -> EntryRestriction.EntryRestriction -> r
entryRestrictionNames asking x = case x of
  EntryRestriction.MkEntryRestriction affected _origins -> affectedNames asking affected

entryRewriteNames :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> EntryRewrite.EntryRewrite ability effect -> r
entryRewriteNames asking onAbility onEffect x = case x of
  EntryRewrite.AsCopy asCopy -> asCopyNames asking onAbility onEffect asCopy
  EntryRewrite.ChoiceOf items -> foldMap (entryOptionNames asking) items
  EntryRewrite.ChoiceByCoinFlip entryFlip -> entryFlipNames asking entryFlip
  EntryRewrite.ChooseQuality _ -> mempty
  EntryRewrite.ChoosePlayer _relation -> mempty
  EntryRewrite.ChooseCardNames filter_ -> filterNames asking filter_
  EntryRewrite.ChooseCardName filter_ -> filterNames asking filter_
  EntryRewrite.WithCounters withCounters -> withCountersNames asking withCounters
  EntryRewrite.EntersWith entersWith -> entersWithNames asking onAbility entersWith
  EntryRewrite.UnderControlOf _relation -> mempty
  EntryRewrite.SacrificeAnyNumber sacrificeAnyNumber -> sacrificeAnyNumberNames asking sacrificeAnyNumber
  EntryRewrite.SacrificeToEnter sacrificeToEnter -> sacrificeToEnterNames asking sacrificeToEnter
  EntryRewrite.ExileFromGraveyard exileFromGraveyard -> exileFromGraveyardNames asking exileFromGraveyard
  EntryRewrite.EntersAttachedTo filter_ -> filterNames asking filter_
  EntryRewrite.Riot -> mempty
  EntryRewrite.ReadAhead -> mempty
  EntryRewrite.Unleash -> mempty
  EntryRewrite.Sunburst -> mempty
  EntryRewrite.ModularSunburst -> mempty
  EntryRewrite.Bloodthirst _natural -> history asking HistoryRead.Damage
  EntryRewrite.Amplify _natural -> mempty
  EntryRewrite.Tribute _natural -> mempty
  EntryRewrite.Compleated _natural -> mempty
  EntryRewrite.Tapped -> mempty
  EntryRewrite.Prepared -> mempty
  EntryRewrite.OrTapped entryPrice -> entryPriceNames asking entryPrice
  EntryRewrite.EntersTransformed -> mempty
  EntryRewrite.RunEffects seq_ -> foldMap onEffect seq_

entryRidersNames :: (Monoid r) => Asking r -> (count -> r) -> (ability -> r) -> EntryRiders.EntryRiders count ability -> r
entryRidersNames asking onCount onAbility x = case x of
  EntryRiders.MkEntryRiders _tapped attacking blocking _transformed counters under _exiledFaceDown attachedTo faceDown _noted characteristics -> entryControllerNames asking under <> foldMap (entryAttackNames asking) attacking <> foldMap (entryBlockNames asking) blocking <> foldMap (counterKindNames asking (keywordNames asking)) (Map.keys counters) <> foldMap onCount counters <> foldMap (entryHostNames asking) attachedTo <> foldMap (faceDownStateNames asking onAbility) faceDown <> foldMap (modificationNames asking onAbility) characteristics

entryControllerNames :: (Monoid r) => Asking r -> EntryController.EntryController -> r
entryControllerNames asking x = case x of
  EntryController.Instructed player -> playerRefNames asking player
  EntryController.Owner -> mempty

equipNames :: (Monoid r) => Asking r -> (keyword -> r) -> Equip.Equip keyword -> r
equipNames asking onKeyword x = case x of
  Equip.MkEquip cost quality _target -> costNames asking onKeyword cost <> foldMap (filterNames asking) quality

exileCardsFromHandNames :: (Monoid r) => Asking r -> ExileCardsFromHand.ExileCardsFromHand keyword -> r
exileCardsFromHandNames asking x = case x of
  ExileCardsFromHand.MkExileCardsFromHand count whichCards -> costAmountNames asking count <> filterNames asking whichCards

exileCardsFromGraveyardNames :: (Monoid r) => Asking r -> ExileCardsFromGraveyard.ExileCardsFromGraveyard keyword -> r
exileCardsFromGraveyardNames asking x = case x of
  ExileCardsFromGraveyard.MkExileCardsFromGraveyard _count whichCards -> filterNames asking whichCards

exileCardsTotallingNames :: (Monoid r) => Asking r -> ExileCardsTotalling.ExileCardsTotalling keyword -> r
exileCardsTotallingNames asking x = case x of
  ExileCardsTotalling.MkExileCardsTotalling _threshold _tally whichCards -> filterNames asking whichCards

exileMaterialsNames :: (Monoid r) => Asking r -> ExileMaterials.ExileMaterials keyword -> r
exileMaterialsNames asking x = case x of
  ExileMaterials.MkExileMaterials _count _orMore whichObjects -> filterNames asking whichObjects

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

foretellCostNames :: (Monoid r) => Asking r -> (keyword -> r) -> ForetellCost.ForetellCost keyword -> r
foretellCostNames asking onKeyword x = case x of
  ForetellCost.Stated cost -> costNames asking onKeyword cost
  ForetellCost.ManaCostReducedBy _manaCost -> mempty

fromOutsideTheGameNames :: (Monoid r) => Asking r -> FromOutsideTheGame.FromOutsideTheGame -> r
fromOutsideTheGameNames asking x = case x of
  FromOutsideTheGame.MkFromOutsideTheGame _count _upTo _destination filter_ _reveal _differentIn _pick -> filterNames asking filter_

fullTextNames :: (Monoid r) => Asking r -> (ability -> r) -> FullText.FullText ability -> r
fullTextNames asking onAbility x = case x of
  FullText.MkFullText graveyard alsoHas -> playerRefNames asking graveyard <> foldMap onAbility alsoHas

-- A granted ability's slots are its OWN: each read resolves against the
-- bindings of the stack object it becomes or of the object it is granted to,
-- which Parun.Engine.Interchangeable.namedByAnother searches and whole-object
-- Eq compares. So inside one, every slot read is Searched, whatever the row
-- granting it stores.
grantedAbilityNames :: (Monoid r) => Asking r -> (card -> r) -> GrantedAbility.GrantedAbility card -> r
grantedAbilityNames asking onCard x =
  let own = asking {context = mempty}
   in case x of
        GrantedAbility.Activated activatedAbility -> activatedAbilityNames own onCard (grantedAbilityNames own onCard) activatedAbility
        GrantedAbility.Triggered triggeredAbility -> triggeredAbilityNames own onCard (grantedAbilityNames own onCard) triggeredAbility
        GrantedAbility.Static staticAbility -> staticAbilityNames own (grantedAbilityNames own onCard) staticAbility
        GrantedAbility.Rules ruleAbilities -> ruleAbilitiesNames own ruleAbilities
        GrantedAbility.Replacement printedReplacement -> printedReplacementNames own onCard (grantedAbilityNames own onCard) (effectNames own onCard (grantedAbilityNames own onCard)) printedReplacement
        GrantedAbility.Player playerStaticAbility -> playerStaticAbilityNames own playerStaticAbility
        GrantedAbility.SelfCostReduction costReduction -> costReductionNames own costReduction
        GrantedAbility.SelfAlternativeCost alternativeCost -> alternativeCostNames own alternativeCost
        GrantedAbility.SelfSpendManaAsThough clause -> spendManaAsThoughNames own clause

handActionNames :: (Monoid r) => Asking r -> (card -> r) -> HandAction.HandAction card -> r
handActionNames asking onCard x = case x of
  HandAction.MkHandAction condition effects -> foldMap (conditionNames asking) condition <> foldMap (effectNames asking onCard (grantedAbilityNames asking onCard)) effects

impendingNames :: (Monoid r) => Asking r -> (keyword -> r) -> Impending.Impending keyword -> r
impendingNames asking onKeyword x = case x of
  Impending.MkImpending _counters cost -> costNames asking onKeyword cost

inZoneNames :: (Monoid r) => Asking r -> InZone.InZone -> r
inZoneNames asking x = case x of
  InZone.MkInZone _zone player -> playerRefNames asking player

instructionRNames :: (Monoid r) => Asking r -> InstructionR.InstructionR -> r
instructionRNames asking x = case x of
  InstructionR.MkInstructionR _instruction whose _rewrite -> controllerRelationNames asking whose

keywordNames :: (Monoid r) => Asking r -> Keyword.Keyword -> r
keywordNames asking x = case x of
  Keyword.Deathtouch -> mempty
  Keyword.Defender -> mempty
  Keyword.DoubleStrike -> mempty
  Keyword.Equip equip -> equipNames asking (keywordNames asking) equip
  Keyword.FirstStrike -> mempty
  Keyword.Flash -> mempty
  Keyword.Flying -> mempty
  Keyword.Haste -> mempty
  Keyword.Hexproof filter_ -> foldMap (filterNames asking) filter_
  Keyword.Indestructible -> mempty
  Keyword.Intimidate -> mempty
  Keyword.Landwalk filter_ -> filterNames asking filter_
  Keyword.Lifelink -> mempty
  Keyword.LivingMetal -> mempty
  Keyword.MoreThanMeetsTheEye cost -> costNames asking (keywordNames asking) cost
  Keyword.Protection protection -> protectionNames asking protection
  Keyword.Reach -> mempty
  Keyword.Shroud -> mempty
  Keyword.Trample -> mempty
  Keyword.TrampleOverPlaneswalkers -> mempty
  Keyword.Vigilance -> mempty
  Keyword.Ward ward -> wardNames asking (keywordNames asking) ward
  Keyword.Banding -> mempty
  Keyword.Rampage _natural -> mempty
  Keyword.CumulativeUpkeep cost -> costNames asking (keywordNames asking) cost
  Keyword.Flanking -> mempty
  Keyword.Phasing -> mempty
  Keyword.Buyback cost -> costNames asking (keywordNames asking) cost
  Keyword.Shadow -> mempty
  Keyword.Cycling cycling -> cyclingNames asking (keywordNames asking) cycling
  Keyword.Echo cost -> costNames asking (keywordNames asking) cost
  Keyword.Horsemanship -> mempty
  Keyword.Fading _natural -> mempty
  Keyword.Kicker cost -> costNames asking (keywordNames asking) cost
  Keyword.Multikicker cost -> costNames asking (keywordNames asking) cost
  Keyword.StickerKicker cost -> costNames asking (keywordNames asking) cost
  Keyword.Flashback cost -> costNames asking (keywordNames asking) cost
  Keyword.Fear -> mempty
  Keyword.Morph morph -> morphNames asking (keywordNames asking) morph
  Keyword.Amplify _natural -> mempty
  Keyword.Provoke -> mempty
  Keyword.Storm -> history asking HistoryRead.Casts
  Keyword.Affinity filter_ -> filterNames asking filter_
  Keyword.Entwine cost -> costNames asking (keywordNames asking) cost
  Keyword.Modular _natural -> mempty
  Keyword.Sunburst -> mempty
  Keyword.Bushido _natural -> mempty
  Keyword.Soulshift _natural -> mempty
  Keyword.Splice splice -> spliceNames asking (keywordNames asking) splice
  Keyword.Offering filter_ -> filterNames asking filter_
  Keyword.Ninjutsu (Ninjutsu.MkNinjutsu cost _variant) -> costNames asking (keywordNames asking) cost
  Keyword.Epic -> mempty
  Keyword.Paradigm -> mempty
  Keyword.Convoke -> mempty
  Keyword.Dredge _natural -> mempty
  Keyword.Bloodthirst _natural -> history asking HistoryRead.Damage
  Keyword.Haunt -> mempty
  Keyword.Replicate cost -> costNames asking (keywordNames asking) cost
  Keyword.Graft _natural -> mempty
  Keyword.Recover cost -> costNames asking (keywordNames asking) cost
  Keyword.Ripple _natural -> mempty
  Keyword.SplitSecond -> mempty
  Keyword.Suspend suspend -> foldMap (suspendNames asking (keywordNames asking)) suspend
  Keyword.Vanishing _natural -> mempty
  Keyword.Absorb _natural -> mempty
  Keyword.AuraSwap cost -> costNames asking (keywordNames asking) cost
  Keyword.Delve -> mempty
  Keyword.Fortify cost -> costNames asking (keywordNames asking) cost
  Keyword.Frenzy _natural -> mempty
  Keyword.Gravestorm -> history asking HistoryRead.Deaths
  Keyword.Poisonous _natural -> mempty
  Keyword.Champion filter_ -> filterNames asking filter_
  Keyword.Changeling -> mempty
  Keyword.Evoke cost -> costNames asking (keywordNames asking) cost
  Keyword.Hideaway _natural -> mempty
  Keyword.Prowl cost -> history asking HistoryRead.Damage <> costNames asking (keywordNames asking) cost
  Keyword.Reinforce reinforce -> reinforceNames asking (keywordNames asking) reinforce
  Keyword.Conspire -> mempty
  Keyword.Persist -> mempty
  Keyword.Wither -> mempty
  Keyword.Devour devour -> devourNames asking devour
  Keyword.Exalted -> mempty
  Keyword.Unearth cost -> costNames asking (keywordNames asking) cost
  Keyword.Cascade -> mempty
  Keyword.Annihilator _natural -> mempty
  Keyword.LevelUp cost -> costNames asking (keywordNames asking) cost
  Keyword.Infect -> mempty
  Keyword.BattleCry -> mempty
  Keyword.LivingWeapon -> mempty
  Keyword.Undying -> mempty
  Keyword.Miracle cost -> costNames asking (keywordNames asking) cost
  Keyword.Soulbond -> mempty
  Keyword.Overload cost -> costNames asking (keywordNames asking) cost
  Keyword.Unleash -> mempty
  Keyword.Cipher -> mempty
  Keyword.Evolve -> mempty
  Keyword.Extort -> mempty
  Keyword.Fuse -> mempty
  Keyword.Tribute _natural -> mempty
  Keyword.Dethrone -> mempty
  Keyword.HiddenAgenda -> mempty
  Keyword.Outlast cost -> costNames asking (keywordNames asking) cost
  Keyword.Prowess -> mempty
  Keyword.Dash cost -> costNames asking (keywordNames asking) cost
  Keyword.Exploit -> mempty
  Keyword.Menace -> mempty
  Keyword.Renown _natural -> mempty
  Keyword.Awaken cost -> costNames asking (keywordNames asking) cost
  Keyword.Devoid -> mempty
  Keyword.Ingest -> mempty
  Keyword.Myriad -> mempty
  Keyword.Surge cost -> history asking HistoryRead.Casts <> costNames asking (keywordNames asking) cost
  Keyword.Skulk -> mempty
  Keyword.SpaceSculptor -> mempty
  Keyword.Emerge emerge -> emergeNames asking (keywordNames asking) emerge
  Keyword.Escalate cost -> costNames asking (keywordNames asking) cost
  Keyword.Melee -> mempty
  Keyword.Crew _natural -> mempty
  Keyword.Fabricate _natural -> mempty
  Keyword.Partner -> mempty
  Keyword.PartnerText _partnerText -> mempty
  Keyword.PartnerWith _cardName -> mempty
  Keyword.ChooseABackground -> mempty
  Keyword.DoctorsCompanion -> mempty
  Keyword.Undaunted -> mempty
  Keyword.Improvise -> mempty
  Keyword.Aftermath -> mempty
  Keyword.Embalm cost -> costNames asking (keywordNames asking) cost
  Keyword.Eternalize cost -> costNames asking (keywordNames asking) cost
  Keyword.Afflict _natural -> mempty
  Keyword.Ascend -> mempty
  Keyword.Assist -> mempty
  Keyword.JumpStart -> mempty
  Keyword.Mentor -> mempty
  Keyword.Afterlife _natural -> mempty
  Keyword.Riot -> mempty
  Keyword.Spectacle cost -> history asking HistoryRead.LifeLosses <> costNames asking (keywordNames asking) cost
  Keyword.Escape cost -> costNames asking (keywordNames asking) cost
  Keyword.Companion (CompanionCondition.EachCard filter_) -> filterNames asking filter_
  Keyword.Companion (CompanionCondition.CardsOverMinimum _) -> mempty
  Keyword.Companion (CompanionCondition.DifferentNames filter_) -> filterNames asking filter_
  Keyword.Companion (CompanionCondition.SharedCardType filter_) -> filterNames asking filter_
  Keyword.Foretell foretellCost -> foretellCostNames asking (keywordNames asking) foretellCost
  Keyword.Demonstrate -> mempty
  Keyword.Daybound -> mempty
  Keyword.Nightbound -> mempty
  Keyword.Decayed -> mempty
  Keyword.Cleave cost -> costNames asking (keywordNames asking) cost
  Keyword.Training -> mempty
  Keyword.Compleated -> mempty
  Keyword.Reconfigure cost -> costNames asking (keywordNames asking) cost
  Keyword.Blitz cost -> costNames asking (keywordNames asking) cost
  Keyword.Casualty _natural -> mempty
  Keyword.ReadAhead -> history asking HistoryRead.Entries
  Keyword.Ravenous -> mempty
  Keyword.Squad cost -> costNames asking (keywordNames asking) cost
  Keyword.Prototype _prototype -> mempty
  Keyword.ForMirrodin -> mempty
  Keyword.Toxic _natural -> mempty
  Keyword.Backup backup -> backupNames asking (keywordNames asking) backup
  Keyword.Bargain -> mempty
  Keyword.Craft craft -> craftNames asking (keywordNames asking) craft
  Keyword.Disguise cost -> costNames asking (keywordNames asking) cost
  Keyword.Plot cost -> costNames asking (keywordNames asking) cost
  Keyword.Saddle _natural -> mempty
  Keyword.Spree -> mempty
  Keyword.Offspring cost -> costNames asking (keywordNames asking) cost
  Keyword.Gift _gift -> mempty
  Keyword.Freerunning cost -> history asking HistoryRead.Damage <> costNames asking (keywordNames asking) cost
  Keyword.Impending impending -> impendingNames asking (keywordNames asking) impending
  Keyword.Exhaust -> mempty
  Keyword.Boast -> history asking HistoryRead.Attacks
  Keyword.PowerUp -> history asking HistoryRead.Entries
  Keyword.ClassLevel _classLevel -> mempty
  Keyword.Forecast -> mempty
  Keyword.StartYourEngines -> mempty
  Keyword.JobSelect -> mempty
  Keyword.Tiered -> mempty
  Keyword.Exert -> mempty
  Keyword.Enlist -> mempty
  Keyword.Bestow cost -> costNames asking (keywordNames asking) cost
  Keyword.Station -> mempty
  Keyword.Mutate cost -> costNames asking (keywordNames asking) cost
  Keyword.UmbraArmor -> mempty
  Keyword.Retrace -> mempty
  Keyword.Mayhem cost -> history asking HistoryRead.Discards <> foldMap (costNames asking (keywordNames asking)) cost
  Keyword.Madness madnessCost -> madnessCostNames asking (keywordNames asking) madnessCost
  Keyword.Rebound -> mempty
  Keyword.Scavenge cost -> costNames asking (keywordNames asking) cost
  Keyword.Encore cost -> costNames asking (keywordNames asking) cost
  Keyword.Teamwork _natural -> mempty
  Keyword.WebSlinging cost -> costNames asking (keywordNames asking) cost
  Keyword.Sneak cost -> costNames asking (keywordNames asking) cost
  Keyword.Increment -> mempty
  Keyword.Storied -> mempty
  Keyword.Mobilize keywordCount -> keywordCountNames asking keywordCount
  Keyword.Firebending keywordCount -> keywordCountNames asking keywordCount
  Keyword.Transmute cost -> costNames asking (keywordNames asking) cost
  Keyword.Transfigure cost -> costNames asking (keywordNames asking) cost
  Keyword.Warp cost -> costNames asking (keywordNames asking) cost
  Keyword.Disturb cost -> costNames asking (keywordNames asking) cost
  Keyword.Harmonize cost -> costNames asking (keywordNames asking) cost
  Keyword.BeamMeUp cost -> costNames asking (keywordNames asking) cost

keywordCountNames :: (Monoid r) => Asking r -> KeywordCount.KeywordCount keyword -> r
keywordCountNames asking x = case x of
  KeywordCount.Fixed _natural -> mempty
  KeywordCount.Power -> mempty
  KeywordCount.Tally keywordTally -> keywordTallyNames asking keywordTally
  KeywordCount.PlayerCounters playerCounterTally -> playerCounterTallyNames asking playerCounterTally

keywordDesignatorNames :: (Monoid r) => Asking r -> KeywordDesignator.KeywordDesignator Keyword.Keyword -> r
keywordDesignatorNames asking x = case x of
  KeywordDesignator.OfFamily _keywordFamily -> mempty
  KeywordDesignator.OfKeyword keyword -> keywordNames asking keyword
  KeywordDesignator.Printed _keywordFamily -> mempty
  KeywordDesignator.OfInstance keyword _ordinal -> keywordNames asking keyword

keywordTallyNames :: (Monoid r) => Asking r -> KeywordTally.KeywordTally keyword -> r
keywordTallyNames asking x = case x of
  KeywordTally.MkKeywordTally scope filter_ -> scopeNames asking scope <> filterNames asking filter_

libraryPlacementNames :: (Monoid r) => Asking r -> LibraryPlacement.LibraryPlacement -> r
libraryPlacementNames asking x = case x of
  LibraryPlacement.Stated _libraryPosition -> mempty
  LibraryPlacement.OwnerChooses -> mempty
  LibraryPlacement.RandomOrder _libraryPosition -> mempty
  LibraryPlacement.ArrangedBy (ArrangedArrival.MkArrangedArrival _libraryPosition chooser) -> playerRefNames asking chooser
  LibraryPlacement.Beneath quantity -> quantityNames asking quantity
  LibraryPlacement.BeneathOrBottom quantity -> quantityNames asking quantity

lifeGainRNames :: (Monoid r) => Asking r -> LifeGainR.LifeGainR -> r
lifeGainRNames asking x = case x of
  LifeGainR.MkLifeGainR whose _rewrite -> controllerRelationNames asking whose

lifeLossPatternNames :: (Monoid r) => Asking r -> LifeLossPattern.LifeLossPattern -> r
lifeLossPatternNames asking x = case x of
  LifeLossPattern.MkLifeLossPattern whose _whichCause -> controllerRelationNames asking whose

lifeLossRNames :: (Monoid r) => Asking r -> LifeLossR.LifeLossR -> r
lifeLossRNames asking x = case x of
  LifeLossR.MkLifeLossR matching _rewrite -> lifeLossPatternNames asking matching

limitUnlessNames :: (Monoid r) => Asking r -> LimitUnless.LimitUnless -> r
limitUnlessNames asking x = case x of
  LimitUnless.MkLimitUnless _limit _comparison attackers unless -> foldMap (affectedNames asking) attackers <> foldMap (conditionNames asking) unless

madnessCostNames :: (Monoid r) => Asking r -> (keyword -> r) -> MadnessCost.MadnessCost keyword -> r
madnessCostNames asking onKeyword x = case x of
  MadnessCost.Stated cost -> costNames asking onKeyword cost
  MadnessCost.OwnManaCost -> mempty

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

hexproofExemptionNames :: (Monoid r) => Asking r -> HexproofExemption.HexproofExemption -> r
hexproofExemptionNames asking x = case x of
  HexproofExemption.MkHexproofExemption permanents _players -> filterNames asking permanents

tapForManaNames :: (Monoid r) => Asking r -> TapForMana.TapForMana -> r
tapForManaNames asking x = case x of
  TapForMana.MkTapForMana permanents restriction -> filterNames asking permanents <> foldMap (manaRestrictionNames asking) restriction

manaRestrictionNames :: (Monoid r) => Asking r -> ManaRestriction.ManaRestriction -> r
manaRestrictionNames asking x = case x of
  ManaRestriction.MkManaRestriction casts activations keywordActivations unlocks turnsFaceUp _prohibits _paysGeneric -> foldMap (filterNames asking) casts <> foldMap (filterNames asking) activations <> foldMap (keywordDesignatorNames asking) keywordActivations <> foldMap (filterNames asking) unlocks <> foldMap (filterNames asking) turnsFaceUp

manaRiderNames :: (Monoid r) => Asking r -> ManaRider.ManaRider -> r
manaRiderNames asking x = case x of
  ManaRider.MkManaRider condition _effect -> filterNames asking condition

measuresNames :: (Monoid r) => Asking r -> Measures.Measures -> r
measuresNames asking x = case x of
  Measures.MkMeasures _measure _comparison operand -> operandNames asking operand

modalNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> Modal.Modal card ability -> r
modalNames asking onCard onAbility x = case x of
  Modal.MkModal modes selection _memory -> foldMap (modeNames asking onCard onAbility) modes <> modeSelectionNames asking selection

modeSelectionNames :: (Monoid r) => Asking r -> ModeSelection.ModeSelection -> r
modeSelectionNames asking x = case x of
  ModeSelection.ChooseExactly _ -> mempty
  ModeSelection.ChooseExactlyWithRepeats _ -> mempty
  ModeSelection.ChooseBetween _ -> mempty
  ModeSelection.ChooseInstead (ChooseInstead.MkChooseInstead condition ordinary instead) -> conditionNames asking condition <> modeSelectionNames asking ordinary <> modeSelectionNames asking instead
  ModeSelection.ChooseUpTo quantity -> quantityNames asking quantity

modeNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> Mode.Mode card ability -> r
modeNames asking onCard onAbility x = case x of
  Mode.MkMode clauses targetSlots -> foldMap (clauseNames asking onCard onAbility) clauses <> foldMap (slotNames asking) (Map.keys targetSlots) <> foldMap (targetSlotNames asking) targetSlots

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

modifiedRollNames :: (Monoid r) => Asking r -> ModifiedRoll.ModifiedRoll -> r
modifiedRollNames asking x = case x of
  ModifiedRoll.MkModifiedRoll _sides _natural _modifier cost _limit -> foldMap (costNames asking (keywordNames asking)) cost

modifyPowerToughnessNames :: (Monoid r) => Asking r -> ModifyPowerToughness.ModifyPowerToughness -> r
modifyPowerToughnessNames asking x = case x of
  ModifyPowerToughness.MkModifyPowerToughness power toughness -> quantityNames asking power <> quantityNames asking toughness

morphNames :: (Monoid r) => Asking r -> (keyword -> r) -> Morph.Morph keyword -> r
morphNames asking onKeyword x = case x of
  Morph.MkMorph cost _variant -> costNames asking onKeyword cost

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

optionalityNames :: (Monoid r) => Asking r -> Optionality.Optionality -> r
optionalityNames asking x = case x of
  Optionality.Mandatory -> mempty
  Optionality.Optional playerRef -> playerRefNames asking playerRef

orElseNames :: (Monoid r) => Asking r -> OrElse.OrElse -> r
orElseNames asking x = case x of
  OrElse.MkOrElse _siblings chooser _villainous -> playerRefNames asking chooser

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

playerCounterTallyNames :: (Monoid r) => Asking r -> PlayerCounterTally.PlayerCounterTally -> r
playerCounterTallyNames asking x = case x of
  PlayerCounterTally.MkPlayerCounterTally player _kind -> playerRefNames asking player

activationCriteriaNames :: (Monoid r) => Asking r -> ActivationCriteria.ActivationCriteria -> r
activationCriteriaNames asking x = case x of
  ActivationCriteria.MkActivationCriteria grantedBy _whichKind _whichLoyalty _whichAbility -> foldMap (keywordDesignatorNames asking) grantedBy

costAdditionNames :: (Monoid r) => Asking r -> CostAddition.CostAddition -> r
costAdditionNames asking x = case x of
  CostAddition.MkCostAddition components _scale -> foldMap (costComponentNames asking (keywordNames asking)) components

costChangeNames :: (Monoid r) => Asking r -> CostChange.CostChange -> r
costChangeNames asking x = case x of
  CostChange.Increase _manaCost -> mempty
  CostChange.Reduce _appliedReduction -> mempty
  CostChange.Add costAddition -> costAdditionNames asking costAddition

perTargetNames :: (Monoid r) => Asking r -> PerTarget.PerTarget -> r
perTargetNames asking x = case x of
  PerTarget.MkPerTarget matching each -> filterNames asking matching <> foldMap (quantityNames asking) each

costModifierNames :: (Monoid r) => Asking r -> CostModifier.CostModifier -> r
costModifierNames asking x = case x of
  CostModifier.MkCostModifier subject matching whichTargets perTarget perEach onlyFirst change -> foldMap (\scope -> history asking (costSubjectHistory subject) <> turnScopeNames asking scope) onlyFirst <> costSubjectNames asking subject <> filterNames asking matching <> foldMap (filterNames asking) whichTargets <> foldMap (perTargetNames asking) perTarget <> foldMap (quantityNames asking) perEach <> costChangeNames asking change

-- The turn's history a cost modifier's `onlyFirst` reads: CR 601.2i's casts
-- for a spell, CR 602.2's activations for an ability.
costSubjectHistory :: CostSubject.CostSubject -> HistoryRead.HistoryRead
costSubjectHistory x = case x of
  CostSubject.Spells -> HistoryRead.Casts
  CostSubject.Activations _ -> HistoryRead.Activations

costSubjectNames :: (Monoid r) => Asking r -> CostSubject.CostSubject -> r
costSubjectNames asking x = case x of
  CostSubject.Spells -> mempty
  CostSubject.Activations activationCriteria -> activationCriteriaNames asking activationCriteria

playerEffectNames :: (Monoid r) => Asking r -> PlayerEffect.PlayerEffect -> r
playerEffectNames asking x = case x of
  PlayerEffect.CantCastSpells -> mempty
  PlayerEffect.CantActivateAbilities keywordDesignator filter_ _kind -> foldMap (keywordDesignatorNames asking) keywordDesignator <> filterNames asking filter_
  PlayerEffect.CantCastMoreThan _natural -> history asking HistoryRead.Casts
  PlayerEffect.ModifyCost costModifier -> costModifierNames asking costModifier
  PlayerEffect.AlternativeActivationCost alternativeActivationCost -> alternativeActivationCostNames asking alternativeActivationCost
  PlayerEffect.PlayAdditionalLands _allowance -> mempty
  PlayerEffect.NoMaximumHandSize -> mempty
  PlayerEffect.SetMaximumHandSize _natural -> mempty
  PlayerEffect.IncreaseMaximumHandSize _natural -> mempty
  PlayerEffect.ReduceMaximumHandSize _natural -> mempty
  PlayerEffect.DontLoseUnspentMana _manaFilter -> mempty
  PlayerEffect.LoseLifeForUnspentMana -> mempty
  PlayerEffect.MayPayLifeForMana {} -> mempty
  PlayerEffect.SpendManaAsThough clause -> spendManaAsThoughNames asking clause
  PlayerEffect.CantBeTargetedBy _playerScope quality -> foldMap (filterNames asking) quality
  PlayerEffect.HasProtectionFrom filter_ -> filterNames asking filter_
  PlayerEffect.CastAsThoughItHadFlash filter_ onlyFirst -> foldMap (\scope -> history asking HistoryRead.Casts <> turnScopeNames asking scope) onlyFirst <> filterNames asking filter_
  PlayerEffect.NextSpell filter_ -> filterNames asking filter_
  PlayerEffect.MayPlayAsThoughItHadFlash filter_ -> filterNames asking filter_
  PlayerEffect.KeywordRiderGrant grant ->
    keywordDesignatorNames asking (KeywordRiderGrant.abilities grant) <> filterNames asking (KeywordRiderGrant.permanents grant)
  -- A once-per-turn grant is spent off CR 602.2's log, by activator
  -- (Activatable.graceUsed).
  PlayerEffect.LoyaltyGrant grant ->
    filterNames asking (LoyaltyGrant.permanents grant) <> case LoyaltyGrant.relaxes grant of
      LoyaltyClause.Window -> mempty
      LoyaltyClause.OncePerTurn -> history asking HistoryRead.Activations
  PlayerEffect.MayTapForMana tapForMana -> tapForManaNames asking tapForMana
  PlayerEffect.CantBeCountered counterProhibition -> counterProhibitionNames asking counterProhibition
  PlayerEffect.RestrictTargeting targetRestriction -> targetRestrictionNames asking (filterNames asking) targetRestriction
  PlayerEffect.TargetPastHexproof hexproofExemption -> hexproofExemptionNames asking hexproofExemption
  PlayerEffect.DamageCantBePrevented damagePattern -> damagePatternNames asking damagePattern
  PlayerEffect.DamageCantBeRedirected damagePattern -> damagePatternNames asking damagePattern
  PlayerEffect.CantSearchLibraries _cantSearchLibraries -> mempty
  PlayerEffect.CantBecomeMonarch -> mempty
  PlayerEffect.StateBasedActionDoesntApply _ -> mempty
  PlayerEffect.CantSetSchemesInMotion -> mempty
  PlayerEffect.EntriesTrigger entries -> filterNames asking (EntriesTrigger.entrants entries)
  PlayerEffect.AdditionalTriggers bearers -> filterNames asking bearers
  PlayerEffect.ControlledWhileSearching -> mempty
  PlayerEffect.BlocksFromPiles -> mempty
  PlayerEffect.YouChoose _declaration -> mempty
  PlayerEffect.ExilesFoundWhileSearching -> mempty
  PlayerEffect.CantAttackWithCreatures -> mempty
  PlayerEffect.CantCastMatching filter_ -> filterNames asking filter_
  PlayerEffect.CastOnlyAtSorcerySpeed -> mempty
  PlayerEffect.CantPlayLands filter_ -> filterNames asking filter_
  PlayerEffect.CastFrom castFromZone -> castFromZoneNames asking castFromZone
  PlayerEffect.PlayLandsFrom inZone -> inZoneNames asking inZone
  PlayerEffect.PlotFrom plotFromZone -> plotFromZoneNames asking plotFromZone
  PlayerEffect.CastFromHandWithoutPayingManaCost freeCastFromHand -> freeCastFromHandNames asking freeCastFromHand
  PlayerEffect.CantGetCounters _playerCounterKind -> mempty
  PlayerEffect.StateCoinFlip statedFlip -> statedFlipNames asking statedFlip
  PlayerEffect.ModifyDieRoll modifiedRoll -> modifiedRollNames asking modifiedRoll
  PlayerEffect.AdditionalVotes _extraVotes -> mempty
  PlayerEffect.AdditionalSurveilCards _natural -> mempty
  PlayerEffect.PutTappedAsYouCascade filter_ -> filterNames asking filter_
  PlayerEffect.CantGainLife -> mempty
  PlayerEffect.CantLoseLife -> mempty
  PlayerEffect.RevealsDraw _reveal -> mempty
  PlayerEffect.Precluded _outcome -> mempty
  PlayerEffect.Shows _shows -> mempty

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
playerStaticAbilityNames asking x = case x of
  PlayerStaticAbility.MkPlayerStaticAbility _scope condition _name effect -> foldMap (conditionNames asking) condition <> playerEffectNames asking effect

freeCastFromHandNames :: (Monoid r) => Asking r -> FreeCastFromHand.FreeCastFromHand -> r
freeCastFromHandNames asking x = case x of
  FreeCastFromHand.MkFreeCastFromHand spells amount -> filterNames asking spells <> foldMap (quantityNames asking) amount

plotFromZoneNames :: (Monoid r) => Asking r -> PlotFromZone.PlotFromZone -> r
plotFromZoneNames asking x = case x of
  PlotFromZone.MkPlotFromZone from matching -> inZoneNames asking from <> filterNames asking matching

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
printedReplacementNames asking onCard onAbility onEffect x = case x of
  PrintedReplacement.MkPrintedReplacement condition effect _functionsFrom _name -> foldMap (conditionNames asking) condition <> replacementEffectNames asking onCard onAbility onEffect effect

projectedCharacteristicsNames :: (Monoid r) => Asking r -> ProjectedCharacteristics.ProjectedCharacteristics -> r
projectedCharacteristicsNames asking x = case x of
  ProjectedCharacteristics.MkProjectedCharacteristics _names _supertypes keywords _colors _manaCost _manaValue _power _toughness _loyalty _defense _intensity characteristicPT _cardTypes _subtypes staticAbilities playerAbilities grantedPlayerAbilities grantedStaticAbilities grantedRuleAbilities specialActions activatedAbilities replacementEffects triggeredAbilities delayedAbilities enchant castingPermissions ruleAbilities _lostAllAbilities _hasFullText _wordChanges textChangedKeywords _combatDamageAmount _combatDamageOptions _grantsStationToughness additionalCosts additionalCostChoices alternativeCosts costReductions grantedCostReductions grantedAlternativeCosts _grantedSpendManaAsThough _halves exceptions mergedDonors prepare alternativeSpell _prototyped spell _circled _definesChosenColor flipped -> foldMap (keywordNames asking) (Map.keys keywords) <> foldMap (characteristicPTNames asking) characteristicPT <> foldMap (staticAbilityNames asking (grantedAbilityNames asking (const mempty))) staticAbilities <> foldMap (playerStaticAbilityNames asking) playerAbilities <> foldMap (playerStaticAbilityNames asking . snd) grantedPlayerAbilities <> foldMap (staticAbilityNames asking (grantedAbilityNames asking (const mempty)) . snd) grantedStaticAbilities <> ruleAbilitiesNames asking grantedRuleAbilities <> foldMap (specialActionNames asking) specialActions <> foldMap (activatedAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) activatedAbilities <> foldMap (printedReplacementNames asking (const mempty) (grantedAbilityNames asking (const mempty)) (effectNames asking (const mempty) (grantedAbilityNames asking (const mempty)))) replacementEffects <> foldMap (triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) triggeredAbilities <> foldMap (triggeredAbilityNames asking (const mempty) (grantedAbilityNames asking (const mempty))) delayedAbilities <> foldMap (targetSlotNames asking) enchant <> foldMap (castingPermissionNames asking) castingPermissions <> ruleAbilitiesNames asking ruleAbilities <> foldMap (keywordNames asking) (Map.keys textChangedKeywords) <> foldMap (costComponentNames asking (keywordNames asking)) additionalCosts <> foldMap (costChoiceNames asking) additionalCostChoices <> foldMap (alternativeCostNames asking) alternativeCosts <> foldMap (costReductionNames asking) costReductions <> foldMap (costReductionNames asking) grantedCostReductions <> foldMap (alternativeCostNames asking) grantedAlternativeCosts <> foldMap (copyExceptionNames asking (grantedAbilityNames asking (const mempty))) exceptions <> foldMap (projectedCharacteristicsNames asking) mergedDonors <> foldMap (faceNames asking (const mempty)) prepare <> foldMap (faceNames asking (const mempty)) alternativeSpell <> modalNames asking (const mempty) (grantedAbilityNames asking (const mempty)) spell <> foldMap (projectedCharacteristicsNames asking) flipped

protectionNames :: (Monoid r) => Asking r -> Protection.Protection keyword -> r
protectionNames asking x = case x of
  Protection.MkProtection quality spares -> filterNames asking quality <> foldMap (filterNames asking) spares

-- Every slot, PlayerRef, Scope, Filter, keyword and nested quantity through
-- Parun.Engine.Children; the arms below are the history reads (CR 733.3) and
-- prevention reads (CR 615.13) a quantity makes of its own, beside them.
quantityNames :: (Monoid r) => Asking r -> Quantity.Quantity -> r
quantityNames asking x =
  let own = case x of
        Quantity.WasBlockedThisTurn -> history asking HistoryRead.Blocks
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

reinforceNames :: (Monoid r) => Asking r -> (keyword -> r) -> Reinforce.Reinforce keyword -> r
reinforceNames asking onKeyword x = case x of
  Reinforce.MkReinforce _amount cost -> costNames asking onKeyword cost

removalCountNames :: (Monoid r) => Asking r -> RemovalCount.RemovalCount -> r
removalCountNames asking x = case x of
  RemovalCount.Exactly quantity -> quantityNames asking quantity
  RemovalCount.UpTo quantity -> quantityNames asking quantity
  RemovalCount.AnyNumber -> mempty
  RemovalCount.All -> mempty

replacementEffectNames :: (Monoid r) => Asking r -> (card -> r) -> (ability -> r) -> (effect -> r) -> ReplacementEffect.ReplacementEffect card ability effect -> r
replacementEffectNames asking onCard onAbility onEffect x = case x of
  ReplacementEffect.ZoneChangeR zoneChangeR -> zoneChangeRNames asking onEffect zoneChangeR
  ReplacementEffect.EntryR entryR -> entryRNames asking onAbility onEffect entryR
  ReplacementEffect.DamageR damageR -> damageRNames asking onEffect damageR
  ReplacementEffect.DestructionR destructionR -> destructionRNames asking destructionR
  ReplacementEffect.CounterR counterR -> counterRNames asking counterR
  ReplacementEffect.TokenR tokenR -> tokenRNames asking onCard tokenR
  ReplacementEffect.TurnUpR turnUpR -> turnUpRNames asking onAbility onEffect turnUpR
  ReplacementEffect.AttachR attachR -> attachRNames asking onAbility onEffect attachR
  ReplacementEffect.UntapR untapR -> untapRNames asking untapR
  ReplacementEffect.LifeLossR lifeLossR -> lifeLossRNames asking lifeLossR
  ReplacementEffect.LifeGainR lifeGainR -> lifeGainRNames asking lifeGainR
  ReplacementEffect.DrawR drawR -> drawRNames asking onEffect drawR
  ReplacementEffect.DrawCountR drawCountR -> drawCountRNames asking drawCountR
  ReplacementEffect.InstructionR instructionR -> instructionRNames asking instructionR
  ReplacementEffect.DieRollR dieRollR -> dieRollRNames asking dieRollR
  ReplacementEffect.PerformerR row -> filterNames asking (PerformerR.performer row)
  ReplacementEffect.ManaR (ManaR.MkManaR matching _) -> filterNames asking matching
  ReplacementEffect.GameLossR (GameLossR.MkGameLossR whose effects) -> controllerRelationNames asking whose <> foldMap onEffect effects
  ReplacementEffect.PlaneswalkR (PlaneswalkR.MkPlaneswalkR whose _cause rewrite) ->
    controllerRelationNames asking whose <> case rewrite of
      PlaneswalkRewrite.ArrangeTopTwo -> mempty
      PlaneswalkRewrite.RunEffects effects -> foldMap onEffect effects
  ReplacementEffect.PhaseR (PhaseR.MkPhaseR (PhasePattern.MkPhasePattern _whichPhase whose) rewrite) -> controllerRelationNames asking whose <> phaseRewriteNames onEffect rewrite

exilePermanentsNames :: (Monoid r) => Asking r -> ExilePermanents.ExilePermanents keyword -> r
exilePermanentsNames asking x = case x of
  ExilePermanents.MkExilePermanents _count whichPermanents -> filterNames asking whichPermanents

returnPermanentsNames :: (Monoid r) => Asking r -> ReturnPermanents.ReturnPermanents keyword -> r
returnPermanentsNames asking x = case x of
  ReturnPermanents.MkReturnPermanents _count whichPermanents -> filterNames asking whichPermanents

ruleAbilitiesNames :: (Monoid r) => Asking r -> RuleAbilities.RuleAbilities -> r
ruleAbilitiesNames asking x = case x of
  RuleAbilities.MkRuleAbilities activationProhibitions attachRestrictions attackCosts rulePermissions attackRequirements blockCosts blockPermissions blockRequirements combatRestrictions counterRestrictions crewRestrictions entryRestrictions sacrificeRestrictions targetRestrictions untapRestrictions -> foldMap (activationProhibitionNames asking) activationProhibitions <> foldMap (attachRestrictionNames asking) attachRestrictions <> foldMap (attackCostNames asking) attackCosts <> foldMap (rulePermissionNames asking) rulePermissions <> foldMap (attackRequirementNames asking) attackRequirements <> foldMap (blockCostNames asking) blockCosts <> foldMap (blockPermissionNames asking) blockPermissions <> foldMap (blockRequirementNames asking) blockRequirements <> foldMap (combatRestrictionNames asking) combatRestrictions <> foldMap (counterRestrictionNames asking) counterRestrictions <> foldMap (crewRestrictionNames asking) crewRestrictions <> foldMap (entryRestrictionNames asking) entryRestrictions <> foldMap (sacrificeRestrictionNames asking) sacrificeRestrictions <> foldMap (targetRestrictionNames asking (affectedNames asking)) targetRestrictions <> foldMap (untapRestrictionNames asking) untapRestrictions

sacrificeNames :: (Monoid r) => Asking r -> Sacrifice.Sacrifice keyword -> r
sacrificeNames asking x = case x of
  Sacrifice.MkSacrifice count whichPermanents -> objectCountNames asking count <> filterNames asking whichPermanents

exileFromGraveyardNames :: (Monoid r) => Asking r -> ExileFromGraveyard.ExileFromGraveyard -> r
exileFromGraveyardNames asking x = case x of
  ExileFromGraveyard.MkExileFromGraveyard filter_ _players count _orGraveyard kinds each -> filterNames asking filter_ <> foldMap (quantityNames asking) (HowMany.quantity count) <> foldMap (counterKindNames asking (keywordNames asking)) kinds <> quantityNames asking each

sacrificeAnyNumberNames :: (Monoid r) => Asking r -> SacrificeAnyNumber.SacrificeAnyNumber -> r
sacrificeAnyNumberNames asking x = case x of
  SacrificeAnyNumber.MkSacrificeAnyNumber filter_ kind each -> filterNames asking filter_ <> foldMap (counterKindNames asking (keywordNames asking)) kind <> quantityNames asking each

sacrificeRestrictionNames :: (Monoid r) => Asking r -> SacrificeRestriction.SacrificeRestriction -> r
sacrificeRestrictionNames asking x = case x of
  SacrificeRestriction.MkSacrificeRestriction affected -> affectedNames asking affected

sacrificeToEnterNames :: (Monoid r) => Asking r -> SacrificeToEnter.SacrificeToEnter -> r
sacrificeToEnterNames asking x = case x of
  SacrificeToEnter.MkSacrificeToEnter _count filter_ -> filterNames asking filter_

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

spliceNames :: (Monoid r) => Asking r -> (keyword -> r) -> Splice.Splice keyword -> r
spliceNames asking onKeyword x = case x of
  Splice.MkSplice onto cost -> filterNames asking onto <> costNames asking onKeyword cost

statedFlipNames :: (Monoid r) => Asking r -> StatedFlip.StatedFlip -> r
statedFlipNames asking x = case x of
  StatedFlip.MkStatedFlip _face _wins firstEachTurn -> if firstEachTurn then history asking HistoryRead.CoinFlips else mempty

-- CR 102.1: a named player's turn reads whatever its reference reads.
turnScopeNames :: (Monoid r) => Asking r -> TurnScope.TurnScope -> r
turnScopeNames asking x = case x of
  TurnScope.PlayersTurn playerRef -> playerRefNames asking playerRef

duringPhaseNames :: (Monoid r) => Asking r -> DuringPhase.DuringPhase -> r
duringPhaseNames asking x = case x of
  DuringPhase.MkDuringPhase _window scope -> turnScopeNames asking scope

staticAbilityNames :: (Monoid r) => Asking r -> (ability -> r) -> StaticAbility.StaticAbility ability -> r
staticAbilityNames asking onAbility x = case x of
  StaticAbility.MkStaticAbility affected condition _functionsFrom lingers _name modifications -> affectedNames asking affected <> foldMap (conditionNames asking) condition <> foldMap (durationNames asking) lingers <> foldMap (modificationNames asking onAbility) modifications

suspendNames :: (Monoid r) => Asking r -> (keyword -> r) -> Suspend.Suspend keyword -> r
suspendNames asking onKeyword x = case x of
  Suspend.MkSuspend _counters cost -> costNames asking onKeyword cost

tapForTotalPowerNames :: (Monoid r) => Asking r -> TapForTotalPower.TapForTotalPower keyword -> r
tapForTotalPowerNames asking x = case x of
  TapForTotalPower.MkTapForTotalPower _totalPower whichPermanents -> filterNames asking whichPermanents

tapPermanentsNames :: (Monoid r) => Asking r -> TapPermanents.TapPermanents keyword -> r
tapPermanentsNames asking x = case x of
  TapPermanents.MkTapPermanents count whichPermanents _sharingACreatureType -> costAmountNames asking count <> filterNames asking whichPermanents

targetSlotNames :: (Monoid r) => Asking r -> TargetSlot.TargetSlot -> r
targetSlotNames asking x = case x of
  TargetSlot.MkTargetSlot pool filter_ count amount chooser perPlayer divided _together asCast -> poolNames asking pool <> foldMap (filterNames asking) filter_ <> foldMap (filterNames asking) asCast <> slotCountNames asking count <> foldMap (quantityNames asking) amount <> foldMap (playerRefNames asking) chooser <> foldMap (slotPerPlayerNames asking) perPlayer <> foldMap (quantityNames asking) divided

tokenPatternNames :: (Monoid r) => Asking r -> TokenPattern.TokenPattern -> r
tokenPatternNames asking x = case x of
  TokenPattern.MkTokenPattern whose whatToken _ -> controllerRelationNames asking whose <> filterNames asking whatToken

tokenPlusNames :: Asking r -> (card -> r) -> TokenPlus.TokenPlus card -> r
tokenPlusNames _asking onCard x = case x of
  TokenPlus.One card -> onCard card
  TokenPlus.ThatMany card -> onCard card
  TokenPlus.Instead card -> onCard card

tokenRNames :: (Monoid r) => Asking r -> (card -> r) -> TokenR.TokenR card -> r
tokenRNames asking onCard x = case x of
  TokenR.MkTokenR matching _scaling plus -> tokenPatternNames asking matching <> foldMap (tokenPlusNames asking onCard) plus

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
triggeredAbilityNames asking onCard onAbility x = case x of
  TriggeredAbility.MkTriggeredAbility condition modal intervening _name limit _functionsFrom -> triggerLimitNames asking limit <> triggerConditionNames asking condition <> modalNames asking onCard onAbility modal <> foldMap (conditionNames asking) intervening

turnUpRNames :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> TurnUpR.TurnUpR ability effect -> r
turnUpRNames asking onAbility onEffect x = case x of
  TurnUpR.MkTurnUpR matching _requiring paying rewrite -> filterNames asking matching <> foldMap (costNames asking (keywordNames asking)) paying <> turnUpRewriteNames asking onAbility onEffect rewrite

turnUpRewriteNames :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> TurnUpRewrite.TurnUpRewrite ability effect -> r
turnUpRewriteNames asking onAbility onEffect x = case x of
  TurnUpRewrite.WithCounters withCounters -> withCountersNames asking withCounters
  TurnUpRewrite.MayAttachTo filter_ -> filterNames asking filter_
  TurnUpRewrite.AsCopy asCopy -> asCopyNames asking onAbility onEffect asCopy

attachRNames :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> AttachR.AttachR ability effect -> r
attachRNames asking onAbility onEffect x = case x of
  AttachR.MkAttachR matching rewrite -> filterNames asking matching <> attachRewriteNames asking onAbility onEffect rewrite

attachRewriteNames :: (Monoid r) => Asking r -> (ability -> r) -> (effect -> r) -> AttachRewrite.AttachRewrite ability effect -> r
attachRewriteNames asking onAbility onEffect x = case x of
  AttachRewrite.AsCopy asCopy -> asCopyNames asking onAbility onEffect asCopy
  AttachRewrite.ChooseQuality _ -> mempty

untapRNames :: (Monoid r) => Asking r -> UntapR.UntapR -> r
untapRNames asking x = case x of
  UntapR.MkUntapR _during rewrite -> untapRewriteNames asking rewrite

untapRestrictionNames :: (Monoid r) => Asking r -> UntapRestriction.UntapRestriction -> r
untapRestrictionNames asking x = case x of
  UntapRestriction.MkUntapRestriction affected _hold while -> affectedNames asking affected <> foldMap (conditionNames asking) while

untapRewriteNames :: (Monoid r) => Asking r -> UntapRewrite.UntapRewrite -> r
untapRewriteNames asking x = case x of
  UntapRewrite.RemoveStunCounter -> mempty
  UntapRewrite.RemoveCounterToUntap counterKind -> counterKindNames asking (keywordNames asking) counterKind

wardNames :: (Monoid r) => Asking r -> (keyword -> r) -> Ward.Ward keyword -> r
wardNames asking onKeyword x = case x of
  Ward.MkWard cost perEach -> costNames asking onKeyword cost <> foldMap (keywordCountNames asking) perEach

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

zoneChangePatternNames :: (Monoid r) => Asking r -> ZoneChangePattern.ZoneChangePattern -> r
zoneChangePatternNames asking x = case x of
  ZoneChangePattern.MkZoneChangePattern _whenDestination whoseObject whatObject _whenDiscarded _causedBy _duringResolution -> controllerRelationNames asking whoseObject <> filterNames asking whatObject

zoneChangeRNames :: (Monoid r) => Asking r -> (effect -> r) -> ZoneChangeR.ZoneChangeR effect -> r
zoneChangeRNames asking onEffect x = case x of
  ZoneChangeR.MkZoneChangeR matching _destination _revealing _shuffling _position _optional whenYouDo -> zoneChangePatternNames asking matching <> foldMap onEffect whenYouDo

zoneScopeNames :: (Monoid r) => Asking r -> ZoneScope.ZoneScope -> r
zoneScopeNames asking x = case x of
  ZoneScope.Scoped _playerScope -> mempty
  ZoneScope.InSlot slotName -> slotNames asking slotName
  ZoneScope.ControllerOfBound slotName -> slotNames asking slotName
  ZoneScope.BoundPlayer _playerId -> mempty
