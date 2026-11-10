module Parun.Engine.Children where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.AbilityActivatedTrigger as AbilityActivatedTrigger
import qualified Parun.Types.AbilityAddsMana as AbilityAddsMana
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.ActivateManaAbilities as ActivateManaAbilities
import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.ActivationsThisTurn as ActivationsThisTurn
import qualified Parun.Types.AffectPlayers as AffectPlayers
import qualified Parun.Types.AffectedPlayers as AffectedPlayers
import qualified Parun.Types.AgainstExiledPile as AgainstExiledPile
import qualified Parun.Types.AgainstLastCardExiledWith as AgainstLastCardExiledWith
import qualified Parun.Types.AgainstSlot as AgainstSlot
import qualified Parun.Types.Aggregation as Aggregation
import qualified Parun.Types.AimedAt as AimedAt
import qualified Parun.Types.AlternativeActivationCost as AlternativeActivationCost
import qualified Parun.Types.Amass as Amass
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.Ante as Ante
import qualified Parun.Types.AnyNumberDiscard as AnyNumberDiscard
import qualified Parun.Types.AnyNumberMatching as AnyNumberMatching
import qualified Parun.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Parun.Types.ArrangeInLibrary as ArrangeInLibrary
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttachAll as AttachAll
import qualified Parun.Types.AttachBound as AttachBound
import qualified Parun.Types.AttachR as AttachR
import qualified Parun.Types.AttachRewrite as AttachRewrite
import qualified Parun.Types.AttachTarget as AttachTarget
import qualified Parun.Types.AttachmentTrigger as AttachmentTrigger
import qualified Parun.Types.AttackCompany as AttackCompany
import qualified Parun.Types.AttackCount as AttackCount
import qualified Parun.Types.AttackTargetRef as AttackTargetRef
import qualified Parun.Types.AttackTrigger as AttackTrigger
import qualified Parun.Types.AttackedTrigger as AttackedTrigger
import qualified Parun.Types.AttackerTarget as AttackerTarget
import qualified Parun.Types.AttackerTrigger as AttackerTrigger
import qualified Parun.Types.Backup as Backup
import qualified Parun.Types.BecomeCopy as BecomeCopy
import qualified Parun.Types.BecomesTapState as BecomesTapState
import qualified Parun.Types.BecomesTargetedTrigger as BecomesTargetedTrigger
import qualified Parun.Types.Behold as Behold
import qualified Parun.Types.Bid as Bid
import qualified Parun.Types.BindAmount as BindAmount
import qualified Parun.Types.Blight as Blight
import qualified Parun.Types.BlockCount as BlockCount
import qualified Parun.Types.BlockTrigger as BlockTrigger
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.CastFrom as CastFrom
import qualified Parun.Types.CastFromZone as CastFromZone
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.CastRepetition as CastRepetition
import qualified Parun.Types.CausedBy as CausedBy
import qualified Parun.Types.ChangeTargets as ChangeTargets
import qualified Parun.Types.ChangeText as ChangeText
import qualified Parun.Types.ChooseCardName as ChooseCardName
import qualified Parun.Types.ChooseGroup as ChooseGroup
import qualified Parun.Types.ChooseNumber as ChooseNumber
import qualified Parun.Types.ChoosePermanents as ChoosePermanents
import qualified Parun.Types.ChoosePlayer as ChoosePlayer
import qualified Parun.Types.ChoosePlayerAtRandom as ChoosePlayerAtRandom
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.CoinFlipTrigger as CoinFlipTrigger
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CommanderColors as CommanderColors
import qualified Parun.Types.CompanionCondition as CompanionCondition
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.CompletedDungeon as CompletedDungeon
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Conjure as Conjure
import qualified Parun.Types.ConjureCards as ConjureCards
import qualified Parun.Types.Connive as Connive
import qualified Parun.Types.ControlPlayer as ControlPlayer
import qualified Parun.Types.ControlSides as ControlSides
import qualified Parun.Types.ControlSlots as ControlSlots
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.CopyOriginal as CopyOriginal
import qualified Parun.Types.CopyStackObject as CopyStackObject
import qualified Parun.Types.CopyTargets as CopyTargets
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostAddition as CostAddition
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostChange as CostChange
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CostModifier as CostModifier
import qualified Parun.Types.CostScale as CostScale
import qualified Parun.Types.CostSubject as CostSubject
import qualified Parun.Types.Count as Count
import qualified Parun.Types.CountedDiscard as CountedDiscard
import qualified Parun.Types.Counter as Counter
import qualified Parun.Types.CounterDestination as CounterDestination
import qualified Parun.Types.CounterHolder as CounterHolder
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLinked as CounterLinked
import qualified Parun.Types.CounterPattern as CounterPattern
import qualified Parun.Types.CounterProhibition as CounterProhibition
import qualified Parun.Types.CounterR as CounterR
import qualified Parun.Types.CounterSubject as CounterSubject
import qualified Parun.Types.CounterTrigger as CounterTrigger
import qualified Parun.Types.CounteringTrigger as CounteringTrigger
import qualified Parun.Types.CountersFromPermanents as CountersFromPermanents
import qualified Parun.Types.CountersOnThis as CountersOnThis
import qualified Parun.Types.Craft as Craft
import qualified Parun.Types.Create as Create
import qualified Parun.Types.CreateCopy as CreateCopy
import qualified Parun.Types.CreateEmblem as CreateEmblem
import qualified Parun.Types.CrewRole as CrewRole
import qualified Parun.Types.Cycling as Cycling
import qualified Parun.Types.DamagePart as DamagePart
import qualified Parun.Types.DamagePattern as DamagePattern
import qualified Parun.Types.DamageR as DamageR
import qualified Parun.Types.DamageRewrite as DamageRewrite
import qualified Parun.Types.DamageTally as DamageTally
import qualified Parun.Types.DamageTrigger as DamageTrigger
import qualified Parun.Types.DamagedParty as DamagedParty
import qualified Parun.Types.DealDamage as DealDamage
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.DealtDamageTo as DealtDamageTo
import qualified Parun.Types.DeclarationTrigger as DeclarationTrigger
import qualified Parun.Types.Designate as Designate
import qualified Parun.Types.Destroy as Destroy
import qualified Parun.Types.DestructionR as DestructionR
import qualified Parun.Types.Devotion as Devotion
import qualified Parun.Types.Devour as Devour
import qualified Parun.Types.DieResult as DieResult
import qualified Parun.Types.DieRollR as DieRollR
import qualified Parun.Types.Discard as Discard
import qualified Parun.Types.DiscardCards as DiscardCards
import qualified Parun.Types.DiscardSubject as DiscardSubject
import qualified Parun.Types.DiscardTrigger as DiscardTrigger
import qualified Parun.Types.Divide as Divide
import qualified Parun.Types.DoesNotUntapNext as DoesNotUntapNext
import qualified Parun.Types.DraftNoteTally as DraftNoteTally
import qualified Parun.Types.Draw as Draw
import qualified Parun.Types.DrawCountR as DrawCountR
import qualified Parun.Types.DrawR as DrawR
import qualified Parun.Types.DrawRewrite as DrawRewrite
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.DurationEvent as DurationEvent
import qualified Parun.Types.DuringPhase as DuringPhase
import qualified Parun.Types.Earthbend as Earthbend
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.Emerge as Emerge
import qualified Parun.Types.EntersTrigger as EntersTrigger
import qualified Parun.Types.EntersWith as EntersWith
import qualified Parun.Types.EntriesTrigger as EntriesTrigger
import qualified Parun.Types.EntryFlip as EntryFlip
import qualified Parun.Types.EntryOption as EntryOption
import qualified Parun.Types.EntryPrice as EntryPrice
import qualified Parun.Types.EntryR as EntryR
import qualified Parun.Types.EntryRewrite as EntryRewrite
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Equip as Equip
import qualified Parun.Types.ExchangeBlocks as ExchangeBlocks
import qualified Parun.Types.ExchangeCardZones as ExchangeCardZones
import qualified Parun.Types.ExchangeOwnership as ExchangeOwnership
import qualified Parun.Types.ExchangeSides as ExchangeSides
import qualified Parun.Types.ExchangeValues as ExchangeValues
import qualified Parun.Types.ExchangeZones as ExchangeZones
import qualified Parun.Types.ExchangedValue as ExchangedValue
import qualified Parun.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Parun.Types.ExileCardsFromHand as ExileCardsFromHand
import qualified Parun.Types.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Types.ExileFromGraveyard as ExileFromGraveyard
import qualified Parun.Types.ExileHaunting as ExileHaunting
import qualified Parun.Types.ExileMaterials as ExileMaterials
import qualified Parun.Types.ExilePermanents as ExilePermanents
import qualified Parun.Types.ExploitTrigger as ExploitTrigger
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.Fight as Fight
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FlipCoin as FlipCoin
import qualified Parun.Types.ForEach as ForEach
import qualified Parun.Types.ForEachNumber as ForEachNumber
import qualified Parun.Types.ForbidAttack as ForbidAttack
import qualified Parun.Types.ForbidBeingBlocked as ForbidBeingBlocked
import qualified Parun.Types.ForetellCost as ForetellCost
import qualified Parun.Types.FreeCast as FreeCast
import qualified Parun.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Parun.Types.FromReference as FromReference
import qualified Parun.Types.GainControl as GainControl
import qualified Parun.Types.GameLossR as GameLossR
import qualified Parun.Types.Gift as Gift
import qualified Parun.Types.GrantLookAtExiled as GrantLookAtExiled
import qualified Parun.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Parun.Types.Grouping as Grouping
import qualified Parun.Types.Guess as Guess
import qualified Parun.Types.HexproofExemption as HexproofExemption
import qualified Parun.Types.HistoryRead as HistoryRead
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.Impending as Impending
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.InstructionR as InstructionR
import qualified Parun.Types.InstructionRewrite as InstructionRewrite
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordCount as KeywordCount
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.KeywordRiderGrant as KeywordRiderGrant
import qualified Parun.Types.KeywordTally as KeywordTally
import qualified Parun.Types.LaterPayment as LaterPayment
import qualified Parun.Types.LeavesTrigger as LeavesTrigger
import qualified Parun.Types.LeavesZoneTrigger as LeavesZoneTrigger
import qualified Parun.Types.LeavingPermanent as LeavingPermanent
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LifeDirection as LifeDirection
import qualified Parun.Types.LifeGainR as LifeGainR
import qualified Parun.Types.LifeGainRewrite as LifeGainRewrite
import qualified Parun.Types.LifeLoss as LifeLoss
import qualified Parun.Types.LifeLossPattern as LifeLossPattern
import qualified Parun.Types.LifeLossR as LifeLossR
import qualified Parun.Types.LifeLossRewrite as LifeLossRewrite
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.LifeTrigger as LifeTrigger
import qualified Parun.Types.LookAt as LookAt
import qualified Parun.Types.LostPermanent as LostPermanent
import qualified Parun.Types.LoyaltyClause as LoyaltyClause
import qualified Parun.Types.LoyaltyGrant as LoyaltyGrant
import qualified Parun.Types.MadnessCost as MadnessCost
import qualified Parun.Types.MakeForetold as MakeForetold
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.ManaFilter as ManaFilter
import qualified Parun.Types.ManaR as ManaR
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.ManaSpecification as ManaSpecification
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Meld as Meld
import qualified Parun.Types.Mill as Mill
import qualified Parun.Types.MillTally as MillTally
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.ModifyTarget as ModifyTarget
import qualified Parun.Types.MonarchTarget as MonarchTarget
import qualified Parun.Types.Morph as Morph
import qualified Parun.Types.MoveCounters as MoveCounters
import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.MoveMana as MoveMana
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.NextDamageInstance as NextDamageInstance
import qualified Parun.Types.NextStepOf as NextStepOf
import qualified Parun.Types.Ninjutsu as Ninjutsu
import qualified Parun.Types.ObjectCount as ObjectCount
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.OfferCast as OfferCast
import qualified Parun.Types.Onset as Onset
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PerTarget as PerTarget
import qualified Parun.Types.PerformerR as PerformerR
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Parun.Types.PermanentIsDealtDamage as PermanentIsDealtDamage
import qualified Parun.Types.PermanentsDealDamageToPlayer as PermanentsDealDamageToPlayer
import qualified Parun.Types.PhasePattern as PhasePattern
import qualified Parun.Types.PhaseR as PhaseR
import qualified Parun.Types.PhaseRewrite as PhaseRewrite
import qualified Parun.Types.PlacesSticker as PlacesSticker
import qualified Parun.Types.PlaneswalkR as PlaneswalkR
import qualified Parun.Types.PlaneswalkRewrite as PlaneswalkRewrite
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerActsOn as PlayerActsOn
import qualified Parun.Types.PlayerCounterTally as PlayerCounterTally
import qualified Parun.Types.PlayerCounters as PlayerCounters
import qualified Parun.Types.PlayerDesignationTally as PlayerDesignationTally
import qualified Parun.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerIsDealtDamage as PlayerIsDealtDamage
import qualified Parun.Types.PlayerQuantity as PlayerQuantity
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerSacrifices as PlayerSacrifices
import qualified Parun.Types.PlaysLand as PlaysLand
import qualified Parun.Types.PlotFromZone as PlotFromZone
import qualified Parun.Types.PreventAllDamage as PreventAllDamage
import qualified Parun.Types.PreventNextDamage as PreventNextDamage
import qualified Parun.Types.Prohibit as Prohibit
import qualified Parun.Types.Protection as Protection
import qualified Parun.Types.PutCounters as PutCounters
import qualified Parun.Types.PutCountersFrom as PutCountersFrom
import qualified Parun.Types.PutIntoZoneTrigger as PutIntoZoneTrigger
import qualified Parun.Types.PutSticker as PutSticker
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RedirectDamage as RedirectDamage
import qualified Parun.Types.Reinforce as Reinforce
import qualified Parun.Types.RemovalCount as RemovalCount
import qualified Parun.Types.RemoveCounters as RemoveCounters
import qualified Parun.Types.RemoveCountersAmong as RemoveCountersAmong
import qualified Parun.Types.RemoveFromCombat as RemoveFromCombat
import qualified Parun.Types.RemovePlayerCounters as RemovePlayerCounters
import qualified Parun.Types.Repeat as Repeat
import qualified Parun.Types.RepeatIf as RepeatIf
import qualified Parun.Types.Replace as Replace
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.RequireAttack as RequireAttack
import qualified Parun.Types.RequireBlock as RequireBlock
import qualified Parun.Types.RestrictedCreatures as RestrictedCreatures
import qualified Parun.Types.ReturnPermanents as ReturnPermanents
import qualified Parun.Types.Reveal as Reveal
import qualified Parun.Types.RevealedThisWay as RevealedThisWay
import qualified Parun.Types.RollDie as RollDie
import qualified Parun.Types.Sacrifice as Sacrifice
import qualified Parun.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Parun.Types.SacrificeEffect as SacrificeEffect
import qualified Parun.Types.SacrificeToEnter as SacrificeToEnter
import qualified Parun.Types.Scaling as Scaling
import qualified Parun.Types.Scope as Scope
import qualified Parun.Types.Search as Search
import qualified Parun.Types.SetClassLevel as SetClassLevel
import qualified Parun.Types.SetHalfLocked as SetHalfLocked
import qualified Parun.Types.SetOwner as SetOwner
import qualified Parun.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Parun.Types.SkipPhase as SkipPhase
import qualified Parun.Types.SlotArity as SlotArity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SpeedDecrease as SpeedDecrease
import qualified Parun.Types.SpellCast as SpellCast
import qualified Parun.Types.SpellTally as SpellTally
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.Splice as Splice
import qualified Parun.Types.StatedFlip as StatedFlip
import qualified Parun.Types.StepBegins as StepBegins
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.Surveil as Surveil
import qualified Parun.Types.Suspend as Suspend
import qualified Parun.Types.TakeExtraTurn as TakeExtraTurn
import qualified Parun.Types.Tap as Tap
import qualified Parun.Types.TapForMana as TapForMana
import qualified Parun.Types.TapForTotalPower as TapForTotalPower
import qualified Parun.Types.TapPermanents as TapPermanents
import qualified Parun.Types.TappedForManaTrigger as TappedForManaTrigger
import qualified Parun.Types.TappedPermanent as TappedPermanent
import qualified Parun.Types.TargetRestriction as TargetRestriction
import qualified Parun.Types.TargetedSubject as TargetedSubject
import qualified Parun.Types.TheseDiscard as TheseDiscard
import qualified Parun.Types.TokenPattern as TokenPattern
import qualified Parun.Types.TokenPlus as TokenPlus
import qualified Parun.Types.TokenR as TokenR
import qualified Parun.Types.TransformTrigger as TransformTrigger
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.TurnFaceDown as TurnFaceDown
import qualified Parun.Types.TurnScope as TurnScope
import qualified Parun.Types.TurnUpR as TurnUpR
import qualified Parun.Types.TurnUpRewrite as TurnUpRewrite
import qualified Parun.Types.Undesignate as Undesignate
import qualified Parun.Types.UntapR as UntapR
import qualified Parun.Types.UntapRewrite as UntapRewrite
import qualified Parun.Types.UntilPaid as UntilPaid
import qualified Parun.Types.Vote as Vote
import qualified Parun.Types.VoteChoices as VoteChoices
import qualified Parun.Types.VoteObjects as VoteObjects
import qualified Parun.Types.VotePlayers as VotePlayers
import qualified Parun.Types.VotesReceived as VotesReceived
import qualified Parun.Types.Ward as Ward
import qualified Parun.Types.WhichCounters as WhichCounters
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.ZoneChangePattern as ZoneChangePattern
import qualified Parun.Types.ZoneChangeR as ZoneChangeR

-- | One visitor per kind of child a DSL tree holds. The traversals below are
-- the ONE place that matches each constructor positionally, so a new
-- constructor or field is a build error there and nowhere else; a structural
-- walk is a traversal run with the visitors it has an opinion about, plus an
-- override arm for each constructor it answers itself (#6808).
data Children keyword f = MkChildren
  { -- | An operand of And, Or, Not or Unwritten, judged of the same candidate.
    connective :: Filter.Filter keyword -> f (Filter.Filter keyword),
    -- | A Filter a node nests, judged of some other object: a host, a target,
    -- a source, a counted permanent.
    nest :: Filter.Filter keyword -> f (Filter.Filter keyword),
    -- | A keyword a node holds, bare or inside a counter kind (CR 122.1b) or
    -- a designator.
    keyword :: keyword -> f keyword,
    -- | A slot name, with the arity its reader reads it at.
    slot :: SlotArity.SlotArity -> SlotName.SlotName -> f SlotName.SlotName,
    -- | An object named outright.
    object :: ObjectId.ObjectId -> f ObjectId.ObjectId,
    -- | A term of a calculation (CR 107.1), read against the same object.
    term :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A count's per-member quantity, read against each member it reaches.
    member :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A quantity read against some other object: a slot's, a linked card's.
    redirected :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A count's scope, whole.
    scope :: Scope.Scope -> f Scope.Scope,
    -- | A player reference, whole.
    player :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef
  }

-- | Every child left as it is: the base a walk overrides.
leaves :: (Applicative f) => Children keyword f
leaves =
  MkChildren
    { connective = pure,
      nest = pure,
      keyword = pure,
      slot = const pure,
      object = pure,
      term = pure,
      member = pure,
      redirected = pure,
      scope = pure,
      player = pure
    }

-- | Every nested Filter, connective or nest alike, handed to one recursion.
descending :: (Applicative f) => (Filter.Filter keyword -> f (Filter.Filter keyword)) -> Children keyword f
descending recur = leaves {connective = recur, nest = recur}

-- | Every nested Quantity, term, member or redirected alike, handed to one
-- recursion.
descendingQuantities :: (Applicative f) => (Quantity.Quantity -> f Quantity.Quantity) -> Children keyword f
descendingQuantities recur = leaves {term = recur, member = recur, redirected = recur}

-- | A Filter's immediate children, each through its visitor.
filterChildren :: (Applicative f) => Children keyword f -> Filter.Filter keyword -> f (Filter.Filter keyword)
filterChildren children predicate = case predicate of
  Filter.HasCardType _ -> pure predicate
  Filter.HasSupertype _ -> pure predicate
  Filter.HasColor _ -> pure predicate
  Filter.Shares trait anchor -> fmap (Filter.Shares trait) (anchorChildren children anchor)
  Filter.HasSubtype _ -> pure predicate
  Filter.HasName _ -> pure predicate
  Filter.HasNameOriginallyPrintedIn _ -> pure predicate
  Filter.NameWordsAtLeast _ -> pure predicate
  Filter.HasKeyword k -> fmap Filter.HasKeyword (keyword children k)
  Filter.HasKeywordFamily _ -> pure predicate
  Filter.Measures measures -> fmap Filter.Measures (measuresChildren children measures)
  Filter.ManaValueParity _ -> pure predicate
  Filter.ManaCostRepeatsSymbol -> pure predicate
  Filter.ManaCostHasSymbol _ -> pure predicate
  Filter.HeldBy _ _ -> pure predicate
  Filter.ControllerIsOwner -> pure predicate
  Filter.ControlledByRole _ -> pure predicate
  -- ONE: `matches` answers it off a slot naming exactly one player.
  Filter.HeldByBound holder name -> fmap (Filter.HeldByBound holder) (slot children SlotArity.One name)
  Filter.HeldByPlayer _ _ -> pure predicate
  Filter.HeldByRecipient _ -> pure predicate
  Filter.IsSource -> pure predicate
  Filter.IsObject oid -> fmap Filter.IsObject (object children oid)
  Filter.TargetsSource -> pure predicate
  Filter.TargetsOnlyOne f -> fmap Filter.TargetsOnlyOne (nest children f)
  Filter.HasSingleTarget -> pure predicate
  Filter.TargetsMatching f -> fmap Filter.TargetsMatching (nest children f)
  Filter.TargetsPlayer _ -> pure predicate
  -- MANY: the whole bound set, so a group is every one of its members.
  Filter.IsBound name -> fmap Filter.IsBound (slot children SlotArity.Many name)
  Filter.IsTarget -> pure predicate
  -- MANY: under a host hop, a group answers with every member's host's
  -- controller.
  Filter.ControllerRelatesToBound relation hop name -> fmap (Filter.ControllerRelatesToBound relation hop) (slot children SlotArity.Many name)
  Filter.HasChosenName _ -> pure predicate
  Filter.HasChosenColor -> pure predicate
  Filter.HasChosenSubtype -> pure predicate
  Filter.HasChosenCardType -> pure predicate
  Filter.IsExiledWithSource _ -> pure predicate
  Filter.IsLastExiledWithSource -> pure predicate
  Filter.OfChosenPlayer -> pure predicate
  Filter.OfRelatedPlayer _ -> pure predicate
  Filter.IsPlayer _ -> pure predicate
  -- ONE: the controller of the one object the slot names (CR 608.2h), read
  -- through Parun.Engine.Filter.slotOneObject.
  Filter.IsControllerOfBound name -> fmap Filter.IsControllerOfBound (slot children SlotArity.One name)
  Filter.Controls margin f -> fmap (Filter.Controls margin) (nest children f)
  Filter.CardsInGraveyardAtLeast _ -> pure predicate
  Filter.OwnsBoundAtLeast n name f -> liftA2 (Filter.OwnsBoundAtLeast n) (slot children SlotArity.Many name) (nest children f)
  Filter.IsAttacking _ -> pure predicate
  Filter.IsAttackingPlayer _ -> pure predicate
  Filter.IsAttackingPlaneswalker _ -> pure predicate
  Filter.IsAttackingBattle _ -> pure predicate
  Filter.DeclaredAttackedThisCombat -> pure predicate
  Filter.IsBlocking -> pure predicate
  Filter.HasBlockStatus _ -> pure predicate
  Filter.DeclaredAttackerThisCombat -> pure predicate
  Filter.DeclaredBlockerThisCombat -> pure predicate
  Filter.DeclaredThisTurn _ -> pure predicate
  Filter.CouldAttackThisTurn -> pure predicate
  Filter.LeftLibraryThisTurn _ -> pure predicate
  Filter.CantCrewVehicles -> pure predicate
  Filter.CrewedSourceThisTurn -> pure predicate
  Filter.ConvokedSourceThisTurn -> pure predicate
  Filter.SaddledSourceThisTurn -> pure predicate
  Filter.DealtDamageThisTurn -> pure predicate
  Filter.DealtDamageTo asked -> fmap Filter.DealtDamageTo (dealtDamageToChildren children asked)
  Filter.EnteredThisTurn _ -> pure predicate
  Filter.CountersPutThisTurn relation kind -> fmap (Filter.CountersPutThisTurn relation) (counterKindChildren children kind)
  Filter.ControlledSinceTurnBegan -> pure predicate
  Filter.AttachedTo f -> fmap Filter.AttachedTo (nest children f)
  Filter.BeforeMove f -> fmap Filter.BeforeMove (nest children f)
  Filter.HasAttached f -> fmap Filter.HasAttached (nest children f)
  Filter.IsAttachedTo anchor -> fmap Filter.IsAttachedTo (anchorChildren children anchor)
  Filter.IsEvaluated -> pure predicate
  Filter.IsHostOfSource -> pure predicate
  Filter.EnteredWithSource -> pure predicate
  Filter.CounterLinkedToSource moved -> fmap Filter.CounterLinkedToSource (counterLinkedChildren children moved)
  Filter.AttachedNoLaterThanSource -> pure predicate
  Filter.CanHostSubject -> pure predicate
  Filter.CanAttachToSubject -> pure predicate
  Filter.HasEntryHost -> pure predicate
  Filter.HostOfSubjectHasCardType _ -> pure predicate
  Filter.IsToken -> pure predicate
  Filter.IsCommander -> pure predicate
  Filter.IsActivatedAbility _ -> pure predicate
  Filter.IsAbility -> pure predicate
  Filter.IsEmblem -> pure predicate
  Filter.FromSource f -> fmap Filter.FromSource (nest children f)
  Filter.IsTapped -> pure predicate
  Filter.IsFaceDown -> pure predicate
  Filter.RepresentedByCard f -> fmap Filter.RepresentedByCard (nest children f)
  Filter.IsExiledFaceDown -> pure predicate
  Filter.Transformed -> pure predicate
  Filter.IsRingBearer -> pure predicate
  Filter.IsPaired -> pure predicate
  Filter.IsPairedWithSource -> pure predicate
  Filter.BlockPartnerOfSource _ -> pure predicate
  Filter.HasDesignation _ -> pure predicate
  Filter.HasCounters kind -> fmap Filter.HasCounters (counterKindChildren children kind)
  Filter.HasCountersOfAnyKind -> pure predicate
  Filter.HasSticker _ -> pure predicate
  Filter.Stickered -> pure predicate
  Filter.HasActivatedAbility _ -> pure predicate
  Filter.IsInZone _ -> pure predicate
  Filter.WasCastFrom _ -> pure predicate
  Filter.TagWasSpent _ -> pure predicate
  Filter.Paid designator -> fmap Filter.Paid (designatorChildren children designator)
  Filter.IsPrototyped -> pure predicate
  Filter.IsPreparedSpell -> pure predicate
  Filter.And fs -> fmap Filter.And (traverse (connective children) fs)
  Filter.Or fs -> fmap Filter.Or (traverse (connective children) fs)
  Filter.Not f -> fmap Filter.Not (connective children f)
  Filter.Unwritten f -> fmap Filter.Unwritten (connective children f)

-- | An anchor's children: its slot, read at MANY (the whole set it names), or
-- the Filter it nests.
anchorChildren :: (Applicative f) => Children keyword f -> Anchor.Anchor (Filter.Filter keyword) -> f (Anchor.Anchor (Filter.Filter keyword))
anchorChildren children anchor = case anchor of
  Anchor.Bound name -> fmap Anchor.Bound (slot children SlotArity.Many name)
  Anchor.Evaluated -> pure anchor
  Anchor.Source -> pure anchor
  Anchor.AnyPermanent group -> fmap Anchor.AnyPermanent (nest children group)
  Anchor.YourCommander -> pure anchor

-- | A comparison's children: the slot its operand reads.
measuresChildren :: (Applicative f) => Children keyword f -> Measures.Measures -> f Measures.Measures
measuresChildren children measures = case measures of
  Measures.MkMeasures measure comparison operand -> fmap (Measures.MkMeasures measure comparison) (operandChildren children operand)

-- | An operand's children: the slot it reads.
operandChildren :: (Applicative f) => Children keyword f -> Operand.Operand -> f Operand.Operand
operandChildren children operand = case operand of
  Operand.Literal _ -> pure operand
  Operand.OfSource _ -> pure operand
  Operand.Own _ -> pure operand
  -- ONE: CR 208.1's comparison wants one object's number, and
  -- Parun.Engine.Projection.framedBySlots declines a slot that names several.
  Operand.OfBound bound -> case bound of
    BoundMeasure.MkBoundMeasure name measure -> fmap (\renamed -> Operand.OfBound (BoundMeasure.MkBoundMeasure renamed measure)) (slot children SlotArity.One name)
  -- AMOUNT: the slot holds a number (Context's boundAmounts), never a recipient.
  Operand.AmountInSlot name -> fmap Operand.AmountInSlot (slot children SlotArity.Amount name)
  Operand.EnclosingAmount -> pure operand
  Operand.OfEachOtherCandidate _ -> pure operand

-- | A damage look-back's children: the slot its party reads, at MANY
-- (membership, IsBound's reading).
dealtDamageToChildren :: (Applicative f) => Children keyword f -> DealtDamageTo.DealtDamageTo -> f DealtDamageTo.DealtDamageTo
dealtDamageToChildren children asked = case asked of
  DealtDamageTo.MkDealtDamageTo party kind window dealer ->
    let rebuild renamed = DealtDamageTo.MkDealtDamageTo renamed kind window dealer
     in case party of
          DamagedParty.InSlot name -> fmap (rebuild . DamagedParty.InSlot) (slot children SlotArity.Many name)
          DamagedParty.Related _ -> pure asked
          DamagedParty.Source -> pure asked
          DamagedParty.Anyone -> pure asked
          DamagedParty.HolderOfCandidate _ -> pure asked

-- | A counter link's children: the keyword its kind may hold.
counterLinkedChildren :: (Applicative f) => Children keyword f -> CounterLinked.CounterLinked keyword -> f (CounterLinked.CounterLinked keyword)
counterLinkedChildren children moved = case moved of
  CounterLinked.PutWith kind -> fmap CounterLinked.PutWith (counterKindChildren children kind)
  CounterLinked.RemovedWith kind -> fmap CounterLinked.RemovedWith (counterKindChildren children kind)

-- | A counter kind's children: CR 122.1b's keyword.
counterKindChildren :: (Applicative f) => Children keyword f -> CounterKind.CounterKind keyword -> f (CounterKind.CounterKind keyword)
counterKindChildren children kind = case kind of
  CounterKind.Keyword k -> fmap CounterKind.Keyword (keyword children k)
  CounterKind.PlusOnePlusOne -> pure kind
  CounterKind.MinusOneMinusOne -> pure kind
  CounterKind.Loyalty -> pure kind
  CounterKind.Lore -> pure kind
  CounterKind.Defense -> pure kind
  CounterKind.Time -> pure kind
  CounterKind.Fade -> pure kind
  CounterKind.Age -> pure kind
  CounterKind.Shield -> pure kind
  CounterKind.Finality -> pure kind
  CounterKind.Stun -> pure kind
  CounterKind.Level -> pure kind
  CounterKind.Hone -> pure kind
  CounterKind.UnsafePowerToughness _ _ -> pure kind
  CounterKind.Named _ -> pure kind

-- | A designator's children: the keyword it names.
designatorChildren :: (Applicative f) => Children keyword f -> KeywordDesignator.KeywordDesignator keyword -> f (KeywordDesignator.KeywordDesignator keyword)
designatorChildren children designator = case designator of
  KeywordDesignator.OfFamily _ -> pure designator
  KeywordDesignator.OfKeyword k -> fmap KeywordDesignator.OfKeyword (keyword children k)
  KeywordDesignator.Printed _ -> pure designator
  KeywordDesignator.OfInstance k n -> fmap (\renamed -> KeywordDesignator.OfInstance renamed n) (keyword children k)

-- | A Quantity's immediate children, each through its visitor.
quantityChildren :: (Applicative f) => Children Keyword.Keyword f -> Quantity.Quantity -> f Quantity.Quantity
quantityChildren children quantity = case quantity of
  Quantity.Literal _ -> pure quantity
  Quantity.Measure _ -> pure quantity
  Quantity.Intensity -> pure quantity
  Quantity.ManaSymbolCount _ -> pure quantity
  -- AMOUNT, for each of the six: the slot's number (Binding.amountOf), its
  -- division (CR 601.2d), whether it is bound at all (CR 701.4b), or a tally,
  -- never one recipient, so none reaches Filter.slotOneObject.
  Quantity.InSlot name -> fmap Quantity.InSlot (slot children SlotArity.Amount name)
  Quantity.Share name -> fmap Quantity.Share (slot children SlotArity.Amount name)
  Quantity.WasBound name -> fmap Quantity.WasBound (slot children SlotArity.Amount name)
  Quantity.BoundCount name -> fmap Quantity.BoundCount (slot children SlotArity.Amount name)
  Quantity.UniqueVowelsOnSticker name -> fmap Quantity.UniqueVowelsOnSticker (slot children SlotArity.Amount name)
  Quantity.VotesReceived (VotesReceived.MkVotesReceived name ref) -> fmap Quantity.VotesReceived (VotesReceived.MkVotesReceived <$> slot children SlotArity.Amount name <*> player children ref)
  Quantity.LettersOnNameStickers _ -> pure quantity
  Quantity.NameStickers -> pure quantity
  Quantity.PowerOfStickers -> pure quantity
  Quantity.ToughnessOfStickers -> pure quantity
  Quantity.Star -> pure quantity
  Quantity.Arithmetic arithmetic -> fmap Quantity.Arithmetic (traverse (term children) arithmetic)
  Quantity.AtMost atMost -> fmap Quantity.AtMost (traverse (term children) atMost)
  Quantity.Count c -> fmap Quantity.Count (countChildren children c)
  Quantity.ManaCount (ManaCount.MkManaCount ref manaFilter) -> fmap (\renamed -> Quantity.ManaCount (ManaCount.MkManaCount renamed manaFilter)) (player children ref)
  Quantity.LifeTotal ref -> fmap Quantity.LifeTotal (player children ref)
  Quantity.StartingLifeTotal ref -> fmap Quantity.StartingLifeTotal (player children ref)
  Quantity.StartingDeckSize ref -> fmap Quantity.StartingDeckSize (player children ref)
  Quantity.Speed ref -> fmap Quantity.Speed (player children ref)
  Quantity.IsMonarch ref -> fmap Quantity.IsMonarch (player children ref)
  Quantity.HasPlayerDesignation (PlayerDesignationTally.MkPlayerDesignationTally ref mark) -> fmap (\renamed -> Quantity.HasPlayerDesignation (PlayerDesignationTally.MkPlayerDesignationTally renamed mark)) (player children ref)
  Quantity.IsStartingPlayer ref -> fmap Quantity.IsStartingPlayer (player children ref)
  Quantity.IsActivePlayer ref -> fmap Quantity.IsActivePlayer (player children ref)
  Quantity.Devotion (Devotion.MkDevotion ref colors) -> fmap (\renamed -> Quantity.Devotion (Devotion.MkDevotion renamed colors)) (player children ref)
  Quantity.PartySize ref -> fmap Quantity.PartySize (player children ref)
  Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally ref kind) -> fmap (\renamed -> Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally renamed kind)) (player children ref)
  Quantity.ObjectCounters kind -> fmap Quantity.ObjectCounters (counterKindChildren children kind)
  Quantity.ObjectCountersOfAnyKind -> pure quantity
  Quantity.HasDesignation _ -> pure quantity
  Quantity.DesignationValue _ -> pure quantity
  Quantity.StoredResultsOfSameValue -> pure quantity
  Quantity.ClassLevel -> pure quantity
  Quantity.WasForetold -> pure quantity
  Quantity.TributeWasPaid -> pure quantity
  Quantity.TimesPaid designator -> fmap Quantity.TimesPaid (designatorChildren children designator)
  Quantity.CastUsing designator -> fmap Quantity.CastUsing (designatorChildren children designator)
  Quantity.TagWasSpent _ -> pure quantity
  Quantity.TagWasSpentOfOwnColor _ -> pure quantity
  Quantity.ChosenColorsItIs -> pure quantity
  Quantity.ChosenNumber -> pure quantity
  Quantity.ManaSpent _ -> pure quantity
  Quantity.WasToken -> pure quantity
  Quantity.WasAttacking -> pure quantity
  Quantity.WasBlocking -> pure quantity
  Quantity.WasBlockedThisTurn -> pure quantity
  Quantity.TimesTappedThisTurn -> pure quantity
  Quantity.ControlGained window ref -> fmap (Quantity.ControlGained window) (player children ref)
  Quantity.PlayedBy ref -> fmap Quantity.PlayedBy (player children ref)
  Quantity.OpponentsAttacked ref -> fmap Quantity.OpponentsAttacked (player children ref)
  Quantity.AttackersDeclaredThisTurn ref -> fmap Quantity.AttackersDeclaredThisTurn (player children ref)
  Quantity.AttackersDeclaredThisCombat -> pure quantity
  Quantity.AttackedInLastTurnOf ref -> fmap Quantity.AttackedInLastTurnOf (player children ref)
  Quantity.AttackersInTheirLastTurn ref -> fmap Quantity.AttackersInTheirLastTurn (player children ref)
  Quantity.TalliedThisTurn tally ref -> fmap (Quantity.TalliedThisTurn tally) (player children ref)
  Quantity.LifeChanged (LifeTally.MkLifeTally ref direction turn) -> fmap (\renamed -> Quantity.LifeChanged (LifeTally.MkLifeTally renamed direction turn)) (player children ref)
  Quantity.PlayersDealtDamage (DamageTally.MkDamageTally dealt turn) -> fmap (\renamed -> Quantity.PlayersDealtDamage (DamageTally.MkDamageTally renamed turn)) (dealtDamageChildren children dealt)
  Quantity.DamageDealtToPlayersThisTurn dealt -> fmap Quantity.DamageDealtToPlayersThisTurn (dealtDamageChildren children dealt)
  Quantity.DamageDealtThisTurn _ -> pure quantity
  Quantity.SpellsCast (SpellTally.MkSpellTally ref turn) -> fmap (\renamed -> Quantity.SpellsCast (SpellTally.MkSpellTally renamed turn)) (player children ref)
  Quantity.TimesThisTurn _ -> pure quantity
  Quantity.ActivationsThisTurn (ActivationsThisTurn.MkActivationsThisTurn ref source criteria) -> fmap Quantity.ActivationsThisTurn (ActivationsThisTurn.MkActivationsThisTurn <$> player children ref <*> nest children source <*> activationCriteriaChildren children criteria)
  Quantity.PermanentsDiedThisTurn -> pure quantity
  Quantity.DamagePreventedThisWay -> pure quantity
  Quantity.SpellsCastUsingThisTurn _ -> pure quantity
  Quantity.SubgamesThisMatch -> pure quantity
  Quantity.DungeonsCompleted ref -> fmap Quantity.DungeonsCompleted (player children ref)
  Quantity.CommanderCasts ref -> fmap Quantity.CommanderCasts (player children ref)
  Quantity.CommanderColors (CommanderColors.MkCommanderColors ref colors) -> fmap (\renamed -> Quantity.CommanderColors (CommanderColors.MkCommanderColors renamed colors)) (player children ref)
  Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon ref name) -> fmap (\renamed -> Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon renamed name)) (player children ref)
  Quantity.DraftNotes (DraftNoteTally.MkDraftNoteTally ref name fold) -> fmap (\renamed -> Quantity.DraftNotes (DraftNoteTally.MkDraftNoteTally renamed name fold)) (player children ref)
  Quantity.EnteredThisTurn -> pure quantity
  Quantity.EnteredFrom from -> fmap Quantity.EnteredFrom (inZoneChildren children from)
  Quantity.WasCastFrom (CastFrom.MkCastFrom caster from) -> fmap Quantity.WasCastFrom (CastFrom.MkCastFrom <$> player children caster <*> inZoneChildren children from)
  Quantity.BlockersBeyond _ -> pure quantity
  -- ONE: the payload is read against the one object the slot names, through
  -- Filter.slotOneObject, which declines a slot naming several.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot name inner) -> fmap Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot <$> slot children SlotArity.One name <*> redirected children inner)
  -- CR 607.2a's pile is a relation on the board, named by no slot.
  Quantity.AgainstExiledPile (AgainstExiledPile.MkAgainstExiledPile pile inner) -> fmap (Quantity.AgainstExiledPile . AgainstExiledPile.MkAgainstExiledPile pile) (redirected children inner)
  Quantity.AgainstLastCardExiledWith (AgainstLastCardExiledWith.MkAgainstLastCardExiledWith criterion inner) -> fmap Quantity.AgainstLastCardExiledWith (AgainstLastCardExiledWith.MkAgainstLastCardExiledWith <$> nest children criterion <*> redirected children inner)
  Quantity.StationMeasure -> pure quantity

-- | A count's children: its scope, the Filter its members are judged by, and
-- the per-member quantity a Greatest or a Total folds.
countChildren :: (Applicative f) => Children Keyword.Keyword f -> Count.Count Quantity.Quantity -> f (Count.Count Quantity.Quantity)
countChildren children c = case c of
  Count.MkCount over criterion aggregation -> Count.MkCount <$> scope children over <*> nest children criterion <*> aggregationChildren children aggregation

-- | An aggregation's children: the per-member quantity, where it has one.
aggregationChildren :: (Applicative f) => Children keyword f -> Aggregation.Aggregation Quantity.Quantity -> f (Aggregation.Aggregation Quantity.Quantity)
aggregationChildren children aggregation = case aggregation of
  Aggregation.Members -> pure aggregation
  Aggregation.Distinct _ -> pure aggregation
  Aggregation.MostSharing _ -> pure aggregation
  Aggregation.DistinctNames -> pure aggregation
  Aggregation.Greatest q -> fmap Aggregation.Greatest (member children q)
  Aggregation.Total q -> fmap Aggregation.Total (member children q)

-- | A damage record's children: the players it was dealt to.
dealtDamageChildren :: (Applicative f) => Children keyword f -> DealtDamage.DealtDamage PlayerRef.PlayerRef -> f (DealtDamage.DealtDamage PlayerRef.PlayerRef)
dealtDamageChildren children dealt = case dealt of
  DealtDamage.MkDealtDamage ref kind by -> fmap (\renamed -> DealtDamage.MkDealtDamage renamed kind by) (player children ref)

-- | A per-player zone's children: whose copy it is.
inZoneChildren :: (Applicative f) => Children keyword f -> InZone.InZone -> f InZone.InZone
inZoneChildren children from = case from of
  InZone.MkInZone zone ref -> fmap (InZone.MkInZone zone) (player children ref)

-- | An activation tally's criteria's children: the keyword its granting
-- ability is designated by.
activationCriteriaChildren :: (Applicative f) => Children Keyword.Keyword f -> ActivationCriteria.ActivationCriteria -> f ActivationCriteria.ActivationCriteria
activationCriteriaChildren children criteria = case criteria of
  ActivationCriteria.MkActivationCriteria grantedBy whichKind whichLoyalty whichAbility -> fmap (\renamed -> ActivationCriteria.MkActivationCriteria renamed whichKind whichLoyalty whichAbility) (traverse (designatorChildren children) grantedBy)

-- | One visitor per kind of child an Effect holds. A payload this slice
-- reaches through is matched positionally in effectChildren; one visited
-- whole is a type with walks of its own outside the effect, whose children
-- are #6814's.
data EffectChildren card ability f = MkEffectChildren
  { -- | A Filter a field holds, judged of the objects it describes.
    onFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A Quantity a field holds.
    onQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A nested effect: a loop's body, a rider, a later payment's effects.
    onEffect :: Effect.Effect card ability -> f (Effect.Effect card ability),
    -- | An object reference, whole.
    onObjectRef :: ObjectRef.ObjectRef -> f ObjectRef.ObjectRef,
    -- | A player reference, whole.
    onPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | A slot name, defined or read.
    onSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | A stated duration (CR 611.2a), whole.
    onDuration :: Duration.Duration -> f Duration.Duration,
    -- | A condition, whole.
    onCondition :: Condition.Condition -> f Condition.Condition,
    -- | A card this effect defines and prints: a token's, an emblem's.
    onCard :: card -> f card,
    -- | A card a conjure names by its Oracle reference (CR 108.1).
    onConjured :: card -> f card,
    -- | The combined back face a meld puts onto the battlefield (CR 713.1a).
    onMelded :: card -> f card,
    -- | An ability this effect quotes or grants.
    onAbility :: ability -> f ability,
    -- | A cost, whole.
    onCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | A keyword named outright: the one an offered cast is offered by.
    onKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | A counter kind (CR 122.1), whole.
    onCounterKind :: CounterKind.CounterKind Keyword.Keyword -> f (CounterKind.CounterKind Keyword.Keyword),
    -- | Which counters a removal takes, whole.
    onWhichCounters :: WhichCounters.WhichCounters Keyword.Keyword -> f (WhichCounters.WhichCounters Keyword.Keyword),
    -- | Which counters a move carries (CR 122.5), whole.
    onMovedKinds :: MovedKinds.MovedKinds -> f MovedKinds.MovedKinds,
    -- | How many counters a removal takes, whole.
    onRemovalCount :: RemovalCount.RemovalCount -> f RemovalCount.RemovalCount,
    -- | A continuous effect's modification (CR 611), whole.
    onModification :: Modification.Modification ability -> f (Modification.Modification ability),
    -- | A player- or rules-modifying effect, whole.
    onPlayerEffect :: PlayerEffect.PlayerEffect -> f PlayerEffect.PlayerEffect,
    -- | A floating replacement effect (CR 614), whole.
    onReplacement :: ReplacementEffect.ReplacementEffect card ability (Effect.Effect card ability) -> f (ReplacementEffect.ReplacementEffect card ability (Effect.Effect card ability)),
    -- | A loop member's payment gate, whole.
    onPayGate :: PayGate.PayGate -> f PayGate.PayGate,
    -- | What an entering permanent enters with (CR 614.1c), whole.
    onEntryRiders :: EntryRiders.EntryRiders Quantity.Quantity ability -> f (EntryRiders.EntryRiders Quantity.Quantity ability),
    -- | Where in a library a moved card goes (CR 401.4), whole.
    onPlacement :: LibraryPlacement.LibraryPlacement -> f LibraryPlacement.LibraryPlacement,
    -- | An exception to the copiable values (CR 707.9), whole.
    onCopyException :: CopyException.CopyException ability -> f (CopyException.CopyException ability),
    -- | A face-down permanent's characteristics (CR 708.2), whole.
    onFaceDown :: FaceDownCharacteristics.FaceDownCharacteristics ability -> f (FaceDownCharacteristics.FaceDownCharacteristics ability),
    -- | A mana addition (CR 605), whole.
    onManaAddition :: ManaAddition.ManaAddition -> f ManaAddition.ManaAddition,
    -- | The players a player effect covers, whole.
    onAffectedPlayers :: AffectedPlayers.AffectedPlayers SlotName.SlotName -> f (AffectedPlayers.AffectedPlayers SlotName.SlotName),
    -- | When a delayed trigger starts watching (CR 603.7a), whole.
    onOnset :: Onset.Onset -> f Onset.Onset,
    -- | How long a moved object stays moved, whole.
    onMoveDuration :: MoveDuration.MoveDuration -> f MoveDuration.MoveDuration,
    -- | What an attack restriction is aimed at, whole.
    onAimedAt :: AimedAt.AimedAt -> f AimedAt.AimedAt,
    -- | A choice of permanents (CR 608.2d), whole.
    onChosenPermanents :: ChosenPermanents.ChosenPermanents -> f ChosenPermanents.ChosenPermanents
  }

-- | Every child of an effect left as it is: the base a walk overrides.
effectLeaves :: (Applicative f) => EffectChildren card ability f
effectLeaves =
  MkEffectChildren
    { onFilter = pure,
      onQuantity = pure,
      onEffect = pure,
      onObjectRef = pure,
      onPlayer = pure,
      onSlot = pure,
      onDuration = pure,
      onCondition = pure,
      onCard = pure,
      onConjured = pure,
      onMelded = pure,
      onAbility = pure,
      onCost = pure,
      onKeyword = pure,
      onCounterKind = pure,
      onWhichCounters = pure,
      onMovedKinds = pure,
      onRemovalCount = pure,
      onModification = pure,
      onPlayerEffect = pure,
      onReplacement = pure,
      onPayGate = pure,
      onEntryRiders = pure,
      onPlacement = pure,
      onCopyException = pure,
      onFaceDown = pure,
      onManaAddition = pure,
      onAffectedPlayers = pure,
      onOnset = pure,
      onMoveDuration = pure,
      onAimedAt = pure,
      onChosenPermanents = pure
    }

-- | An Effect's immediate children, each through its visitor. A nested
-- effect is handed to onEffect and not descended here.
effectChildren :: (Applicative f) => EffectChildren card ability f -> Effect.Effect card ability -> f (Effect.Effect card ability)
effectChildren c effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts dealer excess tally damaged excessTally) ->
    fmap Effect.DealDamage (DealDamage.MkDealDamage <$> traverse (damagePartChildren c) parts <*> traverse (onSlot c) dealer <*> pure excess <*> traverse (onSlot c) tally <*> traverse (onSlot c) damaged <*> traverse (onSlot c) excessTally)
  Effect.Fight (Fight.MkFight first second) -> fmap Effect.Fight (Fight.MkFight <$> onSlot c first <*> onSlot c second)
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref each) ->
    fmap Effect.ModifyTarget (ModifyTarget.MkModifyTarget <$> onDuration c duration <*> onModification c modification <*> onObjectRef c ref <*> traverse (onSlot c) each)
  Effect.ChangeText (ChangeText.MkChangeText families forbidden named duration) -> fmap Effect.ChangeText (ChangeText.MkChangeText families forbidden <$> onSlot c named <*> onDuration c duration)
  Effect.AddMana addition -> fmap Effect.AddMana (onManaAddition c addition)
  Effect.ActivateManaAbilities (ActivateManaAbilities.MkActivateManaAbilities who filter_) -> fmap Effect.ActivateManaAbilities (ActivateManaAbilities.MkActivateManaAbilities <$> onPlayer c who <*> onFilter c filter_)
  Effect.MoveMana (MoveMana.MkMoveMana from to) -> fmap Effect.MoveMana (MoveMana.MkMoveMana <$> onPlayer c from <*> traverse (onPlayer c) to)
  Effect.Search (Search.MkSearch searcher owner zones outside quantity filter_ descriptions upTo destination counters subject named handSlot differentIn exactly andOr) ->
    fmap
      Effect.Search
      ( Search.MkSearch
          <$> onPlayer c searcher
          <*> onPlayer c owner
          <*> pure zones
          <*> pure outside
          <*> traverse (onQuantity c) quantity
          <*> onFilter c filter_
          <*> traverse (onFilter c) descriptions
          <*> pure upTo
          <*> pure destination
          <*> fmap Map.fromList (traverse (\(kind, count) -> (,) <$> onCounterKind c kind <*> onQuantity c count) (Map.toList counters))
          <*> traverse (onSlot c) subject
          <*> traverse (onSlot c) named
          <*> traverse (onSlot c) handSlot
          <*> pure differentIn
          <*> pure exactly
          <*> pure andOr
      )
  Effect.ExileAllGraveyards -> pure effect
  Effect.RestartGame exempt -> fmap Effect.RestartGame (traverse (onObjectRef c) exempt)
  Effect.ControlPlayerNextTurn named -> fmap Effect.ControlPlayerNextTurn (onSlot c named)
  Effect.ControlPlayerThisResolution (ControlPlayer.MkControlPlayer named landsOnly) -> fmap (\renamed -> Effect.ControlPlayerThisResolution (ControlPlayer.MkControlPlayer renamed landsOnly)) (onSlot c named)
  Effect.Destroy (Destroy.MkDestroy ref regenerability named buried permanents) ->
    fmap Effect.Destroy (Destroy.MkDestroy <$> onObjectRef c ref <*> pure regenerability <*> traverse (onSlot c) named <*> traverse (onSlot c) buried <*> traverse (onSlot c) permanents)
  Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect ref sacrificer sacrificed) -> fmap Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect <$> onObjectRef c ref <*> pure sacrificer <*> traverse (onSlot c) sacrificed)
  Effect.Attach named -> fmap Effect.Attach (onSlot c named)
  Effect.AttachAsThoughCreature named -> fmap Effect.AttachAsThoughCreature (onSlot c named)
  Effect.AttachTarget attach -> fmap Effect.AttachTarget (attachTargetChildren c attach)
  Effect.AttachTargetToEach attach -> fmap Effect.AttachTargetToEach (attachTargetChildren c attach)
  Effect.AttachBound (AttachBound.MkAttachBound subject destination) -> fmap Effect.AttachBound (AttachBound.MkAttachBound <$> onSlot c subject <*> onSlot c destination)
  Effect.AttachAll (AttachAll.MkAttachAll subjects destination) -> fmap Effect.AttachAll (AttachAll.MkAttachAll <$> onObjectRef c subjects <*> onFilter c destination)
  Effect.Unattach ref -> fmap Effect.Unattach (onObjectRef c ref)
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone riders named origin placement duration) ->
    fmap Effect.MoveToZone (MoveToZone.MkMoveToZone <$> onObjectRef c ref <*> pure zone <*> onEntryRiders c riders <*> traverse (onSlot c) named <*> pure origin <*> onPlacement c placement <*> traverse (onMoveDuration c) duration)
  Effect.Draw (Draw.MkDraw who quantity named) -> fmap Effect.Draw (Draw.MkDraw <$> onPlayer c who <*> onQuantity c quantity <*> traverse (onSlot c) named)
  Effect.Mill (Mill.MkMill who quantity tally named) -> fmap Effect.Mill (Mill.MkMill <$> onPlayer c who <*> onQuantity c quantity <*> traverse (millTallyChildren c) tally <*> traverse (onSlot c) named)
  Effect.Reveal (Reveal.MkReveal ref named) -> fmap Effect.Reveal (Reveal.MkReveal <$> onObjectRef c ref <*> traverse (onSlot c) named)
  Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame count upTo destination filter_ reveal differentIn pick) ->
    fmap (\rewritten -> Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame count upTo destination rewritten reveal differentIn pick)) (onFilter c filter_)
  Effect.ExileThisSpell -> pure effect
  Effect.LookAt (LookAt.MkLookAt ref named looker) -> fmap Effect.LookAt (LookAt.MkLookAt <$> onObjectRef c ref <*> onSlot c named <*> onPlayer c looker)
  Effect.ArrangeInLibrary (ArrangeInLibrary.MkArrangeInLibrary ref chooser) -> fmap Effect.ArrangeInLibrary (ArrangeInLibrary.MkArrangeInLibrary <$> onObjectRef c ref <*> onPlayer c chooser)
  Effect.Scry x -> fmap Effect.Scry (playerQuantityChildren c x)
  Effect.Surveil (Surveil.MkSurveil who quantity named) -> fmap Effect.Surveil (Surveil.MkSurveil <$> onPlayer c who <*> onQuantity c quantity <*> traverse (onSlot c) named)
  Effect.Fateseal x -> fmap Effect.Fateseal (playerQuantityChildren c x)
  Effect.Clash named -> fmap Effect.Clash (onSlot c named)
  Effect.AsYouCascade named -> fmap Effect.AsYouCascade (onSlot c named)
  Effect.Explore ref -> fmap Effect.Explore (onObjectRef c ref)
  Effect.Connive (Connive.MkConnive quantity ref) -> fmap Effect.Connive (Connive.MkConnive <$> onQuantity c quantity <*> onObjectRef c ref)
  Effect.Discard subject -> fmap Effect.Discard (discardChildren c subject)
  Effect.LoseLife (LifeLoss.MkLifeLoss who quantity cause tally) -> fmap Effect.LoseLife (LifeLoss.MkLifeLoss <$> onPlayer c who <*> onQuantity c quantity <*> pure cause <*> traverse (onSlot c) tally)
  Effect.GainLife x -> fmap Effect.GainLife (playerQuantityChildren c x)
  Effect.ExchangeLifeTotals sides -> fmap Effect.ExchangeLifeTotals (exchangeSidesChildren c sides)
  Effect.ExchangeValues (ExchangeValues.MkExchangeValues one other duration) ->
    fmap Effect.ExchangeValues (ExchangeValues.MkExchangeValues <$> exchangedValueChildren c one <*> exchangedValueChildren c other <*> onDuration c duration)
  Effect.ExchangeZones (ExchangeZones.MkExchangeZones who zones) -> fmap (\renamed -> Effect.ExchangeZones (ExchangeZones.MkExchangeZones renamed zones)) (onPlayer c who)
  Effect.ExchangeCardZones (ExchangeCardZones.MkExchangeCardZones one other) -> fmap Effect.ExchangeCardZones (ExchangeCardZones.MkExchangeCardZones <$> onObjectRef c one <*> onObjectRef c other)
  Effect.SetOwner (SetOwner.MkSetOwner who ref) -> fmap Effect.SetOwner (SetOwner.MkSetOwner <$> onPlayer c who <*> onObjectRef c ref)
  Effect.ExchangeOwnership (ExchangeOwnership.MkExchangeOwnership one other) -> fmap Effect.ExchangeOwnership (ExchangeOwnership.MkExchangeOwnership <$> onObjectRef c one <*> onObjectRef c other)
  Effect.SetLifeTotal x -> fmap Effect.SetLifeTotal (playerQuantityChildren c x)
  Effect.LoseGame who -> fmap Effect.LoseGame (onPlayer c who)
  Effect.WinGame who -> fmap Effect.WinGame (onPlayer c who)
  Effect.DrawGame -> pure effect
  Effect.RedistributeLifeTotals -> pure effect
  Effect.IncreaseSpeed x -> fmap Effect.IncreaseSpeed (playerQuantityChildren c x)
  Effect.DecreaseSpeed (SpeedDecrease.MkSpeedDecrease who quantity floor_) -> fmap Effect.DecreaseSpeed (SpeedDecrease.MkSpeedDecrease <$> onPlayer c who <*> onQuantity c quantity <*> pure floor_)
  Effect.Create (Create.MkCreate quantity card riders named creator linked) ->
    fmap Effect.Create (Create.MkCreate <$> onQuantity c quantity <*> onCard c card <*> onEntryRiders c riders <*> traverse (onSlot c) named <*> onPlayer c creator <*> pure linked)
  Effect.Conjure (Conjure.MkConjure quantity cards selection destination named) ->
    fmap Effect.Conjure (Conjure.MkConjure <$> onQuantity c quantity <*> conjureCardsChildren c cards <*> pure selection <*> pure destination <*> traverse (onSlot c) named)
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref riders named exceptions linked) ->
    fmap Effect.CreateCopy (CreateCopy.MkCreateCopy <$> onQuantity c quantity <*> onObjectRef c ref <*> onEntryRiders c riders <*> traverse (onSlot c) named <*> traverse (onCopyException c) exceptions <*> pure linked)
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject duration exceptions newTargets) ->
    fmap Effect.BecomeCopy (BecomeCopy.MkBecomeCopy <$> copyOriginalChildren c original <*> onObjectRef c subject <*> traverse (onDuration c) duration <*> traverse (onCopyException c) exceptions <*> pure newTargets)
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets quantity copier exceptions) ->
    fmap Effect.CopyStackObject (CopyStackObject.MkCopyStackObject <$> onObjectRef c ref <*> copyTargetsChildren c targets <*> onQuantity c quantity <*> onPlayer c copier <*> traverse (onCopyException c) exceptions)
  Effect.Replace (Replace.MkReplace duration uses origin condition replacement) ->
    fmap Effect.Replace (Replace.MkReplace <$> onDuration c duration <*> pure uses <*> pure origin <*> traverse (onCondition c) condition <*> onReplacement c replacement)
  Effect.SkipPhase (SkipPhase.MkSkipPhase who selector uses duration count) ->
    fmap Effect.SkipPhase (SkipPhase.MkSkipPhase <$> onPlayer c who <*> pure selector <*> pure uses <*> onDuration c duration <*> onQuantity c count)
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration kind ref whatRecipient whoRecipient chosenSource quantity riders) ->
    fmap
      Effect.PreventNextDamage
      ( PreventNextDamage.MkPreventNextDamage
          <$> onDuration c duration
          <*> pure kind
          <*> traverse (onObjectRef c) ref
          <*> traverse (onFilter c) whatRecipient
          <*> pure whoRecipient
          <*> traverse (onFilter c) chosenSource
          <*> onQuantity c quantity
          <*> traverse (onEffect c) riders
      )
  Effect.LaterPayment (LaterPayment.MkLaterPayment duration cost effects) -> fmap Effect.LaterPayment (LaterPayment.MkLaterPayment <$> onDuration c duration <*> onCost c cost <*> traverse (onEffect c) effects)
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration kind ref whatRecipient whoRecipient direction chosenSource whatSource riders) ->
    fmap
      Effect.PreventAllDamage
      ( PreventAllDamage.MkPreventAllDamage
          <$> onDuration c duration
          <*> pure kind
          <*> traverse (onObjectRef c) ref
          <*> traverse (onFilter c) whatRecipient
          <*> pure whoRecipient
          <*> pure direction
          <*> traverse (onFilter c) chosenSource
          <*> onFilter c whatSource
          <*> traverse (onEffect c) riders
      )
  Effect.NextDamageInstance (NextDamageInstance.MkNextDamageInstance duration kind ref chosenSource riders outcome) ->
    fmap Effect.NextDamageInstance (NextDamageInstance.MkNextDamageInstance <$> onDuration c duration <*> pure kind <*> onObjectRef c ref <*> onFilter c chosenSource <*> traverse (onEffect c) riders <*> pure outcome)
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration kind amount from whatRecipient whoRecipient to chosenSource uses whatSource) ->
    fmap
      Effect.RedirectDamage
      ( RedirectDamage.MkRedirectDamage
          <$> onDuration c duration
          <*> pure kind
          <*> traverse (onQuantity c) amount
          <*> traverse (onObjectRef c) from
          <*> traverse (onFilter c) whatRecipient
          <*> pure whoRecipient
          <*> onObjectRef c to
          <*> traverse (onFilter c) chosenSource
          <*> pure uses
          <*> onFilter c whatSource
      )
  Effect.Counter (Counter.MkCounter ref named sources instead) ->
    fmap Effect.Counter (Counter.MkCounter <$> onObjectRef c ref <*> traverse (onSlot c) named <*> traverse (onSlot c) sources <*> traverse (counterDestinationChildren c) instead)
  Effect.PutCounters (PutCounters.MkPutCounters kind quantity ref totalAtMost) ->
    fmap Effect.PutCounters (PutCounters.MkPutCounters <$> onCounterKind c kind <*> onQuantity c quantity <*> onObjectRef c ref <*> pure totalAtMost)
  Effect.Divide (Divide.MkDivide quantity among chooser named) -> fmap Effect.Divide (Divide.MkDivide <$> onQuantity c quantity <*> onObjectRef c among <*> onPlayer c chooser <*> onSlot c named)
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters kind quantity named tally) ->
    fmap Effect.RemoveCounters (RemoveCounters.MkRemoveCounters <$> onCounterKind c kind <*> onQuantity c quantity <*> onSlot c named <*> traverse (onSlot c) tally)
  Effect.RemoveCountersAmong (RemoveCountersAmong.MkRemoveCountersAmong count from kind tally) ->
    fmap Effect.RemoveCountersAmong (RemoveCountersAmong.MkRemoveCountersAmong <$> onRemovalCount c count <*> onObjectRef c from <*> onWhichCounters c kind <*> traverse (onSlot c) tally)
  Effect.MoveCounters (MoveCounters.MkMoveCounters from kinds named to) ->
    fmap Effect.MoveCounters (MoveCounters.MkMoveCounters <$> onObjectRef c from <*> onMovedKinds c kinds <*> traverse (onSlot c) named <*> onObjectRef c to)
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom from kind ref) ->
    fmap Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom <$> onSlot c from <*> traverse (onCounterKind c) kind <*> onObjectRef c ref)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters who kind quantity) ->
    fmap Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters <$> onPlayer c who <*> pure kind <*> onQuantity c quantity)
  Effect.RemovePlayerCounters (RemovePlayerCounters.MkRemovePlayerCounters who kind quantity tally) ->
    fmap Effect.RemovePlayerCounters (RemovePlayerCounters.MkRemovePlayerCounters <$> onPlayer c who <*> pure kind <*> onQuantity c quantity <*> traverse (onSlot c) tally)
  Effect.PayAnyEnergy named -> fmap Effect.PayAnyEnergy (onSlot c named)
  Effect.ChooseNumber (ChooseNumber.MkChooseNumber named upTo chooser) ->
    fmap Effect.ChooseNumber (ChooseNumber.MkChooseNumber <$> onSlot c named <*> traverse (onQuantity c) upTo <*> onPlayer c chooser)
  Effect.BindAmount (BindAmount.MkBindAmount quantity named) -> fmap Effect.BindAmount (BindAmount.MkBindAmount <$> onQuantity c quantity <*> onSlot c named)
  Effect.Tap (Tap.MkTap ref tapped) -> fmap Effect.Tap (Tap.MkTap <$> onObjectRef c ref <*> traverse (onSlot c) tapped)
  Effect.Untap ref -> fmap Effect.Untap (onObjectRef c ref)
  Effect.Detain ref -> fmap Effect.Detain (onObjectRef c ref)
  Effect.Goad ref -> fmap Effect.Goad (onObjectRef c ref)
  Effect.Pair ref -> fmap Effect.Pair (onObjectRef c ref)
  Effect.DoesNotUntapNext (DoesNotUntapNext.MkDoesNotUntapNext ref steps) -> fmap (\rewritten -> Effect.DoesNotUntapNext (DoesNotUntapNext.MkDoesNotUntapNext rewritten steps)) (onObjectRef c ref)
  Effect.Transform ref -> fmap Effect.Transform (onObjectRef c ref)
  Effect.Convert ref -> fmap Effect.Convert (onObjectRef c ref)
  Effect.Flip ref -> fmap Effect.Flip (onObjectRef c ref)
  Effect.Meld (Meld.MkMeld objects result) -> fmap Effect.Meld (Meld.MkMeld <$> onObjectRef c objects <*> onMelded c result)
  Effect.PhaseOut ref -> fmap Effect.PhaseOut (onObjectRef c ref)
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown ref characteristics) -> fmap Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown <$> onObjectRef c ref <*> onFaceDown c characteristics)
  Effect.TurnFaceUp named -> fmap Effect.TurnFaceUp (onSlot c named)
  Effect.RemoveFromCombat (RemoveFromCombat.MkRemoveFromCombat removed unblocking) -> fmap (\rewritten -> Effect.RemoveFromCombat (RemoveFromCombat.MkRemoveFromCombat rewritten unblocking)) (onObjectRef c removed)
  Effect.BecomesBlocked named -> fmap Effect.BecomesBlocked (onSlot c named)
  Effect.BlockAttacker ref -> fmap Effect.BlockAttacker (onObjectRef c ref)
  Effect.SwitchBlockers named -> fmap Effect.SwitchBlockers (onSlot c named)
  Effect.ExchangeBlocks (ExchangeBlocks.MkExchangeBlocks first second) -> fmap Effect.ExchangeBlocks (ExchangeBlocks.MkExchangeBlocks <$> onSlot c first <*> onSlot c second)
  Effect.AddPhases _ -> pure effect
  Effect.EndTurn -> pure effect
  Effect.EndCombatPhase -> pure effect
  Effect.GainControl (GainControl.MkGainControl duration ref to) -> fmap Effect.GainControl (GainControl.MkGainControl <$> onDuration c duration <*> onObjectRef c ref <*> onPlayer c to)
  Effect.ExchangeControl sides -> fmap Effect.ExchangeControl (controlSidesChildren c sides)
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name onset duration ability) ->
    fmap Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name <$> onOnset c onset <*> traverse (onDuration c) duration <*> traverse (onAbility c) ability)
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration players playerEffect) ->
    fmap Effect.AffectPlayers (AffectPlayers.MkAffectPlayers <$> onDuration c duration <*> onAffectedPlayers c players <*> onPlayerEffect c playerEffect)
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration blocker attacker arity) ->
    fmap Effect.RequireBlock (RequireBlock.MkRequireBlock <$> onDuration c duration <*> restrictedChildren c blocker <*> traverse (restrictedChildren c) attacker <*> pure arity)
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration attacker defender) ->
    fmap Effect.RequireAttack (RequireAttack.MkRequireAttack <$> onDuration c duration <*> restrictedChildren c attacker <*> traverse (attackTargetRefChildren c) defender)
  Effect.Prohibit (Prohibit.MkProhibit what duration affected) -> fmap Effect.Prohibit (Prohibit.MkProhibit what <$> onDuration c duration <*> restrictedChildren c affected)
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration affected aimedAt) ->
    fmap Effect.ForbidAttack (ForbidAttack.MkForbidAttack <$> onDuration c duration <*> restrictedChildren c affected <*> traverse (onAimedAt c) aimedAt)
  Effect.ForbidBeingBlocked (ForbidBeingBlocked.MkForbidBeingBlocked duration affected barred) ->
    fmap Effect.ForbidBeingBlocked (ForbidBeingBlocked.MkForbidBeingBlocked <$> onDuration c duration <*> onFilter c affected <*> onFilter c barred)
  Effect.CreateEmblem (CreateEmblem.MkCreateEmblem card who) -> fmap Effect.CreateEmblem (CreateEmblem.MkCreateEmblem <$> onCard c card <*> onPlayer c who)
  Effect.BecomeMonarch target -> fmap Effect.BecomeMonarch (monarchTargetChildren c target)
  Effect.TakeTheInitiative _ -> pure effect
  Effect.BecomeProtector named -> fmap Effect.BecomeProtector (onSlot c named)
  Effect.Designate (Designate.MkDesignate designation named value) -> fmap Effect.Designate (Designate.MkDesignate designation <$> onSlot c named <*> traverse (onQuantity c) value)
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel level named) -> fmap (Effect.SetClassLevel . SetClassLevel.MkSetClassLevel level) (onSlot c named)
  Effect.Undesignate (Undesignate.MkUndesignate designation ref) -> fmap (Effect.Undesignate . Undesignate.MkUndesignate designation) (onObjectRef c ref)
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked every locked named) -> fmap (Effect.SetHalfLocked . SetHalfLocked.MkSetHalfLocked every locked) (onSlot c named)
  Effect.CounterAndMark (PermanentActed.MkPermanentActed action permanent) -> fmap (Effect.CounterAndMark . PermanentActed.MkPermanentActed action) (onSlot c permanent)
  Effect.Mentor named -> fmap Effect.Mentor (onSlot c named)
  Effect.Firebend addition -> fmap Effect.Firebend (onManaAddition c addition)
  Effect.Exploit -> pure effect
  Effect.GiveGift -> pure effect
  Effect.ItBecomes _ -> pure effect
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting card host) -> fmap Effect.ExileHaunting (ExileHaunting.MkExileHaunting <$> onSlot c card <*> onSlot c host)
  Effect.PlaySubgame named -> fmap Effect.PlaySubgame (onSlot c named)
  Effect.ChoosePlayer (ChoosePlayer.MkChoosePlayer pool named) -> fmap (Effect.ChoosePlayer . ChoosePlayer.MkChoosePlayer pool) (onSlot c named)
  Effect.ChoosePlayerAtRandom (ChoosePlayerAtRandom.MkChoosePlayerAtRandom pool named) -> fmap (Effect.ChoosePlayerAtRandom . ChoosePlayerAtRandom.MkChoosePlayerAtRandom pool) (onSlot c named)
  Effect.ChoosePermanents (ChoosePermanents.MkChoosePermanents permanents named pileChooser rest) ->
    fmap Effect.ChoosePermanents (ChoosePermanents.MkChoosePermanents <$> onChosenPermanents c permanents <*> onSlot c named <*> traverse (onPlayer c) pileChooser <*> traverse (onSlot c) rest)
  Effect.ChooseGroup (ChooseGroup.MkChooseGroup among named about) -> fmap Effect.ChooseGroup (ChooseGroup.MkChooseGroup <$> groupingChildren c among <*> onSlot c named <*> traverse (onObjectRef c) about)
  Effect.RollDie (RollDie.MkRollDie sides count modifier reading named other roller highest store) ->
    fmap
      Effect.RollDie
      ( RollDie.MkRollDie sides
          <$> onQuantity c count
          <*> traverse (onQuantity c) modifier
          <*> pure reading
          <*> onSlot c named
          <*> traverse (onSlot c) other
          <*> pure roller
          <*> traverse (onSlot c) highest
          <*> traverse (onSlot c) store
      )
  Effect.Rethrow -> pure effect
  Effect.RerollStoredResults named -> fmap Effect.RerollStoredResults (onSlot c named)
  Effect.FlipCoin (FlipCoin.MkFlipCoin count reading named misses flipper) ->
    fmap Effect.FlipCoin (FlipCoin.MkFlipCoin <$> onQuantity c count <*> pure reading <*> onSlot c named <*> traverse (onSlot c) misses <*> onPlayer c flipper)
  Effect.Guess (Guess.MkGuess subject named guesser proposition right) ->
    fmap Effect.Guess (Guess.MkGuess <$> onObjectRef c subject <*> onSlot c named <*> onPlayer c guesser <*> onFilter c proposition <*> onSlot c right)
  Effect.ExileHandThenDraw -> pure effect
  Effect.NoteManaSpent -> pure effect
  Effect.Proliferate named -> fmap Effect.Proliferate (traverse (onSlot c) named)
  Effect.ChooseCardName (ChooseCardName.MkChooseCardName who restriction among) ->
    fmap Effect.ChooseCardName (ChooseCardName.MkChooseCardName <$> onPlayer c who <*> onFilter c restriction <*> traverse (onSlot c) among)
  Effect.ChooseQuality _ -> pure effect
  Effect.Bolster quantity -> fmap Effect.Bolster (onQuantity c quantity)
  Effect.Amass (Amass.MkAmass who quantity kind named) -> fmap Effect.Amass (Amass.MkAmass <$> onPlayer c who <*> onQuantity c quantity <*> pure kind <*> traverse (onSlot c) named)
  Effect.Blight (Blight.MkBlight who quantity named) -> fmap Effect.Blight (Blight.MkBlight <$> onPlayer c who <*> onQuantity c quantity <*> traverse (onSlot c) named)
  Effect.Earthbend (Earthbend.MkEarthbend quantity ref) -> fmap Effect.Earthbend (Earthbend.MkEarthbend <$> onQuantity c quantity <*> onObjectRef c ref)
  Effect.Airbend ref -> fmap Effect.Airbend (onObjectRef c ref)
  Effect.Assimilate ref -> fmap Effect.Assimilate (onObjectRef c ref)
  Effect.TemptWithTheRing -> pure effect
  Effect.OpenAttraction -> pure effect
  Effect.Assemble ref -> fmap Effect.Assemble (traverse (onObjectRef c) ref)
  Effect.Planeswalk -> pure effect
  Effect.ChaosEnsues -> pure effect
  Effect.Abandon -> pure effect
  Effect.ClaimPrize -> pure effect
  Effect.Forage -> pure effect
  Effect.Populate -> pure effect
  Effect.Learn -> pure effect
  Effect.TimeTravel -> pure effect
  Effect.Recruit -> pure effect
  Effect.Cloak who -> fmap Effect.Cloak (onPlayer c who)
  Effect.ManifestDread who -> fmap Effect.ManifestDread (onPlayer c who)
  Effect.Venture _ -> pure effect
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices players filter_ quantity permanents) ->
    fmap Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices <$> onPlayer c players <*> onFilter c filter_ <*> onQuantity c quantity <*> traverse (onSlot c) permanents)
  Effect.Vote (Vote.MkVote starter choices secret) -> fmap Effect.Vote (Vote.MkVote <$> onPlayer c starter <*> voteChoicesChildren c choices <*> pure secret)
  Effect.Bid (Bid.MkBid opening highBid highBidder) -> fmap Effect.Bid (Bid.MkBid <$> onQuantity c opening <*> onSlot c highBid <*> onSlot c highBidder)
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn who skips count) -> fmap Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn <$> onPlayer c who <*> pure skips <*> onQuantity c count)
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary library ref named) ->
    fmap Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary <$> traverse (onPlayer c) library <*> onObjectRef c ref <*> traverse (onSlot c) named)
  Effect.Ante (Ante.MkAnte who ref named) -> fmap Effect.Ante (Ante.MkAnte <$> onPlayer c who <*> onObjectRef c ref <*> traverse (onSlot c) named)
  Effect.PutSticker (PutSticker.MkPutSticker who ref kinds cap free bound) ->
    fmap Effect.PutSticker (PutSticker.MkPutSticker <$> onPlayer c who <*> onObjectRef c ref <*> pure kinds <*> traverse (onQuantity c) cap <*> pure free <*> traverse (onSlot c) bound)
  Effect.Shuffle who -> fmap Effect.Shuffle (onPlayer c who)
  Effect.OfferCast (OfferCast.MkOfferCast ref caster optionality verb offer repetition copied controlWhileResolving named) ->
    fmap
      Effect.OfferCast
      ( OfferCast.MkOfferCast
          <$> onObjectRef c ref
          <*> onPlayer c caster
          <*> pure optionality
          <*> pure verb
          <*> castOfferChildren c offer
          <*> castRepetitionChildren c repetition
          <*> pure copied
          <*> pure controlWhileResolving
          <*> traverse (onSlot c) named
      )
  Effect.OfferNamedCopy _ -> pure effect
  Effect.OfferNotedCopy offer -> fmap Effect.OfferNotedCopy (castOfferChildren c offer)
  Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile duration who ref spending alternativeCost condition verb increase landEnters upTo zone) ->
    fmap
      Effect.GrantPlayFromExile
      ( GrantPlayFromExile.MkGrantPlayFromExile
          <$> onDuration c duration
          <*> onPlayer c who
          <*> onObjectRef c ref
          <*> pure spending
          <*> pure alternativeCost
          <*> traverse (onCondition c) condition
          <*> pure verb
          <*> pure increase
          <*> pure landEnters
          <*> pure upTo
          <*> pure zone
      )
  Effect.GrantLookAtExiled (GrantLookAtExiled.MkGrantLookAtExiled cards followsExiler) -> fmap (\rewritten -> Effect.GrantLookAtExiled (GrantLookAtExiled.MkGrantLookAtExiled rewritten followsExiler)) (onObjectRef c cards)
  Effect.MakePlotted ref -> fmap Effect.MakePlotted (onObjectRef c ref)
  Effect.MakeForetold (MakeForetold.MkMakeForetold cards reducedBy) -> fmap (\rewritten -> Effect.MakeForetold (MakeForetold.MkMakeForetold rewritten reducedBy)) (onObjectRef c cards)
  Effect.MakeWarped ref -> fmap Effect.MakeWarped (onObjectRef c ref)
  Effect.ForEach (ForEach.MkForEach ref members named body individually payGate startingWith) ->
    fmap Effect.ForEach (ForEach.MkForEach <$> onObjectRef c ref <*> pure members <*> onSlot c named <*> traverse (onEffect c) body <*> pure individually <*> traverse (onPayGate c) payGate <*> traverse (onPlayer c) startingWith)
  Effect.ForEachNumber (ForEachNumber.MkForEachNumber upTo named body) -> fmap Effect.ForEachNumber (ForEachNumber.MkForEachNumber <$> onQuantity c upTo <*> onSlot c named <*> traverse (onEffect c) body)
  Effect.Repeat (Repeat.MkRepeat chooser body gate otherChooser ifAgain) ->
    fmap Effect.Repeat (Repeat.MkRepeat <$> onPlayer c chooser <*> traverse (onEffect c) body <*> traverse (onCondition c) gate <*> traverse (onPlayer c) otherChooser <*> traverse (onEffect c) ifAgain)
  Effect.RepeatIf (RepeatIf.MkRepeatIf process condition ifHolds) -> fmap Effect.RepeatIf (RepeatIf.MkRepeatIf <$> traverse (onEffect c) process <*> onCondition c condition <*> traverse (onEffect c) ifHolds)
  Effect.Heal ref -> fmap Effect.Heal (onObjectRef c ref)
  Effect.ChooseNewTargets ref -> fmap Effect.ChooseNewTargets (onObjectRef c ref)
  Effect.ChangeTargets (ChangeTargets.MkChangeTargets objects onto) -> fmap Effect.ChangeTargets (ChangeTargets.MkChangeTargets <$> onObjectRef c objects <*> traverse (onObjectRef c) onto)

-- | A damage clause's children: its recipients and its amount (CR 120.1).
damagePartChildren :: (Applicative f) => EffectChildren card ability f -> DamagePart.DamagePart -> f DamagePart.DamagePart
damagePartChildren c part = case part of
  DamagePart.MkDamagePart ref quantity -> DamagePart.MkDamagePart <$> onObjectRef c ref <*> onQuantity c quantity

-- | An attachment's children: the attached slot and its destination's Filter.
attachTargetChildren :: (Applicative f) => EffectChildren card ability f -> AttachTarget.AttachTarget -> f AttachTarget.AttachTarget
attachTargetChildren c attach = case attach of
  AttachTarget.MkAttachTarget named filter_ -> AttachTarget.MkAttachTarget <$> onSlot c named <*> onFilter c filter_

-- | A mill tally's children: its slot and the Filter it counts by.
millTallyChildren :: (Applicative f) => EffectChildren card ability f -> MillTally.MillTally -> f MillTally.MillTally
millTallyChildren c tally = case tally of
  MillTally.MkMillTally named filter_ -> MillTally.MkMillTally <$> onSlot c named <*> onFilter c filter_

-- | A player and a count: whose, and how many.
playerQuantityChildren :: (Applicative f) => EffectChildren card ability f -> PlayerQuantity.PlayerQuantity -> f PlayerQuantity.PlayerQuantity
playerQuantityChildren c x = case x of
  PlayerQuantity.MkPlayerQuantity who quantity -> PlayerQuantity.MkPlayerQuantity <$> onPlayer c who <*> onQuantity c quantity

-- | A discard's children (CR 701.9).
discardChildren :: (Applicative f) => EffectChildren card ability f -> Discard.Discard -> f Discard.Discard
discardChildren c subject = case subject of
  Discard.Counted (CountedDiscard.MkCountedDiscard who quantity discarded) ->
    fmap Discard.Counted (CountedDiscard.MkCountedDiscard <$> onPlayer c who <*> onQuantity c quantity <*> traverse (onSlot c) discarded)
  Discard.These (TheseDiscard.MkTheseDiscard cards discarded) -> fmap Discard.These (TheseDiscard.MkTheseDiscard <$> onObjectRef c cards <*> traverse (onSlot c) discarded)
  Discard.AnyNumber (AnyNumberDiscard.MkAnyNumberDiscard who (AnyNumberMatching.MkAnyNumberMatching filter_ atMost) discarded) ->
    fmap Discard.AnyNumber (AnyNumberDiscard.MkAnyNumberDiscard <$> onPlayer c who <*> (AnyNumberMatching.MkAnyNumberMatching <$> onFilter c filter_ <*> traverse (onQuantity c) atMost) <*> traverse (onSlot c) discarded)

-- | A life exchange's sides: the slot naming them (CR 701.12c).
exchangeSidesChildren :: (Applicative f) => EffectChildren card ability f -> ExchangeSides.ExchangeSides -> f ExchangeSides.ExchangeSides
exchangeSidesChildren c sides = case sides of
  ExchangeSides.WithController named -> fmap ExchangeSides.WithController (onSlot c named)
  ExchangeSides.BetweenTargets named -> fmap ExchangeSides.BetweenTargets (onSlot c named)

-- | An exchanged value's children: the player or the creature it is read off
-- (CR 701.12g).
exchangedValueChildren :: (Applicative f) => EffectChildren card ability f -> ExchangedValue.ExchangedValue -> f ExchangedValue.ExchangedValue
exchangedValueChildren c value = case value of
  ExchangedValue.LifeTotal who -> fmap ExchangedValue.LifeTotal (onPlayer c who)
  ExchangedValue.Power ref -> fmap ExchangedValue.Power (onObjectRef c ref)
  ExchangedValue.Toughness ref -> fmap ExchangedValue.Toughness (onObjectRef c ref)

-- | A conjure's candidates: cards it names, the objects it duplicates, or an
-- Oracle reference's Filter and count.
conjureCardsChildren :: (Applicative f) => EffectChildren card ability f -> ConjureCards.ConjureCards card -> f (ConjureCards.ConjureCards card)
conjureCardsChildren c cards = case cards of
  ConjureCards.Written written -> fmap ConjureCards.Written (traverse (onConjured c) written)
  ConjureCards.Duplicate ref -> fmap ConjureCards.Duplicate (onObjectRef c ref)
  ConjureCards.Reference (FromReference.MkFromReference predicate amount) -> fmap ConjureCards.Reference (FromReference.MkFromReference <$> onFilter c predicate <*> traverse (onQuantity c) amount)

-- | A copy's original: the object it is read off, or a name (CR 707.2).
copyOriginalChildren :: (Applicative f) => EffectChildren card ability f -> CopyOriginal.CopyOriginal -> f CopyOriginal.CopyOriginal
copyOriginalChildren c original = case original of
  CopyOriginal.OfObject ref -> fmap CopyOriginal.OfObject (onObjectRef c ref)
  CopyOriginal.Named _ -> pure original

-- | A stack copy's targets: CR 707.10d's candidates, where a ref names them.
copyTargetsChildren :: (Applicative f) => EffectChildren card ability f -> CopyTargets.CopyTargets -> f CopyTargets.CopyTargets
copyTargetsChildren c targets = case targets of
  CopyTargets.Copied -> pure targets
  CopyTargets.ChosenByController -> pure targets
  CopyTargets.ForEach ref -> fmap CopyTargets.ForEach (onObjectRef c ref)
  CopyTargets.Stated ref -> fmap CopyTargets.Stated (onObjectRef c ref)

-- | A counter's alternative destination: the Filter it allows and its slot.
counterDestinationChildren :: (Applicative f) => EffectChildren card ability f -> CounterDestination.CounterDestination -> f CounterDestination.CounterDestination
counterDestinationChildren c destination = case destination of
  CounterDestination.MkCounterDestination zone position only named -> CounterDestination.MkCounterDestination zone position <$> traverse (onFilter c) only <*> traverse (onSlot c) named

-- | A control exchange's sides: the slots naming them (CR 701.12b).
controlSidesChildren :: (Applicative f) => EffectChildren card ability f -> ControlSides.ControlSides -> f ControlSides.ControlSides
controlSidesChildren c sides = case sides of
  ControlSides.WithinSlot named -> fmap ControlSides.WithinSlot (onSlot c named)
  ControlSides.WithSource named -> fmap ControlSides.WithSource (onSlot c named)
  ControlSides.BetweenSlots (ControlSlots.MkControlSlots first second) -> fmap ControlSides.BetweenSlots (ControlSlots.MkControlSlots <$> onSlot c first <*> onSlot c second)

-- | A restriction's creatures: a ref naming them, or a class (CR 611.2c).
restrictedChildren :: (Applicative f) => EffectChildren card ability f -> RestrictedCreatures.RestrictedCreatures ObjectRef.ObjectRef -> f (RestrictedCreatures.RestrictedCreatures ObjectRef.ObjectRef)
restrictedChildren c restricted = case restricted of
  RestrictedCreatures.Named ref -> fmap RestrictedCreatures.Named (onObjectRef c ref)
  RestrictedCreatures.Matching filter_ -> fmap RestrictedCreatures.Matching (onFilter c filter_)

-- | What an attack requirement is aimed at: players or permanents.
attackTargetRefChildren :: (Applicative f) => EffectChildren card ability f -> AttackTargetRef.AttackTargetRef -> f AttackTargetRef.AttackTargetRef
attackTargetRefChildren c defender = case defender of
  AttackTargetRef.Players who -> fmap AttackTargetRef.Players (onPlayer c who)
  AttackTargetRef.Permanents ref -> fmap AttackTargetRef.Permanents (onObjectRef c ref)

-- | Who becomes the monarch: a slot, where one names them (CR 726).
monarchTargetChildren :: (Applicative f) => EffectChildren card ability f -> MonarchTarget.MonarchTarget -> f MonarchTarget.MonarchTarget
monarchTargetChildren c target = case target of
  MonarchTarget.TheController -> pure target
  MonarchTarget.ControllerOfSource -> pure target
  MonarchTarget.InSlot named -> fmap MonarchTarget.InSlot (onSlot c named)

-- | The groups a choice is among: the piles' slots (CR 700.3).
groupingChildren :: (Applicative f) => EffectChildren card ability f -> Grouping.Grouping -> f Grouping.Grouping
groupingChildren c grouping = case grouping of
  Grouping.Sectors -> pure grouping
  Grouping.Piles slots -> fmap Grouping.Piles (traverse (onSlot c) slots)

-- | A vote's choices (CR 701.38b): the objects' Filter and slot, the words'
-- slots, or the players' slot.
voteChoicesChildren :: (Applicative f) => EffectChildren card ability f -> VoteChoices.VoteChoices -> f VoteChoices.VoteChoices
voteChoicesChildren c choices = case choices of
  VoteChoices.Objects (VoteObjects.MkVoteObjects filter_ named) -> fmap VoteChoices.Objects (VoteObjects.MkVoteObjects <$> onFilter c filter_ <*> onSlot c named)
  VoteChoices.Words slots -> fmap VoteChoices.Words (traverse (onSlot c) slots)
  VoteChoices.Players (VotePlayers.MkVotePlayers another named) -> fmap (VoteChoices.Players . VotePlayers.MkVotePlayers another) (onSlot c named)

-- | An offered cast's children: its stated cost (CR 118.9), its restriction
-- and the keyword it is offered by.
castOfferChildren :: (Applicative f) => EffectChildren card ability f -> CastOffer.CastOffer -> f CastOffer.CastOffer
castOfferChildren c offer = case offer of
  CastOffer.MkCastOffer transformed withoutPaying payingInstead spending restriction offeredBy faceDown payableFromSpent reduction ->
    CastOffer.MkCastOffer transformed withoutPaying
      <$> traverse (onCost c) payingInstead
      <*> pure spending
      <*> traverse (onFilter c) restriction
      <*> traverse (onKeyword c) offeredBy
      <*> pure faceDown
      <*> pure payableFromSpent
      <*> pure reduction

-- | How many casts an offer allows: the cap, where a Quantity states one.
castRepetitionChildren :: (Applicative f) => EffectChildren card ability f -> CastRepetition.CastRepetition -> f CastRepetition.CastRepetition
castRepetitionChildren c repetition = case repetition of
  CastRepetition.Once -> pure repetition
  CastRepetition.AnyNumber -> pure repetition
  CastRepetition.WithinTotalManaValue quantity -> fmap CastRepetition.WithinTotalManaValue (onQuantity c quantity)
  CastRepetition.UpTo quantity -> fmap CastRepetition.UpTo (onQuantity c quantity)

-- | One visitor per kind of child a trigger condition (CR 603.1) holds. Its
-- payloads are matched positionally in triggerConditionChildren; a type
-- visited whole has walks of its own outside the trigger.
data TriggerChildren f = MkTriggerChildren
  { -- | A Filter a field holds, judged of the objects it describes.
    triggerFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A nested trigger condition: AnyOf's (CR 603.1b), CausedBy's (CR 609.1).
    triggerNested :: TriggerCondition.TriggerCondition -> f TriggerCondition.TriggerCondition,
    -- | A state trigger's condition (CR 603.8), or an attack trigger's, whole.
    triggerState :: Condition.Condition -> f Condition.Condition,
    -- | A slot a bound subject reads (CR 603.7c).
    triggerSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | The turn an event must fall in (CR 102.1), whole.
    triggerTurn :: TurnScope.TurnScope -> f TurnScope.TurnScope,
    -- | An "Nth time each turn" count, with the events it counts.
    triggerOrdinal :: HistoryRead.HistoryRead -> Natural.Natural -> f Natural.Natural,
    -- | How often the event counts (CR 603.2c), with the events it counts.
    triggerFrequency :: HistoryRead.HistoryRead -> TriggerFrequency.TriggerFrequency -> f TriggerFrequency.TriggerFrequency,
    -- | Which counters a counter trigger watches (CR 122.6), whole.
    triggerCounters :: WhichCounters.WhichCounters Keyword.Keyword -> f (WhichCounters.WhichCounters Keyword.Keyword),
    -- | Which mana a mana trigger watches (CR 106.12a), whole.
    triggerMana :: ManaSpecification.ManaSpecification -> f ManaSpecification.ManaSpecification
  }

-- | Every child of a trigger condition left as it is: the base a walk
-- overrides.
triggerLeaves :: (Applicative f) => TriggerChildren f
triggerLeaves =
  MkTriggerChildren
    { triggerFilter = pure,
      triggerNested = pure,
      triggerState = pure,
      triggerSlot = pure,
      triggerTurn = pure,
      triggerOrdinal = const pure,
      triggerFrequency = const pure,
      triggerCounters = pure,
      triggerMana = pure
    }

-- | A TriggerCondition's immediate children, each through its visitor. A
-- nested condition is handed to triggerNested and not descended here.
triggerConditionChildren :: (Applicative f) => TriggerChildren f -> TriggerCondition.TriggerCondition -> f TriggerCondition.TriggerCondition
triggerConditionChildren c condition = case condition of
  TriggerCondition.Enters (EntersTrigger.MkEntersTrigger permanent batching) -> fmap TriggerCondition.Enters (EntersTrigger.MkEntersTrigger <$> actingChildren c permanent <*> pure batching)
  TriggerCondition.StepBegins (StepBegins.MkStepBegins phase ordinal turn) -> fmap TriggerCondition.StepBegins (StepBegins.MkStepBegins phase <$> traverse (triggerOrdinal c HistoryRead.StepBegins) ordinal <*> triggerTurn c turn)
  TriggerCondition.StateIs state -> fmap TriggerCondition.StateIs (triggerState c state)
  TriggerCondition.DealsDamage (DamageTrigger.MkDamageTrigger actor kind recipients) -> fmap (\renamed -> TriggerCondition.DealsDamage (DamageTrigger.MkDamageTrigger renamed kind recipients)) (actingChildren c actor)
  TriggerCondition.IsDealtDamage (PermanentIsDealtDamage.MkPermanentIsDealtDamage (DealtDamage.MkDealtDamage recipient kind by) batching) ->
    fmap (\renamed -> TriggerCondition.IsDealtDamage (PermanentIsDealtDamage.MkPermanentIsDealtDamage (DealtDamage.MkDealtDamage renamed kind by) batching)) (actingChildren c recipient)
  TriggerCondition.PlayerIsDealtDamage (PlayerIsDealtDamage.MkPlayerIsDealtDamage dealt orTheirPlaneswalkers batching turn) -> fmap (TriggerCondition.PlayerIsDealtDamage . PlayerIsDealtDamage.MkPlayerIsDealtDamage dealt orTheirPlaneswalkers batching) (triggerTurn c turn)
  TriggerCondition.PermanentsDealDamageToPlayer (PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer filter_ kind recipient oneOrMore) ->
    fmap (\renamed -> TriggerCondition.PermanentsDealDamageToPlayer (PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer renamed kind recipient oneOrMore)) (triggerFilter c filter_)
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> pure condition
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> pure condition
  TriggerCondition.PlayerTookInitiative -> pure condition
  TriggerCondition.SelfRevealedForMiracle -> pure condition
  TriggerCondition.RevealedThisWay (RevealedThisWay.MkRevealedThisWay card timing) -> fmap (\renamed -> TriggerCondition.RevealedThisWay (RevealedThisWay.MkRevealedThisWay renamed timing)) (triggerFilter c card)
  TriggerCondition.Discards (DiscardTrigger.MkDiscardTrigger subject way batching) ->
    fmap
      (\renamed -> TriggerCondition.Discards (DiscardTrigger.MkDiscardTrigger renamed way batching))
      ( case subject of
          DiscardSubject.Self -> pure subject
          DiscardSubject.Discarder acts -> fmap DiscardSubject.Discarder (playerActsOnChildren c acts)
      )
  TriggerCondition.PlayerDrawsNthCard (PlayerDrawsNthCard.MkPlayerDrawsNthCard _ _) -> pure condition
  TriggerCondition.Attacks trigger -> fmap TriggerCondition.Attacks (attackTriggerChildren c trigger)
  TriggerCondition.Blocks (BlockTrigger.MkBlockTrigger role creature count) ->
    fmap
      TriggerCondition.Blocks
      ( BlockTrigger.MkBlockTrigger role
          <$> triggerFilter c creature
          <*> ( case count of
                  BlockCount.Once -> pure count
                  BlockCount.EachCreature others -> fmap BlockCount.EachCreature (triggerFilter c others)
                  BlockCount.AtLeast floor_ others -> fmap (BlockCount.AtLeast floor_) (triggerFilter c others)
              )
      )
  TriggerCondition.Leaves (LeavesTrigger.MkLeavesTrigger permanent destinations batching) ->
    fmap
      (\renamed -> TriggerCondition.Leaves (LeavesTrigger.MkLeavesTrigger renamed destinations batching))
      ( case permanent of
          LeavingPermanent.Self -> pure permanent
          LeavingPermanent.Matching filter_ -> fmap LeavingPermanent.Matching (triggerFilter c filter_)
          LeavingPermanent.Attached -> pure permanent
          LeavingPermanent.Haunted -> pure permanent
          LeavingPermanent.Bound slot_ -> fmap LeavingPermanent.Bound (triggerSlot c slot_)
      )
  TriggerCondition.LeavesZone (LeavesZoneTrigger.MkLeavesZoneTrigger card from to whose turn batching) ->
    fmap TriggerCondition.LeavesZone (LeavesZoneTrigger.MkLeavesZoneTrigger <$> actingChildren c card <*> pure from <*> pure to <*> pure whose <*> triggerTurn c turn <*> pure batching)
  TriggerCondition.PutIntoZone (PutIntoZoneTrigger.MkPutIntoZoneTrigger card from to turn duringResolution batching) ->
    fmap TriggerCondition.PutIntoZone (PutIntoZoneTrigger.MkPutIntoZoneTrigger <$> actingChildren c card <*> pure from <*> pure to <*> triggerTurn c turn <*> pure duringResolution <*> pure batching)
  TriggerCondition.BecomesTapState (BecomesTapState.MkBecomesTapState status actor batching paying) -> fmap (\renamed -> TriggerCondition.BecomesTapState (BecomesTapState.MkBecomesTapState status renamed batching paying)) (actingChildren c actor)
  TriggerCondition.TappedForMana (TappedForManaTrigger.MkTappedForManaTrigger permanent who mana) ->
    fmap
      TriggerCondition.TappedForMana
      ( TappedForManaTrigger.MkTappedForManaTrigger
          <$> ( case permanent of
                  TappedPermanent.Attached -> pure permanent
                  TappedPermanent.Matching filter_ -> fmap TappedPermanent.Matching (triggerFilter c filter_)
              )
          <*> pure who
          <*> triggerMana c mana
      )
  TriggerCondition.AbilityAddsMana (AbilityAddsMana.MkAbilityAddsMana who source mana) -> fmap TriggerCondition.AbilityAddsMana (AbilityAddsMana.MkAbilityAddsMana who <$> triggerFilter c source <*> triggerMana c mana)
  TriggerCondition.SelfManaAbilityResolves -> pure condition
  TriggerCondition.Countered (CounteringTrigger.MkCounteringTrigger _ _) -> pure condition
  TriggerCondition.DamageToPlayerPrevented _ -> pure condition
  TriggerCondition.SelfPreventsDamage filter_ -> fmap TriggerCondition.SelfPreventsDamage (triggerFilter c filter_)
  TriggerCondition.LifeChanges (LifeTrigger.MkLifeTrigger direction who turn batching frequency) ->
    fmap
      TriggerCondition.LifeChanges
      ( LifeTrigger.MkLifeTrigger direction who
          <$> triggerTurn c turn
          <*> pure batching
          <*> triggerFrequency
            c
            ( case direction of
                LifeDirection.Gained -> HistoryRead.LifeGains
                LifeDirection.Lost -> HistoryRead.LifeLosses
            )
            frequency
      )
  TriggerCondition.CountersChange (CounterTrigger.MkCounterTrigger kind motion holder threshold grouping) ->
    fmap
      TriggerCondition.CountersChange
      ( CounterTrigger.MkCounterTrigger
          <$> triggerCounters c kind
          <*> pure motion
          <*> ( case holder of
                  CounterHolder.Self -> pure holder
                  CounterHolder.Permanent filter_ -> fmap CounterHolder.Permanent (triggerFilter c filter_)
              )
          <*> pure threshold
          <*> pure grouping
      )
  TriggerCondition.SelfBecomesClassLevel _ -> pure condition
  TriggerCondition.SpellCast (SpellCast.MkSpellCast spell turn zone ordinal phase copies) ->
    fmap TriggerCondition.SpellCast (SpellCast.MkSpellCast <$> actingChildren c spell <*> triggerTurn c turn <*> pure zone <*> traverse (triggerOrdinal c HistoryRead.Casts) ordinal <*> pure phase <*> pure copies)
  TriggerCondition.AbilityActivated (AbilityActivatedTrigger.MkAbilityActivatedTrigger filter_ kind) -> fmap (\renamed -> TriggerCondition.AbilityActivated (AbilityActivatedTrigger.MkAbilityActivatedTrigger renamed kind)) (triggerFilter c filter_)
  TriggerCondition.BecomesTargeted (BecomesTargetedTrigger.MkBecomesTargetedTrigger subject relation kinds batching) ->
    fmap
      (\renamed -> TriggerCondition.BecomesTargeted (BecomesTargetedTrigger.MkBecomesTargetedTrigger renamed relation kinds batching))
      ( case subject of
          TargetedSubject.Permanent permanent -> fmap TargetedSubject.Permanent (actingChildren c permanent)
          TargetedSubject.Player _ -> pure subject
          TargetedSubject.PlayerOrPermanent who permanent -> fmap (TargetedSubject.PlayerOrPermanent who) (actingChildren c permanent)
      )
  TriggerCondition.SelfHalfUnlocked _ -> pure condition
  TriggerCondition.RoomFullyUnlocked _ -> pure condition
  TriggerCondition.AnyOf conditions -> fmap TriggerCondition.AnyOf (traverse (triggerNested c) conditions)
  TriggerCondition.CausedBy (CausedBy.MkCausedBy controller narrowed) -> fmap (TriggerCondition.CausedBy . CausedBy.MkCausedBy controller) (triggerNested c narrowed)
  TriggerCondition.TurnedFaceUp permanent -> fmap TriggerCondition.TurnedFaceUp (actingChildren c permanent)
  TriggerCondition.Transforms (TransformTrigger.MkTransformTrigger permanent into) -> fmap (\renamed -> TriggerCondition.Transforms (TransformTrigger.MkTransformTrigger renamed into)) (actingChildren c permanent)
  TriggerCondition.FaceDownPermanentLeavesRevealed -> pure condition
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated designation filter_) ->
    fmap (TriggerCondition.PermanentBecomesDesignated . PermanentBecomesDesignated.MkPermanentBecomesDesignated designation) (triggerFilter c filter_)
  TriggerCondition.AttachedCreatureMentors -> pure condition
  TriggerCondition.Exploits (ExploitTrigger.MkExploitTrigger exploiter exploited) -> fmap TriggerCondition.Exploits (ExploitTrigger.MkExploitTrigger <$> actingChildren c exploiter <*> triggerFilter c exploited)
  TriggerCondition.Crews role -> case role of
    CrewRole.Vehicle frequency -> fmap (TriggerCondition.Crews . CrewRole.Vehicle) (triggerFrequency c HistoryRead.Crews frequency)
    CrewRole.Crewer -> pure condition
  TriggerCondition.PermanentSacrificed acts -> fmap TriggerCondition.PermanentSacrificed (playerActsOnChildren c acts)
  TriggerCondition.SagaFinalChapterTriggers _ -> pure condition
  TriggerCondition.PlayerBecomesMonarch _ -> pure condition
  TriggerCondition.LosesControl subject -> case subject of
    LostPermanent.Self -> pure condition
    LostPermanent.Bound slot_ -> fmap (TriggerCondition.LosesControl . LostPermanent.Bound) (triggerSlot c slot_)
  TriggerCondition.RoomEntered _ -> pure condition
  TriggerCondition.PlayerLosesGame _ -> pure condition
  TriggerCondition.PlayerPlaysLand (PlaysLand.MkPlaysLand who from filter_) -> fmap (TriggerCondition.PlayerPlaysLand . PlaysLand.MkPlaysLand who from) (triggerFilter c filter_)
  TriggerCondition.PlayerManifestsDread _ -> pure condition
  TriggerCondition.PlayerFacesDilemma _ -> pure condition
  TriggerCondition.ChaosEnsues -> pure condition
  TriggerCondition.PlayerRollsPlaneswalker _ -> pure condition
  TriggerCondition.SelfTurnedUpOffDeck -> pure condition
  TriggerCondition.SelfPlaneswalkedAwayFrom -> pure condition
  TriggerCondition.PlayerRollsResult (DieResult.MkDieResult _ _) -> pure condition
  TriggerCondition.Visit -> pure condition
  TriggerCondition.FlipsCoin (CoinFlipTrigger.MkCoinFlipTrigger _ _) -> pure condition
  TriggerCondition.SelfBecomesPlotted -> pure condition
  TriggerCondition.AttachmentChanges (AttachmentTrigger.MkAttachmentTrigger end direction other) -> fmap (TriggerCondition.AttachmentChanges . AttachmentTrigger.MkAttachmentTrigger end direction) (triggerFilter c other)
  TriggerCondition.Reflexive -> pure condition
  TriggerCondition.PermanentActs (PermanentActed.MkPermanentActed action permanent) -> fmap (TriggerCondition.PermanentActs . PermanentActed.MkPermanentActed action) (actingChildren c permanent)
  TriggerCondition.PlayerActs (PlayerActed.MkPlayerActed _ _) -> pure condition
  TriggerCondition.PlacesSticker (PlacesSticker.MkPlacesSticker placer kinds stickered) -> fmap (TriggerCondition.PlacesSticker . PlacesSticker.MkPlacesSticker placer kinds) (triggerFilter c stickered)

attackTriggerChildren :: (Applicative f) => TriggerChildren f -> AttackTrigger.AttackTrigger -> f AttackTrigger.AttackTrigger
attackTriggerChildren c trigger = case trigger of
  AttackTrigger.Attacker (AttackerTrigger.MkAttackerTrigger creature target company frequency while_) ->
    fmap
      AttackTrigger.Attacker
      ( AttackerTrigger.MkAttackerTrigger
          <$> triggerFilter c creature
          <*> ( case target of
                  AttackerTarget.AnyTarget -> pure target
                  AttackerTarget.Defending _ -> pure target
                  AttackerTarget.Permanent filter_ -> fmap AttackerTarget.Permanent (triggerFilter c filter_)
                  AttackerTarget.PlayerWithMostLife -> pure target
              )
          <*> ( case company of
                  AttackCompany.AnyCompany -> pure company
                  AttackCompany.Alone _ -> pure company
                  AttackCompany.WithAnother filter_ -> fmap AttackCompany.WithAnother (triggerFilter c filter_)
              )
          <*> triggerFrequency c HistoryRead.Attacks frequency
          <*> traverse (triggerState c) while_
      )
  AttackTrigger.Attacked (AttackedTrigger.MkAttackedTrigger who attacked attackers) ->
    fmap
      AttackTrigger.Attacked
      (AttackedTrigger.MkAttackedTrigger who attacked <$> traverse (attackCountChildren c) attackers)
  AttackTrigger.Declaration (DeclarationTrigger.MkDeclarationTrigger who attackers frequency while_) ->
    fmap
      AttackTrigger.Declaration
      ( DeclarationTrigger.MkDeclarationTrigger who
          <$> traverse (attackCountChildren c) attackers
          <*> triggerFrequency c HistoryRead.Attacks frequency
          <*> traverse (triggerState c) while_
      )
  AttackTrigger.Unblocked filter_ -> fmap AttackTrigger.Unblocked (triggerFilter c filter_)

-- CR 508.3: an attack trigger's count of declared creatures, its Filter the one
-- child.
attackCountChildren :: (Applicative f) => TriggerChildren f -> AttackCount.AttackCount -> f AttackCount.AttackCount
attackCountChildren c (AttackCount.MkAttackCount measure floor_ filter_) = AttackCount.MkAttackCount measure floor_ <$> triggerFilter c filter_

actingChildren :: (Applicative f) => TriggerChildren f -> ActingPermanent.ActingPermanent -> f ActingPermanent.ActingPermanent
actingChildren c permanent = case permanent of
  ActingPermanent.Self -> pure permanent
  ActingPermanent.Matching filter_ -> fmap ActingPermanent.Matching (triggerFilter c filter_)

playerActsOnChildren :: (Applicative f) => TriggerChildren f -> PlayerActsOn.PlayerActsOn -> f PlayerActsOn.PlayerActsOn
playerActsOnChildren c (PlayerActsOn.MkPlayerActsOn who filter_) = fmap (PlayerActsOn.MkPlayerActsOn who) (triggerFilter c filter_)

-- | One visitor per kind of child a keyword (CR 702) holds. Its payloads are
-- matched positionally in keywordChildren; a type visited whole has walks of
-- its own outside the keyword.
data KeywordChildren f = MkKeywordChildren
  { -- | A Filter the keyword prints: a quality, a criterion, a [type].
    keywordFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A cost the keyword states (CR 118.1), whole.
    keywordCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | A keyword named inside one: backup's printed abilities (CR 702.165a).
    keywordNested :: Keyword.Keyword -> f Keyword.Keyword,
    -- | The creature type a gift's token prints (CR 702.174f, 702.174i).
    keywordSubtype :: Subtype.Subtype -> f Subtype.Subtype,
    -- | Where a count's tally looks, whole.
    keywordScope :: Scope.Scope -> f Scope.Scope,
    -- | The player whose counters a count reads (CR 122.1).
    keywordPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | The events the engine looks back over to expand or pay the keyword.
    keywordLooksBack :: HistoryRead.HistoryRead -> f ()
  }

-- | Every child of a keyword left as it is: the base a walk overrides.
keywordLeaves :: (Applicative f) => KeywordChildren f
keywordLeaves =
  MkKeywordChildren
    { keywordFilter = pure,
      keywordCost = pure,
      keywordNested = pure,
      keywordSubtype = pure,
      keywordScope = pure,
      keywordPlayer = pure,
      keywordLooksBack = const (pure ())
    }

-- | A Keyword's immediate children, each through its visitor. A nested
-- keyword is handed to keywordNested and not descended here.
keywordChildren :: (Applicative f) => KeywordChildren f -> Keyword.Keyword -> f Keyword.Keyword
keywordChildren c keyword_ = case keyword_ of
  Keyword.Deathtouch -> pure keyword_
  Keyword.Defender -> pure keyword_
  Keyword.DoubleStrike -> pure keyword_
  Keyword.Equip (Equip.MkEquip cost quality onto) -> fmap Keyword.Equip (Equip.MkEquip <$> keywordCost c cost <*> traverse (keywordFilter c) quality <*> pure onto)
  Keyword.FirstStrike -> pure keyword_
  Keyword.Flash -> pure keyword_
  Keyword.Flying -> pure keyword_
  Keyword.Haste -> pure keyword_
  Keyword.Hexproof quality -> fmap Keyword.Hexproof (traverse (keywordFilter c) quality)
  Keyword.Indestructible -> pure keyword_
  Keyword.Intimidate -> pure keyword_
  Keyword.Landwalk filter_ -> fmap Keyword.Landwalk (keywordFilter c filter_)
  Keyword.Lifelink -> pure keyword_
  Keyword.LivingMetal -> pure keyword_
  Keyword.MoreThanMeetsTheEye cost -> fmap Keyword.MoreThanMeetsTheEye (keywordCost c cost)
  Keyword.Protection (Protection.MkProtection quality spares) -> fmap Keyword.Protection (Protection.MkProtection <$> keywordFilter c quality <*> traverse (keywordFilter c) spares)
  Keyword.Reach -> pure keyword_
  Keyword.Shroud -> pure keyword_
  Keyword.Trample -> pure keyword_
  Keyword.TrampleOverPlaneswalkers -> pure keyword_
  Keyword.Vigilance -> pure keyword_
  Keyword.Ward (Ward.MkWard cost perEach) -> fmap Keyword.Ward (Ward.MkWard <$> traverse (keywordCost c) cost <*> traverse (keywordCountChildren c) perEach)
  Keyword.Banding -> pure keyword_
  Keyword.Rampage _natural -> pure keyword_
  Keyword.CumulativeUpkeep cost -> fmap Keyword.CumulativeUpkeep (keywordCost c cost)
  Keyword.Flanking -> pure keyword_
  Keyword.Phasing -> pure keyword_
  Keyword.Buyback cost -> fmap Keyword.Buyback (keywordCost c cost)
  Keyword.Shadow -> pure keyword_
  Keyword.Cycling (Cycling.MkCycling cost searchFor) -> fmap Keyword.Cycling (Cycling.MkCycling <$> keywordCost c cost <*> traverse (keywordFilter c) searchFor)
  Keyword.Echo cost -> fmap Keyword.Echo (keywordCost c cost)
  Keyword.Horsemanship -> pure keyword_
  Keyword.Fading _natural -> pure keyword_
  Keyword.Kicker cost -> fmap Keyword.Kicker (keywordCost c cost)
  Keyword.Multikicker cost -> fmap Keyword.Multikicker (keywordCost c cost)
  Keyword.StickerKicker cost -> fmap Keyword.StickerKicker (keywordCost c cost)
  Keyword.Flashback cost -> fmap Keyword.Flashback (keywordCost c cost)
  Keyword.Fear -> pure keyword_
  Keyword.Morph (Morph.MkMorph cost variant) -> fmap (\paid -> Keyword.Morph (Morph.MkMorph paid variant)) (keywordCost c cost)
  Keyword.Amplify _natural -> pure keyword_
  Keyword.Provoke -> pure keyword_
  Keyword.Storm -> keyword_ <$ keywordLooksBack c HistoryRead.Casts
  Keyword.Affinity filter_ -> fmap Keyword.Affinity (keywordFilter c filter_)
  Keyword.Entwine cost -> fmap Keyword.Entwine (keywordCost c cost)
  Keyword.Modular _natural -> pure keyword_
  Keyword.Sunburst -> pure keyword_
  Keyword.Bushido _natural -> pure keyword_
  Keyword.Soulshift _natural -> pure keyword_
  Keyword.Splice (Splice.MkSplice onto cost) -> fmap Keyword.Splice (Splice.MkSplice <$> keywordFilter c onto <*> keywordCost c cost)
  Keyword.Offering filter_ -> fmap Keyword.Offering (keywordFilter c filter_)
  Keyword.Ninjutsu (Ninjutsu.MkNinjutsu cost variant) -> fmap (\paid -> Keyword.Ninjutsu (Ninjutsu.MkNinjutsu paid variant)) (keywordCost c cost)
  Keyword.Epic -> pure keyword_
  Keyword.Paradigm -> pure keyword_
  Keyword.Convoke -> pure keyword_
  Keyword.Dredge _natural -> pure keyword_
  Keyword.Bloodthirst _natural -> keyword_ <$ keywordLooksBack c HistoryRead.Damage
  Keyword.Haunt -> pure keyword_
  Keyword.Replicate cost -> fmap Keyword.Replicate (keywordCost c cost)
  Keyword.Graft _natural -> pure keyword_
  Keyword.Recover cost -> fmap Keyword.Recover (keywordCost c cost)
  Keyword.Ripple _natural -> pure keyword_
  Keyword.SplitSecond -> pure keyword_
  Keyword.Suspend suspend -> fmap Keyword.Suspend (traverse (\(Suspend.MkSuspend counters cost) -> fmap (Suspend.MkSuspend counters) (keywordCost c cost)) suspend)
  Keyword.Vanishing _natural -> pure keyword_
  Keyword.Absorb _natural -> pure keyword_
  Keyword.AuraSwap cost -> fmap Keyword.AuraSwap (keywordCost c cost)
  Keyword.Delve -> pure keyword_
  Keyword.Fortify cost -> fmap Keyword.Fortify (keywordCost c cost)
  Keyword.Frenzy _natural -> pure keyword_
  Keyword.Gravestorm -> keyword_ <$ keywordLooksBack c HistoryRead.Deaths
  Keyword.Poisonous _natural -> pure keyword_
  Keyword.Champion filter_ -> fmap Keyword.Champion (keywordFilter c filter_)
  Keyword.Changeling -> pure keyword_
  Keyword.Evoke cost -> fmap Keyword.Evoke (keywordCost c cost)
  Keyword.Hideaway _natural -> pure keyword_
  Keyword.Prowl cost -> keywordLooksBack c HistoryRead.Damage *> fmap Keyword.Prowl (keywordCost c cost)
  Keyword.Reinforce (Reinforce.MkReinforce amount cost) -> fmap (Keyword.Reinforce . Reinforce.MkReinforce amount) (keywordCost c cost)
  Keyword.Conspire -> pure keyword_
  Keyword.Persist -> pure keyword_
  Keyword.Wither -> pure keyword_
  Keyword.Devour (Devour.MkDevour quality count) -> fmap (\narrowed -> Keyword.Devour (Devour.MkDevour narrowed count)) (traverse (keywordFilter c) quality)
  Keyword.Exalted -> pure keyword_
  Keyword.Unearth cost -> fmap Keyword.Unearth (keywordCost c cost)
  Keyword.Cascade -> pure keyword_
  Keyword.Annihilator _natural -> pure keyword_
  Keyword.LevelUp cost -> fmap Keyword.LevelUp (keywordCost c cost)
  Keyword.Infect -> pure keyword_
  Keyword.BattleCry -> pure keyword_
  Keyword.LivingWeapon -> pure keyword_
  Keyword.Undying -> pure keyword_
  Keyword.Miracle cost -> fmap Keyword.Miracle (keywordCost c cost)
  Keyword.Soulbond -> pure keyword_
  Keyword.Overload cost -> fmap Keyword.Overload (keywordCost c cost)
  Keyword.Unleash -> pure keyword_
  Keyword.Cipher -> pure keyword_
  Keyword.Evolve -> pure keyword_
  Keyword.Extort -> pure keyword_
  Keyword.Fuse -> pure keyword_
  Keyword.Tribute _natural -> pure keyword_
  Keyword.Dethrone -> pure keyword_
  Keyword.HiddenAgenda -> pure keyword_
  Keyword.Outlast cost -> fmap Keyword.Outlast (keywordCost c cost)
  Keyword.Prowess -> pure keyword_
  Keyword.Dash cost -> fmap Keyword.Dash (keywordCost c cost)
  Keyword.Exploit -> pure keyword_
  Keyword.Menace -> pure keyword_
  Keyword.Renown _natural -> pure keyword_
  Keyword.Awaken cost -> fmap Keyword.Awaken (keywordCost c cost)
  Keyword.Devoid -> pure keyword_
  Keyword.Ingest -> pure keyword_
  Keyword.Myriad -> pure keyword_
  Keyword.Surge cost -> keywordLooksBack c HistoryRead.Casts *> fmap Keyword.Surge (keywordCost c cost)
  Keyword.Skulk -> pure keyword_
  Keyword.SpaceSculptor -> pure keyword_
  Keyword.Emerge (Emerge.MkEmerge cost quality) -> fmap Keyword.Emerge (Emerge.MkEmerge <$> keywordCost c cost <*> traverse (keywordFilter c) quality)
  Keyword.Escalate cost -> fmap Keyword.Escalate (keywordCost c cost)
  Keyword.Melee -> pure keyword_
  Keyword.Crew _natural -> pure keyword_
  Keyword.Fabricate _natural -> pure keyword_
  Keyword.Partner -> pure keyword_
  Keyword.PartnerText _partnerText -> pure keyword_
  Keyword.PartnerWith _cardName -> pure keyword_
  Keyword.ChooseABackground -> pure keyword_
  Keyword.DoctorsCompanion -> pure keyword_
  Keyword.Undaunted -> pure keyword_
  Keyword.Improvise -> pure keyword_
  Keyword.Aftermath -> pure keyword_
  Keyword.Embalm cost -> fmap Keyword.Embalm (keywordCost c cost)
  Keyword.Eternalize cost -> fmap Keyword.Eternalize (keywordCost c cost)
  Keyword.Afflict _natural -> pure keyword_
  Keyword.Ascend -> pure keyword_
  Keyword.Assist -> pure keyword_
  Keyword.JumpStart -> pure keyword_
  Keyword.Mentor -> pure keyword_
  Keyword.Afterlife _natural -> pure keyword_
  Keyword.Riot -> pure keyword_
  Keyword.Spectacle cost -> keywordLooksBack c HistoryRead.LifeLosses *> fmap Keyword.Spectacle (keywordCost c cost)
  Keyword.Escape cost -> fmap Keyword.Escape (keywordCost c cost)
  Keyword.Companion (CompanionCondition.CardsOverMinimum _) -> pure keyword_
  Keyword.Companion (CompanionCondition.EachCard filter_) -> fmap (Keyword.Companion . CompanionCondition.EachCard) (keywordFilter c filter_)
  Keyword.Companion (CompanionCondition.DifferentNames filter_) -> fmap (Keyword.Companion . CompanionCondition.DifferentNames) (keywordFilter c filter_)
  Keyword.Companion (CompanionCondition.SharedCardType filter_) -> fmap (Keyword.Companion . CompanionCondition.SharedCardType) (keywordFilter c filter_)
  Keyword.Foretell (ForetellCost.Stated cost) -> fmap (Keyword.Foretell . ForetellCost.Stated) (keywordCost c cost)
  Keyword.Foretell (ForetellCost.ManaCostReducedBy _) -> pure keyword_
  Keyword.Demonstrate -> pure keyword_
  Keyword.Daybound -> pure keyword_
  Keyword.Nightbound -> pure keyword_
  Keyword.Decayed -> pure keyword_
  Keyword.Cleave cost -> fmap Keyword.Cleave (keywordCost c cost)
  Keyword.Training -> pure keyword_
  Keyword.Compleated -> pure keyword_
  Keyword.Reconfigure cost -> fmap Keyword.Reconfigure (keywordCost c cost)
  Keyword.Blitz cost -> fmap Keyword.Blitz (keywordCost c cost)
  Keyword.Casualty _natural -> pure keyword_
  Keyword.ReadAhead -> keyword_ <$ keywordLooksBack c HistoryRead.Entries
  Keyword.Ravenous -> pure keyword_
  Keyword.Squad cost -> fmap Keyword.Squad (keywordCost c cost)
  Keyword.Prototype _prototype -> pure keyword_
  Keyword.ForMirrodin -> pure keyword_
  Keyword.Toxic _natural -> pure keyword_
  Keyword.Backup (Backup.MkBackup count printedAbove) -> fmap (Keyword.Backup . Backup.MkBackup count . Set.fromList) (traverse (keywordNested c) (Set.toList printedAbove))
  Keyword.Bargain -> pure keyword_
  Keyword.Craft (Craft.MkCraft cost (ExileMaterials.MkExileMaterials count orMore whichObjects)) -> fmap Keyword.Craft (Craft.MkCraft <$> keywordCost c cost <*> fmap (ExileMaterials.MkExileMaterials count orMore) (keywordFilter c whichObjects))
  Keyword.Disguise cost -> fmap Keyword.Disguise (keywordCost c cost)
  Keyword.Plot cost -> fmap Keyword.Plot (keywordCost c cost)
  Keyword.Saddle _natural -> pure keyword_
  Keyword.Spree -> pure keyword_
  Keyword.Offspring cost -> fmap Keyword.Offspring (keywordCost c cost)
  Keyword.Gift (Gift.TappedFish subtype) -> fmap (Keyword.Gift . Gift.TappedFish) (keywordSubtype c subtype)
  Keyword.Gift (Gift.Octopus subtype) -> fmap (Keyword.Gift . Gift.Octopus) (keywordSubtype c subtype)
  Keyword.Gift Gift.Card -> pure keyword_
  Keyword.Gift Gift.Food -> pure keyword_
  Keyword.Gift Gift.Treasure -> pure keyword_
  Keyword.Freerunning cost -> keywordLooksBack c HistoryRead.Damage *> fmap Keyword.Freerunning (keywordCost c cost)
  Keyword.Impending (Impending.MkImpending counters cost) -> fmap (Keyword.Impending . Impending.MkImpending counters) (keywordCost c cost)
  Keyword.Exhaust -> pure keyword_
  Keyword.Boast -> keyword_ <$ keywordLooksBack c HistoryRead.Attacks
  Keyword.PowerUp -> keyword_ <$ keywordLooksBack c HistoryRead.Entries
  Keyword.ClassLevel _classLevel -> pure keyword_
  Keyword.Forecast -> pure keyword_
  Keyword.StartYourEngines -> pure keyword_
  Keyword.JobSelect -> pure keyword_
  Keyword.Tiered -> pure keyword_
  Keyword.Exert -> pure keyword_
  Keyword.Enlist -> pure keyword_
  Keyword.Bestow cost -> fmap Keyword.Bestow (keywordCost c cost)
  Keyword.Station -> pure keyword_
  Keyword.Mutate cost -> fmap Keyword.Mutate (keywordCost c cost)
  Keyword.UmbraArmor -> pure keyword_
  Keyword.Retrace -> pure keyword_
  Keyword.Mayhem cost -> keywordLooksBack c HistoryRead.Discards *> fmap Keyword.Mayhem (traverse (keywordCost c) cost)
  Keyword.Madness (MadnessCost.Stated cost) -> fmap (Keyword.Madness . MadnessCost.Stated) (keywordCost c cost)
  Keyword.Madness MadnessCost.OwnManaCost -> pure keyword_
  Keyword.Rebound -> pure keyword_
  Keyword.Scavenge cost -> fmap Keyword.Scavenge (keywordCost c cost)
  Keyword.Encore cost -> fmap Keyword.Encore (keywordCost c cost)
  Keyword.Teamwork _natural -> pure keyword_
  Keyword.WebSlinging cost -> fmap Keyword.WebSlinging (keywordCost c cost)
  Keyword.Sneak cost -> fmap Keyword.Sneak (keywordCost c cost)
  Keyword.Increment -> pure keyword_
  Keyword.Storied -> pure keyword_
  Keyword.Mobilize n -> fmap Keyword.Mobilize (keywordCountChildren c n)
  Keyword.Firebending n -> fmap Keyword.Firebending (keywordCountChildren c n)
  Keyword.Transmute cost -> fmap Keyword.Transmute (keywordCost c cost)
  Keyword.Transfigure cost -> fmap Keyword.Transfigure (keywordCost c cost)
  Keyword.Warp cost -> fmap Keyword.Warp (keywordCost c cost)
  Keyword.Disturb cost -> fmap Keyword.Disturb (keywordCost c cost)
  Keyword.Harmonize cost -> fmap Keyword.Harmonize (keywordCost c cost)
  Keyword.BeamMeUp cost -> fmap Keyword.BeamMeUp (keywordCost c cost)

-- | A keyword's N (CR 702.21b, 702.181a, 702.189a): only a tally and a player
-- count hold children.
keywordCountChildren :: (Applicative f) => KeywordChildren f -> KeywordCount.KeywordCount Keyword.Keyword -> f (KeywordCount.KeywordCount Keyword.Keyword)
keywordCountChildren c n = case n of
  KeywordCount.Fixed _ -> pure n
  KeywordCount.Power -> pure n
  KeywordCount.Tally (KeywordTally.MkKeywordTally within filter_) -> fmap KeywordCount.Tally (KeywordTally.MkKeywordTally <$> keywordScope c within <*> keywordFilter c filter_)
  KeywordCount.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally whose kind) -> fmap (\counted -> KeywordCount.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally counted kind)) (keywordPlayer c whose)

-- | One visitor per kind of child a Cost holds (CR 118.1). Its own record:
-- a cost is generic in its keyword, and its children (a counter kind, a
-- tally's colour) are not another record's.
data CostChildren keyword f = MkCostChildren
  { -- | A Filter a component holds, judged of the objects paid with.
    costFilter :: Filter.Filter keyword -> f (Filter.Filter keyword),
    -- | A counter kind a component adds or removes (CR 122.1), whole.
    costCounterKind :: CounterKind.CounterKind keyword -> f (CounterKind.CounterKind keyword),
    -- | The slot an unannounced amount is bound under (CR 107.3a, 601.2b).
    costSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | The colour of mana symbols a tally counts (CR 202.1).
    costColor :: Color.Color -> f Color.Color,
    -- | The events the engine looks back over to pay the component.
    costLooksBack :: HistoryRead.HistoryRead -> f ()
  }

-- | Every child of a cost left as it is: the base a walk overrides.
costLeaves :: (Applicative f) => CostChildren keyword f
costLeaves =
  MkCostChildren
    { costFilter = pure,
      costCounterKind = pure,
      costSlot = pure,
      costColor = pure,
      costLooksBack = const (pure ())
    }

-- | A Cost's children: its components', each through costComponentChildren.
-- The mana part holds no child.
costChildren :: (Applicative f) => CostChildren keyword f -> Cost.Cost keyword -> f (Cost.Cost keyword)
costChildren c (Cost.MkCost mana components) = Cost.MkCost mana <$> traverse (costComponentChildren c) components

-- | A CostComponent's children, its payload matched positionally.
costComponentChildren :: (Applicative f) => CostChildren keyword f -> CostComponent.CostComponent keyword -> f (CostComponent.CostComponent keyword)
costComponentChildren c component = case component of
  CostComponent.TapThis -> pure component
  CostComponent.UntapThis -> pure component
  CostComponent.SacrificeThis -> pure component
  CostComponent.ReturnThis -> pure component
  CostComponent.PayLife amount -> CostComponent.PayLife <$> costAmountChildren c amount
  CostComponent.PayComputedLife _ -> pure component
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) -> fmap CostComponent.Sacrifice (Sacrifice.MkSacrifice <$> objectCountChildren c n <*> costFilter c criterion)
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower power criterion) -> fmap CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower power <$> costFilter c criterion)
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion sharing) -> fmap CostComponent.TapPermanents (TapPermanents.MkTapPermanents <$> costAmountChildren c n <*> costFilter c criterion <*> pure sharing)
  CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents n criterion) -> fmap CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents n <$> costFilter c criterion)
  CostComponent.ExilePermanents (ExilePermanents.MkExilePermanents n criterion) -> fmap CostComponent.ExilePermanents (ExilePermanents.MkExilePermanents n <$> costFilter c criterion)
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion selection) -> fmap CostComponent.DiscardCards (DiscardCards.MkDiscardCards <$> objectCountChildren c n <*> costFilter c criterion <*> pure selection)
  CostComponent.DiscardThis _ -> pure component
  CostComponent.PutCardFromHandOntoBattlefield criterion -> CostComponent.PutCardFromHandOntoBattlefield <$> costFilter c criterion
  CostComponent.PayEnergy amount -> CostComponent.PayEnergy <$> costAmountChildren c amount
  -- CR 606.3: once per turn, so paying reads the turn's loyalty activations.
  CostComponent.AddLoyaltyToThis _ -> component <$ costLooksBack c HistoryRead.LoyaltyActivations
  CostComponent.RemoveLoyaltyFromThis amount -> CostComponent.RemoveLoyaltyFromThis <$ costLooksBack c HistoryRead.LoyaltyActivations <*> costAmountChildren c amount
  CostComponent.RemoveCountersFromThis counters -> CostComponent.RemoveCountersFromThis <$> countersOnThisChildren c counters
  CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents n which criterion spread) -> fmap CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents <$> costAmountChildren c n <*> whichCountersChildren c which <*> costFilter c criterion <*> pure spread)
  CostComponent.PutCountersOnThis counters -> CostComponent.PutCountersOnThis <$> countersOnThisChildren c counters
  CostComponent.PutCountersOnPayer _ -> pure component
  CostComponent.Blight amount -> CostComponent.Blight <$> costAmountChildren c amount
  CostComponent.Forage -> pure component
  CostComponent.FlipCoin -> pure component
  CostComponent.ExileThisFrom _ -> pure component
  CostComponent.ExileThis -> pure component
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) -> fmap CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n <$> costFilter c criterion)
  CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n orMore criterion) -> fmap CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n orMore <$> costFilter c criterion)
  CostComponent.ExileTopFromGraveyard criterion -> CostComponent.ExileTopFromGraveyard <$> costFilter c criterion
  CostComponent.CollectEvidence _ -> pure component
  CostComponent.CollectEvidenceOfTargets -> pure component
  CostComponent.ExileCardsTotalling (ExileCardsTotalling.MkExileCardsTotalling threshold tally criterion) -> fmap CostComponent.ExileCardsTotalling (ExileCardsTotalling.MkExileCardsTotalling threshold <$> cardTallyChildren c tally <*> costFilter c criterion)
  CostComponent.ExileCardsFromHand (ExileCardsFromHand.MkExileCardsFromHand n criterion) -> fmap CostComponent.ExileCardsFromHand (ExileCardsFromHand.MkExileCardsFromHand <$> costAmountChildren c n <*> costFilter c criterion)
  CostComponent.RevealCardFromHand criterion -> CostComponent.RevealCardFromHand <$> costFilter c criterion
  CostComponent.Behold (Behold.MkBehold n criterion) -> fmap CostComponent.Behold (Behold.MkBehold n <$> costFilter c criterion)
  CostComponent.BeholdAndExile criterion -> CostComponent.BeholdAndExile <$> costFilter c criterion
  CostComponent.MillCards _ -> pure component
  CostComponent.RevealTopOfLibrary _ -> pure component
  CostComponent.ChooseOpponent _ -> pure component
  CostComponent.Waterbend amount -> CostComponent.Waterbend <$> costAmountChildren c amount
  CostComponent.WaterbendInstead _ -> pure component

-- | An amount a cost component names: only an announced one holds a slot.
costAmountChildren :: (Applicative f) => CostChildren keyword f -> CostAmount.CostAmount -> f CostAmount.CostAmount
costAmountChildren c amount = case amount of
  CostAmount.Fixed _ -> pure amount
  CostAmount.Announced announced -> CostAmount.Announced <$> costSlot c announced

-- | How many objects a component takes: only an exact amount holds a child.
objectCountChildren :: (Applicative f) => CostChildren keyword f -> ObjectCount.ObjectCount -> f ObjectCount.ObjectCount
objectCountChildren c n = case n of
  ObjectCount.Exactly amount -> ObjectCount.Exactly <$> costAmountChildren c amount
  ObjectCount.Every -> pure n

-- | Which counters a removal takes: only a named kind holds a child.
whichCountersChildren :: (Applicative f) => CostChildren keyword f -> WhichCounters.WhichCounters keyword -> f (WhichCounters.WhichCounters keyword)
whichCountersChildren c which = case which of
  WhichCounters.OfKind kind -> WhichCounters.OfKind <$> costCounterKind c kind
  WhichCounters.OfAnyKind -> pure which

-- | Counters put on or removed from the object the cost is on.
countersOnThisChildren :: (Applicative f) => CostChildren keyword f -> CountersOnThis.CountersOnThis keyword -> f (CountersOnThis.CountersOnThis keyword)
countersOnThisChildren c (CountersOnThis.MkCountersOnThis kind n) = CountersOnThis.MkCountersOnThis <$> costCounterKind c kind <*> pure n

-- | What an exile-totalling cost sums: only a colour tally holds a child.
cardTallyChildren :: (Applicative f) => CostChildren keyword f -> CardTally.CardTally -> f CardTally.CardTally
cardTallyChildren c tally = case tally of
  CardTally.ManaValue -> pure tally
  CardTally.ManaSymbolsOf color -> CardTally.ManaSymbolsOf <$> costColor c color

-- | One visitor per kind of child a player effect (CR 611.1's third clause)
-- holds. Its payloads are matched positionally in playerEffectChildren; a type
-- visited whole has walks of its own outside the player effect.
data PlayerEffectChildren f = MkPlayerEffectChildren
  { -- | A Filter a field holds, judged of the objects it describes.
    playerFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A Quantity a field holds: a count a modifier repeats by, a free cast's
    -- mana value limit.
    playerQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A die roll modifier's cost (CR 706.2a), whole.
    playerCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | A cost component an effect adds to a cost (CR 118.8), whole.
    playerCostComponent :: CostComponent.CostComponent Keyword.Keyword -> f (CostComponent.CostComponent Keyword.Keyword),
    -- | The colour of mana symbols an addition is scaled by (CR 107.4).
    playerColor :: Color.Color -> f Color.Color,
    -- | A keyword an ability is designated by (CR 702), whole.
    playerDesignator :: KeywordDesignator.KeywordDesignator Keyword.Keyword -> f (KeywordDesignator.KeywordDesignator Keyword.Keyword),
    -- | What narrows a cost modifier's abilities (CR 602.2b), whole.
    playerActivations :: ActivationCriteria.ActivationCriteria -> f ActivationCriteria.ActivationCriteria,
    -- | The zone a permission reaches into, whole.
    playerZone :: InZone.InZone -> f InZone.InZone,
    -- | The turn an "only the first" limit counts over (CR 500.1), whole.
    playerTurn :: TurnScope.TurnScope -> f TurnScope.TurnScope,
    -- | Which mana an effect keeps or spends (CR 106.4), whole.
    playerManaFilter :: ManaFilter.ManaFilter -> f ManaFilter.ManaFilter,
    -- | A spend-as-though clause (CR 609.4b), whole.
    playerSpending :: SpendManaAsThough.SpendManaAsThough -> f SpendManaAsThough.SpendManaAsThough,
    -- | What tapped-for mana may be spent on (CR 106.6), whole.
    playerManaRestriction :: ManaRestriction.ManaRestriction -> f ManaRestriction.ManaRestriction,
    -- | Which damage an effect matches (CR 120.1), whole.
    playerDamage :: DamagePattern.DamagePattern -> f DamagePattern.DamagePattern,
    -- | The events the engine looks back over to apply the effect.
    playerLooksBack :: HistoryRead.HistoryRead -> f ()
  }

-- | Every child of a player effect left as it is: the base a walk overrides.
playerEffectLeaves :: (Applicative f) => PlayerEffectChildren f
playerEffectLeaves =
  MkPlayerEffectChildren
    { playerFilter = pure,
      playerQuantity = pure,
      playerCost = pure,
      playerCostComponent = pure,
      playerColor = pure,
      playerDesignator = pure,
      playerActivations = pure,
      playerZone = pure,
      playerTurn = pure,
      playerManaFilter = pure,
      playerSpending = pure,
      playerManaRestriction = pure,
      playerDamage = pure,
      playerLooksBack = const (pure ())
    }

-- | A PlayerEffect's children, its payload matched positionally.
playerEffectChildren :: (Applicative f) => PlayerEffectChildren f -> PlayerEffect.PlayerEffect -> f PlayerEffect.PlayerEffect
playerEffectChildren c effect = case effect of
  PlayerEffect.CantCastSpells -> pure effect
  PlayerEffect.CantActivateAbilities designator filter_ kind -> PlayerEffect.CantActivateAbilities <$> traverse (playerDesignator c) designator <*> playerFilter c filter_ <*> pure kind
  -- CR 601.3's limit counts the turn's casts.
  PlayerEffect.CantCastMoreThan _ -> effect <$ playerLooksBack c HistoryRead.Casts
  PlayerEffect.ModifyCost modifier -> PlayerEffect.ModifyCost <$> costModifierChildren c modifier
  PlayerEffect.AlternativeActivationCost (AlternativeActivationCost.MkAlternativeActivationCost grantedBy onlyFirst cost) ->
    fmap PlayerEffect.AlternativeActivationCost (AlternativeActivationCost.MkAlternativeActivationCost <$> playerDesignator c grantedBy <*> traverse (onlyFirstChildren c HistoryRead.Activations) onlyFirst <*> pure cost)
  PlayerEffect.PlayAdditionalLands _ -> pure effect
  PlayerEffect.NoMaximumHandSize -> pure effect
  PlayerEffect.SetMaximumHandSize _ -> pure effect
  PlayerEffect.IncreaseMaximumHandSize _ -> pure effect
  PlayerEffect.ReduceMaximumHandSize _ -> pure effect
  PlayerEffect.DontLoseUnspentMana which -> PlayerEffect.DontLoseUnspentMana <$> playerManaFilter c which
  PlayerEffect.LoseLifeForUnspentMana -> pure effect
  PlayerEffect.MayPayLifeForMana _ _ -> pure effect
  PlayerEffect.SpendManaAsThough clause -> PlayerEffect.SpendManaAsThough <$> playerSpending c clause
  PlayerEffect.CantBeTargetedBy players quality -> PlayerEffect.CantBeTargetedBy players <$> traverse (playerFilter c) quality
  PlayerEffect.HasProtectionFrom quality -> PlayerEffect.HasProtectionFrom <$> playerFilter c quality
  PlayerEffect.CastAsThoughItHadFlash filter_ onlyFirst -> PlayerEffect.CastAsThoughItHadFlash <$> playerFilter c filter_ <*> traverse (onlyFirstChildren c HistoryRead.Casts) onlyFirst
  PlayerEffect.MayPlayAsThoughItHadFlash filter_ -> PlayerEffect.MayPlayAsThoughItHadFlash <$> playerFilter c filter_
  PlayerEffect.KeywordRiderGrant (KeywordRiderGrant.MkKeywordRiderGrant abilities relaxes permanents) ->
    fmap PlayerEffect.KeywordRiderGrant (KeywordRiderGrant.MkKeywordRiderGrant <$> playerDesignator c abilities <*> pure relaxes <*> playerFilter c permanents)
  PlayerEffect.LoyaltyGrant (LoyaltyGrant.MkLoyaltyGrant relaxes permanents) ->
    let -- CR 606.3: a once-per-turn grant is spent off the turn's activations.
        looksBack = case relaxes of
          LoyaltyClause.Window -> pure ()
          LoyaltyClause.OncePerTurn -> playerLooksBack c HistoryRead.Activations
     in fmap PlayerEffect.LoyaltyGrant (LoyaltyGrant.MkLoyaltyGrant relaxes <$ looksBack <*> playerFilter c permanents)
  PlayerEffect.MayTapForMana (TapForMana.MkTapForMana permanents restriction) ->
    fmap PlayerEffect.MayTapForMana (TapForMana.MkTapForMana <$> playerFilter c permanents <*> traverse (playerManaRestriction c) restriction)
  PlayerEffect.CantBeCountered (CounterProhibition.MkCounterProhibition countered counterers) ->
    fmap PlayerEffect.CantBeCountered (CounterProhibition.MkCounterProhibition <$> playerFilter c countered <*> playerFilter c counterers)
  PlayerEffect.NextSpell filter_ -> PlayerEffect.NextSpell <$> playerFilter c filter_
  PlayerEffect.RestrictTargeting (TargetRestriction.MkTargetRestriction affected kind aimers) ->
    fmap PlayerEffect.RestrictTargeting (TargetRestriction.MkTargetRestriction <$> playerFilter c affected <*> pure kind <*> playerFilter c aimers)
  PlayerEffect.TargetPastHexproof (HexproofExemption.MkHexproofExemption permanents players) ->
    fmap PlayerEffect.TargetPastHexproof (HexproofExemption.MkHexproofExemption <$> playerFilter c permanents <*> pure players)
  PlayerEffect.DamageCantBePrevented damage -> PlayerEffect.DamageCantBePrevented <$> playerDamage c damage
  PlayerEffect.DamageCantBeRedirected damage -> PlayerEffect.DamageCantBeRedirected <$> playerDamage c damage
  PlayerEffect.CantSearchLibraries _ -> pure effect
  PlayerEffect.CantBecomeMonarch -> pure effect
  PlayerEffect.StateBasedActionDoesntApply _ -> pure effect
  PlayerEffect.CantSetSchemesInMotion -> pure effect
  PlayerEffect.EntriesTrigger (EntriesTrigger.MkEntriesTrigger entrants triggering) ->
    fmap PlayerEffect.EntriesTrigger (EntriesTrigger.MkEntriesTrigger <$> playerFilter c entrants <*> pure triggering)
  PlayerEffect.AdditionalTriggers bearers -> PlayerEffect.AdditionalTriggers <$> playerFilter c bearers
  PlayerEffect.ControlledWhileSearching -> pure effect
  PlayerEffect.YouChoose _ -> pure effect
  PlayerEffect.BlocksFromPiles -> pure effect
  PlayerEffect.ExilesFoundWhileSearching -> pure effect
  PlayerEffect.CantAttackWithCreatures -> pure effect
  PlayerEffect.CantCastMatching filter_ -> PlayerEffect.CantCastMatching <$> playerFilter c filter_
  PlayerEffect.CastOnlyAtSorcerySpeed -> pure effect
  PlayerEffect.CantPlayLands filter_ -> PlayerEffect.CantPlayLands <$> playerFilter c filter_
  PlayerEffect.CastFrom (CastFromZone.MkCastFromZone from matching limit verb pool additionalCosts reduction spending) ->
    fmap PlayerEffect.CastFrom (CastFromZone.MkCastFromZone <$> playerZone c from <*> playerFilter c matching <*> pure limit <*> pure verb <*> pure pool <*> traverse (playerCostComponent c) additionalCosts <*> pure reduction <*> pure spending)
  PlayerEffect.PlayLandsFrom from -> PlayerEffect.PlayLandsFrom <$> playerZone c from
  PlayerEffect.PlotFrom (PlotFromZone.MkPlotFromZone from matching) ->
    fmap PlayerEffect.PlotFrom (PlotFromZone.MkPlotFromZone <$> playerZone c from <*> playerFilter c matching)
  PlayerEffect.CastWithoutPayingManaCost (FreeCast.MkFreeCast spells amount from) ->
    fmap PlayerEffect.CastWithoutPayingManaCost (FreeCast.MkFreeCast <$> playerFilter c spells <*> traverse (playerQuantity c) amount <*> pure from)
  PlayerEffect.CantGetCounters _ -> pure effect
  -- CR 705.3: "the first coin you flip each turn" counts the turn's flips.
  PlayerEffect.StateCoinFlip (StatedFlip.MkStatedFlip _ _ firstEachTurn) -> effect <$ Monad.when firstEachTurn (playerLooksBack c HistoryRead.CoinFlips)
  PlayerEffect.ModifyDieRoll (ModifiedRoll.MkModifiedRoll sides natural modifier cost limit) ->
    fmap PlayerEffect.ModifyDieRoll (ModifiedRoll.MkModifiedRoll sides natural modifier <$> traverse (playerCost c) cost <*> pure limit)
  PlayerEffect.AdditionalVotes _ -> pure effect
  PlayerEffect.AdditionalSurveilCards _ -> pure effect
  PlayerEffect.PutTappedAsYouCascade filter_ -> PlayerEffect.PutTappedAsYouCascade <$> playerFilter c filter_
  PlayerEffect.CantGainLife -> pure effect
  PlayerEffect.CantLoseLife -> pure effect
  PlayerEffect.Precluded _ -> pure effect
  PlayerEffect.Shows _ -> pure effect
  PlayerEffect.RevealsDraw _ -> pure effect

-- | An "only the first each turn" limit's children: the turn it counts over,
-- and the events it counts.
onlyFirstChildren :: (Applicative f) => PlayerEffectChildren f -> HistoryRead.HistoryRead -> TurnScope.TurnScope -> f TurnScope.TurnScope
onlyFirstChildren c counted turn = playerLooksBack c counted *> playerTurn c turn

-- | A cost modifier's children (CR 601.2f, 602.2b).
costModifierChildren :: (Applicative f) => PlayerEffectChildren f -> CostModifier.CostModifier -> f CostModifier.CostModifier
costModifierChildren c (CostModifier.MkCostModifier subject matching whichTargets perTarget perEach onlyFirst change) =
  CostModifier.MkCostModifier
    <$> costSubjectChildren c subject
    <*> playerFilter c matching
    <*> traverse (playerFilter c) whichTargets
    <*> traverse (perTargetChildren c) perTarget
    <*> traverse (playerQuantity c) perEach
    <*> traverse (onlyFirstChildren c (costSubjectHistory subject)) onlyFirst
    <*> costChangeChildren c change

-- | The turn's history a cost modifier's "only the first" reads: CR 601.2i's
-- casts for a spell, CR 602.2's activations for an ability.
costSubjectHistory :: CostSubject.CostSubject -> HistoryRead.HistoryRead
costSubjectHistory subject = case subject of
  CostSubject.Spells -> HistoryRead.Casts
  CostSubject.Activations _ -> HistoryRead.Activations

-- | What a cost modifier applies to: only abilities hold a child.
costSubjectChildren :: (Applicative f) => PlayerEffectChildren f -> CostSubject.CostSubject -> f CostSubject.CostSubject
costSubjectChildren c subject = case subject of
  CostSubject.Spells -> pure subject
  CostSubject.Activations criteria -> CostSubject.Activations <$> playerActivations c criteria

-- | How a cost modifier counts targets (CR 601.2f).
perTargetChildren :: (Applicative f) => PlayerEffectChildren f -> PerTarget.PerTarget -> f PerTarget.PerTarget
perTargetChildren c (PerTarget.MkPerTarget matching each) = PerTarget.MkPerTarget <$> playerFilter c matching <*> traverse (playerQuantity c) each

-- | How a cost modifier changes the cost: only an addition holds children.
costChangeChildren :: (Applicative f) => PlayerEffectChildren f -> CostChange.CostChange -> f CostChange.CostChange
costChangeChildren c change = case change of
  CostChange.Increase _ -> pure change
  CostChange.Reduce _ -> pure change
  CostChange.Add (CostAddition.MkCostAddition components scale) -> fmap CostChange.Add (CostAddition.MkCostAddition <$> traverse (playerCostComponent c) components <*> costScaleChildren c scale)

-- | How often an addition applies: only a per-symbol scale names a colour.
costScaleChildren :: (Applicative f) => PlayerEffectChildren f -> CostScale.CostScale -> f CostScale.CostScale
costScaleChildren c scale = case scale of
  CostScale.Once -> pure scale
  CostScale.PerColoredSymbol color -> CostScale.PerColoredSymbol <$> playerColor c color

-- | One visitor per kind of child a replacement effect (CR 614.1) holds. Its
-- event classes' payloads are matched positionally in
-- replacementEffectChildren; a type visited whole has walks of its own
-- outside the replacement effect.
data ReplacementChildren card ability effect f = MkReplacementChildren
  { -- | A Filter a field holds, judged of the objects it describes.
    replacementFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A Quantity a field holds: a scaling's addend, a ceiling, a count.
    replacementQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | An effect the replacement runs: instead, as a rider, or when you do.
    replacementEffect :: effect -> f effect,
    -- | An ability an entering permanent gains (CR 614.1c).
    replacementAbility :: ability -> f ability,
    -- | A token a token replacement adds (CR 614.1a).
    replacementCard :: card -> f card,
    -- | A keyword an entering permanent gains (CR 614.1c).
    replacementKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | A subtype an entry option adds (CR 208.2b).
    replacementSubtype :: Subtype.Subtype -> f Subtype.Subtype,
    -- | A counter kind (CR 122.1), whole.
    replacementCounterKind :: CounterKind.CounterKind Keyword.Keyword -> f (CounterKind.CounterKind Keyword.Keyword),
    -- | The cost a turn-up procedure must have paid (CR 702.37b), whole: engine-baked.
    replacementProcedureCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | Whose object, event or turn the replacement watches (CR 109.5), whole.
    replacementWhose :: ControllerRelation.ControllerRelation -> f ControllerRelation.ControllerRelation,
    -- | Where redirected damage goes (CR 614.9), whole.
    replacementRecipient :: Recipient.Recipient -> f Recipient.Recipient,
    -- | Which damage the replacement watches (CR 120.1), whole.
    replacementDamage :: DamagePattern.DamagePattern -> f DamagePattern.DamagePattern,
    -- | Counters an object enters or turns up with (CR 614.1c), whole.
    replacementCounters :: WithCounters.WithCounters -> f WithCounters.WithCounters,
    -- | An "as a copy" rewrite (CR 707.5), whole.
    replacementCopy :: AsCopy.AsCopy ability effect -> f (AsCopy.AsCopy ability effect),
    -- | The events the engine looks back over to apply the replacement.
    replacementLooksBack :: HistoryRead.HistoryRead -> f ()
  }

-- | Every child of a replacement effect left as it is: the base a walk overrides.
replacementLeaves :: (Applicative f) => ReplacementChildren card ability effect f
replacementLeaves =
  MkReplacementChildren
    { replacementFilter = pure,
      replacementQuantity = pure,
      replacementEffect = pure,
      replacementAbility = pure,
      replacementCard = pure,
      replacementKeyword = pure,
      replacementSubtype = pure,
      replacementCounterKind = pure,
      replacementProcedureCost = pure,
      replacementWhose = pure,
      replacementRecipient = pure,
      replacementDamage = pure,
      replacementCounters = pure,
      replacementCopy = pure,
      replacementLooksBack = const (pure ())
    }

-- | A ReplacementEffect's children, its payload matched positionally.
replacementEffectChildren :: (Applicative f) => ReplacementChildren card ability effect f -> ReplacementEffect.ReplacementEffect card ability effect -> f (ReplacementEffect.ReplacementEffect card ability effect)
replacementEffectChildren c replacement = case replacement of
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR (ZoneChangePattern.MkZoneChangePattern whenDestination whose what discarded causedBy resolving) destination revealing shuffling position optional whenYouDo) ->
    fmap ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR <$> (ZoneChangePattern.MkZoneChangePattern whenDestination <$> replacementWhose c whose <*> replacementFilter c what <*> pure discarded <*> pure causedBy <*> pure resolving) <*> pure destination <*> pure revealing <*> pure shuffling <*> pure position <*> pure optional <*> traverse (replacementEffect c) whenYouDo)
  ReplacementEffect.EntryR (EntryR.MkEntryR matching rewrite) -> fmap ReplacementEffect.EntryR (EntryR.MkEntryR <$> replacementFilter c matching <*> entryRewriteChildren c rewrite)
  ReplacementEffect.DamageR (DamageR.MkDamageR matching rewrite riders) ->
    fmap ReplacementEffect.DamageR (DamageR.MkDamageR <$> replacementDamage c matching <*> damageRewriteChildren c rewrite <*> traverse (replacementEffect c) riders)
  ReplacementEffect.DestructionR (DestructionR.MkDestructionR matching rewrite) -> fmap ReplacementEffect.DestructionR (DestructionR.MkDestructionR <$> traverse (replacementFilter c) matching <*> pure rewrite)
  ReplacementEffect.CounterR (CounterR.MkCounterR (CounterPattern.MkCounterPattern kind subject whose onWhat onWho) scaling) ->
    fmap ReplacementEffect.CounterR (CounterR.MkCounterR <$> (CounterPattern.MkCounterPattern <$> traverse (replacementCounterKind c) kind <*> counterSubjectChildren c subject <*> replacementWhose c whose <*> replacementFilter c onWhat <*> traverse (replacementWhose c) onWho) <*> scalingChildren c scaling)
  ReplacementEffect.TokenR (TokenR.MkTokenR (TokenPattern.MkTokenPattern whose whatToken effectsOnly) scaling plus) ->
    fmap ReplacementEffect.TokenR (TokenR.MkTokenR <$> (TokenPattern.MkTokenPattern <$> replacementWhose c whose <*> replacementFilter c whatToken <*> pure effectsOnly) <*> traverse (scalingChildren c) scaling <*> traverse (tokenPlusChildren c) plus)
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR matching requiring paying rewrite) ->
    fmap ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR <$> replacementFilter c matching <*> pure requiring <*> traverse (replacementProcedureCost c) paying <*> turnUpRewriteChildren c rewrite)
  ReplacementEffect.AttachR (AttachR.MkAttachR matching rewrite) -> fmap ReplacementEffect.AttachR (AttachR.MkAttachR <$> replacementFilter c matching <*> attachRewriteChildren c rewrite)
  ReplacementEffect.UntapR (UntapR.MkUntapR during rewrite) -> fmap ReplacementEffect.UntapR (UntapR.MkUntapR during <$> untapRewriteChildren c rewrite)
  ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR (LifeLossPattern.MkLifeLossPattern whose cause) rewrite) ->
    fmap ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR <$> (LifeLossPattern.MkLifeLossPattern <$> replacementWhose c whose <*> pure cause) <*> lifeLossRewriteChildren c rewrite)
  ReplacementEffect.LifeGainR (LifeGainR.MkLifeGainR whose rewrite) -> fmap ReplacementEffect.LifeGainR (LifeGainR.MkLifeGainR <$> replacementWhose c whose <*> lifeGainRewriteChildren c rewrite)
  -- CR 504.1: "except the first in your draw step" reads the turn's log back to the step's beginning.
  ReplacementEffect.DrawR (DrawR.MkDrawR whose exceptFirst rewrite) ->
    fmap ReplacementEffect.DrawR (DrawR.MkDrawR <$> replacementWhose c whose <*> (exceptFirst <$ Monad.when exceptFirst (replacementLooksBack c HistoryRead.StepBegins)) <*> drawRewriteChildren c rewrite)
  ReplacementEffect.DrawCountR (DrawCountR.MkDrawCountR whose atLeast rewrite) -> fmap ReplacementEffect.DrawCountR (DrawCountR.MkDrawCountR <$> replacementWhose c whose <*> pure atLeast <*> pure rewrite)
  ReplacementEffect.InstructionR (InstructionR.MkInstructionR instruction whose rewrite) ->
    fmap ReplacementEffect.InstructionR (InstructionR.MkInstructionR instruction <$> replacementWhose c whose <*> instructionRewriteChildren c rewrite)
  ReplacementEffect.DieRollR (DieRollR.MkDieRollR whose rewrite) -> fmap ReplacementEffect.DieRollR (DieRollR.MkDieRollR <$> replacementWhose c whose <*> pure rewrite)
  ReplacementEffect.PerformerR (PerformerR.MkPerformerR instruction performer rewrite) ->
    fmap ReplacementEffect.PerformerR (PerformerR.MkPerformerR instruction <$> replacementFilter c performer <*> instructionRewriteChildren c rewrite)
  ReplacementEffect.GameLossR (GameLossR.MkGameLossR whose instead) -> fmap ReplacementEffect.GameLossR (GameLossR.MkGameLossR <$> replacementWhose c whose <*> traverse (replacementEffect c) instead)
  ReplacementEffect.PlaneswalkR (PlaneswalkR.MkPlaneswalkR whose cause rewrite) ->
    fmap ReplacementEffect.PlaneswalkR (PlaneswalkR.MkPlaneswalkR <$> replacementWhose c whose <*> pure cause <*> planeswalkRewriteChildren c rewrite)
  ReplacementEffect.ManaR (ManaR.MkManaR matching produces) -> fmap ReplacementEffect.ManaR (ManaR.MkManaR <$> replacementFilter c matching <*> pure produces)
  ReplacementEffect.PhaseR (PhaseR.MkPhaseR (PhasePattern.MkPhasePattern which whose) rewrite) ->
    fmap ReplacementEffect.PhaseR (PhaseR.MkPhaseR . PhasePattern.MkPhasePattern which <$> replacementWhose c whose <*> phaseRewriteChildren c rewrite)

-- | What an entry replacement does (CR 614.1c, 614.1d).
entryRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> EntryRewrite.EntryRewrite ability effect -> f (EntryRewrite.EntryRewrite ability effect)
entryRewriteChildren c rewrite = case rewrite of
  EntryRewrite.AsCopy copy -> EntryRewrite.AsCopy <$> replacementCopy c copy
  EntryRewrite.ChoiceOf options -> EntryRewrite.ChoiceOf <$> traverse (entryOptionChildren c) options
  EntryRewrite.ChoiceByCoinFlip (EntryFlip.MkEntryFlip heads tails) -> fmap EntryRewrite.ChoiceByCoinFlip (EntryFlip.MkEntryFlip <$> entryOptionChildren c heads <*> entryOptionChildren c tails)
  EntryRewrite.ChooseQuality _ -> pure rewrite
  EntryRewrite.ChoosePlayer _ -> pure rewrite
  EntryRewrite.ChooseCardNames names -> EntryRewrite.ChooseCardNames <$> replacementFilter c names
  EntryRewrite.ChooseCardName names -> EntryRewrite.ChooseCardName <$> replacementFilter c names
  EntryRewrite.WithCounters counters -> EntryRewrite.WithCounters <$> replacementCounters c counters
  EntryRewrite.EntersWith (EntersWith.MkEntersWith counters keywords abilities) ->
    fmap EntryRewrite.EntersWith (EntersWith.MkEntersWith <$> traverse (replacementCounters c) counters <*> traverseSet (replacementKeyword c) keywords <*> traverse (replacementAbility c) abilities)
  EntryRewrite.UnderControlOf _ -> pure rewrite
  EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber filter_ kind each) ->
    fmap EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber <$> replacementFilter c filter_ <*> traverse (replacementCounterKind c) kind <*> replacementQuantity c each)
  EntryRewrite.SacrificeToEnter (SacrificeToEnter.MkSacrificeToEnter count filter_) -> fmap EntryRewrite.SacrificeToEnter (SacrificeToEnter.MkSacrificeToEnter count <$> replacementFilter c filter_)
  EntryRewrite.ExileFromGraveyard (ExileFromGraveyard.MkExileFromGraveyard filter_ players count orGraveyard kinds each) ->
    fmap EntryRewrite.ExileFromGraveyard (ExileFromGraveyard.MkExileFromGraveyard <$> replacementFilter c filter_ <*> pure players <*> howManyChildren c count <*> pure orGraveyard <*> traverse (replacementCounterKind c) kinds <*> replacementQuantity c each)
  EntryRewrite.EntersAttachedTo filter_ -> EntryRewrite.EntersAttachedTo <$> replacementFilter c filter_
  EntryRewrite.Riot -> pure rewrite
  EntryRewrite.ReadAhead -> pure rewrite
  EntryRewrite.Unleash -> pure rewrite
  EntryRewrite.Sunburst -> pure rewrite
  EntryRewrite.ModularSunburst -> pure rewrite
  -- CR 702.54a: bloodthirst reads whether an opponent was dealt damage this turn.
  EntryRewrite.Bloodthirst _ -> rewrite <$ replacementLooksBack c HistoryRead.Damage
  EntryRewrite.Amplify _ -> pure rewrite
  EntryRewrite.Tribute _ -> pure rewrite
  EntryRewrite.Compleated _ -> pure rewrite
  EntryRewrite.Tapped -> pure rewrite
  EntryRewrite.Prepared -> pure rewrite
  EntryRewrite.OrTapped price -> EntryRewrite.OrTapped <$> entryPriceChildren c price
  EntryRewrite.EntersTransformed -> pure rewrite
  EntryRewrite.RunEffects effects -> EntryRewrite.RunEffects <$> traverse (replacementEffect c) effects

-- | One option an entering permanent may be (CR 208.2b).
entryOptionChildren :: (Applicative f) => ReplacementChildren card ability effect f -> EntryOption.EntryOption -> f EntryOption.EntryOption
entryOptionChildren c (EntryOption.MkEntryOption power toughness keywords subtypes) =
  EntryOption.MkEntryOption power toughness <$> traverseSet (replacementKeyword c) keywords <*> traverseSet (replacementSubtype c) subtypes

-- | The price an entering permanent's controller may pay to avoid entering tapped (CR 614.1c).
entryPriceChildren :: (Applicative f) => ReplacementChildren card ability effect f -> EntryPrice.EntryPrice -> f EntryPrice.EntryPrice
entryPriceChildren c price = case price of
  EntryPrice.PayLife _ -> pure price
  EntryPrice.Reveal filter_ -> EntryPrice.Reveal <$> replacementFilter c filter_
  EntryPrice.Behold filter_ -> EntryPrice.Behold <$> replacementFilter c filter_

-- | What a damage replacement or prevention does (CR 614.1a, 615.1a).
damageRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> DamageRewrite.DamageRewrite effect -> f (DamageRewrite.DamageRewrite effect)
damageRewriteChildren c rewrite = case rewrite of
  DamageRewrite.PreventAll -> pure rewrite
  DamageRewrite.PreventRemovingShieldCounter -> pure rewrite
  DamageRewrite.PreventRemovingCounters kind -> DamageRewrite.PreventRemovingCounters <$> replacementCounterKind c kind
  DamageRewrite.PreventNext _ -> pure rewrite
  DamageRewrite.PreventAllBut _ -> pure rewrite
  DamageRewrite.PreventUpTo most -> DamageRewrite.PreventUpTo <$> replacementQuantity c most
  DamageRewrite.SetAmount _ -> pure rewrite
  DamageRewrite.Scale scaling -> DamageRewrite.Scale <$> scalingChildren c scaling
  DamageRewrite.Redirect recipient -> DamageRewrite.Redirect <$> replacementRecipient c recipient
  DamageRewrite.RedirectNext amount recipient -> DamageRewrite.RedirectNext amount <$> replacementRecipient c recipient
  DamageRewrite.RedirectMatching filter_ -> DamageRewrite.RedirectMatching <$> replacementFilter c filter_
  DamageRewrite.RunEffects effects -> DamageRewrite.RunEffects <$> traverse (replacementEffect c) effects
  DamageRewrite.TurnFaceUp -> pure rewrite
  DamageRewrite.AlsoToSourceController -> pure rewrite
  DamageRewrite.HealSourceFirst -> pure rewrite

-- | What a draw replacement does (CR 614.1a).
drawRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> DrawRewrite.DrawRewrite effect -> f (DrawRewrite.DrawRewrite effect)
drawRewriteChildren c rewrite = case rewrite of
  DrawRewrite.GainLife _ -> pure rewrite
  DrawRewrite.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame count upTo destination filter_ reveal differentIn pick) ->
    fmap (\rewritten -> DrawRewrite.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame count upTo destination rewritten reveal differentIn pick)) (replacementFilter c filter_)
  DrawRewrite.Dredge _ -> pure rewrite
  DrawRewrite.YouDraw -> pure rewrite
  DrawRewrite.OthersMayPayLife _ -> pure rewrite
  DrawRewrite.FromGraveyardOrLose -> pure rewrite
  DrawRewrite.RevealUntilChosenKind -> pure rewrite
  DrawRewrite.DrawInstead _ -> pure rewrite
  DrawRewrite.DiscardThenDrawOrMill -> pure rewrite
  DrawRewrite.KeepOneOfTopThenDraw count -> DrawRewrite.KeepOneOfTopThenDraw <$> replacementQuantity c count
  DrawRewrite.MaySkipFor effects -> DrawRewrite.MaySkipFor <$> traverse (replacementEffect c) effects

-- | What a turn-up replacement does (CR 614.1e).
turnUpRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> TurnUpRewrite.TurnUpRewrite ability effect -> f (TurnUpRewrite.TurnUpRewrite ability effect)
turnUpRewriteChildren c rewrite = case rewrite of
  TurnUpRewrite.WithCounters counters -> TurnUpRewrite.WithCounters <$> replacementCounters c counters
  TurnUpRewrite.MayAttachTo filter_ -> TurnUpRewrite.MayAttachTo <$> replacementFilter c filter_
  TurnUpRewrite.AsCopy copy -> TurnUpRewrite.AsCopy <$> replacementCopy c copy

-- | What an attachment replacement does (CR 701.3a).
attachRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> AttachRewrite.AttachRewrite ability effect -> f (AttachRewrite.AttachRewrite ability effect)
attachRewriteChildren c rewrite = case rewrite of
  AttachRewrite.AsCopy copy -> AttachRewrite.AsCopy <$> replacementCopy c copy
  AttachRewrite.ChooseQuality _ -> pure rewrite

-- | What an untap replacement does (CR 614.1a).
untapRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> UntapRewrite.UntapRewrite -> f UntapRewrite.UntapRewrite
untapRewriteChildren c rewrite = case rewrite of
  UntapRewrite.RemoveStunCounter -> pure rewrite
  UntapRewrite.RemoveCounterToUntap kind -> UntapRewrite.RemoveCounterToUntap <$> replacementCounterKind c kind

-- | What a life loss replacement does (CR 614.1a).
lifeLossRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> LifeLossRewrite.LifeLossRewrite -> f LifeLossRewrite.LifeLossRewrite
lifeLossRewriteChildren c rewrite = case rewrite of
  LifeLossRewrite.LeaveAtLeast _ -> pure rewrite
  LifeLossRewrite.Scaled scaling -> LifeLossRewrite.Scaled <$> scalingChildren c scaling
  LifeLossRewrite.ExileFromTopOfYourLibrary -> pure rewrite
  LifeLossRewrite.GainInstead -> pure rewrite

-- | What a life gain replacement does (CR 614.1a).
lifeGainRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> LifeGainRewrite.LifeGainRewrite -> f LifeGainRewrite.LifeGainRewrite
lifeGainRewriteChildren c rewrite = case rewrite of
  LifeGainRewrite.Scaled scaling -> LifeGainRewrite.Scaled <$> scalingChildren c scaling
  LifeGainRewrite.DrawInstead -> pure rewrite
  LifeGainRewrite.LoseInstead -> pure rewrite

-- | What an instruction replacement does (CR 614.1a).
instructionRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> InstructionRewrite.InstructionRewrite -> f InstructionRewrite.InstructionRewrite
instructionRewriteChildren c rewrite = case rewrite of
  InstructionRewrite.Scaled scaling -> InstructionRewrite.Scaled <$> scalingChildren c scaling
  InstructionRewrite.DrawInstead -> pure rewrite
  InstructionRewrite.RollD20Instead -> pure rewrite
  InstructionRewrite.DrawFirst -> pure rewrite

-- | What a planeswalk replacement does (CR 701.31).
planeswalkRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> PlaneswalkRewrite.PlaneswalkRewrite effect -> f (PlaneswalkRewrite.PlaneswalkRewrite effect)
planeswalkRewriteChildren c rewrite = case rewrite of
  PlaneswalkRewrite.ArrangeTopTwo -> pure rewrite
  PlaneswalkRewrite.RunEffects effects -> PlaneswalkRewrite.RunEffects <$> traverse (replacementEffect c) effects

-- | What a step or phase replacement does (CR 614.1b, 614.10b).
phaseRewriteChildren :: (Applicative f) => ReplacementChildren card ability effect f -> PhaseRewrite.PhaseRewrite effect -> f (PhaseRewrite.PhaseRewrite effect)
phaseRewriteChildren c rewrite = case rewrite of
  PhaseRewrite.Skip -> pure rewrite
  PhaseRewrite.MaySkipFor effects -> PhaseRewrite.MaySkipFor <$> traverse (replacementEffect c) effects

-- | The tokens a token replacement adds (CR 614.1a).
tokenPlusChildren :: (Applicative f) => ReplacementChildren card ability effect f -> TokenPlus.TokenPlus card -> f (TokenPlus.TokenPlus card)
tokenPlusChildren c plus = case plus of
  TokenPlus.One card -> TokenPlus.One <$> replacementCard c card
  TokenPlus.ThatMany card -> TokenPlus.ThatMany <$> replacementCard c card
  TokenPlus.Instead card -> TokenPlus.Instead <$> replacementCard c card

-- | How a replacement resizes an event (CR 614.1a): only an addition holds a child.
scalingChildren :: (Applicative f) => ReplacementChildren card ability effect f -> Scaling.Scaling -> f Scaling.Scaling
scalingChildren c scaling = case scaling of
  Scaling.Multiply _ -> pure scaling
  Scaling.AddMore addend -> Scaling.AddMore <$> replacementQuantity c addend
  Scaling.Subtract _ -> pure scaling
  Scaling.Halve -> pure scaling

-- | Who puts the counters a counter replacement watches: only a player relation is a child.
counterSubjectChildren :: (Applicative f) => ReplacementChildren card ability effect f -> CounterSubject.CounterSubject -> f CounterSubject.CounterSubject
counterSubjectChildren c subject = case subject of
  CounterSubject.ByEffect -> pure subject
  CounterSubject.ByPlayer whose -> CounterSubject.ByPlayer <$> replacementWhose c whose
  CounterSubject.ByAnything -> pure subject

-- | How many cards an entry exiles (CR 608.2d).
howManyChildren :: (Applicative f) => ReplacementChildren card ability effect f -> HowMany.HowMany -> f HowMany.HowMany
howManyChildren c count = case count of
  HowMany.Exactly n -> HowMany.Exactly <$> replacementQuantity c n
  HowMany.UpTo n -> HowMany.UpTo <$> traverse (replacementQuantity c) n

-- | One visitor per kind of child a condition holds: a CR 611.2b "for as long
-- as", a CR 603.8 state trigger's, a CR 603.4 intervening "if".
data ConditionChildren f = MkConditionChildren
  { -- | A Quantity a comparison measures or compares against.
    conditionQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A nested condition: Any's or All's.
    conditionNested :: Condition.Condition -> f Condition.Condition,
    -- | The turns a phase window looks at (CR 102.1), whole.
    conditionTurn :: TurnScope.TurnScope -> f TurnScope.TurnScope
  }

-- | Every child of a condition left as it is: the base a walk overrides.
conditionLeaves :: (Applicative f) => ConditionChildren f
conditionLeaves =
  MkConditionChildren
    { conditionQuantity = pure,
      conditionNested = pure,
      conditionTurn = pure
    }

-- | A Condition's immediate children, each through its visitor. A nested
-- condition is handed to conditionNested and not descended here.
conditionChildren :: (Applicative f) => ConditionChildren f -> Condition.Condition -> f Condition.Condition
conditionChildren c condition = case condition of
  Condition.Compares (Compares.MkCompares measured comparison threshold) -> fmap Condition.Compares (Compares.MkCompares <$> conditionQuantity c measured <*> pure comparison <*> conditionQuantity c threshold)
  Condition.Any conditions -> Condition.Any <$> traverse (conditionNested c) conditions
  Condition.All conditions -> Condition.All <$> traverse (conditionNested c) conditions
  Condition.During (DuringPhase.MkDuringPhase window scope) -> fmap Condition.During (DuringPhase.MkDuringPhase window <$> conditionTurn c scope)

-- | One visitor per kind of child a duration (CR 611.2) holds.
data DurationChildren f = MkDurationChildren
  { -- | The player whose turn or step ends the duration, or who may pay to end it.
    durationPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | A "for as long as" condition (CR 611.2b), whole.
    durationCondition :: Condition.Condition -> f Condition.Condition,
    -- | The price that ends the duration (CR 116.2c), whole.
    durationCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | A slot the duration reads: the effect it lasts until used, the card it lasts until cast.
    durationSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | The spells whose casting ends the duration (CR 601.2i).
    durationFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | The card whose casting ends the duration, baked (CR 400.7).
    durationObject :: ObjectId.ObjectId -> f ObjectId.ObjectId
  }

-- | Every child of a duration left as it is: the base a walk overrides.
durationLeaves :: (Applicative f) => DurationChildren f
durationLeaves =
  MkDurationChildren
    { durationPlayer = pure,
      durationCondition = pure,
      durationCost = pure,
      durationSlot = pure,
      durationFilter = pure,
      durationObject = pure
    }

-- | A Duration's children, its payload matched positionally.
durationChildren :: (Applicative f) => DurationChildren f -> Duration.Duration -> f Duration.Duration
durationChildren c duration = case duration of
  Duration.UntilEndOfTurn -> pure duration
  Duration.Indefinite -> pure duration
  Duration.Perpetual -> pure duration
  Duration.UntilYourNextTurn -> pure duration
  Duration.UntilNextStepOf (NextStepOf.MkNextStepOf step player) -> fmap Duration.UntilNextStepOf (NextStepOf.MkNextStepOf step <$> durationPlayer c player)
  Duration.UntilEndOfYourNextTurn -> pure duration
  Duration.UntilEndOfNextTurnOf player -> Duration.UntilEndOfNextTurnOf <$> durationPlayer c player
  Duration.DuringNextTurnOf player -> Duration.DuringNextTurnOf <$> durationPlayer c player
  Duration.DuringNextUntapStepOf player -> Duration.DuringNextUntapStepOf <$> durationPlayer c player
  Duration.DuringYourNextTurn -> pure duration
  Duration.DuringThatExtraTurn -> pure duration
  Duration.ForAsLongAs condition -> Duration.ForAsLongAs <$> durationCondition c condition
  Duration.UntilEndOfCombat -> pure duration
  Duration.UntilEndOfCombatOnYourNextTurn -> pure duration
  Duration.UntilPaid (UntilPaid.MkUntilPaid payer cost) -> fmap Duration.UntilPaid (UntilPaid.MkUntilPaid <$> durationPlayer c payer <*> durationCost c cost)
  Duration.UntilUsed slot -> Duration.UntilUsed <$> traverse (durationSlot c) slot
  Duration.UntilEvent event -> Duration.UntilEvent <$> durationEventChildren c event

-- | The event that ends a duration (CR 611.2a): a filter, a slot or a baked card.
durationEventChildren :: (Applicative f) => DurationChildren f -> DurationEvent.DurationEvent -> f DurationEvent.DurationEvent
durationEventChildren c event = case event of
  DurationEvent.SourceTurnedFaceDown -> pure event
  DurationEvent.SpellCast criterion -> DurationEvent.SpellCast <$> durationFilter c criterion
  DurationEvent.Planeswalked -> pure event
  DurationEvent.SourceExiledCard -> pure event
  DurationEvent.SourceLeftBattlefield -> pure event
  DurationEvent.BoundCardCast slot -> DurationEvent.BoundCardCast <$> durationSlot c slot
  DurationEvent.CardCast card -> DurationEvent.CardCast <$> durationObject c card

-- | A set's members, each through a visitor; members the visitor maps together merge.
traverseSet :: (Applicative f, Ord a) => (a -> f a) -> Set.Set a -> f (Set.Set a)
traverseSet f = fmap Set.fromList . traverse f . Set.toList
