module Parun.Engine.Children where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Binding as Binding.Engine
import qualified Parun.Types.AbilityAddsMana as AbilityAddsMana
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.ActivateManaAbilities as ActivateManaAbilities
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.ActivationProhibition as ActivationProhibition
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.ActivationsThisTurn as ActivationsThisTurn
import qualified Parun.Types.ActiveLaterPayment as ActiveLaterPayment
import qualified Parun.Types.ActiveReplacement as ActiveReplacement
import qualified Parun.Types.AffectPlayers as AffectPlayers
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.AffectedPlayers as AffectedPlayers
import qualified Parun.Types.AffectedUnless as AffectedUnless
import qualified Parun.Types.AfterObjectTurn as AfterObjectTurn
import qualified Parun.Types.AgainstExiledPile as AgainstExiledPile
import qualified Parun.Types.AgainstLastCardExiledWith as AgainstLastCardExiledWith
import qualified Parun.Types.AgainstSlot as AgainstSlot
import qualified Parun.Types.Aggregation as Aggregation
import qualified Parun.Types.AimedAt as AimedAt
import qualified Parun.Types.AloneUnless as AloneUnless
import qualified Parun.Types.AlternativeActivationCost as AlternativeActivationCost
import qualified Parun.Types.AlternativeCost as AlternativeCost
import qualified Parun.Types.Amass as Amass
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.Ante as Ante
import qualified Parun.Types.AnyNumberDiscard as AnyNumberDiscard
import qualified Parun.Types.AnyNumberMatching as AnyNumberMatching
import qualified Parun.Types.Arithmetic as Arithmetic
import qualified Parun.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Parun.Types.ArrangeInLibrary as ArrangeInLibrary
import qualified Parun.Types.ArrangedArrival as ArrangedArrival
import qualified Parun.Types.AsCopy as AsCopy
import qualified Parun.Types.AttachAll as AttachAll
import qualified Parun.Types.AttachBound as AttachBound
import qualified Parun.Types.AttachR as AttachR
import qualified Parun.Types.AttachRestriction as AttachRestriction
import qualified Parun.Types.AttachRewrite as AttachRewrite
import qualified Parun.Types.AttachTarget as AttachTarget
import qualified Parun.Types.AttachedToBound as AttachedToBound
import qualified Parun.Types.AttachmentTrigger as AttachmentTrigger
import qualified Parun.Types.AttackCompany as AttackCompany
import qualified Parun.Types.AttackCost as AttackCost
import qualified Parun.Types.AttackCount as AttackCount
import qualified Parun.Types.AttackLimitUnless as AttackLimitUnless
import qualified Parun.Types.AttackRequirement as AttackRequirement
import qualified Parun.Types.AttackTargetRef as AttackTargetRef
import qualified Parun.Types.AttackTrigger as AttackTrigger
import qualified Parun.Types.AttackedTrigger as AttackedTrigger
import qualified Parun.Types.AttackerTarget as AttackerTarget
import qualified Parun.Types.AttackerTrigger as AttackerTrigger
import qualified Parun.Types.Backup as Backup
import qualified Parun.Types.BeamMeUp as BeamMeUp
import qualified Parun.Types.BecomeCopy as BecomeCopy
import qualified Parun.Types.BecomesTapState as BecomesTapState
import qualified Parun.Types.BecomesTargetedTrigger as BecomesTargetedTrigger
import qualified Parun.Types.Behold as Behold
import qualified Parun.Types.Bid as Bid
import qualified Parun.Types.BindAmount as BindAmount
import qualified Parun.Types.BindLeaders as BindLeaders
import qualified Parun.Types.BindPlayers as BindPlayers
import qualified Parun.Types.Binding as Binding
import qualified Parun.Types.Blight as Blight
import qualified Parun.Types.BlockCost as BlockCost
import qualified Parun.Types.BlockCount as BlockCount
import qualified Parun.Types.BlockPermission as BlockPermission
import qualified Parun.Types.BlockRequirement as BlockRequirement
import qualified Parun.Types.BlockTrigger as BlockTrigger
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.CandidateId as CandidateId
import qualified Parun.Types.CantAttackPlayer as CantAttackPlayer
import qualified Parun.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Parun.Types.CantBlockCreatures as CantBlockCreatures
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardAbilities as CardAbilities
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.CardsFromHand as CardsFromHand
import qualified Parun.Types.CastFrom as CastFrom
import qualified Parun.Types.CastFromZone as CastFromZone
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.CastRepetition as CastRepetition
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.CastingRestriction as CastingRestriction
import qualified Parun.Types.CausedBy as CausedBy
import qualified Parun.Types.ChangeTargets as ChangeTargets
import qualified Parun.Types.ChangeText as ChangeText
import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.ChooseCardName as ChooseCardName
import qualified Parun.Types.ChooseGroup as ChooseGroup
import qualified Parun.Types.ChooseInstead as ChooseInstead
import qualified Parun.Types.ChooseNumber as ChooseNumber
import qualified Parun.Types.ChoosePermanents as ChoosePermanents
import qualified Parun.Types.ChoosePlayer as ChoosePlayer
import qualified Parun.Types.ChoosePlayerAtRandom as ChoosePlayerAtRandom
import qualified Parun.Types.ChooseQuality as ChooseQuality
import qualified Parun.Types.Chooser as Chooser
import qualified Parun.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Parun.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.Types.ChosenCardInHand as ChosenCardInHand
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.ChosenQuality as ChosenQuality
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.Cloak as Cloak
import qualified Parun.Types.CoinFlipTrigger as CoinFlipTrigger
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CombatRestriction as CombatRestriction
import qualified Parun.Types.CommanderColors as CommanderColors
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule
import qualified Parun.Types.CompanionCondition as CompanionCondition
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.CompletedDungeon as CompletedDungeon
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Conjure as Conjure
import qualified Parun.Types.ConjureCards as ConjureCards
import qualified Parun.Types.Connive as Connive
import qualified Parun.Types.ControlPlayer as ControlPlayer
import qualified Parun.Types.ControlPlayerNext as ControlPlayerNext
import qualified Parun.Types.ControlSides as ControlSides
import qualified Parun.Types.ControlSlots as ControlSlots
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CopyClause as CopyClause
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.CopyOriginal as CopyOriginal
import qualified Parun.Types.CopyStackObject as CopyStackObject
import qualified Parun.Types.CopyTargets as CopyTargets
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostAddition as CostAddition
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostBasis as CostBasis
import qualified Parun.Types.CostChange as CostChange
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CostKeyword as CostKeyword
import qualified Parun.Types.CostModifier as CostModifier
import qualified Parun.Types.CostReduction as CostReduction
import qualified Parun.Types.CostScale as CostScale
import qualified Parun.Types.CostSubject as CostSubject
import qualified Parun.Types.CouldProduce as CouldProduce
import qualified Parun.Types.Count as Count
import qualified Parun.Types.CountedDiscard as CountedDiscard
import qualified Parun.Types.Counter as Counter
import qualified Parun.Types.CounterDestination as CounterDestination
import qualified Parun.Types.CounterHolder as CounterHolder
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLimit as CounterLimit
import qualified Parun.Types.CounterLinked as CounterLinked
import qualified Parun.Types.CounterLossR as CounterLossR
import qualified Parun.Types.CounterPattern as CounterPattern
import qualified Parun.Types.CounterProhibition as CounterProhibition
import qualified Parun.Types.CounterR as CounterR
import qualified Parun.Types.CounterRestriction as CounterRestriction
import qualified Parun.Types.CounterSubject as CounterSubject
import qualified Parun.Types.CounterTrigger as CounterTrigger
import qualified Parun.Types.Counterability as Counterability
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
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.DamageRewrite as DamageRewrite
import qualified Parun.Types.DamageTally as DamageTally
import qualified Parun.Types.DamageTrigger as DamageTrigger
import qualified Parun.Types.DamagedParty as DamagedParty
import qualified Parun.Types.DealDamage as DealDamage
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.DealtDamageTo as DealtDamageTo
import qualified Parun.Types.DeclarationTrigger as DeclarationTrigger
import qualified Parun.Types.DefinedX as DefinedX
import qualified Parun.Types.DelayedTrigger as DelayedTrigger
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
import qualified Parun.Types.DungeonRoom as DungeonRoom
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.DurationEvent as DurationEvent
import qualified Parun.Types.DuringPhase as DuringPhase
import qualified Parun.Types.EachCardFromAmong as EachCardFromAmong
import qualified Parun.Types.EachCardInExiledPile as EachCardInExiledPile
import qualified Parun.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Parun.Types.EachCardInHand as EachCardInHand
import qualified Parun.Types.Earthbend as Earthbend
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.Emerge as Emerge
import qualified Parun.Types.EntersTrigger as EntersTrigger
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
import qualified Parun.Types.ExchangeBlocks as ExchangeBlocks
import qualified Parun.Types.ExchangeCardZones as ExchangeCardZones
import qualified Parun.Types.ExchangeOwnership as ExchangeOwnership
import qualified Parun.Types.ExchangeSides as ExchangeSides
import qualified Parun.Types.ExchangeValues as ExchangeValues
import qualified Parun.Types.ExchangeZones as ExchangeZones
import qualified Parun.Types.ExchangedValue as ExchangedValue
import qualified Parun.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Parun.Types.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Types.ExileFromGraveyard as ExileFromGraveyard
import qualified Parun.Types.ExileHaunting as ExileHaunting
import qualified Parun.Types.ExileMaterials as ExileMaterials
import qualified Parun.Types.ExilePermanents as ExilePermanents
import qualified Parun.Types.Expiry as Expiry
import qualified Parun.Types.ExploitTrigger as ExploitTrigger
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.Fight as Fight
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FlipCoin as FlipCoin
import qualified Parun.Types.ForEach as ForEach
import qualified Parun.Types.ForEachNumber as ForEachNumber
import qualified Parun.Types.ForbidAttack as ForbidAttack
import qualified Parun.Types.ForbidBeingBlocked as ForbidBeingBlocked
import qualified Parun.Types.ForetellCost as ForetellCost
import qualified Parun.Types.FreeCast as FreeCast
import qualified Parun.Types.FromAnywhere as FromAnywhere
import qualified Parun.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Parun.Types.FromReference as FromReference
import qualified Parun.Types.FullText as FullText
import qualified Parun.Types.GainControl as GainControl
import qualified Parun.Types.GameLossR as GameLossR
import qualified Parun.Types.Gift as Gift
import qualified Parun.Types.GrantLookAtExiled as GrantLookAtExiled
import qualified Parun.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.Grouping as Grouping
import qualified Parun.Types.Guess as Guess
import qualified Parun.Types.HandAction as HandAction
import qualified Parun.Types.HexproofExemption as HexproofExemption
import qualified Parun.Types.HistoryRead as HistoryRead
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.Impending as Impending
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.InstructionR as InstructionR
import qualified Parun.Types.InstructionRewrite as InstructionRewrite
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordCostRestriction as KeywordCostRestriction
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
import qualified Parun.Types.LifeExchange as LifeExchange
import qualified Parun.Types.LifeGainR as LifeGainR
import qualified Parun.Types.LifeGainRewrite as LifeGainRewrite
import qualified Parun.Types.LifeLoss as LifeLoss
import qualified Parun.Types.LifeLossPattern as LifeLossPattern
import qualified Parun.Types.LifeLossR as LifeLossR
import qualified Parun.Types.LifeLossRewrite as LifeLossRewrite
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.LifeTrigger as LifeTrigger
import qualified Parun.Types.LimitUnless as LimitUnless
import qualified Parun.Types.LookAt as LookAt
import qualified Parun.Types.LostPermanent as LostPermanent
import qualified Parun.Types.LoyaltyClause as LoyaltyClause
import qualified Parun.Types.LoyaltyGrant as LoyaltyGrant
import qualified Parun.Types.MadnessCost as MadnessCost
import qualified Parun.Types.MakeForetold as MakeForetold
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.ManaFilter as ManaFilter
import qualified Parun.Types.ManaProduction as ManaProduction
import qualified Parun.Types.ManaR as ManaR
import qualified Parun.Types.ManaRestriction as ManaRestriction
import qualified Parun.Types.ManaRider as ManaRider
import qualified Parun.Types.ManaRiderEffect as ManaRiderEffect
import qualified Parun.Types.ManaSpecification as ManaSpecification
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ManifestDread as ManifestDread
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Meld as Meld
import qualified Parun.Types.Mill as Mill
import qualified Parun.Types.MillTally as MillTally
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.ModifyTarget as ModifyTarget
import qualified Parun.Types.MonarchTarget as MonarchTarget
import qualified Parun.Types.Morph as Morph
import qualified Parun.Types.MoveCounters as MoveCounters
import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.MoveMana as MoveMana
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.NewX as NewX
import qualified Parun.Types.NextDamageInstance as NextDamageInstance
import qualified Parun.Types.NextStepOf as NextStepOf
import qualified Parun.Types.Ninjutsu as Ninjutsu
import qualified Parun.Types.ObjectCount as ObjectCount
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.OfferCast as OfferCast
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
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PermanentActsTrigger as PermanentActsTrigger
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
import qualified Parun.Types.PlayerSlot as PlayerSlot
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.PlayersWho as PlayersWho
import qualified Parun.Types.PlaysLand as PlaysLand
import qualified Parun.Types.PlotFromZone as PlotFromZone
import qualified Parun.Types.Plus as Plus
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.Power as Power
import qualified Parun.Types.PregameExile as PregameExile
import qualified Parun.Types.PreventAllDamage as PreventAllDamage
import qualified Parun.Types.PreventNextDamage as PreventNextDamage
import qualified Parun.Types.Prevention as Prevention
import qualified Parun.Types.PreventionRider as PreventionRider
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.Prohibit as Prohibit
import qualified Parun.Types.Protection as Protection
import qualified Parun.Types.PutCounters as PutCounters
import qualified Parun.Types.PutCountersFrom as PutCountersFrom
import qualified Parun.Types.PutIntoZoneTrigger as PutIntoZoneTrigger
import qualified Parun.Types.PutSticker as PutSticker
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.RandomCard as RandomCard
import qualified Parun.Types.RandomCardInGraveyard as RandomCardInGraveyard
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
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.Sacrifice as Sacrifice
import qualified Parun.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Parun.Types.SacrificeEffect as SacrificeEffect
import qualified Parun.Types.SacrificeRestriction as SacrificeRestriction
import qualified Parun.Types.SacrificeToEnter as SacrificeToEnter
import qualified Parun.Types.Scaling as Scaling
import qualified Parun.Types.Scope as Scope
import qualified Parun.Types.Search as Search
import qualified Parun.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Parun.Types.SetClassLevel as SetClassLevel
import qualified Parun.Types.SetHalfLocked as SetHalfLocked
import qualified Parun.Types.SetOwner as SetOwner
import qualified Parun.Types.SetX as SetX
import qualified Parun.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Parun.Types.SkipPhase as SkipPhase
import qualified Parun.Types.SlotArity as SlotArity
import qualified Parun.Types.SlotCount as SlotCount
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SpecialAction as SpecialAction
import qualified Parun.Types.SpeedDecrease as SpeedDecrease
import qualified Parun.Types.SpellCast as SpellCast
import qualified Parun.Types.SpellTally as SpellTally
import qualified Parun.Types.SpendManaAsThough as SpendManaAsThough
import qualified Parun.Types.SpendScope as SpendScope
import qualified Parun.Types.Splice as Splice
import qualified Parun.Types.StatedFlip as StatedFlip
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.StepBegins as StepBegins
import qualified Parun.Types.StepOfControllerOf as StepOfControllerOf
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
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.TargetedSubject as TargetedSubject
import qualified Parun.Types.TheseDiscard as TheseDiscard
import qualified Parun.Types.TokenPattern as TokenPattern
import qualified Parun.Types.TokenPlus as TokenPlus
import qualified Parun.Types.TokenR as TokenR
import qualified Parun.Types.TopOfLibrary as TopOfLibrary
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.Types.TotalCap as TotalCap
import qualified Parun.Types.Toughness as Toughness
import qualified Parun.Types.TransformTrigger as TransformTrigger
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.TriggerLimit as TriggerLimit
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnFaceDown as TurnFaceDown
import qualified Parun.Types.TurnOrdinal as TurnOrdinal
import qualified Parun.Types.TurnScope as TurnScope
import qualified Parun.Types.TurnUpR as TurnUpR
import qualified Parun.Types.TurnUpRewrite as TurnUpRewrite
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Types.Undesignate as Undesignate
import qualified Parun.Types.UntapR as UntapR
import qualified Parun.Types.UntapRestriction as UntapRestriction
import qualified Parun.Types.UntapRewrite as UntapRewrite
import qualified Parun.Types.UntilPaid as UntilPaid
import qualified Parun.Types.Vote as Vote
import qualified Parun.Types.VoteChoices as VoteChoices
import qualified Parun.Types.VoteObjects as VoteObjects
import qualified Parun.Types.VotePlayers as VotePlayers
import qualified Parun.Types.VotesReceived as VotesReceived
import qualified Parun.Types.Ward as Ward
import qualified Parun.Types.WhenSpent as WhenSpent
import qualified Parun.Types.WhichCounters as WhichCounters
import qualified Parun.Types.While as While
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.ZoneChangePattern as ZoneChangePattern
import qualified Parun.Types.ZoneChangeR as ZoneChangeR
import qualified Parun.Types.ZoneScope as ZoneScope

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
  Filter.Noted _ -> pure predicate
  Filter.HasChosenSubtype -> pure predicate
  Filter.HasChosenCardType -> pure predicate
  Filter.IsExiledWithSource _ -> pure predicate
  Filter.IsLastExiledWithSource -> pure predicate
  Filter.OfChosenPlayer -> pure predicate
  Filter.OfRelatedPlayer _ -> pure predicate
  Filter.OfSpecificPlayer _ -> pure predicate
  Filter.IsPlayer _ -> pure predicate
  -- ONE: the controller of the one object the slot names (CR 608.2h), read
  -- through Parun.Engine.Filter.slotOneObject.
  Filter.IsControllerOfBound name -> fmap Filter.IsControllerOfBound (slot children SlotArity.One name)
  Filter.Controls margin f -> fmap (Filter.Controls margin) (nest children f)
  Filter.CardsInGraveyardAtLeast _ -> pure predicate
  Filter.OwnsBoundAtLeast n name f -> liftA2 (Filter.OwnsBoundAtLeast n) (slot children SlotArity.Many name) (nest children f)
  Filter.TappedForManaThisTurn f -> fmap Filter.TappedForManaThisTurn (nest children f)
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
  Filter.CantPayKeywordCost _ -> pure predicate
  Filter.CrewedSourceThisTurn -> pure predicate
  Filter.ConvokedSourceThisTurn -> pure predicate
  Filter.SaddledSourceThisTurn -> pure predicate
  Filter.DealtDamageThisTurn by -> fmap Filter.DealtDamageThisTurn (traverse (nest children) by)
  Filter.LostLifeThisTurn -> pure predicate
  Filter.ActivatedThisTurn -> pure predicate
  Filter.DealtDamageTo asked -> fmap Filter.DealtDamageTo (dealtDamageToChildren children asked)
  Filter.EnteredThisTurn _ -> pure predicate
  Filter.CountersPutThisTurn relation kind -> fmap (Filter.CountersPutThisTurn relation) (traverse (counterKindChildren children) kind)
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
  Filter.IsActivatedAbility criteria -> fmap Filter.IsActivatedAbility (activationCriteriaChildren children criteria)
  Filter.IsAbility -> pure predicate
  Filter.IsEmblem -> pure predicate
  Filter.FromSource f -> fmap Filter.FromSource (nest children f)
  Filter.IsTapped -> pure predicate
  Filter.IsFaceDown -> pure predicate
  Filter.RepresentedByCard f -> fmap Filter.RepresentedByCard (nest children f)
  Filter.IsExiledFaceDown -> pure predicate
  Filter.IsWarped -> pure predicate
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
  Filter.HasAbility -> pure predicate
  Filter.IsInZone _ -> pure predicate
  Filter.WasCastFrom _ -> pure predicate
  Filter.TagWasSpent _ -> pure predicate
  Filter.Paid designator -> fmap Filter.Paid (designatorChildren children designator)
  Filter.IsPrototyped -> pure predicate
  Filter.IsPreparedSpell -> pure predicate
  Filter.HasInsetFrame _ -> pure predicate
  Filter.And fs -> fmap Filter.And (traverse (connective children) fs)
  Filter.Or fs -> fmap Filter.Or (traverse (connective children) fs)
  Filter.Not f -> fmap Filter.Not (connective children f)
  Filter.Unwritten f -> fmap Filter.Unwritten (connective children f)

-- | An anchor's children: its slot, read at MANY (the whole set it names), or
-- the Filter it nests.
anchorChildren :: (Applicative f) => Children keyword f -> Anchor.Anchor (Filter.Filter keyword) -> f (Anchor.Anchor (Filter.Filter keyword))
anchorChildren children anchor = case anchor of
  Anchor.Bound held name -> fmap (Anchor.Bound held) (slot children SlotArity.Many name)
  Anchor.Evaluated -> pure anchor
  Anchor.Source -> pure anchor
  Anchor.AnyPermanent group -> fmap Anchor.AnyPermanent (nest children group)
  Anchor.YourCommander -> pure anchor
  Anchor.Host -> pure anchor

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
  Operand.YourCounters _ -> pure operand

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
  Quantity.Convokers -> pure quantity
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
  Quantity.PhasesBegunThisTurn _ -> pure quantity
  Quantity.SubgamesThisMatch -> pure quantity
  Quantity.DungeonsCompleted ref -> fmap Quantity.DungeonsCompleted (player children ref)
  Quantity.CommanderCasts ref -> fmap Quantity.CommanderCasts (player children ref)
  Quantity.CommanderColors (CommanderColors.MkCommanderColors ref colors) -> fmap (\renamed -> Quantity.CommanderColors (CommanderColors.MkCommanderColors renamed colors)) (player children ref)
  Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon ref name) -> fmap (\renamed -> Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon renamed name)) (player children ref)
  Quantity.DraftNotes (DraftNoteTally.MkDraftNoteTally ref name fold) -> fmap (\renamed -> Quantity.DraftNotes (DraftNoteTally.MkDraftNoteTally renamed name fold)) (player children ref)
  Quantity.EnteredThisTurn -> pure quantity
  Quantity.EnteredFrom from -> fmap Quantity.EnteredFrom (inZoneChildren children from)
  Quantity.WasCastFrom (CastFrom.MkCastFrom caster from) -> fmap Quantity.WasCastFrom (CastFrom.MkCastFrom <$> player children caster <*> traverse (inZoneChildren children) from)
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

-- | Activation criteria's children: the keyword its granting ability is
-- designated by.
activationCriteriaChildren :: (Applicative f) => Children keyword f -> ActivationCriteria.ActivationCriteria keyword -> f (ActivationCriteria.ActivationCriteria keyword)
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
    onChosenPermanents :: ChosenPermanents.ChosenPermanents -> f ChosenPermanents.ChosenPermanents,
    -- | A subtype a choice of a quality lists (CR 205.3).
    onSubtype :: Subtype.Subtype -> f Subtype.Subtype
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
      onChosenPermanents = pure,
      onSubtype = pure
    }

-- | An Effect's immediate children, each through its visitor. A nested
-- effect is handed to onEffect and not descended here.
effectChildren :: (Applicative f) => EffectChildren card ability f -> Effect.Effect card ability -> f (Effect.Effect card ability)
effectChildren c effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts dealer dealers excess tally damaged excessTally) ->
    fmap Effect.DealDamage (DealDamage.MkDealDamage <$> traverse (damagePartChildren c) parts <*> traverse (onSlot c) dealer <*> traverse (onObjectRef c) dealers <*> pure excess <*> traverse (onSlot c) tally <*> traverse (onSlot c) damaged <*> traverse (onSlot c) excessTally)
  Effect.Fight (Fight.MkFight first second hop excessTally) -> fmap Effect.Fight (Fight.MkFight <$> onSlot c first <*> onSlot c second <*> pure hop <*> traverse (onSlot c) excessTally)
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref each) ->
    fmap Effect.ModifyTarget (ModifyTarget.MkModifyTarget <$> onDuration c duration <*> onModification c modification <*> onObjectRef c ref <*> traverse (onSlot c) each)
  Effect.ChangeText (ChangeText.MkChangeText families forbidden named duration) -> fmap Effect.ChangeText (ChangeText.MkChangeText families forbidden <$> onSlot c named <*> onDuration c duration)
  Effect.AddMana addition -> fmap Effect.AddMana (onManaAddition c addition)
  Effect.ActivateManaAbilities (ActivateManaAbilities.MkActivateManaAbilities who filter_) -> fmap Effect.ActivateManaAbilities (ActivateManaAbilities.MkActivateManaAbilities <$> onPlayer c who <*> onFilter c filter_)
  Effect.MoveMana (MoveMana.MkMoveMana from to) -> fmap Effect.MoveMana (MoveMana.MkMoveMana <$> onPlayer c from <*> traverse (onPlayer c) to)
  Effect.Search (Search.MkSearch searcher owner zones outside quantity filter_ descriptions upTo destination counters subject named handSlot librarySlot differentIn exactly andOr top) ->
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
          <*> traverse (onSlot c) librarySlot
          <*> pure differentIn
          <*> pure exactly
          <*> pure andOr
          <*> traverse (onQuantity c) top
      )
  Effect.ExileAllGraveyards -> pure effect
  Effect.RestartGame exempt -> fmap Effect.RestartGame (traverse (onObjectRef c) exempt)
  Effect.ControlPlayerNext (ControlPlayerNext.MkControlPlayerNext named during) -> fmap (\renamed -> Effect.ControlPlayerNext (ControlPlayerNext.MkControlPlayerNext renamed during)) (onSlot c named)
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
  Effect.Reveal (Reveal.MkReveal ref named revealer) -> fmap Effect.Reveal (Reveal.MkReveal <$> onObjectRef c ref <*> traverse (onSlot c) named <*> traverse (onPlayer c) revealer)
  Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame count upTo destination filter_ reveal differentIn pick) ->
    fmap (\rewritten -> Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame count upTo destination rewritten reveal differentIn pick)) (onFilter c filter_)
  Effect.ExileThisSpell -> pure effect
  Effect.LookAt (LookAt.MkLookAt ref named looker) -> fmap Effect.LookAt (LookAt.MkLookAt <$> onObjectRef c ref <*> onSlot c named <*> onPlayer c looker)
  Effect.ArrangeInLibrary (ArrangeInLibrary.MkArrangeInLibrary ref chooser end) -> fmap Effect.ArrangeInLibrary (ArrangeInLibrary.MkArrangeInLibrary <$> onObjectRef c ref <*> onPlayer c chooser <*> pure end)
  Effect.Scry x -> fmap Effect.Scry (playerQuantityChildren c x)
  Effect.Surveil (Surveil.MkSurveil who quantity named) -> fmap Effect.Surveil (Surveil.MkSurveil <$> onPlayer c who <*> onQuantity c quantity <*> traverse (onSlot c) named)
  Effect.Fateseal x -> fmap Effect.Fateseal (playerQuantityChildren c x)
  Effect.Clash named -> fmap Effect.Clash (onSlot c named)
  Effect.AsYouCascade named -> fmap Effect.AsYouCascade (onSlot c named)
  Effect.Explore ref -> fmap Effect.Explore (onObjectRef c ref)
  Effect.Connive (Connive.MkConnive quantity ref) -> fmap Effect.Connive (Connive.MkConnive <$> onQuantity c quantity <*> onObjectRef c ref)
  Effect.SetX (SetX.MkSetX value ref) -> fmap Effect.SetX (SetX.MkSetX <$> newXChildren c value <*> onObjectRef c ref)
  Effect.Discard subject -> fmap Effect.Discard (discardChildren c subject)
  Effect.LoseLife (LifeLoss.MkLifeLoss who quantity cause tally) -> fmap Effect.LoseLife (LifeLoss.MkLifeLoss <$> onPlayer c who <*> onQuantity c quantity <*> pure cause <*> traverse (onSlot c) tally)
  Effect.GainLife x -> fmap Effect.GainLife (playerQuantityChildren c x)
  Effect.ExchangeLifeTotals (LifeExchange.MkLifeExchange sides tally) -> fmap Effect.ExchangeLifeTotals (LifeExchange.MkLifeExchange <$> exchangeSidesChildren c sides <*> traverse (onSlot c) tally)
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
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref riders named exceptions creator linked) ->
    fmap Effect.CreateCopy (CreateCopy.MkCreateCopy <$> onQuantity c quantity <*> onObjectRef c ref <*> onEntryRiders c riders <*> traverse (onSlot c) named <*> traverse (onCopyException c) exceptions <*> onPlayer c creator <*> pure linked)
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject duration exceptions newTargets) ->
    fmap Effect.BecomeCopy (BecomeCopy.MkBecomeCopy <$> copyOriginalChildren c original <*> onObjectRef c subject <*> traverse (onDuration c) duration <*> traverse (onCopyException c) exceptions <*> pure newTargets)
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets quantity copier exceptions named) ->
    fmap Effect.CopyStackObject (CopyStackObject.MkCopyStackObject <$> onObjectRef c ref <*> copyTargetsChildren c targets <*> onQuantity c quantity <*> onPlayer c copier <*> traverse (onCopyException c) exceptions <*> traverse (onSlot c) named)
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
  Effect.NextDamageInstance (NextDamageInstance.MkNextDamageInstance duration kind ref direction chosenSource riders outcome) ->
    fmap Effect.NextDamageInstance (NextDamageInstance.MkNextDamageInstance <$> onDuration c duration <*> pure kind <*> onObjectRef c ref <*> pure direction <*> onFilter c chosenSource <*> traverse (onEffect c) riders <*> pure outcome)
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
  Effect.BindPlayers (BindPlayers.MkBindPlayers who named) -> fmap Effect.BindPlayers (BindPlayers.MkBindPlayers <$> onPlayer c who <*> onSlot c named)
  Effect.BindLeaders (BindLeaders.MkBindLeaders among measure named) -> fmap Effect.BindLeaders (BindLeaders.MkBindLeaders <$> onSlot c among <*> pure measure <*> onSlot c named)
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
  Effect.GainControl (GainControl.MkGainControl duration ref named to) -> fmap Effect.GainControl (GainControl.MkGainControl <$> onDuration c duration <*> onObjectRef c ref <*> traverse (onSlot c) named <*> onPlayer c to)
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
  Effect.ChooseQuality (ChooseQuality.MkChooseQuality quality linked) -> fmap Effect.ChooseQuality (ChooseQuality.MkChooseQuality <$> chosenQualityChildren (onSubtype c) (onFilter c) quality <*> pure linked)
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
  Effect.ChaosEnsues ref -> fmap Effect.ChaosEnsues (traverse (onObjectRef c) ref)
  Effect.Abandon -> pure effect
  Effect.SetInMotion ref -> fmap Effect.SetInMotion (onObjectRef c ref)
  Effect.ClaimPrize -> pure effect
  Effect.Forage -> pure effect
  Effect.Populate -> pure effect
  Effect.Learn -> pure effect
  Effect.TimeTravel -> pure effect
  Effect.Recruit -> pure effect
  Effect.Discover quantity -> fmap Effect.Discover (onQuantity c quantity)
  Effect.Cloak (Cloak.MkCloak who library) -> fmap Effect.Cloak (Cloak.MkCloak <$> onPlayer c who <*> traverse (onPlayer c) library)
  Effect.ManifestDread (ManifestDread.MkManifestDread who quantity named) -> fmap Effect.ManifestDread (ManifestDread.MkManifestDread <$> onPlayer c who <*> onQuantity c quantity <*> traverse (onSlot c) named)
  Effect.Venture _ -> pure effect
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices players who filter_ quantity permanents) ->
    fmap Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices <$> onPlayer c players <*> traverse (onFilter c) who <*> onFilter c filter_ <*> onQuantity c quantity <*> traverse (onSlot c) permanents)
  Effect.Vote (Vote.MkVote starter choices secret upToOne) -> fmap Effect.Vote (Vote.MkVote <$> onPlayer c starter <*> voteChoicesChildren c choices <*> pure secret <*> pure upToOne)
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
  Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile duration who ref spending alternativeCost condition verb increase landEnters upTo keywords zone) ->
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
          <*> traverse (onKeyword c) keywords
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

-- | A SetX's new value: the quantity it becomes, where it states one.
newXChildren :: (Applicative f) => EffectChildren card ability f -> NewX.NewX -> f NewX.NewX
newXChildren c newX = case newX of
  NewX.Becomes quantity -> fmap NewX.Becomes (onQuantity c quantity)
  NewX.Multiplied _ -> pure newX

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
  VoteChoices.Objects (VoteObjects.MkVoteObjects filter_ named anyVotes) -> fmap VoteChoices.Objects (VoteObjects.MkVoteObjects <$> onFilter c filter_ <*> onSlot c named <*> pure anyVotes)
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

-- | How many casts an offer allows: the cap, where Quantities state one.
castRepetitionChildren :: (Applicative f) => EffectChildren card ability f -> CastRepetition.CastRepetition -> f CastRepetition.CastRepetition
castRepetitionChildren c repetition = case repetition of
  CastRepetition.Once -> pure repetition
  CastRepetition.AnyNumber -> pure repetition
  CastRepetition.WithinTotal (TotalCap.MkTotalCap weight most) -> fmap CastRepetition.WithinTotal (TotalCap.MkTotalCap <$> onQuantity c weight <*> onQuantity c most)
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
  TriggerCondition.DealsDamage (DamageTrigger.MkDamageTrigger actor kind recipients atLeast) -> (\renamed qualified -> TriggerCondition.DealsDamage (DamageTrigger.MkDamageTrigger renamed kind qualified atLeast)) <$> actingChildren c actor <*> traverseSet (damageRecipientChildren c) recipients
  TriggerCondition.IsDealtDamage (PermanentIsDealtDamage.MkPermanentIsDealtDamage (DealtDamage.MkDealtDamage recipient kind by) batching excess) ->
    fmap (\renamed -> TriggerCondition.IsDealtDamage (PermanentIsDealtDamage.MkPermanentIsDealtDamage (DealtDamage.MkDealtDamage renamed kind by) batching excess)) (actingChildren c recipient)
  TriggerCondition.PlayerIsDealtDamage (PlayerIsDealtDamage.MkPlayerIsDealtDamage dealt orTheirPlaneswalkers batching turn) -> fmap (TriggerCondition.PlayerIsDealtDamage . PlayerIsDealtDamage.MkPlayerIsDealtDamage dealt orTheirPlaneswalkers batching) (triggerTurn c turn)
  TriggerCondition.PermanentsDealDamageToPlayer (PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer filter_ kind recipient oneOrMore anyController) ->
    fmap (\renamed -> TriggerCondition.PermanentsDealDamageToPlayer (PermanentsDealDamageToPlayer.MkPermanentsDealDamageToPlayer renamed kind recipient oneOrMore anyController)) (triggerFilter c filter_)
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
  TriggerCondition.Countered (CounteringTrigger.MkCounteringTrigger {}) -> pure condition
  TriggerCondition.DamageToPlayerPrevented _ -> pure condition
  TriggerCondition.SelfPreventsDamage filter_ -> fmap TriggerCondition.SelfPreventsDamage (triggerFilter c filter_)
  TriggerCondition.LifeChanges (LifeTrigger.MkLifeTrigger direction who turn batching frequency exactly) ->
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
          <*> pure exactly
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
  TriggerCondition.SpellCast (SpellCast.MkSpellCast spell turn zone ordinal phase copies while_) ->
    fmap TriggerCondition.SpellCast (SpellCast.MkSpellCast <$> actingChildren c spell <*> triggerTurn c turn <*> pure zone <*> traverse (triggerOrdinal c HistoryRead.Casts) ordinal <*> pure phase <*> pure copies <*> traverse (triggerState c) while_)
  TriggerCondition.AbilityActivated filter_ -> fmap TriggerCondition.AbilityActivated (triggerFilter c filter_)
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
  TriggerCondition.CausedBy (CausedBy.MkCausedBy controller sourceKeyword narrowed) -> fmap (TriggerCondition.CausedBy . CausedBy.MkCausedBy controller sourceKeyword) (triggerNested c narrowed)
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
  TriggerCondition.AbilityWatch _ -> pure condition
  TriggerCondition.PlayerBecomesMonarch _ -> pure condition
  TriggerCondition.LosesControl subject -> case subject of
    LostPermanent.Self -> pure condition
    LostPermanent.Bound slot_ -> fmap (TriggerCondition.LosesControl . LostPermanent.Bound) (triggerSlot c slot_)
  TriggerCondition.RoomEntered _ -> pure condition
  TriggerCondition.PlayerLosesGame _ -> pure condition
  TriggerCondition.PlayerPlaysLand (PlaysLand.MkPlaysLand who from filter_) -> fmap (TriggerCondition.PlayerPlaysLand . PlaysLand.MkPlaysLand who from) (triggerFilter c filter_)
  TriggerCondition.PlayerManifestsDread _ -> pure condition
  TriggerCondition.PlayerFacesDilemma _ -> pure condition
  TriggerCondition.PlayerCommitsCrime _ -> pure condition
  TriggerCondition.PlayerExpends _ -> pure condition
  TriggerCondition.ChaosEnsues -> pure condition
  TriggerCondition.PlayerRollsPlaneswalker _ -> pure condition
  TriggerCondition.TurnedUpOffDeck subject -> fmap TriggerCondition.TurnedUpOffDeck (actingChildren c subject)
  TriggerCondition.SelfPlaneswalkedAwayFrom -> pure condition
  TriggerCondition.PlayerRollsResult (DieResult.MkDieResult _ _) -> pure condition
  TriggerCondition.Visit -> pure condition
  TriggerCondition.FlipsCoin (CoinFlipTrigger.MkCoinFlipTrigger _ _) -> pure condition
  TriggerCondition.SelfBecomesPlotted -> pure condition
  TriggerCondition.AttachmentChanges (AttachmentTrigger.MkAttachmentTrigger end direction other) -> fmap (TriggerCondition.AttachmentChanges . AttachmentTrigger.MkAttachmentTrigger end direction) (triggerFilter c other)
  TriggerCondition.Reflexive -> pure condition
  TriggerCondition.PermanentActs (PermanentActsTrigger.MkPermanentActsTrigger (PermanentActed.MkPermanentActed action permanent) batching) -> fmap (\p -> TriggerCondition.PermanentActs (PermanentActsTrigger.MkPermanentActsTrigger (PermanentActed.MkPermanentActed action p) batching)) (actingChildren c permanent)
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

-- CR 120.1: a permanent recipient's qualifier, read as actingChildren reads an actor's.
damageRecipientChildren :: (Applicative f) => TriggerChildren f -> DamageRecipient.DamageRecipient -> f DamageRecipient.DamageRecipient
damageRecipientChildren c recipient = case recipient of
  DamageRecipient.Player _ -> pure recipient
  DamageRecipient.Permanent cardType qualifier -> DamageRecipient.Permanent cardType <$> triggerFilter c qualifier

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
  Keyword.Equip (Equip.MkEquip cost quality onto limit) -> fmap Keyword.Equip (Equip.MkEquip <$> keywordCost c cost <*> traverse (keywordFilter c) quality <*> pure onto <*> pure limit)
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
  Keyword.BandsWithOther quality -> fmap Keyword.BandsWithOther (keywordFilter c quality)
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
  Keyword.DoubleAgenda -> pure keyword_
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
  Keyword.Gift Gift.ExtraTurn -> pure keyword_
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
  Keyword.BeamMeUp (BeamMeUp.MkBeamMeUp cost unreturnable) -> fmap Keyword.BeamMeUp (BeamMeUp.MkBeamMeUp <$> keywordCost c cost <*> traverse (keywordFilter c) unreturnable)

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
    -- | The slot an unannounced amount is bound under (CR 107.3a, 601.2b), or
    -- that names the permanent a counter cost is on (CountersOnThis.holder).
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
  CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents n which criterion spread) -> fmap CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents <$> objectCountChildren c n <*> whichCountersChildren c which <*> costFilter c criterion <*> pure spread)
  CostComponent.PutCountersOnThis counters -> CostComponent.PutCountersOnThis <$> countersOnThisChildren c counters
  CostComponent.PutCountersOnPayer _ -> pure component
  CostComponent.Blight amount -> CostComponent.Blight <$> costAmountChildren c amount
  CostComponent.Forage -> pure component
  CostComponent.FlipCoin -> pure component
  CostComponent.ExileThisFrom _ -> pure component
  CostComponent.ExileThis -> pure component
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) -> fmap CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard <$> costAmountChildren c n <*> costFilter c criterion)
  CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n orMore criterion) -> fmap CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n orMore <$> costFilter c criterion)
  CostComponent.ExileTopFromGraveyard criterion -> CostComponent.ExileTopFromGraveyard <$> costFilter c criterion
  CostComponent.CollectEvidence _ -> pure component
  CostComponent.CollectEvidenceOfTargets -> pure component
  CostComponent.ExileCardsTotalling (ExileCardsTotalling.MkExileCardsTotalling threshold tally criterion) -> fmap CostComponent.ExileCardsTotalling (ExileCardsTotalling.MkExileCardsTotalling threshold <$> cardTallyChildren c tally <*> costFilter c criterion)
  CostComponent.ExileCardsFromHand (CardsFromHand.MkCardsFromHand n criterion) -> fmap CostComponent.ExileCardsFromHand (CardsFromHand.MkCardsFromHand <$> costAmountChildren c n <*> costFilter c criterion)
  CostComponent.RevealCardsFromHand (CardsFromHand.MkCardsFromHand n criterion) -> fmap CostComponent.RevealCardsFromHand (CardsFromHand.MkCardsFromHand <$> costAmountChildren c n <*> costFilter c criterion)
  CostComponent.Behold (Behold.MkBehold n criterion chosen zone) -> fmap CostComponent.Behold ((\f -> Behold.MkBehold n f chosen zone) <$> costFilter c criterion)
  CostComponent.BeholdAndExile criterion -> CostComponent.BeholdAndExile <$> costFilter c criterion
  CostComponent.MillCards _ -> pure component
  CostComponent.RevealTopOfLibrary _ -> pure component
  CostComponent.ChooseOpponent _ -> pure component
  CostComponent.ChooseSubtype _ -> pure component
  CostComponent.Waterbend amount -> CostComponent.Waterbend <$> costAmountChildren c amount
  CostComponent.WaterbendInstead _ -> pure component
  CostComponent.TapInsteadOfMana criterion -> CostComponent.TapInsteadOfMana <$> costFilter c criterion
  CostComponent.VariableAtLeast _ -> pure component

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
countersOnThisChildren c (CountersOnThis.MkCountersOnThis kind n holder) = CountersOnThis.MkCountersOnThis <$> costCounterKind c kind <*> costAmountChildren c n <*> traverse (costSlot c) holder

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
    playerActivations :: ActivationCriteria.ActivationCriteria Keyword.Keyword -> f (ActivationCriteria.ActivationCriteria Keyword.Keyword),
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
    -- | A keyword an effect gives a spell (CR 611.2f), whole.
    playerKeyword :: Keyword.Keyword -> f Keyword.Keyword,
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
      playerKeyword = pure,
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
  PlayerEffect.IncreaseDevotion _ -> pure effect
  PlayerEffect.ReduceMaximumHandSize _ -> pure effect
  PlayerEffect.DontLoseUnspentMana which becomes -> (\named -> PlayerEffect.DontLoseUnspentMana named becomes) <$> playerManaFilter c which
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
  PlayerEffect.NextSpell filter_ keywords -> PlayerEffect.NextSpell <$> playerFilter c filter_ <*> traverse (playerKeyword c) keywords
  PlayerEffect.RestrictTargeting (TargetRestriction.MkTargetRestriction affected kind aimers) ->
    fmap PlayerEffect.RestrictTargeting (TargetRestriction.MkTargetRestriction <$> playerFilter c affected <*> pure kind <*> playerFilter c aimers)
  PlayerEffect.TargetPastHexproof (HexproofExemption.MkHexproofExemption permanents players) ->
    fmap PlayerEffect.TargetPastHexproof (HexproofExemption.MkHexproofExemption <$> playerFilter c permanents <*> pure players)
  PlayerEffect.DamageCantBePrevented damage -> PlayerEffect.DamageCantBePrevented <$> playerDamage c damage
  PlayerEffect.DamageCantBeRedirected damage -> PlayerEffect.DamageCantBeRedirected <$> playerDamage c damage
  PlayerEffect.CantSearchLibraries _ -> pure effect
  PlayerEffect.SearchesTopOfLibraries _ -> pure effect
  PlayerEffect.StateBasedActionDoesntApply action exempted -> PlayerEffect.StateBasedActionDoesntApply action <$> playerFilter c exempted
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
  PlayerEffect.CastFrom (CastFromZone.MkCastFromZone from matching limit verb pool additionalCosts reduction spending alternativeCost entersWith amount) ->
    fmap PlayerEffect.CastFrom (CastFromZone.MkCastFromZone <$> playerZone c from <*> playerFilter c matching <*> pure limit <*> pure verb <*> pure pool <*> traverse (playerCostComponent c) additionalCosts <*> pure reduction <*> pure spending <*> pure alternativeCost <*> traverse (\(WithCounters.MkWithCounters counters) -> WithCounters.MkWithCounters <$> traverse (playerQuantity c) counters) entersWith <*> traverse (playerQuantity c) amount)
  PlayerEffect.PlayLandsFrom from -> PlayerEffect.PlayLandsFrom <$> playerZone c from
  PlayerEffect.PlotFrom (PlotFromZone.MkPlotFromZone from matching) ->
    fmap PlayerEffect.PlotFrom (PlotFromZone.MkPlotFromZone <$> playerZone c from <*> playerFilter c matching)
  PlayerEffect.CastWithoutPayingManaCost (FreeCast.MkFreeCast spells amount from onlyNth) ->
    fmap PlayerEffect.CastWithoutPayingManaCost (FreeCast.MkFreeCast <$> playerFilter c spells <*> traverse (playerQuantity c) amount <*> pure from <*> traverse (onlyNthChildren c HistoryRead.Casts) onlyNth)
  PlayerEffect.CantGetCounters _ -> pure effect
  -- CR 705.3: "the first coin you flip each turn" counts the turn's flips.
  PlayerEffect.StateCoinFlip (StatedFlip.MkStatedFlip _ _ firstEachTurn) -> effect <$ Monad.when firstEachTurn (playerLooksBack c HistoryRead.CoinFlips)
  PlayerEffect.ModifyDieRoll (ModifiedRoll.MkModifiedRoll sides natural modifier cost limit) ->
    fmap PlayerEffect.ModifyDieRoll (ModifiedRoll.MkModifiedRoll sides natural modifier <$> traverse (playerCost c) cost <*> pure limit)
  PlayerEffect.AdditionalVotes _ -> pure effect
  PlayerEffect.AdditionalSurveilCards _ -> pure effect
  PlayerEffect.PutTappedAsYouCascade filter_ -> PlayerEffect.PutTappedAsYouCascade <$> playerFilter c filter_
  PlayerEffect.Cant _ -> pure effect
  PlayerEffect.Precluded _ -> pure effect
  PlayerEffect.Shows _ -> pure effect
  PlayerEffect.RevealsDraw _ -> pure effect

-- | An "only the first (or nth) each turn" limit's children: the turn it
-- counts over, and the events it counts.
onlyFirstChildren :: (Applicative f) => PlayerEffectChildren f -> HistoryRead.HistoryRead -> TurnScope.TurnScope -> f TurnScope.TurnScope
onlyFirstChildren c counted turn = playerLooksBack c counted *> playerTurn c turn

-- | The same over a limit naming its ordinal, which has no children.
onlyNthChildren :: (Applicative f) => PlayerEffectChildren f -> HistoryRead.HistoryRead -> TurnOrdinal.TurnOrdinal -> f TurnOrdinal.TurnOrdinal
onlyNthChildren c counted (TurnOrdinal.MkTurnOrdinal turn n) = flip TurnOrdinal.MkTurnOrdinal n <$> onlyFirstChildren c counted turn

-- | A cost modifier's children (CR 601.2f, 602.2b).
costModifierChildren :: (Applicative f) => PlayerEffectChildren f -> CostModifier.CostModifier -> f CostModifier.CostModifier
costModifierChildren c (CostModifier.MkCostModifier subject matching whichTargets perTarget perEach countedAgainst onlyNth change) =
  CostModifier.MkCostModifier
    <$> costSubjectChildren c subject
    <*> playerFilter c matching
    <*> traverse (playerFilter c) whichTargets
    <*> traverse (perTargetChildren c) perTarget
    <*> traverse (playerQuantity c) perEach
    <*> pure countedAgainst
    <*> traverse (onlyNthChildren c (costSubjectHistory subject)) onlyNth
    <*> costChangeChildren c change

-- | The turn's history a cost modifier's "only the nth" reads: CR 601.2i's
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
    -- | A subtype an entry option adds (CR 208.2b) or a choice of a quality lists (CR 205.3).
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
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR (ZoneChangePattern.MkZoneChangePattern whenDestination whose what discarded causedBy resolving) destination revealing shuffling position optional counters whenYouDo riders) ->
    fmap ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR <$> (ZoneChangePattern.MkZoneChangePattern whenDestination <$> replacementWhose c whose <*> replacementFilter c what <*> pure discarded <*> pure causedBy <*> pure resolving) <*> pure destination <*> pure revealing <*> pure shuffling <*> pure position <*> pure optional <*> traverse (replacementCounters c) counters <*> traverse (replacementEffect c) whenYouDo <*> traverse (replacementEffect c) riders)
  ReplacementEffect.EntryR (EntryR.MkEntryR matching rewrite) -> fmap ReplacementEffect.EntryR (EntryR.MkEntryR <$> replacementFilter c matching <*> entryRewriteChildren c rewrite)
  ReplacementEffect.DamageR (DamageR.MkDamageR matching rewrite riders) ->
    fmap ReplacementEffect.DamageR (DamageR.MkDamageR <$> replacementDamage c matching <*> damageRewriteChildren c rewrite <*> traverse (replacementEffect c) riders)
  ReplacementEffect.DestructionR (DestructionR.MkDestructionR matching rewrite) -> fmap ReplacementEffect.DestructionR (DestructionR.MkDestructionR <$> traverse (replacementFilter c) matching <*> pure rewrite)
  ReplacementEffect.CounterR (CounterR.MkCounterR (CounterPattern.MkCounterPattern kind subject whose onWhat onWho) scaling) ->
    fmap ReplacementEffect.CounterR (CounterR.MkCounterR <$> (CounterPattern.MkCounterPattern <$> traverse (replacementCounterKind c) kind <*> counterSubjectChildren c subject <*> replacementWhose c whose <*> replacementFilter c onWhat <*> traverse (replacementWhose c) onWho) <*> scalingChildren c scaling)
  ReplacementEffect.TokenR (TokenR.MkTokenR (TokenPattern.MkTokenPattern whose whatToken effectsOnly firstEachTurn) scaling plus optional) ->
    fmap ReplacementEffect.TokenR (TokenR.MkTokenR <$> (TokenPattern.MkTokenPattern <$> replacementWhose c whose <*> replacementFilter c whatToken <*> pure effectsOnly <*> pure firstEachTurn) <*> traverse (scalingChildren c) scaling <*> traverse (tokenPlusChildren c) plus <*> pure optional)
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR matching requiring paying rewrite) ->
    fmap ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR <$> replacementFilter c matching <*> pure requiring <*> traverse (replacementProcedureCost c) paying <*> turnUpRewriteChildren c rewrite)
  ReplacementEffect.AttachR (AttachR.MkAttachR matching rewrite) -> fmap ReplacementEffect.AttachR (AttachR.MkAttachR <$> replacementFilter c matching <*> attachRewriteChildren c rewrite)
  ReplacementEffect.UntapR (UntapR.MkUntapR during rewrite) -> fmap ReplacementEffect.UntapR (UntapR.MkUntapR during <$> untapRewriteChildren c rewrite)
  ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR (LifeLossPattern.MkLifeLossPattern whose cause) rewrite) ->
    fmap ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR <$> (LifeLossPattern.MkLifeLossPattern <$> replacementWhose c whose <*> pure cause) <*> lifeLossRewriteChildren c rewrite)
  ReplacementEffect.LifeGainR (LifeGainR.MkLifeGainR whose rewrite) -> fmap ReplacementEffect.LifeGainR (LifeGainR.MkLifeGainR <$> replacementWhose c whose <*> lifeGainRewriteChildren c rewrite)
  ReplacementEffect.CounterLossR (CounterLossR.MkCounterLossR onWhat kind floor_) ->
    fmap ReplacementEffect.CounterLossR (CounterLossR.MkCounterLossR <$> replacementFilter c onWhat <*> replacementCounterKind c kind <*> pure floor_)
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
  EntryRewrite.ChooseQuality quality -> EntryRewrite.ChooseQuality <$> chosenQualityChildren (replacementSubtype c) (replacementFilter c) quality
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
    fmap EntryRewrite.ExileFromGraveyard (ExileFromGraveyard.MkExileFromGraveyard <$> replacementFilter c filter_ <*> pure players <*> howManyChildren (replacementQuantity c) count <*> pure orGraveyard <*> traverse (replacementCounterKind c) kinds <*> replacementQuantity c each)
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
  EntryRewrite.EntersTapState _ -> pure rewrite
  EntryRewrite.Prepared -> pure rewrite
  EntryRewrite.OrTapped price -> EntryRewrite.OrTapped <$> entryPriceChildren c price
  EntryRewrite.PayAnyLife cap -> EntryRewrite.PayAnyLife <$> traverse (replacementQuantity c) cap
  EntryRewrite.EntersTransformed -> pure rewrite
  EntryRewrite.RunEffects effects -> EntryRewrite.RunEffects <$> traverse (replacementEffect c) effects
  EntryRewrite.InOrder rewrites -> EntryRewrite.InOrder <$> traverse (entryRewriteChildren c) rewrites

-- | The subtypes a choice of a quality lists (CR 205.3) and the Filter a
-- choice of an exiled card states: its children, as a colour, a card type, a
-- number and a CR 614.12c anchor word hold none.
chosenQualityChildren :: (Applicative f) => (Subtype.Subtype -> f Subtype.Subtype) -> (Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword)) -> ChosenQuality.ChosenQuality -> f ChosenQuality.ChosenQuality
chosenQualityChildren onListed onCards quality = case quality of
  ChosenQuality.Colors _ -> pure quality
  ChosenQuality.Subtype family among -> ChosenQuality.Subtype family <$> traverse (traverse onListed) among
  ChosenQuality.CardType _ -> pure quality
  ChosenQuality.Number _ -> pure quality
  ChosenQuality.Parity -> pure quality
  ChosenQuality.Word _ -> pure quality
  ChosenQuality.ExiledCard pile cards -> ChosenQuality.ExiledCard pile <$> onCards cards

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
  DrawRewrite.RunEffects optional effects -> DrawRewrite.RunEffects optional <$> traverse (replacementEffect c) effects

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
  AttachRewrite.ChooseQuality quality -> AttachRewrite.ChooseQuality <$> chosenQualityChildren (replacementSubtype c) (replacementFilter c) quality

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
  InstructionRewrite.ReturnSourceInstead -> pure rewrite
  InstructionRewrite.SetTo _ -> pure rewrite

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
  TokenPlus.InsteadCopies copied -> TokenPlus.InsteadCopies <$> replacementFilter c copied

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

-- | How many cards a choice takes (CR 608.2d), its count through the visitor.
howManyChildren :: (Applicative f) => (Quantity.Quantity -> f Quantity.Quantity) -> HowMany.HowMany -> f HowMany.HowMany
howManyChildren onCount count = case count of
  HowMany.Exactly n -> HowMany.Exactly <$> onCount n
  HowMany.UpTo n -> HowMany.UpTo <$> traverse onCount n

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
  Condition.During (DuringPhase.MkDuringPhase window turns) -> fmap Condition.During (DuringPhase.MkDuringPhase window <$> conditionTurn c turns)

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
  Duration.UntilNextStepOf (NextStepOf.MkNextStepOf step whose) -> fmap Duration.UntilNextStepOf (NextStepOf.MkNextStepOf step <$> durationPlayer c whose)
  Duration.UntilEndOfYourNextTurn -> pure duration
  Duration.UntilEndOfNextTurnOf whose -> Duration.UntilEndOfNextTurnOf <$> durationPlayer c whose
  Duration.DuringNextTurnOf whose -> Duration.DuringNextTurnOf <$> durationPlayer c whose
  Duration.DuringNextUntapStepOf whose -> Duration.DuringNextUntapStepOf <$> durationPlayer c whose
  Duration.DuringYourNextTurn -> pure duration
  Duration.DuringThatExtraTurn -> pure duration
  Duration.DuringThatCombatPhase -> pure duration
  Duration.ForAsLongAs condition -> Duration.ForAsLongAs <$> durationCondition c condition
  Duration.UntilEndOfCombat -> pure duration
  Duration.UntilEndOfCombatOnYourNextTurn -> pure duration
  Duration.UntilPaid (UntilPaid.MkUntilPaid payer cost) -> fmap Duration.UntilPaid (UntilPaid.MkUntilPaid <$> durationPlayer c payer <*> durationCost c cost)
  Duration.UntilUsed used -> Duration.UntilUsed <$> traverse (durationSlot c) used
  Duration.UntilEvent event -> Duration.UntilEvent <$> durationEventChildren c event

-- | The event that ends a duration (CR 611.2a): a filter, a slot or a baked card.
durationEventChildren :: (Applicative f) => DurationChildren f -> DurationEvent.DurationEvent -> f DurationEvent.DurationEvent
durationEventChildren c event = case event of
  DurationEvent.SourceTurnedFaceDown -> pure event
  DurationEvent.SpellCast criterion -> DurationEvent.SpellCast <$> durationFilter c criterion
  DurationEvent.Planeswalked -> pure event
  DurationEvent.SourceExiledCard -> pure event
  DurationEvent.SourceLeftBattlefield -> pure event
  DurationEvent.BoundCardCast bound -> DurationEvent.BoundCardCast <$> durationSlot c bound
  DurationEvent.CardCast card -> DurationEvent.CardCast <$> durationObject c card

-- | One visitor per kind of child an ability record holds: an activated (CR
-- 602), triggered (CR 603) or static (CR 604) ability, a printed replacement
-- (CR 614), a player static ability, a self cost reduction (CR 601.2f) or an
-- alternative cost (CR 118.9).
data AbilityChildren card ability effect f = MkAbilityChildren
  { -- | A cost: an activation cost (CR 602.1a), a choice inside one, an alternative cost.
    abilityCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | A Quantity: a ceiling on X or a defined X (CR 101.1, 107.3c), a reduction's multiplier.
    abilityQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | The modes an activated or triggered ability chooses among (CR 700.2), whole.
    abilityModal :: Modal.Modal card ability -> f (Modal.Modal card ability),
    -- | A clause of an "activate only" rider (CR 602.5b), whole.
    abilityRestriction :: ActivationRestriction.ActivationRestriction -> f ActivationRestriction.ActivationRestriction,
    -- | An "as long as" gate (CR 611.3a) or an intervening "if" (CR 603.4), whole.
    abilityCondition :: Condition.Condition -> f Condition.Condition,
    -- | The keyword an activated ability is under (CR 702), whole.
    abilityKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | A triggered ability's trigger condition (CR 603.1), whole.
    abilityTrigger :: TriggerCondition.TriggerCondition -> f TriggerCondition.TriggerCondition,
    -- | A triggered ability's "only once" rider (CR 603.2h).
    abilityLimit :: TriggerLimit.TriggerLimit -> f TriggerLimit.TriggerLimit,
    -- | The objects a static ability affects (CR 611.3a), whole.
    abilityAffected :: Affected.Affected -> f Affected.Affected,
    -- | How long a static ability's effect lingers after its source leaves (CR 611.2).
    abilityDuration :: Duration.Duration -> f Duration.Duration,
    -- | A static ability's continuous effect (CR 613.1), whole.
    abilityModification :: Modification.Modification ability -> f (Modification.Modification ability),
    -- | A player static ability's effect (CR 613.10), whole.
    abilityPlayerEffect :: PlayerEffect.PlayerEffect -> f PlayerEffect.PlayerEffect,
    -- | A printed replacement's effect (CR 614.1), whole.
    abilityReplacement :: ReplacementEffect.ReplacementEffect card ability effect -> f (ReplacementEffect.ReplacementEffect card ability effect),
    -- | The targets a self cost reduction asks for (CR 601.2c).
    abilityFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A granted rule-affecting static ability (CR 613.11), whole.
    abilityRules :: RuleAbilities.RuleAbilities -> f RuleAbilities.RuleAbilities,
    -- | A granted permission about the mana spent to cast its object (CR 609.4b), whole.
    abilitySpendMana :: SpendManaAsThough.SpendManaAsThough -> f SpendManaAsThough.SpendManaAsThough,
    -- | The slot an activated ability binds its announced player under (CR 115.10a).
    abilitySlot :: SlotName.SlotName -> f SlotName.SlotName
  }

-- | Every child of an ability record left as it is: the base a walk overrides.
abilityLeaves :: (Applicative f) => AbilityChildren card ability effect f
abilityLeaves =
  MkAbilityChildren
    { abilityCost = pure,
      abilityQuantity = pure,
      abilityModal = pure,
      abilityRestriction = pure,
      abilityCondition = pure,
      abilityKeyword = pure,
      abilityTrigger = pure,
      abilityLimit = pure,
      abilityAffected = pure,
      abilityDuration = pure,
      abilityModification = pure,
      abilityPlayerEffect = pure,
      abilityReplacement = pure,
      abilityFilter = pure,
      abilityRules = pure,
      abilitySpendMana = pure,
      abilitySlot = pure
    }

-- | An activated ability's children. Each choice's costs go to abilityCost.
activatedAbilityChildren :: (Applicative f) => AbilityChildren card ability effect f -> ActivatedAbility.ActivatedAbility card ability -> f (ActivatedAbility.ActivatedAbility card ability)
activatedAbilityChildren c (ActivatedAbility.MkActivatedAbility cost costChoices maximumX definedX modal restrictions activator condition name stamp copyability chosenPlayer) =
  ActivatedAbility.MkActivatedAbility
    <$> abilityCost c cost
    <*> traverse (fmap CostChoice.MkCostChoice . traverse (abilityCost c) . CostChoice.unwrap) costChoices
    <*> traverse (abilityQuantity c) maximumX
    -- CR 607.2a's pile is a relation on the board, named by no slot.
    <*> traverse (\(DefinedX.MkDefinedX quantity choice) -> fmap (`DefinedX.MkDefinedX` choice) (abilityQuantity c quantity)) definedX
    <*> abilityModal c modal
    <*> traverse (abilityRestriction c) restrictions
    <*> pure activator
    <*> traverse (abilityCondition c) condition
    <*> pure name
    <*> traverse (abilityKeyword c) stamp
    <*> pure copyability
    <*> traverse (\(PlayerSlot.MkPlayerSlot players named) -> PlayerSlot.MkPlayerSlot players <$> abilitySlot c named) chosenPlayer

-- | A triggered ability's children.
triggeredAbilityChildren :: (Applicative f) => AbilityChildren card ability effect f -> TriggeredAbility.TriggeredAbility card ability -> f (TriggeredAbility.TriggeredAbility card ability)
triggeredAbilityChildren c (TriggeredAbility.MkTriggeredAbility condition modal intervening name limit functionsFrom) =
  TriggeredAbility.MkTriggeredAbility
    <$> abilityTrigger c condition
    <*> abilityModal c modal
    <*> traverse (abilityCondition c) intervening
    <*> pure name
    <*> abilityLimit c limit
    <*> pure functionsFrom

-- | A static ability's children.
staticAbilityChildren :: (Applicative f) => AbilityChildren card ability effect f -> StaticAbility.StaticAbility ability -> f (StaticAbility.StaticAbility ability)
staticAbilityChildren c (StaticAbility.MkStaticAbility affected condition functionsFrom lingers name modifications) =
  StaticAbility.MkStaticAbility
    <$> abilityAffected c affected
    <*> traverse (abilityCondition c) condition
    <*> pure functionsFrom
    <*> traverse (abilityDuration c) lingers
    <*> pure name
    <*> traverse (abilityModification c) modifications

-- | A player static ability's children.
playerStaticAbilityChildren :: (Applicative f) => AbilityChildren card ability effect f -> PlayerStaticAbility.PlayerStaticAbility -> f PlayerStaticAbility.PlayerStaticAbility
playerStaticAbilityChildren c (PlayerStaticAbility.MkPlayerStaticAbility players condition name effect) =
  PlayerStaticAbility.MkPlayerStaticAbility players
    <$> traverse (abilityCondition c) condition
    <*> pure name
    <*> abilityPlayerEffect c effect

-- | A printed replacement's children.
printedReplacementChildren :: (Applicative f) => AbilityChildren card ability effect f -> PrintedReplacement.PrintedReplacement card ability effect -> f (PrintedReplacement.PrintedReplacement card ability effect)
printedReplacementChildren c (PrintedReplacement.MkPrintedReplacement condition effect functionsFrom name) =
  PrintedReplacement.MkPrintedReplacement
    <$> traverse (abilityCondition c) condition
    <*> abilityReplacement c effect
    <*> pure functionsFrom
    <*> pure name

-- | A self cost reduction's children.
costReductionChildren :: (Applicative f) => AbilityChildren card ability effect f -> CostReduction.CostReduction -> f CostReduction.CostReduction
costReductionChildren c (CostReduction.MkCostReduction amount perEach condition whichTargets direction) =
  CostReduction.MkCostReduction amount
    <$> abilityQuantity c perEach
    <*> traverse (abilityCondition c) condition
    <*> traverse (abilityFilter c) whichTargets
    <*> pure direction

-- | An alternative cost's children.
alternativeCostChildren :: (Applicative f) => AbilityChildren card ability effect f -> AlternativeCost.AlternativeCost -> f AlternativeCost.AlternativeCost
alternativeCostChildren c (AlternativeCost.MkAlternativeCost condition cost) =
  AlternativeCost.MkAlternativeCost
    <$> traverse (abilityCondition c) condition
    <*> abilityCost c cost

-- | A granted ability (CR 113.3), its record descended with the same visitors.
-- An ability nested inside it (a mode's grant, a modification's) is reached
-- through abilityModal or abilityModification and not descended here.
grantedAbilityChildren :: (Applicative f) => AbilityChildren card (GrantedAbility.GrantedAbility card) (Effect.Effect card (GrantedAbility.GrantedAbility card)) f -> GrantedAbility.GrantedAbility card -> f (GrantedAbility.GrantedAbility card)
grantedAbilityChildren c granted = case granted of
  GrantedAbility.Activated activated -> GrantedAbility.Activated <$> activatedAbilityChildren c activated
  GrantedAbility.Triggered triggered -> GrantedAbility.Triggered <$> triggeredAbilityChildren c triggered
  GrantedAbility.Static static -> GrantedAbility.Static <$> staticAbilityChildren c static
  GrantedAbility.Rules rules -> GrantedAbility.Rules <$> abilityRules c rules
  GrantedAbility.Replacement printed -> GrantedAbility.Replacement <$> printedReplacementChildren c printed
  GrantedAbility.Player playerStatic -> GrantedAbility.Player <$> playerStaticAbilityChildren c playerStatic
  GrantedAbility.SelfCostReduction reduction -> GrantedAbility.SelfCostReduction <$> costReductionChildren c reduction
  GrantedAbility.SelfAlternativeCost alternative -> GrantedAbility.SelfAlternativeCost <$> alternativeCostChildren c alternative
  GrantedAbility.SelfSpendManaAsThough clause -> GrantedAbility.SelfSpendManaAsThough <$> abilitySpendMana c clause

-- | One visitor per kind of child a modal payload holds (CR 700.2): its modes,
-- their clauses (CR 608.2c) and target slots (CR 115.1), and the instruction
-- the chosen modes share.
data ModalChildren card ability f = MkModalChildren
  { -- | A mode selection's "instead" gate (CR 700.2), whole.
    modalInstead :: Condition.Condition -> f Condition.Condition,
    -- | A "choose up to" count (CR 700.2), whole.
    modalQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A clause's own "if" gate (CR 608.2c), whole.
    modalCondition :: Condition.Condition -> f Condition.Condition,
    -- | Who announces an "or" (CR 608.2d) or answers a "may" (CR 603.5).
    modalPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | A cost paid on resolution (CR 118.12), whole.
    modalPayGate :: PayGate.PayGate -> f PayGate.PayGate,
    -- | One of a clause's instructions, whole.
    modalEffect :: Effect.Effect card ability -> f (Effect.Effect card ability),
    -- | A mode's target slot's name (CR 601.2c).
    modalSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | A mode's target slot (CR 601.2c), whole.
    modalTargetSlot :: TargetSlot.TargetSlot -> f TargetSlot.TargetSlot
  }

-- | Every child of a modal payload left as it is: the base a walk overrides.
modalLeaves :: (Applicative f) => ModalChildren card ability f
modalLeaves =
  MkModalChildren
    { modalInstead = pure,
      modalQuantity = pure,
      modalCondition = pure,
      modalPlayer = pure,
      modalPayGate = pure,
      modalEffect = pure,
      modalSlot = pure,
      modalTargetSlot = pure
    }

-- | A modal payload's children.
modalChildren :: (Applicative f) => ModalChildren card ability f -> Modal.Modal card ability -> f (Modal.Modal card ability)
modalChildren c (Modal.MkModal modes selection memory shared) =
  Modal.MkModal
    <$> traverse (modeChildren c) modes
    <*> modeSelectionChildren c selection
    <*> pure memory
    <*> traverse (clauseChildren c) shared

-- | How many modes a player chooses, its payload matched positionally.
modeSelectionChildren :: (Applicative f) => ModalChildren card ability f -> ModeSelection.ModeSelection -> f ModeSelection.ModeSelection
modeSelectionChildren c selection = case selection of
  ModeSelection.ChooseExactly _ -> pure selection
  ModeSelection.ChooseExactlyWithRepeats _ -> pure selection
  ModeSelection.ChooseBetween _ -> pure selection
  ModeSelection.ChooseBetweenWithRepeats _ -> pure selection
  ModeSelection.ChooseInstead (ChooseInstead.MkChooseInstead condition ordinary instead) -> fmap ModeSelection.ChooseInstead (ChooseInstead.MkChooseInstead <$> modalInstead c condition <*> modeSelectionChildren c ordinary <*> modeSelectionChildren c instead)
  ModeSelection.ChooseUpTo quantity -> ModeSelection.ChooseUpTo <$> modalQuantity c quantity
  ModeSelection.ChoosePawprintsWithRepeats _ -> pure selection

-- | A mode's children. Slots a visitor renames together merge.
modeChildren :: (Applicative f) => ModalChildren card ability f -> Mode.Mode card ability -> f (Mode.Mode card ability)
modeChildren c (Mode.MkMode clauses targetSlots) =
  Mode.MkMode
    <$> traverse (clauseChildren c) clauses
    <*> fmap Map.fromList (traverse (\(name, target) -> (,) <$> modalSlot c name <*> modalTargetSlot c target) (Map.toList targetSlots))

-- | A clause's children.
clauseChildren :: (Applicative f) => ModalChildren card ability f -> Clause.Clause card ability -> f (Clause.Clause card ability)
clauseChildren c (Clause.MkClause ifTaken condition orElse optionality payGate effects) =
  Clause.MkClause ifTaken
    <$> traverse (modalCondition c) condition
    <*> traverse (\(OrElse.MkOrElse siblings chooser villainous) -> (\who -> OrElse.MkOrElse siblings who villainous) <$> modalPlayer c chooser) orElse
    <*> optionalityChildren c optionality
    <*> traverse (modalPayGate c) payGate
    <*> traverse (modalEffect c) effects

-- | Whether a clause is optional, and who answers its "may" (CR 603.5).
optionalityChildren :: (Applicative f) => ModalChildren card ability f -> Optionality.Optionality -> f Optionality.Optionality
optionalityChildren c optionality = case optionality of
  Optionality.Mandatory -> pure optionality
  Optionality.Optional who -> Optionality.Optional <$> modalPlayer c who

-- | One visitor per kind of child a face holds (CR 712.6): its keywords (CR
-- 702), its abilities (CR 113.1), its spell (CR 700.2), its costs (CR 118.8, 118.9) and
-- the rule-affecting rows it prints. Its name and type line are leaves, since
-- CR 612.2a changes them together.
data FaceChildren card f = MkFaceChildren
  { -- | A printed power (CR 208.1), whole.
    facePower :: Power.Power -> f Power.Power,
    -- | A printed toughness (CR 208.1), whole.
    faceToughness :: Toughness.Toughness -> f Toughness.Toughness,
    -- | A deck-construction rule a commander prints (CR 903.5), whole.
    faceCommanderDeckRule :: CommanderDeckRule.CommanderDeckRule -> f CommanderDeckRule.CommanderDeckRule,
    -- | A printed exile before the game (CR 607.2n), whole.
    facePregameExile :: PregameExile.PregameExile -> f PregameExile.PregameExile,
    -- | A keyword (CR 702), whole. Keywords a visitor maps together merge, their counts summed.
    faceKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | A colour of the color indicator (CR 204), whole.
    faceColor :: Color.Color -> f Color.Color,
    -- | A characteristic-defining power and toughness (CR 208.2a), whole.
    faceCharacteristicPT :: CharacteristicPT.CharacteristicPT -> f CharacteristicPT.CharacteristicPT,
    -- | A static ability (CR 604), whole.
    faceStatic :: StaticAbility.StaticAbility (GrantedAbility.GrantedAbility card) -> f (StaticAbility.StaticAbility (GrantedAbility.GrantedAbility card)),
    -- | The spell's modes (CR 700.2), whole.
    faceModal :: Modal.Modal card (GrantedAbility.GrantedAbility card) -> f (Modal.Modal card (GrantedAbility.GrantedAbility card)),
    -- | An activated ability (CR 602), whole.
    faceActivated :: ActivatedAbility.ActivatedAbility card (GrantedAbility.GrantedAbility card) -> f (ActivatedAbility.ActivatedAbility card (GrantedAbility.GrantedAbility card)),
    -- | A printed replacement (CR 614.1), whole.
    faceReplacement :: PrintedReplacement.PrintedReplacement card (GrantedAbility.GrantedAbility card) (Effect.Effect card (GrantedAbility.GrantedAbility card)) -> f (PrintedReplacement.PrintedReplacement card (GrantedAbility.GrantedAbility card) (Effect.Effect card (GrantedAbility.GrantedAbility card))),
    -- | A triggered ability (CR 603), printed or delayed (CR 603.7), whole.
    faceTriggered :: TriggeredAbility.TriggeredAbility card (GrantedAbility.GrantedAbility card) -> f (TriggeredAbility.TriggeredAbility card (GrantedAbility.GrantedAbility card)),
    -- | A dungeon's room (CR 309.4), whole.
    faceRoom :: DungeonRoom.DungeonRoom card -> f (DungeonRoom.DungeonRoom card),
    -- | A permission to cast (CR 601.3), whole.
    faceCastingPermission :: CastingPermission.CastingPermission -> f CastingPermission.CastingPermission,
    -- | A restriction on casting (CR 601.3), whole.
    faceCastingRestriction :: CastingRestriction.CastingRestriction -> f CastingRestriction.CastingRestriction,
    -- | An Aura's enchant slot (CR 303.4a), whole.
    faceTargetSlot :: TargetSlot.TargetSlot -> f TargetSlot.TargetSlot,
    -- | Whether the spell can be countered (CR 113.6g), whole.
    faceCounterability :: Counterability.Counterability -> f Counterability.Counterability,
    -- | An additional cost (CR 118.8), whole.
    faceCostComponent :: CostComponent.CostComponent Keyword.Keyword -> f (CostComponent.CostComponent Keyword.Keyword),
    -- | A choice among additional costs (CR 118.8), whole.
    faceCostChoice :: CostChoice.CostChoice -> f CostChoice.CostChoice,
    -- | A mode's own cost (CR 700.2h), whole.
    faceCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | A ceiling on X (CR 107.3), whole.
    faceQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | An alternative cost (CR 118.9), whole.
    faceAlternativeCost :: AlternativeCost.AlternativeCost -> f AlternativeCost.AlternativeCost,
    -- | A self cost reduction (CR 601.2f), whole.
    faceCostReduction :: CostReduction.CostReduction -> f CostReduction.CostReduction,
    -- | A player static ability (CR 613.10), whole.
    facePlayerStatic :: PlayerStaticAbility.PlayerStaticAbility -> f PlayerStaticAbility.PlayerStaticAbility,
    -- | A blocking requirement (CR 509.1c), whole.
    faceBlockRequirement :: BlockRequirement.BlockRequirement -> f BlockRequirement.BlockRequirement,
    -- | A blocking permission (CR 509.1b), whole.
    faceBlockPermission :: BlockPermission.BlockPermission -> f BlockPermission.BlockPermission,
    -- | An attacking requirement (CR 508.1d), whole.
    faceAttackRequirement :: AttackRequirement.AttackRequirement -> f AttackRequirement.AttackRequirement,
    -- | A combat restriction (CR 508.1c, 509.1b), whole.
    faceCombatRestriction :: CombatRestriction.CombatRestriction -> f CombatRestriction.CombatRestriction,
    -- | A restriction on sacrificing (CR 701.21a), whole.
    faceSacrificeRestriction :: SacrificeRestriction.SacrificeRestriction -> f SacrificeRestriction.SacrificeRestriction,
    -- | A restriction on untapping (CR 502.3), whole.
    faceUntapRestriction :: UntapRestriction.UntapRestriction -> f UntapRestriction.UntapRestriction,
    -- | A restriction on crewing (CR 702.122a), whole.
    faceKeywordCostRestriction :: KeywordCostRestriction.KeywordCostRestriction -> f KeywordCostRestriction.KeywordCostRestriction,
    -- | A restriction on targeting (CR 613.11), whole.
    faceTargetRestriction :: TargetRestriction.TargetRestriction Affected.Affected -> f (TargetRestriction.TargetRestriction Affected.Affected),
    -- | A rule-changing permission (CR 613.11), whole.
    faceRulePermission :: RulePermission.RulePermission -> f RulePermission.RulePermission,
    -- | A restriction on attaching (CR 701.3a), whole.
    faceAttachRestriction :: AttachRestriction.AttachRestriction -> f AttachRestriction.AttachRestriction,
    -- | A restriction on entering the battlefield (CR 613.11), whole.
    faceEntryRestriction :: EntryRestriction.EntryRestriction -> f EntryRestriction.EntryRestriction,
    -- | A restriction on placing counters (CR 122.1), whole.
    faceCounterRestriction :: CounterRestriction.CounterRestriction -> f CounterRestriction.CounterRestriction,
    -- | A prohibition on activating abilities (CR 602.5), whole.
    faceActivationProhibition :: ActivationProhibition.ActivationProhibition -> f ActivationProhibition.ActivationProhibition,
    -- | A cost to attack (CR 508.1h), whole.
    faceAttackCost :: AttackCost.AttackCost -> f AttackCost.AttackCost,
    -- | A cost to block (CR 509.1d), whole.
    faceBlockCost :: BlockCost.BlockCost -> f BlockCost.BlockCost,
    -- | A limit on how many counters of a kind a permanent keeps (CR 122.4), whole.
    faceCounterLimit :: CounterLimit.CounterLimit -> f CounterLimit.CounterLimit,
    -- | An action taken from a hand at the mulligan or the game's start (CR 103.6), whole.
    faceHandAction :: HandAction.HandAction card -> f (HandAction.HandAction card),
    -- | A special action (CR 116.2), whole.
    faceSpecialAction :: SpecialAction.SpecialAction -> f SpecialAction.SpecialAction
  }

-- | Every child of a face left as it is: the base a walk overrides.
faceLeaves :: (Applicative f) => FaceChildren card f
faceLeaves =
  MkFaceChildren
    { facePower = pure,
      faceToughness = pure,
      faceCommanderDeckRule = pure,
      facePregameExile = pure,
      faceKeyword = pure,
      faceColor = pure,
      faceCharacteristicPT = pure,
      faceStatic = pure,
      faceModal = pure,
      faceActivated = pure,
      faceReplacement = pure,
      faceTriggered = pure,
      faceRoom = pure,
      faceCastingPermission = pure,
      faceCastingRestriction = pure,
      faceTargetSlot = pure,
      faceCounterability = pure,
      faceCostComponent = pure,
      faceCostChoice = pure,
      faceCost = pure,
      faceQuantity = pure,
      faceAlternativeCost = pure,
      faceCostReduction = pure,
      facePlayerStatic = pure,
      faceBlockRequirement = pure,
      faceBlockPermission = pure,
      faceAttackRequirement = pure,
      faceCombatRestriction = pure,
      faceSacrificeRestriction = pure,
      faceUntapRestriction = pure,
      faceKeywordCostRestriction = pure,
      faceTargetRestriction = pure,
      faceRulePermission = pure,
      faceAttachRestriction = pure,
      faceEntryRestriction = pure,
      faceCounterRestriction = pure,
      faceActivationProhibition = pure,
      faceAttackCost = pure,
      faceBlockCost = pure,
      faceCounterLimit = pure,
      faceHandAction = pure,
      faceSpecialAction = pure
    }

-- | A face's children. A delayed ability's name and a mode cost's index are keys, left as they are.
faceChildren :: (Applicative f) => FaceChildren card f -> Face.Face card -> f (Face.Face card)
faceChildren c (Face.MkFace name oracleText manaCost typeLine power toughness loyalty defense startingIntensity vanguard canBeYourCommander claimsStartingPlayer additionalHand anteOnly circlesTwo deckLimit commanderDeckRules pregameExile keywords colorIndicator characteristicPT characteristicColors staticAbilities spell activatedAbilities replacementEffects triggeredAbilities delayedAbilities rooms dungeonEntryQuality castingPermissions castingRestrictions enchant counterability copyability additionalCosts additionalCostChoices modeCosts modeChooser maximumX minimumX alternativeCosts costReductions playerAbilities blockRequirements blockPermissions attackRequirements combatRestrictions sacrificeRestrictions untapRestrictions keywordCostRestrictions targetRestrictions rulePermissions attachRestrictions entryRestrictions counterRestrictions activationProhibitions attackCosts blockCosts counterLimits mulliganActions openingHandActions specialActions) =
  Face.MkFace name oracleText manaCost typeLine
    <$> traverse (facePower c) power
    <*> traverse (faceToughness c) toughness
    <*> pure loyalty
    <*> pure defense
    <*> pure startingIntensity
    <*> pure vanguard
    <*> pure canBeYourCommander
    <*> pure claimsStartingPlayer
    <*> pure additionalHand
    <*> pure anteOnly
    <*> pure circlesTwo
    <*> pure deckLimit
    <*> traverseSet (faceCommanderDeckRule c) commanderDeckRules
    <*> traverse (facePregameExile c) pregameExile
    <*> fmap (Map.fromListWith (+)) (traverse (\(printed, count) -> (\k -> (k, count)) <$> faceKeyword c printed) (Map.toList keywords))
    <*> traverseSet (faceColor c) colorIndicator
    <*> traverse (faceCharacteristicPT c) characteristicPT
    <*> pure characteristicColors
    <*> traverse (faceStatic c) staticAbilities
    <*> faceModal c spell
    <*> traverse (faceActivated c) activatedAbilities
    <*> traverse (faceReplacement c) replacementEffects
    <*> traverse (faceTriggered c) triggeredAbilities
    <*> traverse (faceTriggered c) delayedAbilities
    <*> traverse (faceRoom c) rooms
    <*> pure dungeonEntryQuality
    <*> traverse (faceCastingPermission c) castingPermissions
    <*> traverse (faceCastingRestriction c) castingRestrictions
    <*> traverse (faceTargetSlot c) enchant
    <*> faceCounterability c counterability
    <*> pure copyability
    <*> traverse (faceCostComponent c) additionalCosts
    <*> traverse (faceCostChoice c) additionalCostChoices
    <*> traverse (faceCost c) modeCosts
    <*> pure modeChooser
    <*> traverse (faceQuantity c) maximumX
    <*> pure minimumX
    <*> traverse (faceAlternativeCost c) alternativeCosts
    <*> traverse (faceCostReduction c) costReductions
    <*> traverse (facePlayerStatic c) playerAbilities
    <*> traverse (faceBlockRequirement c) blockRequirements
    <*> traverse (faceBlockPermission c) blockPermissions
    <*> traverse (faceAttackRequirement c) attackRequirements
    <*> traverse (faceCombatRestriction c) combatRestrictions
    <*> traverse (faceSacrificeRestriction c) sacrificeRestrictions
    <*> traverse (faceUntapRestriction c) untapRestrictions
    <*> traverse (faceKeywordCostRestriction c) keywordCostRestrictions
    <*> traverse (faceTargetRestriction c) targetRestrictions
    <*> traverse (faceRulePermission c) rulePermissions
    <*> traverse (faceAttachRestriction c) attachRestrictions
    <*> traverse (faceEntryRestriction c) entryRestrictions
    <*> traverse (faceCounterRestriction c) counterRestrictions
    <*> traverse (faceActivationProhibition c) activationProhibitions
    <*> traverse (faceAttackCost c) attackCosts
    <*> traverse (faceBlockCost c) blockCosts
    <*> traverse (faceCounterLimit c) counterLimits
    <*> traverse (faceHandAction c) mulliganActions
    <*> traverse (faceHandAction c) openingHandActions
    <*> traverse (faceSpecialAction c) specialActions

-- | A card's children: each of its faces (CR 712.6), whole.
cardChildren :: (Applicative f) => (Face.Face Card.Card -> f (Face.Face Card.Card)) -> Card.Card -> f Card.Card
cardChildren onFace (Card.MkCard layout faces) = Card.MkCard layout <$> traverse onFace faces

-- | One visitor per kind of child a stored row holds: a replacement or
-- prevention effect in force (CR 614.1a, 615.1a), a delayed triggered ability
-- (CR 603.7a), a later payment's offer (CR 116.2c), a pending effect, and the
-- expiry each carries (CR 611.2a).
data StoredChildren f = MkStoredChildren
  { -- | A replacement or prevention effect in force (CR 614.1a, 615.1a), whole.
    storedReplacement :: ReplacementEffect.ReplacementEffect Card.Card (GrantedAbility.GrantedAbility Card.Card) (Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)) -> f (ReplacementEffect.ReplacementEffect Card.Card (GrantedAbility.GrantedAbility Card.Card) (Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card))),
    -- | A delayed triggered ability (CR 603.7a), whole.
    storedTriggered :: TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> f (TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)),
    -- | An effect a rider or a pending row runs (CR 608.2c), whole.
    storedEffect :: Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card) -> f (Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)),
    -- | The price a later payment asks (CR 116.2c), whole.
    storedCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | A "for as long as" condition (CR 611.2b) or a replacement's gate, whole.
    storedCondition :: Condition.Condition -> f Condition.Condition,
    -- | The event that ends an expiry (CR 611.2a), whole.
    storedDurationEvent :: DurationEvent.DurationEvent -> f DurationEvent.DurationEvent,
    -- | A delayed trigger's slot binding (CR 603.7c), whole.
    storedBinding :: Binding.Binding -> f Binding.Binding,
    -- | A chosen target or a shield's recipient (CR 601.2c), whole.
    storedRecipient :: Recipient.Recipient -> f Recipient.Recipient,
    -- | The shield a prevention row was spent from (CR 615.1a), whole.
    storedCandidate :: CandidateId.CandidateId -> f CandidateId.CandidateId,
    -- | A slot name: a row's environment, or the effect an expiry lasts until used.
    storedSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | An object id held outright: a source, a bound object (CR 400.7).
    storedObject :: ObjectId.ObjectId -> f ObjectId.ObjectId
  }

-- | Every child of a stored row left as it is: the base a walk overrides.
storedLeaves :: (Applicative f) => StoredChildren f
storedLeaves =
  MkStoredChildren
    { storedReplacement = pure,
      storedTriggered = pure,
      storedEffect = pure,
      storedCost = pure,
      storedCondition = pure,
      storedDurationEvent = pure,
      storedBinding = pure,
      storedRecipient = pure,
      storedCandidate = pure,
      storedSlot = pure,
      storedObject = pure
    }

-- | A replacement or prevention effect in force's children (CR 614.1a, 615.1a).
activeReplacementChildren :: (Applicative f) => StoredChildren f -> ActiveReplacement.ActiveReplacement -> f ActiveReplacement.ActiveReplacement
activeReplacementChildren c (ActiveReplacement.MkActiveReplacement effect source controller timestamp expiry uses origin condition rider slots choices) =
  ActiveReplacement.MkActiveReplacement
    <$> storedReplacement c effect
    <*> storedObject c source
    <*> pure controller
    <*> pure timestamp
    <*> expiryChildren c expiry
    <*> pure uses
    <*> pure origin
    <*> traverse (storedCondition c) condition
    <*> traverse (preventionRiderChildren c) rider
    <*> traverseSlots c (traverseSet (storedObject c)) slots
    <*> pure choices

-- | A delayed triggered ability's children (CR 603.7a).
delayedTriggerChildren :: (Applicative f) => StoredChildren f -> DelayedTrigger.DelayedTrigger -> f DelayedTrigger.DelayedTrigger
delayedTriggerChildren c (DelayedTrigger.MkDelayedTrigger ability source controller bindings delayed window expiry createdAt) =
  DelayedTrigger.MkDelayedTrigger
    <$> storedTriggered c ability
    <*> storedObject c source
    <*> pure controller
    <*> traverseSlots c (storedBinding c) bindings
    <*> traverse (storedTriggered c) delayed
    <*> pure window
    <*> traverse (expiryChildren c) expiry
    <*> pure createdAt

-- | A later payment's children (CR 116.2c): its price, its payoff and its expiry.
activeLaterPaymentChildren :: (Applicative f) => StoredChildren f -> ActiveLaterPayment.ActiveLaterPayment -> f ActiveLaterPayment.ActiveLaterPayment
activeLaterPaymentChildren c (ActiveLaterPayment.MkActiveLaterPayment cost payoff createdAt expiry) =
  ActiveLaterPayment.MkActiveLaterPayment
    <$> storedCost c cost
    <*> preventionRiderChildren c payoff
    <*> pure createdAt
    <*> expiryChildren c expiry

-- | A prevention row's children (CR 615.1a): its shield, the damage it stopped
-- per source and recipient, and its rider. Sources or recipients the visitors
-- map together merge, their amounts summed.
preventionChildren :: (Applicative f) => StoredChildren f -> Prevention.Prevention -> f Prevention.Prevention
preventionChildren c (Prevention.MkPrevention by amounts rider) =
  Prevention.MkPrevention
    <$> storedCandidate c by
    <*> fmap (Map.fromListWith (Map.unionWith (+))) (traverse (\(source, stopped) -> (,) <$> storedObject c source <*> fmap (Map.fromListWith (+)) (traverse (\(recipient, amount) -> (\r -> (r, amount)) <$> storedRecipient c recipient) (Map.toList stopped))) (Map.toList amounts))
    <*> traverse (preventionRiderChildren c) rider

-- | A prevention rider's children: what it runs, in its environment (CR 608.2c).
preventionRiderChildren :: (Applicative f) => StoredChildren f -> PreventionRider.PreventionRider -> f PreventionRider.PreventionRider
preventionRiderChildren c (PreventionRider.MkPreventionRider effects targets controller source) =
  PreventionRider.MkPreventionRider
    <$> traverse (storedEffect c) effects
    <*> traverseSlots c (traverseSet (storedRecipient c)) targets
    <*> pure controller
    <*> storedObject c source

-- | A pending entry effect's children: the entering object and what it runs.
pendingEntryEffectChildren :: (Applicative f) => StoredChildren f -> PendingEntryEffect.PendingEntryEffect -> f PendingEntryEffect.PendingEntryEffect
pendingEntryEffectChildren c (PendingEntryEffect.MkPendingEntryEffect entering controller effects) =
  PendingEntryEffect.MkPendingEntryEffect
    <$> storedObject c entering
    <*> pure controller
    <*> traverse (storedEffect c) effects

-- | A pending replacement effect's children: what it runs, in its environment (CR 608.2c).
pendingReplacementEffectChildren :: (Applicative f) => StoredChildren f -> PendingReplacementEffect.PendingReplacementEffect -> f PendingReplacementEffect.PendingReplacementEffect
pendingReplacementEffectChildren c (PendingReplacementEffect.MkPendingReplacementEffect effects targets controller source) =
  PendingReplacementEffect.MkPendingReplacementEffect
    <$> traverse (storedEffect c) effects
    <*> traverseSlots c (traverseSet (storedRecipient c)) targets
    <*> pure controller
    <*> storedObject c source

-- | An expiry's children (CR 611.2a), its payload matched positionally.
expiryChildren :: (Applicative f) => StoredChildren f -> Expiry.Expiry -> f Expiry.Expiry
expiryChildren c expiry = case expiry of
  Expiry.AtCleanup -> pure expiry
  Expiry.Never -> pure expiry
  Expiry.Perpetual -> pure expiry
  Expiry.While (While.MkWhile you condition) -> fmap Expiry.While (While.MkWhile you <$> storedCondition c condition)
  Expiry.AtTurnOf _whose -> pure expiry
  Expiry.AtStepOf _stepBegan -> pure expiry
  Expiry.AtStepOfControllerOf (StepOfControllerOf.MkStepOfControllerOf phase whose) -> fmap Expiry.AtStepOfControllerOf (StepOfControllerOf.MkStepOfControllerOf phase <$> storedObject c whose)
  Expiry.AtEndOfTurnOf _afterTurn -> pure expiry
  Expiry.DuringTurnOf _afterTurn -> pure expiry
  Expiry.DuringUntapStepOf _whose -> pure expiry
  Expiry.DuringTurnOfControllerOf (AfterObjectTurn.MkAfterObjectTurn whose turn) -> fmap Expiry.DuringTurnOfControllerOf (AfterObjectTurn.MkAfterObjectTurn <$> storedObject c whose <*> pure turn)
  Expiry.DuringExtraTurn _timestamp -> pure expiry
  Expiry.DuringAddedCombatPhase _behind -> pure expiry
  Expiry.AtEndOf _phaseSelector -> pure expiry
  Expiry.AtEndOfCombatOn _afterTurn -> pure expiry
  Expiry.WhenPaid (PaidExpiry.MkPaidExpiry payer cost resolution) -> fmap Expiry.WhenPaid (PaidExpiry.MkPaidExpiry payer <$> storedCost c cost <*> storedObject c resolution)
  Expiry.WhenUsed used -> Expiry.WhenUsed <$> traverse (storedSlot c) used
  Expiry.OnEvent (EventExpiry.MkEventExpiry controller event) -> fmap Expiry.OnEvent (EventExpiry.MkEventExpiry controller <$> storedDurationEvent c event)

-- A slot-keyed environment's keys through storedSlot and each value through
-- the given visitor.
traverseSlots :: (Applicative f) => StoredChildren f -> (a -> f a) -> Map.Map SlotName.SlotName a -> f (Map.Map SlotName.SlotName a)
traverseSlots c onValue = fmap Map.fromList . traverse (\(name, value) -> (,) <$> storedSlot c name <*> onValue value) . Map.toList

-- | A set's members, each through a visitor; members the visitor maps together merge.
traverseSet :: (Applicative f, Ord a) => (a -> f a) -> Set.Set a -> f (Set.Set a)
traverseSet f = fmap Set.fromList . traverse f . Set.toList

-- | One visitor per kind of child an object reference holds: the objects an
-- instruction acts on (CR 608.2c).
data ObjectRefChildren f = MkObjectRefChildren
  { -- | A Filter a field holds, judged of the objects it describes.
    refFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A Quantity a field holds: a count of cards, a walk's weight.
    refQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A slot the reference reads: a bound group, an attached-to object.
    refSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | A player a field names: whose library, who chooses.
    refPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | Whose zones a walk reaches, whole.
    refZones :: ZoneScope.ZoneScope -> f ZoneScope.ZoneScope,
    -- | Who chooses a card out of a graveyard (CR 608.2d), whole.
    refChooser :: Chooser.Chooser -> f Chooser.Chooser,
    -- | A nested reference: AtRandom's, AllOf's.
    refNested :: ObjectRef.ObjectRef -> f ObjectRef.ObjectRef,
    -- | The events the engine looks back over to resolve the reference.
    refLooksBack :: HistoryRead.HistoryRead -> f ()
  }

-- | Every child of an object reference left as it is: the base a walk overrides.
objectRefLeaves :: (Applicative f) => ObjectRefChildren f
objectRefLeaves =
  MkObjectRefChildren
    { refFilter = pure,
      refQuantity = pure,
      refSlot = pure,
      refPlayer = pure,
      refZones = pure,
      refChooser = pure,
      refNested = pure,
      refLooksBack = const (pure ())
    }

-- | An ObjectRef's immediate children, its payload matched positionally. A
-- nested reference is handed to refNested and not descended here.
objectRefChildren :: (Applicative f) => ObjectRefChildren f -> ObjectRef.ObjectRef -> f ObjectRef.ObjectRef
objectRefChildren c ref = case ref of
  ObjectRef.InSlot bound -> ObjectRef.InSlot <$> refSlot c bound
  ObjectRef.EachMatching filter_ -> ObjectRef.EachMatching <$> refFilter c filter_
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard graveyards filter_) -> fmap ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard <$> refZones c graveyards <*> refFilter c filter_)
  ObjectRef.EachCardInYourHand -> pure ref
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand hands filter_) -> fmap ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand <$> refZones c hands <*> traverse (refFilter c) filter_)
  ObjectRef.EachCardInYourLibrary filter_ -> ObjectRef.EachCardInYourLibrary <$> traverse (refFilter c) filter_
  ObjectRef.EachCardYouOwn filter_ -> ObjectRef.EachCardYouOwn <$> refFilter c filter_
  ObjectRef.EachCardInExiledPile (EachCardInExiledPile.MkEachCardInExiledPile pile filter_) -> fmap ObjectRef.EachCardInExiledPile (EachCardInExiledPile.MkEachCardInExiledPile pile <$> traverse (refFilter c) filter_)
  ObjectRef.EachCardEncodedOnSource filter_ -> ObjectRef.EachCardEncodedOnSource <$> traverse (refFilter c) filter_
  ObjectRef.EachSpell filter_ -> ObjectRef.EachSpell <$> refFilter c filter_
  ObjectRef.EachAbility filter_ -> ObjectRef.EachAbility <$> refFilter c filter_
  ObjectRef.EachOnStack filter_ -> ObjectRef.EachOnStack <$> refFilter c filter_
  ObjectRef.Players (PlayersWho.MkPlayersWho players who) -> fmap ObjectRef.Players (PlayersWho.MkPlayersWho <$> refPlayer c players <*> traverse (refFilter c) who)
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary seat count deck) -> fmap ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary <$> refPlayer c seat <*> refQuantity c count <*> pure deck)
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil seat filter_ weight count deck) -> fmap ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil <$> refPlayer c seat <*> refFilter c filter_ <*> refQuantity c weight <*> refQuantity c count <*> pure deck)
  ObjectRef.TopOfGraveyard seat -> ObjectRef.TopOfGraveyard <$> refPlayer c seat
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser players filter_ count) ->
    fmap ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard <$> refChooser c chooser <*> refZones c players <*> refFilter c filter_ <*> howManyChildren (refQuantity c) count)
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand seat filter_ count) -> fmap ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand <$> refPlayer c seat <*> refFilter c filter_ <*> howManyChildren (refQuantity c) count)
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong bound filter_ count total perColor chooser pileChooser faceUpPile selection) ->
    fmap
      ObjectRef.ChosenCardFromAmong
      ( ChosenCardFromAmong.MkChosenCardFromAmong
          <$> refSlot c bound
          <*> refFilter c filter_
          <*> howManyChildren (refQuantity c) count
          <*> traverse (\(TotalCap.MkTotalCap weight most) -> TotalCap.MkTotalCap <$> refQuantity c weight <*> refQuantity c most) total
          <*> traverse (refFilter c) perColor
          <*> refPlayer c chooser
          <*> traverse (refPlayer c) pileChooser
          <*> pure faceUpPile
          <*> pure selection
      )
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong bound filter_) -> fmap ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong <$> refSlot c bound <*> refFilter c filter_)
  ObjectRef.RandomCard (RandomCard.MkRandomCard zone seat filter_ count) -> fmap ObjectRef.RandomCard (RandomCard.MkRandomCard zone <$> refPlayer c seat <*> refFilter c filter_ <*> refQuantity c count)
  ObjectRef.RandomCardInGraveyard (RandomCardInGraveyard.MkRandomCardInGraveyard players filter_ count) -> fmap ObjectRef.RandomCardInGraveyard (RandomCardInGraveyard.MkRandomCardInGraveyard <$> refZones c players <*> refFilter c filter_ <*> refQuantity c count)
  ObjectRef.AtRandom inner -> ObjectRef.AtRandom <$> refNested c inner
  ObjectRef.ChosenPermanents choice -> ObjectRef.ChosenPermanents <$> chosenPermanentsChildren c choice
  ObjectRef.AttachedToBound (AttachedToBound.MkAttachedToBound bound filter_) -> fmap ObjectRef.AttachedToBound (AttachedToBound.MkAttachedToBound <$> refSlot c bound <*> refFilter c filter_)
  ObjectRef.AttackedBy bound -> ObjectRef.AttackedBy <$> refSlot c bound
  -- CR 400.7j: which object the slot's card became is read off the zone moves.
  ObjectRef.FromAnywhere (FromAnywhere.MkFromAnywhere bound among reach) -> refLooksBack c HistoryRead.Moves *> fmap ObjectRef.FromAnywhere (FromAnywhere.MkFromAnywhere <$> refSlot c bound <*> traverse (refSlot c) among <*> pure reach)
  ObjectRef.AllOf refs -> ObjectRef.AllOf <$> traverse (refNested c) refs

-- | A battlefield choice's children (CR 608.2d): what it picks among, who
-- picks, how many and within what total.
chosenPermanentsChildren :: (Applicative f) => ObjectRefChildren f -> ChosenPermanents.ChosenPermanents -> f ChosenPermanents.ChosenPermanents
chosenPermanentsChildren c (ChosenPermanents.MkChosenPermanents filter_ chooser count total zone) =
  ChosenPermanents.MkChosenPermanents
    <$> refFilter c filter_
    <*> refPlayer c chooser
    <*> howManyChildren (refQuantity c) count
    <*> traverse (\(TotalCap.MkTotalCap weight most) -> TotalCap.MkTotalCap <$> refQuantity c weight <*> refQuantity c most) total
    <*> pure zone

-- | One visitor per kind of child a rule-affecting row holds (CR 613.11): a
-- restriction on declaring attackers or blockers (CR 508.1c, 509.1b), a cost to
-- attack or block (CR 508.1h, 509.1d), and the other rows of a RuleAbilities.
data RuleChildren f = MkRuleChildren
  { -- | The objects a rule applies to.
    ruleAffected :: Affected.Affected -> f Affected.Affected,
    -- | A Filter a field holds: the creatures on the other side, the company,
    -- the attachers, the aimers.
    ruleFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | The gate a rule applies under, whole.
    ruleCondition :: Condition.Condition -> f Condition.Condition,
    -- | A Quantity a field holds: a counted share, a block amount.
    ruleQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A taxed creature's fixed share, whole.
    ruleCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | The kind of counter a counter restriction or limit names (CR 122.1), whole.
    ruleCounterKind :: CounterKind.CounterKind Keyword.Keyword -> f (CounterKind.CounterKind Keyword.Keyword)
  }

-- | Every child of a rule left as it is: the base a walk overrides.
ruleLeaves :: (Applicative f) => RuleChildren f
ruleLeaves =
  MkRuleChildren
    { ruleAffected = pure,
      ruleFilter = pure,
      ruleCondition = pure,
      ruleQuantity = pure,
      ruleCost = pure,
      ruleCounterKind = pure
    }

-- | A RuleAbilities' children (CR 613.11), row by row.
ruleAbilitiesChildren :: (Applicative f) => RuleChildren f -> RuleAbilities.RuleAbilities -> f RuleAbilities.RuleAbilities
ruleAbilitiesChildren c (RuleAbilities.MkRuleAbilities activationProhibitions attachRestrictions attackCosts rulePermissions attackRequirements blockCosts blockPermissions blockRequirements combatRestrictions counterRestrictions keywordCostRestrictions entryRestrictions sacrificeRestrictions targetRestrictions untapRestrictions counterLimits) =
  RuleAbilities.MkRuleAbilities
    <$> traverse (activationProhibitionChildren c) activationProhibitions
    <*> traverse (attachRestrictionChildren c) attachRestrictions
    <*> traverse (attackCostChildren c) attackCosts
    <*> traverse (rulePermissionChildren c) rulePermissions
    <*> traverse (attackRequirementChildren c) attackRequirements
    <*> traverse (blockCostChildren c) blockCosts
    <*> traverse (blockPermissionChildren c) blockPermissions
    <*> traverse (blockRequirementChildren c) blockRequirements
    <*> traverse (combatRestrictionChildren c) combatRestrictions
    <*> traverse (counterRestrictionChildren c) counterRestrictions
    <*> traverse (keywordCostRestrictionChildren c) keywordCostRestrictions
    <*> traverse (entryRestrictionChildren c) entryRestrictions
    <*> traverse (sacrificeRestrictionChildren c) sacrificeRestrictions
    <*> traverse (targetRestrictionChildren c) targetRestrictions
    <*> traverse (untapRestrictionChildren c) untapRestrictions
    <*> traverse (counterLimitChildren c) counterLimits

-- | An activation prohibition's children (CR 602.5). Its ability kind,
-- prohibited costs and ability name are left as they are.
activationProhibitionChildren :: (Applicative f) => RuleChildren f -> ActivationProhibition.ActivationProhibition -> f ActivationProhibition.ActivationProhibition
activationProhibitionChildren c (ActivationProhibition.MkActivationProhibition affected kind costs name) =
  ActivationProhibition.MkActivationProhibition <$> ruleAffected c affected <*> pure kind <*> pure costs <*> pure name

-- | An attach restriction's children.
attachRestrictionChildren :: (Applicative f) => RuleChildren f -> AttachRestriction.AttachRestriction -> f AttachRestriction.AttachRestriction
attachRestrictionChildren c (AttachRestriction.MkAttachRestriction affected attachers) =
  AttachRestriction.MkAttachRestriction <$> ruleAffected c affected <*> ruleFilter c attachers

-- | A rule permission's children. The rule it waives is left as it is.
rulePermissionChildren :: (Applicative f) => RuleChildren f -> RulePermission.RulePermission -> f RulePermission.RulePermission
rulePermissionChildren c (RulePermission.MkRulePermission affected while waives) =
  RulePermission.MkRulePermission <$> ruleAffected c affected <*> traverse (ruleCondition c) while <*> pure waives

-- | An attack requirement's children (CR 508.1d). The defender it names and
-- its arity are left as they are.
attackRequirementChildren :: (Applicative f) => RuleChildren f -> AttackRequirement.AttackRequirement -> f AttackRequirement.AttackRequirement
attackRequirementChildren c (AttackRequirement.MkAttackRequirement subject object_ while arity) =
  AttackRequirement.MkAttackRequirement <$> ruleAffected c subject <*> pure object_ <*> traverse (ruleCondition c) while <*> pure arity

-- | A block permission's children.
blockPermissionChildren :: (Applicative f) => RuleChildren f -> BlockPermission.BlockPermission -> f BlockPermission.BlockPermission
blockPermissionChildren c (BlockPermission.MkBlockPermission affected additional while) =
  BlockPermission.MkBlockPermission <$> ruleAffected c affected <*> traverse (ruleQuantity c) additional <*> traverse (ruleCondition c) while

-- | A block requirement's children (CR 509.1c). Its arity and blocker count are
-- left as they are.
blockRequirementChildren :: (Applicative f) => RuleChildren f -> BlockRequirement.BlockRequirement -> f BlockRequirement.BlockRequirement
blockRequirementChildren c (BlockRequirement.MkBlockRequirement subject attacker while arity blockers) =
  BlockRequirement.MkBlockRequirement <$> traverse (ruleAffected c) subject <*> traverse (ruleAffected c) attacker <*> traverse (ruleCondition c) while <*> pure arity <*> pure blockers

-- | A counter restriction's children (CR 122.1).
counterRestrictionChildren :: (Applicative f) => RuleChildren f -> CounterRestriction.CounterRestriction -> f CounterRestriction.CounterRestriction
counterRestrictionChildren c (CounterRestriction.MkCounterRestriction affected kind) =
  CounterRestriction.MkCounterRestriction <$> ruleAffected c affected <*> traverse (ruleCounterKind c) kind

-- | A counter limit's children (CR 122.4). Its N is left as it is.
counterLimitChildren :: (Applicative f) => RuleChildren f -> CounterLimit.CounterLimit -> f CounterLimit.CounterLimit
counterLimitChildren c (CounterLimit.MkCounterLimit kind atMost) =
  CounterLimit.MkCounterLimit <$> ruleCounterKind c kind <*> pure atMost

-- | A keyword cost restriction's children (CR 702.122d, 702.196b).
keywordCostRestrictionChildren :: (Applicative f) => RuleChildren f -> KeywordCostRestriction.KeywordCostRestriction -> f KeywordCostRestriction.KeywordCostRestriction
keywordCostRestrictionChildren c (KeywordCostRestriction.MkKeywordCostRestriction part affected) =
  KeywordCostRestriction.MkKeywordCostRestriction part <$> ruleAffected c affected

-- | An entry restriction's children. Its origin zones are left as they are.
entryRestrictionChildren :: (Applicative f) => RuleChildren f -> EntryRestriction.EntryRestriction -> f EntryRestriction.EntryRestriction
entryRestrictionChildren c (EntryRestriction.MkEntryRestriction affected origins) =
  EntryRestriction.MkEntryRestriction <$> ruleAffected c affected <*> pure origins

-- | A sacrifice restriction's children (CR 701.21).
sacrificeRestrictionChildren :: (Applicative f) => RuleChildren f -> SacrificeRestriction.SacrificeRestriction -> f SacrificeRestriction.SacrificeRestriction
sacrificeRestrictionChildren c (SacrificeRestriction.MkSacrificeRestriction affected) =
  SacrificeRestriction.MkSacrificeRestriction <$> ruleAffected c affected

-- | A target restriction's children. Its stack object kind is left as it is.
targetRestrictionChildren :: (Applicative f) => RuleChildren f -> TargetRestriction.TargetRestriction Affected.Affected -> f (TargetRestriction.TargetRestriction Affected.Affected)
targetRestrictionChildren c (TargetRestriction.MkTargetRestriction affected kind aimers) =
  TargetRestriction.MkTargetRestriction <$> ruleAffected c affected <*> pure kind <*> ruleFilter c aimers

-- | An untap restriction's children (CR 502.3). What it holds back is left as
-- it is.
untapRestrictionChildren :: (Applicative f) => RuleChildren f -> UntapRestriction.UntapRestriction -> f UntapRestriction.UntapRestriction
untapRestrictionChildren c (UntapRestriction.MkUntapRestriction affected hold while) =
  UntapRestriction.MkUntapRestriction <$> ruleAffected c affected <*> pure hold <*> traverse (ruleCondition c) while

-- | One visitor per kind of child an Affected holds.
data AffectedChildren f = MkAffectedChildren
  { -- | A frozen id set (CR 611.2c), whole.
    affectedObjects :: Set.Set ObjectId.ObjectId -> f (Set.Set ObjectId.ObjectId),
    -- | The Filter a matching arm holds.
    affectedFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | How long a permission's card stays covered (CR 611.3d), whole.
    affectedDuration :: Duration.Duration -> f Duration.Duration
  }

-- | An Affected's children.
affectedChildren :: (Applicative f) => AffectedChildren f -> Affected.Affected -> f Affected.Affected
affectedChildren c affected = case affected of
  Affected.TheseObjects objects -> Affected.TheseObjects <$> affectedObjects c objects
  Affected.Matching filter_ -> Affected.Matching <$> affectedFilter c filter_
  Affected.MatchingAnywhere filter_ -> Affected.MatchingAnywhere <$> affectedFilter c filter_
  Affected.MatchingOffBattlefield filter_ -> Affected.MatchingOffBattlefield <$> affectedFilter c filter_
  Affected.Attached -> pure affected
  Affected.AttachedPlayerControls filter_ -> Affected.AttachedPlayerControls <$> affectedFilter c filter_
  Affected.PlayedThisWay duration -> Affected.PlayedThisWay <$> affectedDuration c duration

-- | One visitor per kind of child a Modification holds (CR 613.1).
data ModificationChildren ability f = MkModificationChildren
  { -- | A keyword gained or lost (CR 613.1f), whole.
    modificationKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | A keyword a backup grant carries, stamped from its source as the
    -- ability was put on the stack (CR 702.165d), whole.
    modificationStampedKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | An enchant ability's target slot, gained or lost (CR 702.5a), whole.
    modificationTargetSlot :: TargetSlot.TargetSlot -> f TargetSlot.TargetSlot,
    -- | A permission to be cast (CR 601.3), whole.
    modificationCastingPermission :: CastingPermission.CastingPermission -> f CastingPermission.CastingPermission,
    -- | A quoted ability granted, alone or with a full text (CR 612.6).
    modificationAbility :: ability -> f ability,
    -- | A Filter a grant or a name change holds.
    modificationFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A restriction on activating a lent card's abilities, whole.
    modificationRestriction :: ActivationRestriction.ActivationRestriction -> f ActivationRestriction.ActivationRestriction,
    -- | The name of the abilities an object loses.
    modificationAbilityName :: AbilityName.AbilityName -> f AbilityName.AbilityName,
    -- | A Quantity a power, toughness or intensity change holds.
    modificationQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | The player whose graveyard a full text is read from (CR 612.6).
    modificationPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | The land type an object becomes or gains (CR 305.7).
    modificationLandSubtype :: Subtype.Subtype -> f Subtype.Subtype,
    -- | The creature type an object becomes or gains (CR 205.1a, 205.1b).
    modificationCreatureSubtype :: Subtype.Subtype -> f Subtype.Subtype,
    -- | The colours an object becomes or gains (CR 105.3), whole.
    modificationColors :: Set.Set Color.Color -> f (Set.Set Color.Color)
  }

-- | Every child of a Modification left as it is: the base a walk overrides.
modificationLeaves :: (Applicative f) => ModificationChildren ability f
modificationLeaves =
  MkModificationChildren
    { modificationKeyword = pure,
      modificationStampedKeyword = pure,
      modificationTargetSlot = pure,
      modificationCastingPermission = pure,
      modificationAbility = pure,
      modificationFilter = pure,
      modificationRestriction = pure,
      modificationAbilityName = pure,
      modificationQuantity = pure,
      modificationPlayer = pure,
      modificationLandSubtype = pure,
      modificationCreatureSubtype = pure,
      modificationColors = pure
    }

-- | A designator's written keyword, as modificationKeyword walks it.
modificationDesignator :: (Applicative f) => ModificationChildren ability f -> KeywordDesignator.KeywordDesignator Keyword.Keyword -> f (KeywordDesignator.KeywordDesignator Keyword.Keyword)
modificationDesignator c designator = case designator of
  KeywordDesignator.OfKeyword k -> KeywordDesignator.OfKeyword <$> modificationKeyword c k
  KeywordDesignator.OfInstance k n -> (`KeywordDesignator.OfInstance` n) <$> modificationKeyword c k
  KeywordDesignator.OfFamily _ -> pure designator
  KeywordDesignator.Printed _ -> pure designator

-- | A Modification's children (CR 613.1). Its change modes, card types,
-- supertypes, unfamilied subtypes, stored word change, names, controllers,
-- keyword families and combat damage markers are left as they are; a cost
-- keyword's only child is a beam me up's unreturnable Filter.
modificationChildren :: (Applicative f) => ModificationChildren ability f -> Modification.Modification ability -> f (Modification.Modification ability)
modificationChildren c modification = case modification of
  Modification.GainKeyword gained -> Modification.GainKeyword <$> modificationKeyword c gained
  Modification.GainKeywordAtManaCost costKeyword ->
    Modification.GainKeywordAtManaCost <$> case costKeyword of
      CostKeyword.Flashback -> pure costKeyword
      CostKeyword.Scavenge -> pure costKeyword
      CostKeyword.Unearth -> pure costKeyword
      CostKeyword.Embalm -> pure costKeyword
      CostKeyword.Encore -> pure costKeyword
      CostKeyword.Disguise -> pure costKeyword
      CostKeyword.Blitz -> pure costKeyword
      CostKeyword.Harmonize -> pure costKeyword
      CostKeyword.Mayhem -> pure costKeyword
      CostKeyword.Replicate -> pure costKeyword
      CostKeyword.Emerge -> pure costKeyword
      CostKeyword.BeamMeUp unreturnable -> CostKeyword.BeamMeUp <$> traverse (modificationFilter c) unreturnable
  Modification.GainEnchant targetSlot -> Modification.GainEnchant <$> modificationTargetSlot c targetSlot
  Modification.LoseEnchant targetSlot -> Modification.LoseEnchant <$> modificationTargetSlot c targetSlot
  Modification.GainCastingPermission permission -> Modification.GainCastingPermission <$> modificationCastingPermission c permission
  Modification.GainAbility ability -> Modification.GainAbility <$> modificationAbility c ability
  Modification.GainAbilitiesOfSource keywords -> Modification.GainAbilitiesOfSource . Set.fromList <$> traverse (modificationStampedKeyword c) (Set.toList keywords)
  Modification.GainKeywordsAmong designator zone among ->
    (\d -> Modification.GainKeywordsAmong d zone) <$> modificationDesignator c designator <*> modificationFilter c among
  Modification.GainCardAbilities (CardAbilities.MkCardAbilities from cards triggered restrictions) ->
    fmap Modification.GainCardAbilities (CardAbilities.MkCardAbilities from <$> modificationFilter c cards <*> pure triggered <*> traverse (modificationRestriction c) restrictions)
  Modification.GainAbilitiesOfStickers -> pure modification
  Modification.LoseAllAbilities -> pure modification
  Modification.LoseNamedAbility name -> Modification.LoseNamedAbility <$> modificationAbilityName c name
  Modification.LoseKeyword lost -> Modification.LoseKeyword <$> modificationKeyword c lost
  Modification.LoseKeywordFamily _ -> pure modification
  Modification.CantHaveKeyword designator -> Modification.CantHaveKeyword <$> modificationDesignator c designator
  Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness power toughness) ->
    fmap Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness <$> traverse (modificationQuantity c) power <*> traverse (modificationQuantity c) toughness)
  Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness power toughness) ->
    fmap Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness <$> modificationQuantity c power <*> modificationQuantity c toughness)
  Modification.LandSubtype mode subtype -> Modification.LandSubtype mode <$> modificationLandSubtype c subtype
  Modification.ChosenSubtype {} -> pure modification
  Modification.CreatureSubtype mode subtype -> Modification.CreatureSubtype mode <$> modificationCreatureSubtype c subtype
  Modification.AddEveryCreatureSubtype -> pure modification
  Modification.LoseEverySubtype _ -> pure modification
  Modification.SetCreatureSubtypesOfLastCardExiledWith filter_ -> Modification.SetCreatureSubtypesOfLastCardExiledWith <$> modificationFilter c filter_
  Modification.Subtype {} -> pure modification
  Modification.RemoveSubtype _ -> pure modification
  Modification.CardType {} -> pure modification
  Modification.LoseCardType _ -> pure modification
  Modification.AddSupertype _ -> pure modification
  Modification.RemoveSupertype _ -> pure modification
  Modification.ChangeWord _ -> pure modification
  Modification.ExchangeTextBoxes -> pure modification
  Modification.AddNamesMatching filter_ -> Modification.AddNamesMatching <$> modificationFilter c filter_
  Modification.HasFullText (FullText.MkFullText graveyard alsoHas) ->
    fmap Modification.HasFullText (FullText.MkFullText <$> modificationPlayer c graveyard <*> traverse (modificationAbility c) alsoHas)
  Modification.SetName _ -> pure modification
  Modification.InsertNameWords _ -> pure modification
  Modification.SetController _ -> pure modification
  Modification.SetControllerToSource -> pure modification
  Modification.Colors mode colors -> Modification.Colors mode <$> modificationColors c colors
  Modification.ChosenColor _ -> pure modification
  Modification.SwitchPowerToughness -> pure modification
  Modification.AssignCombatDamage _ -> pure modification
  Modification.MayAssignCombatDamage _ -> pure modification
  Modification.GrantsStationToughness -> pure modification
  Modification.LoyaltyIsToughness -> pure modification
  Modification.Intensify quantity -> Modification.Intensify <$> modificationQuantity c quantity

-- | A combat restriction's children (CR 508.1c, 509.1b). Its name, limit,
-- comparison, company count and defenders are left as they are.
combatRestrictionChildren :: (Applicative f) => RuleChildren f -> CombatRestriction.CombatRestriction -> f CombatRestriction.CombatRestriction
combatRestrictionChildren c restriction = case restriction of
  CombatRestriction.CantAttack x -> CombatRestriction.CantAttack <$> affectedUnlessChildren x
  CombatRestriction.CantBlock x -> CombatRestriction.CantBlock <$> affectedUnlessChildren x
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy affected blockers frame unless name) ->
    fmap CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy <$> ruleAffected c affected <*> ruleFilter c blockers <*> pure frame <*> traverse (ruleCondition c) unless <*> pure name)
  CombatRestriction.CantBlockCreatures (CantBlockCreatures.MkCantBlockCreatures affected attackers unless name) ->
    fmap CombatRestriction.CantBlockCreatures (CantBlockCreatures.MkCantBlockCreatures <$> ruleAffected c affected <*> ruleFilter c attackers <*> traverse (ruleCondition c) unless <*> pure name)
  CombatRestriction.CantAttackPlayer (CantAttackPlayer.MkCantAttackPlayer affected defenders kinds unless name) ->
    fmap CombatRestriction.CantAttackPlayer (CantAttackPlayer.MkCantAttackPlayer <$> ruleAffected c affected <*> pure defenders <*> pure kinds <*> traverse (ruleCondition c) unless <*> pure name)
  CombatRestriction.CantAttackAlone x -> CombatRestriction.CantAttackAlone <$> aloneUnlessChildren x
  CombatRestriction.CantAttackMoreThan (AttackLimitUnless.MkAttackLimitUnless limit defenders unless) ->
    fmap CombatRestriction.CantAttackMoreThan (AttackLimitUnless.MkAttackLimitUnless limit defenders <$> traverse (ruleCondition c) unless)
  CombatRestriction.CantBlockAlone x -> CombatRestriction.CantBlockAlone <$> aloneUnlessChildren x
  CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless limit comparison attackers unless) ->
    fmap CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless limit comparison <$> traverse (ruleAffected c) attackers <*> traverse (ruleCondition c) unless)
  where
    affectedUnlessChildren (AffectedUnless.MkAffectedUnless affected unless name amount) =
      AffectedUnless.MkAffectedUnless <$> ruleAffected c affected <*> traverse (ruleCondition c) unless <*> pure name <*> traverse (ruleQuantity c) amount
    aloneUnlessChildren (AloneUnless.MkAloneUnless affected alongside others unless name) =
      AloneUnless.MkAloneUnless <$> ruleAffected c affected <*> traverse (ruleFilter c) alongside <*> pure others <*> traverse (ruleCondition c) unless <*> pure name

-- | A cost to attack's children (CR 508.1h). Its scope is left as it is.
attackCostChildren :: (Applicative f) => RuleChildren f -> AttackCost.AttackCost -> f AttackCost.AttackCost
attackCostChildren c (AttackCost.MkAttackCost subject perAttacker scope_ while) =
  AttackCost.MkAttackCost <$> ruleAffected c subject <*> perCreatureChildren c perAttacker <*> pure scope_ <*> traverse (ruleCondition c) while

-- | A cost to block's children (CR 509.1d).
blockCostChildren :: (Applicative f) => RuleChildren f -> BlockCost.BlockCost -> f BlockCost.BlockCost
blockCostChildren c (BlockCost.MkBlockCost subject perBlocker attackers while) =
  BlockCost.MkBlockCost <$> ruleAffected c subject <*> perCreatureChildren c perBlocker <*> traverse (ruleFilter c) attackers <*> traverse (ruleCondition c) while

-- | One taxed creature's share of a cost to attack or block (CR 508.1h, 509.1d).
perCreatureChildren :: (Applicative f) => RuleChildren f -> PerCreature.PerCreature -> f PerCreature.PerCreature
perCreatureChildren c share = case share of
  PerCreature.Fixed cost -> PerCreature.Fixed <$> ruleCost c cost
  PerCreature.Counted quantity -> PerCreature.Counted <$> ruleQuantity c quantity

-- | One visitor per kind of child a mana payload holds: an addition (CR
-- 106.1), its spending restriction and rider (CR 106.6), and a clause spending
-- mana as though it were another type (CR 609.4b).
data ManaChildren f = MkManaChildren
  { -- | A Filter a field holds: a restriction's spells, a rider's objects, the
    -- permanents a type could come from (CR 106.7).
    manaCriterion :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | How much mana an addition adds.
    manaQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A player a field names: who adds the mana, whose pool is counted.
    manaPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | The slot a production reads: the permanent tapped for mana (CR 106.12a).
    manaSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | The keyword whose activations a restriction admits, whole.
    manaDesignator :: KeywordDesignator.KeywordDesignator Keyword.Keyword -> f (KeywordDesignator.KeywordDesignator Keyword.Keyword),
    -- | The mana a spend-as-though clause speaks about (CR 609.4b), whole.
    manaSpent :: ManaFilter.ManaFilter -> f ManaFilter.ManaFilter,
    -- | The types a spend-as-though clause lets that mana pay as (CR 609.4b), whole.
    manaAsThough :: Set.Set ManaType.ManaType -> f (Set.Set ManaType.ManaType),
    -- | The keyword a rider grants the spell its mana paid for (CR 106.6).
    manaKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | How long a rider's grant lasts (CR 611.2).
    manaDuration :: Duration.Duration -> f Duration.Duration
  }

-- | Every child of a mana payload left as it is: the base a walk overrides.
manaLeaves :: (Applicative f) => ManaChildren f
manaLeaves =
  MkManaChildren
    { manaCriterion = pure,
      manaQuantity = pure,
      manaPlayer = pure,
      manaSlot = pure,
      manaDesignator = pure,
      manaSpent = pure,
      manaAsThough = pure,
      manaKeyword = pure,
      manaDuration = pure
    }

-- | A mana addition's children (CR 106.1). Its retention is left as it is.
manaAdditionChildren :: (Applicative f) => ManaChildren f -> ManaAddition.ManaAddition -> f ManaAddition.ManaAddition
manaAdditionChildren c (ManaAddition.MkManaAddition adder production count retention restriction rider whenSpent choosesColor) =
  ManaAddition.MkManaAddition
    <$> manaPlayer c adder
    <*> manaProductionChildren c production
    <*> manaQuantity c count
    <*> pure retention
    <*> traverse (manaRestrictionChildren c) restriction
    <*> traverse (manaRiderChildren c) rider
    <*> traverse (\(WhenSpent.MkWhenSpent casts ability) -> WhenSpent.MkWhenSpent <$> manaCriterion c casts <*> pure ability) whenSpent
    <*> pure choosesColor

-- | A mana production's children. The types a permanent could produce (CR
-- 106.7) and the counted pool's types (CR 701.10f) are left as they are.
manaProductionChildren :: (Applicative f) => ManaChildren f -> ManaProduction.ManaProduction -> f ManaProduction.ManaProduction
manaProductionChildren c production = case production of
  ManaProduction.OfType _ -> pure production
  ManaProduction.OneOf _ -> pure production
  ManaProduction.AnyColor _ -> pure production
  ManaProduction.Chosen -> pure production
  ManaProduction.SnowSymbol -> pure production
  ManaProduction.Noted -> pure production
  ManaProduction.CouldProduce (CouldProduce.MkCouldProduce among types) -> fmap ManaProduction.CouldProduce (CouldProduce.MkCouldProduce <$> manaCriterion c among <*> pure types)
  ManaProduction.Pooled (ManaCount.MkManaCount whose filter_) -> fmap ManaProduction.Pooled (ManaCount.MkManaCount <$> manaPlayer c whose <*> pure filter_)
  ManaProduction.ProducedByTapped -> production <$ manaSlot c Binding.Engine.manaSource
  ManaProduction.InCommanderIdentity -> pure production
  ManaProduction.Circled -> pure production
  ManaProduction.DraftNoted _ -> pure production
  ManaProduction.EachColorAmong among -> fmap ManaProduction.EachColorAmong (manaCriterion c among)

-- | A spending restriction's children (CR 106.6). Whether it prohibits and
-- whether it pays generic costs are left as they are.
manaRestrictionChildren :: (Applicative f) => ManaChildren f -> ManaRestriction.ManaRestriction -> f ManaRestriction.ManaRestriction
manaRestrictionChildren c (ManaRestriction.MkManaRestriction casts activations keywordActivations unlocks turnsFaceUp prohibits paysGeneric) =
  ManaRestriction.MkManaRestriction
    <$> traverse (manaCriterion c) casts
    <*> traverse (manaCriterion c) activations
    <*> traverse (manaDesignator c) keywordActivations
    <*> traverse (manaCriterion c) unlocks
    <*> traverse (manaCriterion c) turnsFaceUp
    <*> pure prohibits
    <*> pure paysGeneric

-- | A mana rider's children (CR 106.6).
manaRiderChildren :: (Applicative f) => ManaChildren f -> ManaRider.ManaRider -> f ManaRider.ManaRider
manaRiderChildren c (ManaRider.MkManaRider condition effect) =
  ManaRider.MkManaRider <$> manaCriterion c condition <*> manaRiderEffectChildren c effect

-- | A mana rider's payload's children (CR 106.6).
manaRiderEffectChildren :: (Applicative f) => ManaChildren f -> ManaRiderEffect.ManaRiderEffect -> f ManaRiderEffect.ManaRiderEffect
manaRiderEffectChildren c effect = case effect of
  ManaRiderEffect.CantBeCountered -> pure effect
  ManaRiderEffect.GainsKeyword granted lasting -> ManaRiderEffect.GainsKeyword <$> manaKeyword c granted <*> manaDuration c lasting

-- | A spend-as-though clause's children (CR 609.4b). Whether it is the only
-- way that mana pays is left as it is.
spendManaAsThoughChildren :: (Applicative f) => ManaChildren f -> SpendManaAsThough.SpendManaAsThough -> f SpendManaAsThough.SpendManaAsThough
spendManaAsThoughChildren c (SpendManaAsThough.MkSpendManaAsThough which asThough only payments) =
  SpendManaAsThough.MkSpendManaAsThough
    <$> manaSpent c which
    <*> manaAsThough c asThough
    <*> pure only
    <*> spendScopeChildren c payments

-- | A spend-as-though clause's scope's children (CR 609.4b).
spendScopeChildren :: (Applicative f) => ManaChildren f -> SpendScope.SpendScope -> f SpendScope.SpendScope
spendScopeChildren c payments = case payments of
  SpendScope.EveryPayment -> pure payments
  SpendScope.Casting spell -> SpendScope.Casting <$> manaCriterion c spell
  SpendScope.Activating source -> SpendScope.Activating <$> manaCriterion c source

-- | One visitor per kind of child a damage pattern holds: the damage a
-- prevention or replacement effect matches (CR 614.1a, 615.1a).
data DamagePatternChildren f = MkDamagePatternChildren
  { -- | A Filter a field holds: the source's, the recipient's.
    damageFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | The one recipient the damage is dealt to, whole.
    damageRecipient :: Recipient.Recipient -> f Recipient.Recipient,
    -- | The one source the damage is dealt by (CR 400.7).
    damageObject :: ObjectId.ObjectId -> f ObjectId.ObjectId,
    -- | The slot the recipient is bound to.
    damageSlot :: SlotName.SlotName -> f SlotName.SlotName
  }

-- | A damage pattern's children. Its damage kind and the recipient's relation
-- to the controller are left as they are.
damagePatternChildren :: (Applicative f) => DamagePatternChildren f -> DamagePattern.DamagePattern -> f DamagePattern.DamagePattern
damagePatternChildren c (DamagePattern.MkDamagePattern whichKind whatSource whatRecipient whoRecipient whichRecipient whichSource boundRecipient) =
  DamagePattern.MkDamagePattern whichKind
    <$> damageFilter c whatSource
    <*> traverse (damageFilter c) whatRecipient
    <*> pure whoRecipient
    <*> traverse (damageRecipient c) whichRecipient
    <*> traverse (damageObject c) whichSource
    <*> traverse (damageSlot c) boundRecipient

-- | One visitor per kind of child a copy or entry payload holds: a copy's
-- "enter as a copy" rewrite (CR 707.5, 614.1c), its CR 707.9 exceptions, an
-- entry's riders (CR 614.1c) and a face-down listing (CR 708.2).
data CopyChildren ability effect f = MkCopyChildren
  { -- | A Filter a field holds: the objects a copy may copy, a chosen host
    -- (CR 303.4f).
    copyFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A Quantity a field holds: a counter amount (CR 614.1c).
    copyQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | The kind of counter a row places (CR 122.1).
    copyCounterKind :: CounterKind.CounterKind Keyword.Keyword -> f (CounterKind.CounterKind Keyword.Keyword),
    -- | A keyword a copy gains (CR 707.9a).
    copyKeyword :: Keyword.Keyword -> f Keyword.Keyword,
    -- | A subtype a copy exception names (CR 707.9b).
    copySubtype :: Subtype.Subtype -> f Subtype.Subtype,
    -- | A colour a copy exception names (CR 707.9b).
    copyColor :: Color.Color -> f Color.Color,
    -- | A quoted ability a copy gains (CR 707.9a) or a listing grants.
    copyAbility :: ability -> f ability,
    -- | An effect a copy's "when you do" holds (CR 707.9g).
    copyEffect :: effect -> f effect,
    -- | A copy clause's condition (CR 707.9f).
    copyCondition :: Condition.Condition -> f Condition.Condition,
    -- | How long a copy lasts.
    copyDuration :: Duration.Duration -> f Duration.Duration,
    -- | A slot an entry reads: what it attacks or blocks, its host.
    copySlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | The player an entry puts it under (CR 110.2a).
    copyPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | A characteristic an entry sets (CR 611.2e), whole.
    copyModification :: Modification.Modification ability -> f (Modification.Modification ability),
    -- | A face-down listing's type line (CR 708.2), whole.
    copyTypeLine :: TypeLine.TypeLine -> f TypeLine.TypeLine,
    -- | A face-down listing's power.
    copyPower :: Power.Power -> f Power.Power,
    -- | A face-down listing's toughness.
    copyToughness :: Toughness.Toughness -> f Toughness.Toughness,
    -- | A keyword a face-down listing grants.
    copyListedKeyword :: Keyword.Keyword -> f Keyword.Keyword
  }

-- | Every child of a copy or entry payload left as it is: the base a walk overrides.
copyLeaves :: (Applicative f) => CopyChildren ability effect f
copyLeaves =
  MkCopyChildren
    { copyFilter = pure,
      copyQuantity = pure,
      copyCounterKind = pure,
      copyKeyword = pure,
      copySubtype = pure,
      copyColor = pure,
      copyAbility = pure,
      copyEffect = pure,
      copyCondition = pure,
      copyDuration = pure,
      copySlot = pure,
      copyPlayer = pure,
      copyModification = pure,
      copyTypeLine = pure,
      copyPower = pure,
      copyToughness = pure,
      copyListedKeyword = pure
    }

-- | A copy rewrite's children (CR 707.5). Whether it enters tapped, what it
-- copies among, who receives it and whether it is optional are left as they
-- are.
asCopyChildren :: (Applicative f) => CopyChildren ability effect f -> AsCopy.AsCopy ability effect -> f (AsCopy.AsCopy ability effect)
asCopyChildren c (AsCopy.MkAsCopy eligible clauses tapped whenYouDo among duration recipient optional) =
  AsCopy.MkAsCopy
    <$> copyFilter c eligible
    <*> traverse (copyClauseChildren c) clauses
    <*> pure tapped
    <*> traverse (copyEffect c) whenYouDo
    <*> pure among
    <*> traverse (copyDuration c) duration
    <*> pure recipient
    <*> pure optional

-- | One clause of a copy's exceptions: its condition (CR 707.9f), its
-- exceptions and its counters (CR 707.9e).
copyClauseChildren :: (Applicative f) => CopyChildren ability effect f -> CopyClause.CopyClause ability -> f (CopyClause.CopyClause ability)
copyClauseChildren c (CopyClause.MkCopyClause condition exceptions counters) =
  CopyClause.MkCopyClause
    <$> traverse (copyCondition c) condition
    <*> traverse (copyExceptionChildren c) exceptions
    <*> traverse (withCountersChildren c) counters

-- | A CR 707.9 exception's children. A power and toughness, card types,
-- supertypes and a name are left as they are.
copyExceptionChildren :: (Applicative f) => CopyChildren ability effect f -> CopyException.CopyException ability -> f (CopyException.CopyException ability)
copyExceptionChildren c exception = case exception of
  CopyException.SetPowerToughness _ -> pure exception
  CopyException.GainKeywords keywords -> CopyException.GainKeywords <$> traverseSet (copyKeyword c) keywords
  CopyException.GainThisAbility -> pure exception
  CopyException.CardTypes {} -> pure exception
  CopyException.AddSubtypes subtypes -> CopyException.AddSubtypes <$> traverseSet (copySubtype c) subtypes
  CopyException.AddSupertypes _ -> pure exception
  CopyException.SetCreatureSubtypes subtypes -> CopyException.SetCreatureSubtypes <$> traverseSet (copySubtype c) subtypes
  CopyException.RemoveSupertypes _ -> pure exception
  CopyException.SetName _ -> pure exception
  CopyException.SetColors colors -> CopyException.SetColors <$> traverseSet (copyColor c) colors
  CopyException.NoManaCost -> pure exception
  CopyException.GainAbility ability -> CopyException.GainAbility <$> copyAbility c ability
  CopyException.DontCopyColors -> pure exception

-- | A counter row's children (CR 614.1c): each kind and its amount.
withCountersChildren :: (Applicative f) => CopyChildren ability effect f -> WithCounters.WithCounters -> f WithCounters.WithCounters
withCountersChildren c (WithCounters.MkWithCounters counters) = WithCounters.MkWithCounters <$> traverseCounters c counters

-- | Each kind of counter in a row and its amount. Two kinds a walk maps onto
-- one merge into one amount, their sum (CR 208.2), rather than keeping an
-- arbitrary survivor: counters with the same name are interchangeable (CR
-- 122.1), so the two rows are one tally. The sum stays unevaluated, so a Count
-- in it is still read against the board the entry happens on.
traverseCounters :: (Applicative f) => CopyChildren ability effect f -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Quantity.Quantity -> f (Map.Map (CounterKind.CounterKind Keyword.Keyword) Quantity.Quantity)
traverseCounters c =
  fmap (Map.fromListWith (\greater lesser -> Quantity.Arithmetic (Arithmetic.Plus (Plus.MkPlus lesser greater))))
    . traverse (\(kind, amount) -> (,) <$> copyCounterKind c kind <*> copyQuantity c amount)
    . Map.toList

-- | An entry's riders (CR 614.1c). Whether it enters tapped or transformed,
-- exiled face down or noted are left as they are.
entryRidersChildren :: (Applicative f) => CopyChildren ability effect f -> EntryRiders.EntryRiders Quantity.Quantity ability -> f (EntryRiders.EntryRiders Quantity.Quantity ability)
entryRidersChildren c (EntryRiders.MkEntryRiders tapped attacking blocking transformed counters under exiledFaceDown attachedTo faceDown noted characteristics) =
  EntryRiders.MkEntryRiders tapped
    <$> traverse attack attacking
    <*> traverse block blocking
    <*> pure transformed
    <*> traverseCounters c counters
    <*> controller under
    <*> pure exiledFaceDown
    <*> traverse host attachedTo
    <*> traverse (faceDownStateChildren c) faceDown
    <*> pure noted
    <*> traverse (copyModification c) characteristics
  where
    attack a = case a of
      EntryAttack.Chosen -> pure a
      EntryAttack.SameAs name -> EntryAttack.SameAs <$> copySlot c name
      EntryAttack.UnderPlayer name -> EntryAttack.UnderPlayer <$> copySlot c name
      EntryAttack.ThatPlayer name -> EntryAttack.ThatPlayer <$> copySlot c name
    block b = case b of
      EntryBlock.Chosen -> pure b
      EntryBlock.Specified name -> EntryBlock.Specified <$> copySlot c name
    controller u = case u of
      EntryController.Instructed who -> EntryController.Instructed <$> copyPlayer c who
      EntryController.Owner -> pure u
    host h = case h of
      EntryHost.Specified name -> EntryHost.Specified <$> copySlot c name
      EntryHost.Chosen selection filter_ -> EntryHost.Chosen selection <$> copyFilter c filter_

-- | A face-down state's listing (CR 708.2). Why it is face down and the events
-- turning it up are left as they are.
faceDownStateChildren :: (Applicative f) => CopyChildren ability effect f -> FaceDownState.FaceDownState ability -> f (FaceDownState.FaceDownState ability)
faceDownStateChildren c (FaceDownState.MkFaceDownState reason listed turnsUpBefore) =
  FaceDownState.MkFaceDownState reason <$> faceDownChildren c listed <*> pure turnsUpBefore

-- | A face-down listing's children (CR 708.2).
faceDownChildren :: (Applicative f) => CopyChildren ability effect f -> FaceDownCharacteristics.FaceDownCharacteristics ability -> f (FaceDownCharacteristics.FaceDownCharacteristics ability)
faceDownChildren c (FaceDownCharacteristics.MkFaceDownCharacteristics typeLine power toughness keywords abilities) =
  FaceDownCharacteristics.MkFaceDownCharacteristics
    <$> copyTypeLine c typeLine
    <*> traverse (copyPower c) power
    <*> traverse (copyToughness c) toughness
    <*> traverseSet (copyListedKeyword c) keywords
    <*> traverse (copyAbility c) abilities

-- | A characteristic-defining power and toughness's two Quantities (CR 208.2a).
characteristicPTChildren :: (Applicative f) => (Quantity.Quantity -> f Quantity.Quantity) -> CharacteristicPT.CharacteristicPT -> f CharacteristicPT.CharacteristicPT
characteristicPTChildren visit (CharacteristicPT.MkCharacteristicPT power toughness) =
  CharacteristicPT.MkCharacteristicPT <$> visit power <*> visit toughness

-- | One visitor per kind of child a target slot (CR 115.1, 601.2c), a casting
-- permission (CR 601.3) or an activation restriction (CR 602.5) holds.
data PermissionChildren f = MkPermissionChildren
  { -- | A Filter a slot holds: its targets', a spell's "as cast" one.
    permissionFilter :: Filter.Filter Keyword.Keyword -> f (Filter.Filter Keyword.Keyword),
    -- | A Quantity a slot holds: a computed count (CR 601.2c), an amount, a
    -- division (CR 601.2d).
    permissionQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | The player who chooses a slot's targets.
    permissionPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | Whose graveyard a slot's pool reaches, whole.
    permissionZones :: ZoneScope.ZoneScope -> f ZoneScope.ZoneScope,
    -- | The slot naming each player a per-player slot is announced for.
    permissionSlot :: SlotName.SlotName -> f SlotName.SlotName,
    -- | A permission's or a restriction's condition, whole.
    permissionCondition :: Condition.Condition -> f Condition.Condition,
    -- | A cost a graveyard cast pays on top (CR 118.8), whole.
    permissionCost :: CostComponent.CostComponent Keyword.Keyword -> f (CostComponent.CostComponent Keyword.Keyword),
    -- | The turns a restriction admits (CR 102.1), whole.
    permissionTurn :: TurnScope.TurnScope -> f TurnScope.TurnScope,
    -- | The mana a restriction lets pay for the activation (CR 602.2b), whole.
    permissionMana :: ManaSpecification.ManaSpecification -> f ManaSpecification.ManaSpecification,
    -- | The events the engine looks back over to judge a restriction.
    permissionLooksBack :: HistoryRead.HistoryRead -> f ()
  }

-- | Every child of a target slot, casting permission or activation
-- restriction left as it is: the base a walk overrides.
permissionLeaves :: (Applicative f) => PermissionChildren f
permissionLeaves =
  MkPermissionChildren
    { permissionFilter = pure,
      permissionQuantity = pure,
      permissionPlayer = pure,
      permissionZones = pure,
      permissionSlot = pure,
      permissionCondition = pure,
      permissionCost = pure,
      permissionTurn = pure,
      permissionMana = pure,
      permissionLooksBack = const (pure ())
    }

-- | A target slot's children (CR 115.1, 601.2c). A printed count, the players
-- a per-player slot is announced for and its "together" rider are left as
-- they are; its total's weight and ceiling are quantities.
targetSlotChildren :: (Applicative f) => PermissionChildren f -> TargetSlot.TargetSlot -> f TargetSlot.TargetSlot
targetSlotChildren c (TargetSlot.MkTargetSlot pool filter_ count amount chooser perPlayer divided together total asCast) =
  TargetSlot.MkTargetSlot
    <$> poolChildren pool
    <*> traverse (permissionFilter c) filter_
    <*> slotCountChildren count
    <*> traverse (permissionQuantity c) amount
    <*> traverse (permissionPlayer c) chooser
    <*> traverse perPlayerChildren perPlayer
    <*> traverse (permissionQuantity c) divided
    <*> pure together
    <*> traverse (\(TotalCap.MkTotalCap weight most) -> TotalCap.MkTotalCap <$> permissionQuantity c weight <*> permissionQuantity c most) total
    <*> traverse (permissionFilter c) asCast
  where
    poolChildren p = case p of
      Pool.Creatures -> pure p
      Pool.Players -> pure p
      Pool.AnyTarget -> pure p
      Pool.Permanents -> pure p
      Pool.Spells -> pure p
      Pool.Abilities -> pure p
      Pool.SpellsAndPermanents -> pure p
      Pool.SpellsAndAbilities -> pure p
      Pool.PlayersAndPlaneswalkers -> pure p
      Pool.PermanentsAndPlayers -> pure p
      Pool.CardsInGraveyard zones -> Pool.CardsInGraveyard <$> permissionZones c zones
      Pool.CardsInExile -> pure p
      Pool.CardsInAnte -> pure p
      Pool.CreaturesAndCardsInGraveyard zones -> Pool.CreaturesAndCardsInGraveyard <$> permissionZones c zones
    slotCountChildren n = case n of
      SlotCount.Printed _ -> pure n
      SlotCount.AnnouncedX -> pure n
      SlotCount.UpToAnnouncedX -> pure n
      SlotCount.UpToComputed bound -> SlotCount.UpToComputed <$> permissionQuantity c bound
    perPlayerChildren (PlayerSlot.MkPlayerSlot players thatPlayer) =
      PlayerSlot.MkPlayerSlot players <$> permissionSlot c thatPlayer

-- | A casting permission's children (CR 601.3). A keyword family and a flash
-- cast's additional mana cost are left as they are.
castingPermissionChildren :: (Applicative f) => PermissionChildren f -> CastingPermission.CastingPermission -> f CastingPermission.CastingPermission
castingPermissionChildren c permission = case permission of
  CastingPermission.CastFromLibraryWhileSearching -> pure permission
  CastingPermission.CastFromGraveyard family additional -> CastingPermission.CastFromGraveyard family <$> traverse (permissionCost c) additional
  CastingPermission.PlayFromGraveyard -> pure permission
  CastingPermission.CastAsThoughItHadFlash condition additional arms -> CastingPermission.CastAsThoughItHadFlash <$> traverse (permissionCondition c) condition <*> pure additional <*> pure arms

-- | An activation restriction's children (CR 602.5). A phase window, a combat
-- step and a count of activations are left as they are; "before this step"
-- reads when steps began (CR 506.8a).
activationRestrictionChildren :: (Applicative f) => PermissionChildren f -> ActivationRestriction.ActivationRestriction -> f ActivationRestriction.ActivationRestriction
activationRestrictionChildren c restriction = case restriction of
  ActivationRestriction.SorcerySpeed -> pure restriction
  ActivationRestriction.DuringPhase (DuringPhase.MkDuringPhase window scope_) -> fmap ActivationRestriction.DuringPhase (DuringPhase.MkDuringPhase window <$> permissionTurn c scope_)
  ActivationRestriction.DuringTurn scope_ -> ActivationRestriction.DuringTurn <$> permissionTurn c scope_
  ActivationRestriction.AttackedThisStep -> pure restriction
  ActivationRestriction.AfterBlockersDeclared -> pure restriction
  ActivationRestriction.DuringCombatBefore _ -> pure restriction
  ActivationRestriction.BeforeStep _ -> restriction <$ permissionLooksBack c HistoryRead.StepBegins
  ActivationRestriction.OnlyIf condition -> ActivationRestriction.OnlyIf <$> permissionCondition c condition
  ActivationRestriction.TimesEver _ -> pure restriction
  ActivationRestriction.TimesEachTurn _ -> pure restriction
  ActivationRestriction.DuringThrow -> pure restriction
  ActivationRestriction.InstantSpeed -> pure restriction
  ActivationRestriction.SpendOnly specified -> ActivationRestriction.SpendOnly <$> permissionMana c specified

-- | One visitor per kind of child a pay gate (CR 118.12), a counter move's
-- kinds (CR 122.5), a counter removal's count or a library placement (CR
-- 401.7) holds.
data CountChildren f = MkCountChildren
  { -- | A Quantity: a count of counters, a depth in a library, a gate's "for
    -- each" scale or announced X.
    countQuantity :: Quantity.Quantity -> f Quantity.Quantity,
    -- | A player: the one a gate offers its cost to, the one who orders an
    -- arrival in a library.
    countPlayer :: PlayerRef.PlayerRef -> f PlayerRef.PlayerRef,
    -- | The kind of counter a move names (CR 122.1).
    countKind :: CounterKind.CounterKind Keyword.Keyword -> f (CounterKind.CounterKind Keyword.Keyword),
    -- | One of the costs a gate offers.
    countCost :: Cost.Cost Keyword.Keyword -> f (Cost.Cost Keyword.Keyword),
    -- | The described cost a gate offers (CR 118.6), whole.
    countBasis :: CostBasis.CostBasis -> f CostBasis.CostBasis
  }

-- | Every child of a gate or a counter move left as it is: the base a walk overrides.
countLeaves :: (Applicative f) => CountChildren f
countLeaves =
  MkCountChildren
    { countQuantity = pure,
      countPlayer = pure,
      countKind = pure,
      countCost = pure,
      countBasis = pure
    }

-- | A pay gate's children (CR 118.12). Its branch, its obligation, the clause
-- it is offered at and the act it marks are left as they are.
payGateChildren :: (Applicative f) => CountChildren f -> PayGate.PayGate -> f PayGate.PayGate
payGateChildren c (PayGate.MkPayGate payer (CostChoice.MkCostChoice costs) basis branch obligation perEach x offeredAt marks) =
  PayGate.MkPayGate
    <$> countPlayer c payer
    <*> fmap CostChoice.MkCostChoice (traverse (countCost c) costs)
    <*> traverse (countBasis c) basis
    <*> pure branch
    <*> pure obligation
    <*> traverse (countQuantity c) perEach
    <*> traverse (countQuantity c) x
    <*> pure offeredAt
    <*> pure marks

-- | The children of the counters a move carries (CR 122.5).
movedKindsChildren :: (Applicative f) => CountChildren f -> MovedKinds.MovedKinds -> f MovedKinds.MovedKinds
movedKindsChildren c kinds = case kinds of
  MovedKinds.Every -> pure kinds
  MovedKinds.Named kind quantity -> MovedKinds.Named <$> countKind c kind <*> countQuantity c quantity
  MovedKinds.EveryOfKind kind -> MovedKinds.EveryOfKind <$> countKind c kind
  MovedKinds.Chosen quantity -> MovedKinds.Chosen <$> countQuantity c quantity
  MovedKinds.AnyNumber -> pure kinds
  MovedKinds.AtLeastOne -> pure kinds
  MovedKinds.AnyNumberOfKind kind -> MovedKinds.AnyNumberOfKind <$> countKind c kind
  MovedKinds.EachAbsentKind -> pure kinds
  MovedKinds.UpToOneChosen -> pure kinds

-- | A counter removal's count's children.
removalCountChildren :: (Applicative f) => CountChildren f -> RemovalCount.RemovalCount -> f RemovalCount.RemovalCount
removalCountChildren c count = case count of
  RemovalCount.Exactly quantity -> RemovalCount.Exactly <$> countQuantity c quantity
  RemovalCount.UpTo quantity -> RemovalCount.UpTo <$> countQuantity c quantity
  RemovalCount.AnyNumber -> pure count
  RemovalCount.All -> pure count

-- | A library placement's children (CR 401.7). A stated end is left as it is.
libraryPlacementChildren :: (Applicative f) => CountChildren f -> LibraryPlacement.LibraryPlacement -> f LibraryPlacement.LibraryPlacement
libraryPlacementChildren c placement = case placement of
  LibraryPlacement.Stated _ -> pure placement
  LibraryPlacement.OwnerChooses -> pure placement
  LibraryPlacement.RandomOrder _ -> pure placement
  LibraryPlacement.ArrangedBy (ArrangedArrival.MkArrangedArrival position chooser) -> LibraryPlacement.ArrangedBy . ArrangedArrival.MkArrangedArrival position <$> countPlayer c chooser
  LibraryPlacement.Beneath quantity -> LibraryPlacement.Beneath <$> countQuantity c quantity
  LibraryPlacement.BeneathOrBottom quantity -> LibraryPlacement.BeneathOrBottom <$> countQuantity c quantity
