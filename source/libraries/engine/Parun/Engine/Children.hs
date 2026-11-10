module Parun.Engine.Children where

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
import qualified Parun.Types.Amass as Amass
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.Ante as Ante
import qualified Parun.Types.AnyNumberDiscard as AnyNumberDiscard
import qualified Parun.Types.AnyNumberMatching as AnyNumberMatching
import qualified Parun.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Parun.Types.ArrangeInLibrary as ArrangeInLibrary
import qualified Parun.Types.AttachAll as AttachAll
import qualified Parun.Types.AttachBound as AttachBound
import qualified Parun.Types.AttachTarget as AttachTarget
import qualified Parun.Types.AttackTargetRef as AttackTargetRef
import qualified Parun.Types.BecomeCopy as BecomeCopy
import qualified Parun.Types.Bid as Bid
import qualified Parun.Types.BindAmount as BindAmount
import qualified Parun.Types.Blight as Blight
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.CastFrom as CastFrom
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.CastRepetition as CastRepetition
import qualified Parun.Types.ChangeTargets as ChangeTargets
import qualified Parun.Types.ChangeText as ChangeText
import qualified Parun.Types.ChooseCardName as ChooseCardName
import qualified Parun.Types.ChooseGroup as ChooseGroup
import qualified Parun.Types.ChooseNumber as ChooseNumber
import qualified Parun.Types.ChoosePermanents as ChoosePermanents
import qualified Parun.Types.ChoosePlayer as ChoosePlayer
import qualified Parun.Types.ChoosePlayerAtRandom as ChoosePlayerAtRandom
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.CommanderColors as CommanderColors
import qualified Parun.Types.CompletedDungeon as CompletedDungeon
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Conjure as Conjure
import qualified Parun.Types.ConjureCards as ConjureCards
import qualified Parun.Types.Connive as Connive
import qualified Parun.Types.ControlPlayer as ControlPlayer
import qualified Parun.Types.ControlSides as ControlSides
import qualified Parun.Types.ControlSlots as ControlSlots
import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.CopyOriginal as CopyOriginal
import qualified Parun.Types.CopyStackObject as CopyStackObject
import qualified Parun.Types.CopyTargets as CopyTargets
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.Count as Count
import qualified Parun.Types.CountedDiscard as CountedDiscard
import qualified Parun.Types.Counter as Counter
import qualified Parun.Types.CounterDestination as CounterDestination
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLinked as CounterLinked
import qualified Parun.Types.Create as Create
import qualified Parun.Types.CreateCopy as CreateCopy
import qualified Parun.Types.CreateEmblem as CreateEmblem
import qualified Parun.Types.DamagePart as DamagePart
import qualified Parun.Types.DamageTally as DamageTally
import qualified Parun.Types.DamagedParty as DamagedParty
import qualified Parun.Types.DealDamage as DealDamage
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.DealtDamageTo as DealtDamageTo
import qualified Parun.Types.Designate as Designate
import qualified Parun.Types.Destroy as Destroy
import qualified Parun.Types.Devotion as Devotion
import qualified Parun.Types.Discard as Discard
import qualified Parun.Types.Divide as Divide
import qualified Parun.Types.DoesNotUntapNext as DoesNotUntapNext
import qualified Parun.Types.DraftNoteTally as DraftNoteTally
import qualified Parun.Types.Draw as Draw
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Earthbend as Earthbend
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.ExchangeBlocks as ExchangeBlocks
import qualified Parun.Types.ExchangeCardZones as ExchangeCardZones
import qualified Parun.Types.ExchangeOwnership as ExchangeOwnership
import qualified Parun.Types.ExchangeSides as ExchangeSides
import qualified Parun.Types.ExchangeValues as ExchangeValues
import qualified Parun.Types.ExchangeZones as ExchangeZones
import qualified Parun.Types.ExchangedValue as ExchangedValue
import qualified Parun.Types.ExileHaunting as ExileHaunting
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.Fight as Fight
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FlipCoin as FlipCoin
import qualified Parun.Types.ForEach as ForEach
import qualified Parun.Types.ForEachNumber as ForEachNumber
import qualified Parun.Types.ForbidAttack as ForbidAttack
import qualified Parun.Types.ForbidBeingBlocked as ForbidBeingBlocked
import qualified Parun.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Parun.Types.FromReference as FromReference
import qualified Parun.Types.GainControl as GainControl
import qualified Parun.Types.GrantLookAtExiled as GrantLookAtExiled
import qualified Parun.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Parun.Types.Grouping as Grouping
import qualified Parun.Types.Guess as Guess
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.LaterPayment as LaterPayment
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LifeLoss as LifeLoss
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.LookAt as LookAt
import qualified Parun.Types.MakeForetold as MakeForetold
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Meld as Meld
import qualified Parun.Types.Mill as Mill
import qualified Parun.Types.MillTally as MillTally
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyTarget as ModifyTarget
import qualified Parun.Types.MonarchTarget as MonarchTarget
import qualified Parun.Types.MoveCounters as MoveCounters
import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.MoveMana as MoveMana
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.NextDamageInstance as NextDamageInstance
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.OfferCast as OfferCast
import qualified Parun.Types.Onset as Onset
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PlayerCounterTally as PlayerCounterTally
import qualified Parun.Types.PlayerCounters as PlayerCounters
import qualified Parun.Types.PlayerDesignationTally as PlayerDesignationTally
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerQuantity as PlayerQuantity
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerSacrifices as PlayerSacrifices
import qualified Parun.Types.PreventAllDamage as PreventAllDamage
import qualified Parun.Types.PreventNextDamage as PreventNextDamage
import qualified Parun.Types.Prohibit as Prohibit
import qualified Parun.Types.PutCounters as PutCounters
import qualified Parun.Types.PutCountersFrom as PutCountersFrom
import qualified Parun.Types.PutSticker as PutSticker
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.RedirectDamage as RedirectDamage
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
import qualified Parun.Types.Reveal as Reveal
import qualified Parun.Types.RollDie as RollDie
import qualified Parun.Types.SacrificeEffect as SacrificeEffect
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
import qualified Parun.Types.SpellTally as SpellTally
import qualified Parun.Types.Surveil as Surveil
import qualified Parun.Types.TakeExtraTurn as TakeExtraTurn
import qualified Parun.Types.Tap as Tap
import qualified Parun.Types.TheseDiscard as TheseDiscard
import qualified Parun.Types.TurnFaceDown as TurnFaceDown
import qualified Parun.Types.Undesignate as Undesignate
import qualified Parun.Types.Vote as Vote
import qualified Parun.Types.VoteChoices as VoteChoices
import qualified Parun.Types.VoteObjects as VoteObjects
import qualified Parun.Types.VotePlayers as VotePlayers
import qualified Parun.Types.VotesReceived as VotesReceived
import qualified Parun.Types.WhichCounters as WhichCounters

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
  Filter.IsAttacking -> pure predicate
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
  Effect.Search (Search.MkSearch searcher owner zones outside quantity filter_ descriptions upTo destination subject named handSlot differentIn exactly andOr) ->
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
  Effect.Proliferate -> pure effect
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
