-- Parun.Engine.Card's lints over the slots an ability reads and declares (CR
-- 603.7, CR 602.2b): delayed, triggered and activated abilities, and the lints
-- that catch a reserved or shadowed slot. The three AbilityName joins are here
-- too -- CR 603.7's arming, CR 613.1f's named removal and CR 116.2d's ignore --
-- each an equality between the names one side writes and the names the other
-- reads. Split out of Parun.CardSpec, which keeps the machinery and the lints
-- over modes, references and tokens.
module Parun.AbilitySlotLintSpec where

-- Aliased Condition.Type, matching Parun.Types.Count below and the project-wide
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- Dotted, because Parun.Types.Keyword already holds the short alias here (the
-- The json sublibrary's own modules, for the CR 701.3a completeness cross-check
-- The logic module, alongside Parun.Types.Modal below: unambiguous under one
-- alias because the two modules export disjoint names (TriggerSpec's
-- alone: it counts the atom in a card's ENCODED form, which is a traversal of the
-- convention (FilterSpec/CardSpec's Filter.Type note): Parun.Engine.Condition may
-- hand-maintained one below.
-- later be imported and must not collide.
-- precedent), and Modal.allEffects is how this lint reaches an activated or
-- reverse of TriggerSpec's split).
-- the evaluator module Parun.Engine.Filter may later be imported and must not collide.
-- triggered ability's effects (Card.allEffects only reaches the spell).
-- whole card written by somebody else and so an independent witness to the

import qualified Data.Foldable as Foldable
import qualified Data.Functor.Const as Const
import qualified Data.Functor.Identity as Identity
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Parun.CardSpec (anyFace, anyFaceOrMinted, armedReads, cardAuthoredEffects, cardCounts, cardFilters, cardResolutionEffects, collectsEvidenceAsCost, createToken, declaresVariable, delayedDeclarations, discardsAsCost, effectCounts, effectGrants, effectWithNested, exilesCardAsCost, faceModals, grantedAbilities, grantedActivatedAbilities, grantedStaticAbilities, grantedTriggeredAbilities, instantLine, lintMode, mintingSpell, modalActivated, modalClauseConditions, modalSlotsOffend, modificationAbilities, oneEffectActivated, oneEffectTrigger, payGateBound, removesCountersAsCost, sacrificesAsCost, spellCostsOf, triggerConditionSlots, vanillaFace, waterbendsAsCost, withMinted)
import qualified Parun.Codec.Card as Card.Codec
import qualified Parun.Codec.Condition as Condition.Codec
import qualified Parun.Codec.GrantedAbility as GrantedAbility.Codec
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Card as Card
import qualified Parun.Engine.Children as Children
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Event.Binding as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Keyword as Keyword.Engine
import qualified Parun.Engine.Mana as Mana
import qualified Parun.Engine.ManaAbility as ManaAbility
import qualified Parun.Engine.Mint as Mint
import qualified Parun.Engine.Modal as Modal
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.QuantitySlot as QuantitySlot
import qualified Parun.Engine.Resolve as Resolve
import qualified Parun.Engine.Resolve.Slots as Resolve
import qualified Parun.Engine.RuleAbilities as RuleAbilities.Engine
import qualified Parun.Engine.Target as Target
import Parun.FilterPositionLintSpec (filterAtomsOf, jsonAtoms)
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.ActivationProhibition as ActivationProhibition
import qualified Parun.Types.ActivationsThisTurn as ActivationsThisTurn
import qualified Parun.Types.Activator as Activator
import qualified Parun.Types.AffectPlayers as AffectPlayers
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.AffectedPlayers as AffectedPlayers
import qualified Parun.Types.AgainstExiledPile as AgainstExiledPile
import qualified Parun.Types.AgainstSlot as AgainstSlot
import qualified Parun.Types.Aggregation as Aggregation
import qualified Parun.Types.Arithmetic as Arithmetic
import qualified Parun.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Parun.Types.AttachAll as AttachAll
import qualified Parun.Types.AttackTrigger as AttackTrigger
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.BoundReading as BoundReading
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardAbilities as CardAbilities
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CastFromZone as CastFromZone
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.ChoosePlayer as ChoosePlayer
import qualified Parun.Types.Chooser as Chooser
import qualified Parun.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition.Type
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CostModifier as CostModifier
import qualified Parun.Types.CostSubject as CostSubject
import qualified Parun.Types.Count as Count.Type
import qualified Parun.Types.CountedDiscard as CountedDiscard
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CountersFromPermanents as CountersFromPermanents
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.DeclarationTrigger as DeclarationTrigger
import qualified Parun.Types.DefinedX as DefinedX
import qualified Parun.Types.DifferentIn as DifferentIn
import qualified Parun.Types.Discard as Discard
import qualified Parun.Types.Draw as Draw
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntersTrigger as EntersTrigger
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.EventShape as EventShape
import qualified Parun.Types.ExiledPile as ExiledPile
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.ForEach as ForEach
import qualified Parun.Types.Gift as Gift
import qualified Parun.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Parun.Types.GrantZone as GrantZone
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordInstance as KeywordInstance
import qualified Parun.Types.LeavesTrigger as LeavesTrigger
import qualified Parun.Types.LeavingPermanent as LeavingPermanent
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.LifeDirection as LifeDirection
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.LostPermanent as LostPermanent
import qualified Parun.Types.ManaAddition as ManaAddition
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeMemory as ModeMemory
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyTarget as ModifyTarget
import qualified Parun.Types.MoveCardsFromGraveyard as MoveCardsFromGraveyard
import qualified Parun.Types.MoveCounters as MoveCounters
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.MovedKinds as MovedKinds
import qualified Parun.Types.ObjectCount as ObjectCount
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.Onset as Onset
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.Optionality as Optionality
import qualified Parun.Types.PayBranch as PayBranch
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PayObligation as PayObligation
import qualified Parun.Types.PermissionLimit as PermissionLimit
import qualified Parun.Types.PermissionPool as PermissionPool
import qualified Parun.Types.PermissionVerb as PermissionVerb
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerIsDealtDamage as PlayerIsDealtDamage
import qualified Parun.Types.PlayerQuantity as PlayerQuantity
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerSacrifices as PlayerSacrifices
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.PlayerSlot as PlayerSlot
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.Plus as Plus
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.Sacrifice as Sacrifice
import qualified Parun.Types.SacrificeEffect as SacrificeEffect
import qualified Parun.Types.Sacrificer as Sacrificer
import qualified Parun.Types.Scope as Scope
import qualified Parun.Types.SlotArity as SlotArity
import qualified Parun.Types.SlotCount as SlotCount
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SourceDealtDamage as SourceDealtDamage
import qualified Parun.Types.SpecialAction as SpecialAction
import qualified Parun.Types.SpellCast as SpellCast
import qualified Parun.Types.StaticAbility as StaticAbility
import qualified Parun.Types.StepBegins as StepBegins
import qualified Parun.Types.Tap as Tap
import qualified Parun.Types.TapPermanents as TapPermanents
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TargetCount as TargetCount
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.Together as Together
import qualified Parun.Types.TopOfLibrary as TopOfLibrary
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnScope as TurnScope
import qualified Parun.Types.TurnTally as TurnTally
import qualified Parun.Types.WhenSpent as WhenSpent
import qualified Parun.Types.WhichTurn as WhichTurn
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneScope as ZoneScope

-- CR 400.1: "each player has their own library, hand, and graveyard. The
-- other zones are shared by all players." Battlefield/Stack/Exile/Command/Ante
-- are shared; Library/Hand/Graveyard are per-player.
isSharedZone :: Zone.Zone -> Bool
isSharedZone zone = case zone of
  Zone.Library -> False
  Zone.Hand -> False
  Zone.Graveyard -> False
  Zone.Battlefield -> True
  Zone.Stack -> True
  Zone.Exile -> True
  Zone.Command -> True
  Zone.Ante -> True

-- A Count over a shared zone paired with anything but Relative AnyPlayer names a
-- per-player fold over a zone no player individually owns -- permitted by the
-- type, not by the rules. Parun.Codec.InZone.undividedShared is what ENFORCES
-- this, at the decoder, so a card file carrying the pairing never reaches a
-- registry; this restates the rule over the loaded pool rather than calling
-- that predicate, so the two have to agree independently, see #161.
scopeOffends :: Scope.Scope keyword -> Bool
scopeOffends scope = case scope of
  Scope.InZone (InZone.MkInZone zone ref) -> isSharedZone zone && ref /= PlayerRef.Relative PlayerRelation.AnyPlayer
  Scope.InHistory _ -> False
  -- No zone at all, shared or otherwise: this scope folds the players a
  -- PlayerRef names rather than a copy of a zone each of them owns, so the
  -- pairing the lint rejects cannot arise.
  Scope.OverPlayers _ -> False
  -- No zone at all either: this scope folds the objects a binding names,
  -- wherever they are, so there is no per-player copy of a zone for the pairing
  -- the lint rejects to arise in.
  Scope.OverBound {} -> False
  -- The InZone arm's pairing, asked of a position in a zone.
  Scope.InPosition _ inZone -> scopeOffends (Scope.InZone inZone)
  -- Every permanent at once, so no player's copy of a zone to pair with.
  Scope.AsTurnBegan -> False
  -- Every zone at once, so no player's copy of one to pair with.
  Scope.Anywhere -> False

cardOffendsSharedZoneScope :: Face.Face Card.Type.Card -> Bool
cardOffendsSharedZoneScope card =
  any (scopeOffends . Count.Type.scope) (cardCounts card)

-- CR 601.2c's OTHER dataflow question, asked of the same modes: a slot whose
-- count may exceed one holds a set of recipients, and only a reader that takes a
-- set can see all of them (Parun.Types.SlotArity). A card aiming "up to two target
-- creatures" at an opcode that names one object would affect NEITHER of them --
-- Parun.Engine.Binding.onlyOne declines a slot naming several rather than picking
-- one -- and no compiler catches that, so it is caught here.
--
-- Read off the same Resolve.modeSlots the D4 lint reads, whose join keeps the
-- narrower arity: a slot two effects of one mode read both ways is One. The two
-- lints ask different questions of the one map -- D4 asks its KEYS whether the
-- slot is read at all, this asks its VALUES how many recipients the read sees --
-- which is what SlotArity.Amount parts: a Quantity.InSlot names the slot's
-- amount rather than its objects (Parun.Engine.Binding.amountOf), so it is a read
-- D4 must count and no arity claim for this lint to reject a plural slot on;
-- see #2774.
modalCountsOffend :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
modalCountsOffend modal =
  let modeOffends mode =
        let read_ = Resolve.modeSlots mode
            plural targetSlot = SlotCount.plural (TargetSlot.count targetSlot)
            offends slot targetSlot = plural targetSlot && Map.lookup slot read_ == Just SlotArity.One
         in or (Map.elems (Map.mapWithKey offends (Mode.targetSlots mode)))
   in any modeOffends (Modal.modes modal)

-- The TRIGGERED-ability half of the D4 dataflow lint: every slot one of a
-- triggered ability's effects READS must be a slot something binds for that
-- ability, and every slot it DECLARES must be read. Without the first half, an
-- effect naming CR 400.7e's `became` under a condition that never binds it
-- loads, places its trigger, misses the lookup and silently no-ops (Resolve's
-- MoveToZone arm moves nothing for a slot that names no object); without the
-- second, an ability announces a target it ignores.
--
-- The spell lint's EQUALITY, which modalSlotsOffend now applies to every carrier
-- (#1043). It was a subset check for as long as the bound side was UNIONED into
-- the declared one, which Parun.Engine.Binding.triggerSource's comment shows is
-- mutually unsatisfiable with the "a reserved slot is never a declared target
-- slot" rule; subtracting the bound names from the READ side instead -- what the
-- spell lint always did with its cast-time pair -- is what makes the equality
-- statable here.
--
-- What answers a read, and why each part of it answers one:
--
--   * Binding.triggerSource (CR 113.7, the object whose ability triggered),
--     Binding.you (CR 109.5, the ability's controller) and Binding.thisAbility
--     (CR 603.3, the ability's own object) are stamped for EVERY triggered
--     ability as it is placed (Engine.placeBorne), so they need no agreement
--     with the condition. `you` is stamped for every
--     ACTIVATION and every SPELL too, by rule 109.5's other sentences -- which is
--     why the spell lint subtracts it on the read side rather than listing it
--     here.
--   * Event.eventBindingSlots is the condition-SPECIFIC half -- CR 400.7e's
--     `became`, CR 702.70a's `thatPlayer` -- and is the whole point of this
--     lint. Event.eventBindingSlotsSometimes beside it, the slots that condition
--     binds for some of its events and not all: CR 400.7e binds `became` for a
--     public destination and withholds it for a hidden one, and a payload that
--     finds nothing there is doing what the rule says rather than the silent
--     no-op this lint catches.
--   * Resolve.definedSlots covers a slot the ability's own effects MINT rather
--     than read: a Create's token (CR 603.7c's "it"), a PlaySubgame's winner.
--     The same exemption every carrier takes.
--   * payGateBound, the slot a CR 118.12 cost paid at resolution binds (Feed the
--     Pack). Every carrier takes that one too.
--   * the ability's own declared target slots (CR 601.2c / 700.2c) are the
--     ordinary chosen targets -- the side modalSlotsOffend compares AGAINST, one
--     MODE's at a time, so a mode reading a slot only another mode declares is
--     caught and so is a mode declaring a slot only another mode reads.
--
-- The first bullet's three are what this passes to modalSlotsOffend as
-- `abilityBound`: they are stamped for the ability, not for a mode, so every
-- mode gets them.
triggeredAbilityOffends :: TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
triggeredAbilityOffends = triggeredAbilityOffendsGiven (const Set.empty) Set.empty

-- triggeredAbilityOffends with `inherited` answering reads too: the slots the
-- bearer's FACE guarantees rather than the condition, CR 400.7d's cost record.
-- And with `armed` reading for the delayed abilities a mode arms, which only
-- the bearer's face can name (CardSpec.armedReads).
triggeredAbilityOffendsGiven :: (Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Set.Set SlotName.SlotName) -> Set.Set SlotName.SlotName -> TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
triggeredAbilityOffendsGiven armed inherited ability =
  modalSlotsOffend
    armed
    ( Set.unions
        [ inherited,
          Set.fromList [Binding.triggerSource, Binding.you, Binding.thisAbility],
          Event.eventBindingSlots (TriggeredAbility.condition ability),
          Event.eventBindingSlotsSometimes (TriggeredAbility.condition ability),
          payGateBound (TriggeredAbility.modal ability)
        ]
    )
    (TriggeredAbility.modal ability)

-- The ACTIVATED-ability half of the same lint: every slot one of an
-- activated ability's effects READS must be a slot the ACTIVATION binds, and
-- every slot it DECLARES must be read. Without the first half, an ability naming
-- CR 109.5's `you` loads, activates, misses the lookup and silently no-ops,
-- exactly as an unbound `became` does above.
--
-- The same EQUALITY as every other carrier (#1043); see modalSlotsOffend and the
-- triggered lint above for why subtracting the bound names from the read side is
-- what makes it statable.
--
-- What answers a read is what Parun.Engine.Activate.activateAbility stamps on the
-- ability object as it goes on the stack, and nothing else:
--
--   * Binding.triggerSource. CR 113.7: "The source of an activated ability on
--     the stack is the object whose ability was activated" -- stamped for every
--     activation, so Longtusk Cub's "put a +1/+1 counter on Longtusk Cub" is a
--     slot read.
--   * the ability's own declared target slots, one MODE's at a time
--     (modalSlotsOffend). CR 602.2b: "The remainder of the process for
--     activating an ability is identical to the process for casting a spell
--     listed in rules 601.2b-i", which is what routes an activation through CR
--     601.2c's target announcement -- and CR 700.2c scopes it to the chosen
--     mode.
--   * Binding.you. CR 109.5: "For an activated ability, this is the player who
--     activated the ability" -- stamped for every activation alongside the
--     source slot, so Brothers of Fire's "and 1 damage to you" is a slot read.
--     Cast.castSpell stamps it for every SPELL as well (Char), which the spell
--     lint takes on its read side.
--   * Binding.thisAbility. CR 602.2a: "That ability is created on the stack as
--     an object that's not a card" -- stamped for every activation beside the
--     two above, so Forsworn Paladin's "if mana from a Treasure was spent to
--     activate this ability" is a slot read. Not `triggerSource`, which names
--     the ability's SOURCE and carries the record of a different payment.
--   * Binding.variableX, and ONLY when the ability's own cost prints an {X}:
--     CR 601.2b's "the player announces the value of that variable", measured
--     against what CR 602.2b calls "an activated ability's analog to a spell's
--     mana cost ... its activation cost" (Cinder Elemental). A printed X is an
--     ordinary slot read since #14 retired Quantity.X, so it arrives here like
--     any other, and the activation really does bind it -- leaving it out would
--     reject a read that works. The cast side's bullet in cardOffends below
--     states the same thing about a spell's cost.
--   * Resolve.definedSlots, the slot an effect of this ability MINTS rather than
--     reads. The same exemption every sibling carrier takes.
--   * payGateBound, the slot its own CR 118.12 cost binds as the ability
--     resolves -- written on the same ability object, by Resolve rather than
--     Activate.
--   * Binding.definingCard, and only when a choice defines the ability's X
--     (DefinedX.choice): CR 601.2b's announcement binds the chosen card, so
--     Caller of the Untamed's "that card" is an ordinary slot read.
--
-- What is NOT on it is the point:
--
--   * both event slots (CR 400.7e's `became`, CR 702.70a's `thatPlayer`): an
--     activation is not an event, so Parun.Engine.Event.Binding.eventBindings never runs
--     for one.
--   * Binding.chosenModes (CR 700.2), which IS stamped and is still not an
--     exemption: its binding carries a mode set and nothing else, so no effect
--     read can be answered from it -- Resolve reads a slot as a recipient
--     (Binding.targetsOf) or as an amount (Binding.amountOf), and both are
--     Nothing there. Admitting it would exempt a read that silently no-ops,
--     which is the failure this lint exists to catch.
--
-- SCOPE: the abilities that reach Activate. CR 605.3b's mana abilities take
-- another road -- one "doesn't go on the stack, so it can't be targeted,
-- countered, or otherwise responded to. Rather, it resolves immediately after it
-- is activated" -- so Cost.tapForManaWith pays the route's cost, adds the mana
-- and runs the rest through Resolve.performManaAbility (CR 405.6c). The
-- payment's bound slots price the YIELD -- Priest of Yawgmoth's "the sacrificed
-- artifact's mana value" -- and their amounts reach the rest beside
-- Binding.triggerSource and Binding.you -- Springjack Pasture's "You gain X
-- life".
--
-- Unprinted: the objects they name reaching the rest, so an object exemption
-- this lint grants is wider than a mana ability's other effects get;
-- manaEffectSlotOffends ("CR 405.6c no card writes a mana ability whose other
-- effects read an object its cost paid") keeps that read out of card data.
activatedAbilityOffends :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
activatedAbilityOffends = activatedAbilityOffendsGiven (const Set.empty)

-- activatedAbilityOffends with `armed` reading for the delayed abilities a mode
-- arms, which only the bearer's face can name (CardSpec.armedReads).
activatedAbilityOffendsGiven :: (Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Set.Set SlotName.SlotName) -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
activatedAbilityOffendsGiven armed ability =
  let announcedX =
        if declaresVariable (ActivatedAbility.cost ability)
          then Set.singleton Binding.variableX
          else Set.empty
      sacrificed =
        if sacrificesAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.sacrificedPermanent
          else Set.empty
      tapped = turnedAsCost (ActivatedAbility.cost ability)
      tappedForTotal =
        if tapsForTotalPowerAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.tappedForTotalPower
          else Set.empty
      exiled =
        if exilesCardAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.exiledCard
          else Set.empty
      exiledPermanents =
        if exilesPermanentsAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.exiledPermanent
          else Set.empty
      totalled =
        if exilesTotallingAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.totalledExiles
          else Set.empty
      discarded =
        if discardsAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.discardedCard
          else Set.empty
      graveyarded =
        if graveyardsExiledAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.graveyardedCard
          else Set.empty
      removed =
        if removesCountersAsCost (ActivatedAbility.cost ability)
          then Set.singleton Binding.removedCounters
          else Set.empty
      defining =
        if any (Maybe.isJust . DefinedX.choice) (ActivatedAbility.definedX ability)
          then Set.singleton Binding.definingCard
          else Set.empty
      -- Binding.setGranter's condition: a name acquiredFrom gave.
      granter =
        if Maybe.isJust (Binding.acquiredSource =<< ActivatedAbility.name ability)
          then Set.singleton Binding.grantingPermanent
          else Set.empty
      -- CR 115.10a: the player the activation announced (Lodestone Bauble).
      chosen = foldMap (Set.singleton . PlayerSlot.slot) (ActivatedAbility.chosenPlayer ability)
   in modalSlotsOffend armed (Set.unions [Set.fromList [Binding.triggerSource, Binding.you, Binding.thisAbility], announcedX, defining, granter, chosen, sacrificed, tapped, tappedForTotal, exiled, exiledPermanents, totalled, discarded, graveyarded, removed, payGateBound (ActivatedAbility.modal ability)]) (ActivatedAbility.modal ability)

-- The slots this cost's taps or untaps of permanents the payer CHOOSES bind:
-- CR 601.2h's payment binds Binding.tappedPermanent, or
-- Binding.untappedPermanent for an untap (Cost.payComponent's TapPermanents arm,
-- folded on by Activate), so Unerring Sling's "the tapped creature's power" and
-- Benthic Explorers' "that land" are ordinary slot reads.
--
-- CostComponent.TapThis is deliberately not counted: CR 107.5 taps the SOURCE,
-- which CR 113.7's `triggerSource` already names.
--
-- CostComponent.TapForTotalPower is counted by tapsForTotalPowerAsCost below
-- instead, under a slot of its own.
--
-- Not offered on the CAST side (cardOffends below), though Cast.castSpell now
-- folds a payment's slots onto the spell: no printing in `data/cards/` taps as an
-- additional cost to cast, so the exemption would fence nothing. Living Destiny's
-- reveal, Fling's sacrifice and Grab the Prize's discard are among the cast-side
-- payment slots that lint admits.
turnedAsCost :: Cost.Type.Cost Keyword.Keyword -> Set.Set SlotName.SlotName
turnedAsCost cost = Set.fromList [Binding.tapStatePermanent (TapPermanents.becoming turned) | CostComponent.TapPermanents turned <- Cost.Type.components cost]

-- Does this cost tap permanents to reach a THRESHOLD? turnedAsCost's shape, and the
-- same reason one rule over: CR 601.2h's payment binds
-- Binding.tappedForTotalPower (Cost.payComponent's TapForTotalPower arm, folded on
-- by Activate), which is CR 702.122b's "crews a Vehicle" relation.
--
-- Its own question and not a second answer to turnedAsCost's: a cost carrying both
-- components (data/cards/synthetic-crewed-battery.json) binds both slots, and one
-- carrying either binds only that one.
tapsForTotalPowerAsCost :: Cost.Type.Cost Keyword.Keyword -> Bool
tapsForTotalPowerAsCost =
  let isThresholdTap component = case component of
        CostComponent.TapForTotalPower {} -> True
        _ -> False
   in any isThresholdTap . Cost.Type.components

-- Does this cost put an exiled card into a graveyard? Parun.CardSpec's
-- exilesCardAsCost's shape:
-- CR 601.2h's payment binds Binding.graveyardedCard (Cost.payComponent's
-- PutExiledIntoGraveyard arm, folded on by Activate), so Jester's Scepter's
-- "that card" is an ordinary slot read. Not offered on the CAST side: no
-- printing carries this component as an additional cost to cast.
graveyardsExiledAsCost :: Cost.Type.Cost Keyword.Keyword -> Bool
graveyardsExiledAsCost =
  let isGraveyarding component = case component of
        CostComponent.PutExiledIntoGraveyard _ -> True
        _ -> False
   in any isGraveyarding . Cost.Type.components

-- Does this cost EXILE permanents the payer CHOOSES? sacrificesAsCost's shape:
-- CR 601.2h's payment binds Binding.exiledPermanent (Cost.payComponent's
-- ExilePermanents arm), so Food Chain's "the exiled creature's mana value" is an
-- ordinary slot read.
exilesPermanentsAsCost :: Cost.Type.Cost Keyword.Keyword -> Bool
exilesPermanentsAsCost =
  let isExile component = case component of
        CostComponent.ExilePermanents {} -> True
        _ -> False
   in any isExile . Cost.Type.components

-- Does this cost exile cards to reach a THRESHOLD? exilesPermanentsAsCost's
-- shape: CR 601.2h's payment binds Binding.totalledExiles (Cost.payComponent's
-- ExileCardsTotalling arm), so Baron Helmut Zemo's "copy those exiled cards" is
-- an ordinary slot read.
exilesTotallingAsCost :: Cost.Type.Cost Keyword.Keyword -> Bool
exilesTotallingAsCost =
  let isExile component = case component of
        CostComponent.ExileCardsTotalling {} -> True
        _ -> False
   in any isExile . Cost.Type.components

-- Does this cost exile cards Parun.Engine.Cost.linkPaidExiles links to its
-- source on the activated road (CR 607.2a)? Its ExilePermanents,
-- ExileCardsFromHand, BeholdAndExile, MoveCardsFromGraveyard-to-exile and
-- CollectEvidence arms; CollectEvidenceOfTargets is fixed to the last.
linksExilesAsCost :: Cost.Type.Cost Keyword.Keyword -> Bool
linksExilesAsCost =
  let links component = case component of
        CostComponent.ExilePermanents {} -> True
        CostComponent.ExileCardsFromHand {} -> True
        CostComponent.BeholdAndExile {} -> True
        CostComponent.MoveCardsFromGraveyard exile -> MoveCardsFromGraveyard.destination exile == GraveyardCostDestination.Exile
        CostComponent.CollectEvidence {} -> True
        CostComponent.CollectEvidenceOfTargets -> True
        _ -> False
   in any links . Cost.Type.components

-- CR 607.2a / 605.1a: does a face pair a mana ability whose cost exiles cards
-- (linksExilesAsCost) with a reader of the pile it would link -- its own
-- abilities' (OwnAbilities, LastChosen) or that ability's by name?
--
-- Unprinted: a mana ability's cost has no stack object to name its ability, so
-- Parun.Engine.Cost.linkPaidExiles links nothing for it; this keeps the pairing
-- out of card data (Cryptex has the cost and no reader).
manaExileLinkOffends :: Face.Face Card.Type.Card -> Bool
manaExileLinkOffends face =
  let exiling = filter (\ability -> ManaAbility.isManaAbility ability && linksExilesAsCost (ActivatedAbility.cost ability)) (Face.activatedAbilities face <> grantedActivatedAbilities face)
      names = Set.fromList (Maybe.mapMaybe ActivatedAbility.name exiling)
      readsPile atom = case atom of
        Filter.Type.IsExiledWithSource ExiledPile.OwnAbilities -> [True]
        Filter.Type.IsExiledWithSource ExiledPile.LastChosen -> [True]
        Filter.Type.IsExiledWithSource (ExiledPile.OfAbility name) -> [Set.member name names]
        _ -> []
   in not (null exiling) && any (or . filterAtomsOf readsPile . snd) (cardFilters face)

-- CR 605.1a / 601.2h: does a mana ability's yield read a slot its cost's payment
-- binds (manaPaymentSlots) that Parun.Engine.Mana.pricedCostSlots does not
-- price?
--
-- Unprinted: a yield reading what its cost tapped, discarded, revealed or
-- returned, the several permanents it sacrificed or exiled, or counters it
-- removes X of. Parun.Engine.Mana.manaSuppliesGiven supplies it as the offer
-- prices it, with the slot unbound, so a cast it would pay for goes unoffered;
-- this keeps the shape out of card data.
manaYieldSlotOffends :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
manaYieldSlotOffends ability =
  let readOf addition = QuantitySlot.slots (ManaAddition.count addition) <> Map.keysSet (Resolve.productionSlots (ManaAddition.production addition))
      yieldReads = foldMap readOf (Maybe.mapMaybe ManaAbility.manaProduced (Modal.allEffects (ActivatedAbility.modal ability)))
   in ManaAbility.isManaAbility ability && not (Set.null (Set.intersection yieldReads manaPaymentSlots Set.\\ Mana.pricedCostSlots (ActivatedAbility.cost ability)))

-- The slots Parun.Engine.Cost.payComponent's arms bind.
manaPaymentSlots :: Set.Set SlotName.SlotName
manaPaymentSlots =
  Set.union
    manaPaymentAmountSlots
    ( Set.fromList
        [ Binding.sacrificedPermanent,
          Binding.exiledPermanent,
          Binding.exiledCard,
          Binding.totalledExiles,
          Binding.craftMaterials,
          Binding.tappedPermanent,
          Binding.untappedPermanent,
          Binding.tappedForTotalPower,
          Binding.discardedCard,
          Binding.revealedCard,
          Binding.returnedPermanent,
          Binding.graveyardedCard,
          Binding.collectedEvidence,
          Binding.beheldObject
        ]
    )

-- The manaPaymentSlots whose binding is an amount (Binding.toAmount), which
-- Parun.Engine.Cost.tapForManaWith publishes on Scratch.ambientAmounts.
manaPaymentAmountSlots :: Set.Set SlotName.SlotName
manaPaymentAmountSlots = Set.fromList [Binding.removedCounters, Binding.loyaltyRemoved, Binding.waterbendCost, Binding.blightCost]

-- CR 405.6c / 608.2h: does a mana ability's other effects, clause conditions
-- or gates read an object its own cost's payment binds?
--
-- Unprinted: a mana ability's effects other than its yield reading the objects
-- its cost paid with ("You gain life equal to the sacrificed creature's
-- toughness"). CR 605.3b leaves no ability object to carry them, so
-- Parun.Engine.Cost.tapForManaWith hands those effects only the amounts, and the
-- read falls through to the source permanent's bindings; this keeps the shape
-- out of card data.
manaEffectSlotOffends :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
manaEffectSlotOffends ability =
  let modal = ActivatedAbility.modal ability
      unyielded clause = clause {Clause.effects = Seq.filter (Maybe.isNothing . ManaAbility.manaProduced) (Clause.effects clause)}
      otherReads mode =
        let stripped = mode {Mode.clauses = fmap unyielded (Mode.clauses mode)}
         in Map.keysSet (Resolve.modeSlots stripped) Set.\\ Resolve.definedSlots (Foldable.toList (Mode.allEffects stripped))
      everyRead = foldMap otherReads (Foldable.toList (Modal.modes modal) <> [Modal.sharedMode modal])
   in ManaAbility.isManaAbility ability && not (Set.null (Set.intersection everyRead (manaPaymentSlots Set.\\ manaPaymentAmountSlots)))

-- CR 603.7 / 109.5: does this card arm a delayed ability "on your next turn"
-- whose condition is not scoped to its controller's turn?
--
-- Parun.Types.Onset.FromNextTurnOf carries BOTH halves of that phrase on its
-- own: Event.armOnset stores TurnWindow.NextTurnOf and
-- Event.settleOnsets pins the entry to the one turn that turns out to be, whose
-- active player is the entry's controller. So the ability's own
-- TriggerCondition.StepBegins carrying @Relative You@ is redundant
-- for FIRING.
--
-- It is not redundant in the DATA, which is what this lint is about: a card that
-- arms with the onset but scopes with @Relative AnyPlayer@ has printed an "each" the window
-- would silently narrow to the controller's turn, so its text would mean
-- something the card does not say. That is what this rejects.
--
-- Onset.FromThatExtraTurn is held to the same pairing: "that turn" is an extra
-- turn the controller takes (Final Fortune), so its step is theirs.
--
-- A dangling name (an onset naming an ability the card does not declare) is
-- ALSO an offence here, and deliberately not silently accepted: the neighbouring
-- "every armed delayed ability is declared" lint is what reports it precisely,
-- and answering False for it here would let a card that offends both pass this
-- one.
onsetOffends :: Face.Face Card.Type.Card -> Bool
onsetOffends card =
  let scoped name = case Map.lookup name (Face.delayedAbilities card) of
        Nothing -> False
        Just ability -> Event.controllerTurnScoped (TriggeredAbility.condition ability)
      -- A carried arm is held to the same pairing off the ability it carries.
      carriedOffends effect = case effect of
        Effect.ArmDelayedTrigger arm
          | Resolve.namesOwnTurn (ArmDelayedTrigger.onset arm) ->
              maybe False (not . Event.controllerTurnScoped . TriggeredAbility.condition) (Game.carriedDelayedAbility arm)
        _ -> False
   in not (all scoped (Set.toList (Resolve.onsetGatedAbilities (cardAuthoredEffects card)))) || any carriedOffends (cardAuthoredEffects card)

-- modalActivated's TRIGGERED twin, so the per-mode lint can be shown to hand
-- `abilityBound` -- the condition's event slots and CR 109.5's `you` -- to EVERY
-- mode rather than only the first.
-- Does any of these abilities DECLARE a target slot under a name already in
-- `defined`? Split from the sweep below so the rejecting direction can be put to
-- a hand-built ability, which no committed card supplies.
shadowsSlots :: Set.Set SlotName.SlotName -> [TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Bool
shadowsSlots defined abilities =
  let declaredOf ability =
        foldMap (Map.keysSet . Mode.targetSlots) (Modal.modes (TriggeredAbility.modal ability))
   in not (Set.disjoint defined (foldMap declaredOf abilities))

-- shadowsSlots for one face: the slots its own effects define, against the target
-- slots its delayed abilities declare.
shadowsDefinedSlot :: Face.Face Card.Type.Card -> Bool
shadowsDefinedSlot card =
  shadowsSlots
    (Resolve.definedSlots (cardAuthoredEffects card))
    (delayedDeclarations card)

modalTrigger ::
  TriggerCondition.TriggerCondition ->
  [Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] ->
  TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
modalTrigger condition modes =
  Mint.triggerOf
    condition
    Nothing
    (Modal.MkModal (Seq.fromList modes) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)

-- The slots an ARMING carrier declares as targets: the three carriers above that
-- can arm a CR 603.7 delayed ability, which is all of them except the delayed
-- abilities themselves. CR 603.7c captures the whole environment of the object
-- that armed, so a target chosen for the arming spell is in the delayed ability's
-- bindings -- Ray of Command's "when you lose control of the creature" reads the
-- creature the spell targeted.
--
-- Its own declared target slots are EXCLUDED on purpose: the delayed-ability read lint
-- compares against those separately, per mode, and folding them in here would make
-- that comparison vacuous.
--
-- LOOSE about WHICH carrier armed, because nothing here tracks that: a delayed
-- ability reading a slot declared by an activated ability that does not arm it
-- would pass. Tightening it means threading the arming site through, which no card
-- in the pool needs -- Ray of Command arms from the spell that declares the slot.
armingTargetSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingTargetSlots card =
  Set.unions
    ( Map.keysSet (Card.allTargetSlots card)
        : fmap
          (Map.keysSet . Modal.allTargetSlots)
          ( fmap ActivatedAbility.modal (Face.activatedAbilities card)
              <> fmap TriggeredAbility.modal (Face.triggeredAbilities card)
          )
          -- A GRANTED arming ability's, whose quoted text is this card's too:
          -- Endless Whispers' "choose target opponent".
          <> fmap (Map.keysSet . Modal.allTargetSlots . ActivatedAbility.modal) (grantedActivatedAbilities card)
          <> fmap (Map.keysSet . Modal.allTargetSlots . TriggeredAbility.modal) (grantedTriggeredAbilities card)
    )

-- The other half of CR 603.7c's captured environment: the CR 603.2 event slots a
-- TRIGGERED arming ability's own condition stamped before it resolved. Ivory
-- Gargoyle's delayed return reads CR 400.7e's `became` -- the graveyard card the
-- dies trigger's own event named -- which no target slot and no Create can
-- supply.
--
-- Triggered abilities only, delayed ones included: a spell and an activation
-- are not events, so Parun.Engine.Event.Binding.eventBindings never runs for
-- either and neither can put an event slot in the environment it captures.
--
-- LOOSE about WHICH carrier armed, armingTargetSlots' own caveat and for its
-- reason: nothing here tracks that, so a delayed ability reading a slot bound by
-- a triggered ability that does not arm it would pass. Ivory Gargoyle prints one
-- triggered ability and it is the one that arms.
armingEventSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingEventSlots card =
  Set.unions
    ( fmap
        (\ability -> Set.union (Event.eventBindingSlots (TriggeredAbility.condition ability)) (Event.eventBindingSlotsSometimes (TriggeredAbility.condition ability)))
        (Face.triggeredAbilities card)
        -- And a granted trigger's: Endless Whispers' quoted dies trigger names
        -- `became` for its delayed return.
        <> fmap
          (\ability -> Set.union (Event.eventBindingSlots (TriggeredAbility.condition ability)) (Event.eventBindingSlotsSometimes (TriggeredAbility.condition ability)))
          (grantedTriggeredAbilities card)
        -- And a DELAYED ability's that itself arms one: Event.delayedPending
        -- stamps its event slots over the environment it captured, and its arm
        -- captures them again (CR 603.7c) -- Dr. Leonard "Bones" McCoy's "when it
        -- dies this turn" names `became` for its end-step return.
        <> fmap
          (\ability -> Set.union (Event.eventBindingSlots (TriggeredAbility.condition ability)) (Event.eventBindingSlotsSometimes (TriggeredAbility.condition ability)))
          (filter armsAnother (delayedDeclarations card))
    )
  where
    armsAnother ability = not (null [() | Effect.ArmDelayedTrigger _ <- concatMap effectWithNested (Modal.allEffects (TriggeredAbility.modal ability))])

-- The third half: the reserved slot an ACTIVATED arming ability's own cost
-- bound as it was paid (CR 400.7j), which Parun.Engine.Activate folds onto the
-- ability and CR 603.7c then captures -- Calim, Djinn Emperor's reflexive "return
-- Calim from your graveyard" reading Binding.discardedCard, The Guardian of
-- Forever's "that card" reading Binding.sacrificedPermanent -- and the player its
-- announcement chose (CR 115.10a, Lodestone Bauble's "that player"). LOOSE about
-- which activated ability armed, armingTargetSlots' caveat.
armingCostSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingCostSlots card =
  Set.unions
    ( fmap
        ( \ability ->
            let cost = ActivatedAbility.cost ability
             in Set.fromList ([Binding.discardedCard | discardsAsCost cost] <> [Binding.sacrificedPermanent | sacrificesAsCost cost] <> [Binding.exiledCard | exilesCardAsCost cost] <> foldMap (pure . PlayerSlot.slot) (ActivatedAbility.chosenPlayer ability))
        )
        (Face.activatedAbilities card)
    )

-- The fourth half: CR 106.6's spend trigger, armed at payment with "that spell"
-- bound as Binding.castSpell (Parun.Engine.Cost.armSpendTriggers) -- Pyromancer's
-- Goggles' copy. LOOSE about which ability that AddMana names, armingTargetSlots'
-- caveat.
armingSpendSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingSpendSlots card = if Set.null (spendArmedAbilities card) then Set.empty else Set.singleton Binding.castSpell

-- The fifth half: the reserved slot a CR 118.12 cost paid AS the arming spell or
-- ability RESOLVES binds (payGateBound), which Parun.Engine.Resolve.Common.foldPaid
-- writes onto the resolving object before CR 603.7c captures it -- Bolg of the
-- North's reflexive "that creature's power". LOOSE about which carrier armed,
-- armingTargetSlots' caveat.
armingPayGateSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingPayGateSlots card =
  Set.unions
    ( payGateBound (Face.spell card)
        : fmap (payGateBound . ActivatedAbility.modal) (Face.activatedAbilities card)
          <> fmap (payGateBound . TriggeredAbility.modal) (Face.triggeredAbilities card)
    )

-- The delayed abilities a face's AddMana instructions arm as their mana is spent.
spendArmedAbilities :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
spendArmedAbilities card =
  let named effect = case effect of
        Effect.AddMana addition -> fmap WhenSpent.ability (ManaAddition.whenSpent addition)
        Effect.Firebend addition -> fmap WhenSpent.ability (ManaAddition.whenSpent addition)
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named (cardAuthoredEffects card))

-- The delayed abilities a face's own casting permissions create as it is cast
-- under them (CR 603.7g, Spider Climb).
castArmedAbilities :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
castArmedAbilities card = Set.fromList [name | CastingPermission.CastAsThoughItHadFlash _ _ _ (Just name) <- Face.castingPermissions card]

-- The sixth half: the reserved slot Parun.Engine.Cast.armCastUnder binds the
-- spell under, and Parun.Engine.Stack.armBecame the permanent it becomes.
-- LOOSE about which ability reads it, armingSpendSlots' caveat.
armingCastSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingCastSlots card = if Set.null (castArmedAbilities card) then Set.empty else Set.singleton Keyword.Engine.becameSlot

-- Does a slot of this mode bound itself by a fold over a sibling slot (CR
-- 400.7j) in any shape but the one Target.membersGauge answers
-- exactly? Past Target.subsetBudget such a bound is judged fillable unasked
-- (Target.jointlyFillableGiven), and no printed card writes one.
siblingBoundOffends :: Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
siblingBoundOffends mode =
  let slots = Mode.targetSlots mode
      declared = Map.keysSet slots
      foldsSibling count = case Count.Type.scope count of
        Scope.OverBound _ name -> Set.member name declared
        _ -> False
      bounds slot = Maybe.maybeToList (TargetSlot.amount slot) <> Maybe.maybeToList (SlotCount.quantity (TargetSlot.count slot)) <> Maybe.maybeToList (TargetSlot.divided slot)
      offends slot = any (any foldsSibling . QuantitySlot.nestedCounts) (bounds slot) && Maybe.isNothing (Target.membersGauge declared slots slot)
   in any offends slots

-- Every modal a face carries, printed or granted.
siblingBoundCardOffends :: Face.Face Card.Type.Card -> Bool
siblingBoundCardOffends face =
  any (any siblingBoundOffends . Modal.modes) (faceModals face <> fmap ActivatedAbility.modal (grantedActivatedAbilities face) <> fmap TriggeredAbility.modal (grantedTriggeredAbilities face))

-- Does a spell or activated ability of this face, printed or granted, have a
-- mode reading a sibling mode's target slot (Target.crossModeReads)? Only a
-- trigger's placement weeds the selections no announcement completes
-- (Target.announceableSelections), and no printed spell or activation writes
-- one: "each mode must target a different player" is printed on triggers only.
crossModeCardOffends :: Face.Face Card.Type.Card -> Bool
crossModeCardOffends face =
  any Target.crossModeReads (Face.spell face : fmap ActivatedAbility.modal (Face.activatedAbilities face <> grantedActivatedAbilities face))

-- Does this mode write an apart or crowded joint check
-- Parun.Engine.Cast.castAimable prices only up to Target.subsetBudget
-- (Target.cappedWalk), or a wide Differing slot beside another slot, which
-- may share its candidates (Target.matchedSlots)? A named slot is split where
-- its printed count takes at most one target.
cappedWalkOffends :: Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
cappedWalkOffends mode =
  let slots = Mode.targetSlots mode
      declared = Map.keysSet slots
      single slot = case TargetSlot.count slot of
        SlotCount.Printed range -> maybe False (<= 1) (TargetCount.most range)
        _ -> False
      splittable name = maybe False (\slot -> single slot && not (Target.dependsOnSlot (TargetSlot.pool slot))) (Map.lookup name slots)
      split = Set.filter splittable (Set.fromList (Maybe.mapMaybe (Target.scopeSlot . TargetSlot.pool) (Map.elems slots)))
      wide name = maybe False (not . single) (Map.lookup name slots)
      differing slot = case TargetSlot.together slot of
        Just (Together.Differing _) -> True
        _ -> False
   in ((not (Set.null (Target.apartSlots split slots)) || not (null (Target.crowdedPairs declared slots))) && any wide (Map.keys slots))
        || (Map.size slots > 1 && any (\(name, slot) -> differing slot && wide name) (Map.toList slots))

-- Every modal a face carries, printed or granted, against cappedWalkOffends.
cappedWalkCardOffends :: Face.Face Card.Type.Card -> Bool
cappedWalkCardOffends face =
  any (any cappedWalkOffends . Modal.modes) (faceModals face <> fmap ActivatedAbility.modal (grantedActivatedAbilities face) <> fmap TriggeredAbility.modal (grantedTriggeredAbilities face))

abilitySlotLintSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> [(FilePath, Either Text.Text Card.Type.Card)] -> n ()
abilitySlotLintSpec s registry corpus = Spec.describe s "Lint" $ do
  -- The AbilityName half of the D4 dataflow lint (CR 603.7): an
  -- ArmDelayedTrigger naming an ability the card does not declare is a FAILING
  -- TEST, never a trigger that silently never fires. Equality, not subset: a
  -- declared ability nothing arms is dead card text.
  --
  -- SCOPE: `cardResolutionEffects`, every effect the card authors -- its
  -- abilities' as well as its spell modes' -- and not `Card.allEffects`, which is
  -- the spell modes alone. Meandering
  -- Towershell is what makes the difference load-bearing: it arms from a
  -- TRIGGERED ability, so the narrower view saw a declared entry that nothing
  -- appeared to arm and failed the equality outright.
  --
  -- The "every slot a delayed ability reads is one its card defines" lint below
  -- takes the same wide view, for the same reason: nothing about where a Create
  -- binds its minted tokens is peculiar to a spell mode.
  --
  -- cardAuthoredEffects and not cardResolutionEffects, for the reason that
  -- function gives: a CR 103.6 opening-hand action can arm one. A casting
  -- permission names one too (castArmedAbilities).
  Spec.it s "every armed delayed ability is declared, and every declared one is armed" $ do
    ps <- S.allPrintings s corpus
    let cardOffends card =
          Set.union (castArmedAbilities card) (Resolve.armedAbilities (cardAuthoredEffects card)) /= Map.keysSet (Face.delayedAbilities card)
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling or unused delayed abilities" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 603.7a / 613.1f: an arm inside a QUOTED ability carries its delayed
  -- ability rather than naming one, since the creature using the quotation is
  -- not the card that printed it and Game.armedDelayedAbility would look the name
  -- up on the wrong card. Splinter Twin and Sakashima the Impostor are the
  -- printings.
  Spec.it s "CR 603.7a every arm inside a quoted ability carries its delayed ability" $ do
    ps <- S.allPrintings s corpus
    let quotedEffects card =
          concatMap effectWithNested (concatMap (Modal.allEffects . ActivatedAbility.modal) (grantedActivatedAbilities card) <> concatMap (Modal.allEffects . TriggeredAbility.modal) (grantedTriggeredAbilities card))
        namesOnly effect = case effect of
          Effect.ArmDelayedTrigger arm -> Maybe.isNothing (ArmDelayedTrigger.ability arm)
          _ -> False
        cardOffends card = any namesOnly (quotedEffects card)
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no quoted arm naming its delayed ability" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 603.7: what an arm carries is a delayed TRIGGERED ability, the only kind
  -- Game.carriedDelayedAbility reads.
  Spec.it s "CR 603.7 every carried delayed ability is a triggered one" $ do
    ps <- S.allPrintings s corpus
    let notTriggered effect = case effect of
          Effect.ArmDelayedTrigger arm -> case ArmDelayedTrigger.ability arm of
            Just (GrantedAbility.Triggered _) -> False
            Just _ -> True
            Nothing -> False
          _ -> False
        cardOffends card = any notTriggered (cardAuthoredEffects card)
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no carried ability of another kind" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 106.6 / 603.7a: a spend trigger fires when it is armed, the payment having
  -- already been the event (Parun.Engine.Cost.armSpendTriggers), so the ability it
  -- names has to say so. Any other condition would wait for an event that never
  -- comes.
  Spec.it s "CR 603.7a every ability a spend trigger names is reflexive" $ do
    ps <- S.allPrintings s corpus
    let cardOffends card = any (\name -> fmap TriggeredAbility.condition (Map.lookup name (Face.delayedAbilities card)) /= Just TriggerCondition.Reflexive) (Set.toList (spendArmedAbilities card))
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no spend trigger waiting on another event" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint above joins names WITHIN a card; this one keeps that namespace clear
  -- of rule 702's. Parun.Engine.Keyword.mintedDelayedAbilities declares decayed's
  -- "sacrifice it at end of combat" under a name of its own, and
  -- Parun.Engine.Resolve looks a card's declarations up first -- so a card printing
  -- the same name would shadow the rule for any of its permanents holding the
  -- keyword.
  Spec.it s "CR 603.7 no card declares a delayed ability rule 702 already names" $ do
    ps <- S.allPrintings s corpus
    let cardOffends card = not (Map.null (Map.restrictKeys (Face.delayedAbilities card) (Map.keysSet Keyword.Engine.mintedDelayedAbilities)))
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no card shadows a minted delayed ability" (fmap (S.nameOf . Printing.card) offenders) []
  -- The OTHER AbilityName join (CR 613.1f), the delayed-ability lint's shape one
  -- rule over: a Modification.LoseNamedAbility naming an ability its face does
  -- not declare is a FAILING TEST, never a removal that silently removes nothing.
  -- Equality, not subset: a named ability nothing removes is a name for nobody.
  --
  -- Per FACE, because the name is written on one face's ability and read by that
  -- same face's text -- so two faces may reuse a name without colliding.
  --
  -- CR 607.2a's ExiledPile.OfAbility is the name's third reader, wherever a pile
  -- is written (namedExileReferences), and joins the same namespace: a
  -- reference naming no exiling ability of its face would read an empty pile.
  -- It must name an ACTIVATED ability, the one carrier
  -- Parun.Engine.Resolve.Slots.resolvingAbilityName files a name for.
  --
  -- ActivationCriteria.whichAbility is the fourth (namedActivationCriteria),
  -- and must name an ACTIVATED ability too: a CR 601.2f cost reduction or a CR
  -- 602.2 count naming anything else finds nothing. A GRANTED cost reduction
  -- (namedGrantedActivationCriteria) must name an activated ability the SAME
  -- static ability grants, since both land on the grantee (CR 613.1f): The
  -- Dominion Bracelet's "This ability costs {X} less to activate".
  --
  -- A removal may name any carrier, the rule and player abilities CR 116.2d's
  -- ignore names included (Synthetic Grounded Sentry, Synthetic Hushed Warden),
  -- so the "unused" half reads every reader: a named ability is used when a
  -- removal, an exile reference, a cost reduction or an ignore reads it.
  Spec.it s "CR 613.1f/607.2a/601.2f every named removal, exile reference or cost reduction names an ability its face declares, and every named ability is read" $ do
    ps <- S.allPrintings s corpus
    let offenders = filter (anyFace abilityNameOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling or unused ability names" (fmap (S.nameOf . Printing.card) offenders) []
  -- The case above's exile half in every position a pile can be written --
  -- a Quantity, a Filter and a CR 601.3 permission pool -- on a fixture whose
  -- one activated ability is named "warden". Each pile naming "warden" is
  -- accepted and the same pile naming "nobody" rejected, a pair differing in
  -- the name alone, so no position is silently unread.
  Spec.it s "CR 607.2a a named exile pile is read in a quantity, a filter and a permission pool" $ do
    let named n = AbilityName.MkAbilityName (Text.pack n)
        pileOf n = ExiledPile.OfAbility (named n)
        you = PlayerRef.Relative PlayerRelation.You
        positions =
          [ ("quantity", \n -> Effect.Draw (Draw.MkDraw you (Quantity.Type.AgainstExiledPile (AgainstExiledPile.MkAgainstExiledPile (pileOf n) (Quantity.Type.Literal 1))) Nothing Nothing)),
            ("filter", \n -> Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices you Nothing (Filter.Type.IsExiledWithSource (pileOf n)) (Quantity.Type.Literal 1) Nothing)),
            ("permission pool", \n -> Effect.AffectPlayers (AffectPlayers.MkAffectPlayers Duration.UntilEndOfTurn (AffectedPlayers.Scoped (PlayerScope.Related PlayerRelation.You)) (PlayerEffect.CastFrom (CastFromZone.MkCastFromZone (Just (InZone.MkInZone Zone.Exile (PlayerRef.Relative PlayerRelation.AnyPlayer))) (Filter.Type.And []) PermissionLimit.Unlimited PermissionVerb.Cast (PermissionPool.InExiledPile (pileOf n)) [] 0 ManaSpending.AsProduced Nothing Nothing False Nothing Set.empty False))))
          ]
        fixture effect = (vanillaFace "Synthetic Dangling Pile" instantLine) {Face.activatedAbilities = [(oneEffectActivated Nothing effect) {ActivatedAbility.name = Just (named "warden")}]}
        verdict (label, plant) = (label, abilityNameOffends (fixture (plant "warden")), abilityNameOffends (fixture (plant "nobody")))
    Spec.assertEqWith
      s
      "each position's own name is accepted and a dangling one rejected"
      (fmap verdict positions)
      (fmap (\(label, _) -> (label, False, True)) positions)
  -- See manaExileLinkOffends. When a printed card trips this, its PR links a
  -- mana ability's cost exiles, proves it on that card and deletes the lint.
  Spec.it s "CR 607.2a no card writes a mana ability whose cost exiles cards beside a reader of their pile" $ do
    ps <- S.allPrintings s corpus
    let exiling face = any (\ability -> ManaAbility.isManaAbility ability && linksExilesAsCost (ActivatedAbility.cost ability)) (Face.activatedAbilities face)
        offenders = filter (anyFace manaExileLinkOffends . Printing.card) ps
    Spec.assertBool s (any (anyFace exiling . Printing.card) ps) "the pool has a mana ability whose cost exiles cards"
    Spec.assertEqWith s "no mana ability's cost exiles are read" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint itself, on a doctored Cryptex given "{3}: Draw a card for each
  -- card exiled with" it. Each pile its mana ability would link is rejected;
  -- the same reader is accepted beside the mana ability stripped of collect
  -- evidence, and a pile named for its other ability.
  Spec.it s "the lint itself catches a mana ability whose cost exiles cards beside a reader of their pile" $ do
    cryptex <- S.printingOf s registry "Cryptex"
    let evidence = AbilityName.MkAbilityName (Text.pack "evidence")
        unlock = AbilityName.MkAbilityName (Text.pack "unlock")
        face = S.combinedFace cryptex
        you = PlayerRef.Relative PlayerRelation.You
        reader pile = oneEffectActivated Nothing (Effect.Draw (Draw.MkDraw you (Quantity.Type.AgainstExiledPile (AgainstExiledPile.MkAgainstExiledPile pile (Quantity.Type.Literal 1))) Nothing Nothing))
        nameBoth = zipWith (\name ability -> ability {ActivatedAbility.name = Just name}) [evidence, unlock]
        stripped ability = if ManaAbility.isManaAbility ability then ability {ActivatedAbility.cost = (ActivatedAbility.cost ability) {Cost.Type.components = filter (not . isEvidence) (Cost.Type.components (ActivatedAbility.cost ability))}} else ability
        isEvidence component = case component of
          CostComponent.CollectEvidence {} -> True
          _ -> False
        doctored edit pile = face {Face.activatedAbilities = edit (Face.activatedAbilities face) <> [reader pile]}
    Spec.assertBool s (not (manaExileLinkOffends face)) "the real Cryptex is accepted"
    Spec.assertBool s (manaExileLinkOffends (doctored id ExiledPile.OwnAbilities)) "its own abilities' pile is rejected"
    Spec.assertBool s (manaExileLinkOffends (doctored id ExiledPile.LastChosen)) "the last chosen card of that pile is rejected"
    Spec.assertBool s (manaExileLinkOffends (doctored nameBoth (ExiledPile.OfAbility evidence))) "the mana ability's own named pile is rejected"
    Spec.assertBool s (not (manaExileLinkOffends (doctored nameBoth (ExiledPile.OfAbility unlock)))) "a pile named for its other ability is accepted"
    Spec.assertBool s (not (manaExileLinkOffends (doctored (fmap stripped) ExiledPile.OwnAbilities))) "the mana ability without collect evidence is accepted"
  -- See manaYieldSlotOffends. When a printed card trips this, its PR prices
  -- that yield in Parun.Engine.Mana.manaSuppliesGiven, proves it on that card
  -- and deletes the lint.
  Spec.it s "CR 605.1a no card writes a mana ability whose yield reads an unpriced payment of its cost" $ do
    ps <- S.allPrintings s corpus
    let abilities face = Face.activatedAbilities face <> grantedActivatedAbilities face
        offenders = filter (anyFace (any manaYieldSlotOffends . abilities) . Printing.card) ps
    Spec.assertEqWith s "no mana ability's yield reads an unpriced payment" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint itself, on a doctored Priest of Yawgmoth ("Sacrifice an artifact:
  -- Add an amount of {B} equal to the sacrificed artifact's mana value") and a
  -- doctored Red Mana Battery.
  Spec.it s "the lint itself catches a mana ability whose yield reads an unpriced payment of its cost" $ do
    priest <- S.printingOf s registry "Priest of Yawgmoth"
    battery <- S.printingOf s registry "Red Mana Battery"
    let manaOf printing = filter ManaAbility.isManaAbility (Face.activatedAbilities (S.combinedFace printing))
        onComponents f ability = ability {ActivatedAbility.cost = (ActivatedAbility.cost ability) {Cost.Type.components = concatMap f (Cost.Type.components (ActivatedAbility.cost ability))}}
        onYields f ability =
          let modal = ActivatedAbility.modal ability
           in ability {ActivatedAbility.modal = modal {Modal.modes = fmap (\mode -> mode {Mode.clauses = fmap (\clause -> clause {Clause.effects = fmap f (Clause.effects clause)}) (Mode.clauses mode)}) (Modal.modes modal)}}
        reading slot effect = case effect of
          Effect.AddMana addition
            | Quantity.Type.AgainstSlot against <- ManaAddition.count addition ->
                Effect.AddMana addition {ManaAddition.count = Quantity.Type.AgainstSlot against {AgainstSlot.slot = slot}}
          _ -> effect
        sacrificing f component = case component of
          CostComponent.Sacrifice sacrifice -> f sacrifice
          _ -> [component]
        twice sacrifice = [CostComponent.Sacrifice sacrifice {Sacrifice.count = ObjectCount.Exactly (CostAmount.Fixed 2)}]
        alongside sacrifice = [CostComponent.Sacrifice sacrifice, CostComponent.Sacrifice sacrifice]
        turning state sacrifice = [CostComponent.TapPermanents (TapPermanents.MkTapPermanents (CostAmount.Fixed 1) (Sacrifice.whichPermanents sacrifice) False state)]
        announced component = case component of
          CostComponent.RemoveCounters removal -> [CostComponent.RemoveCounters removal {CountersFromPermanents.count = ObjectCount.Exactly (CostAmount.Announced Binding.variableX)}]
          _ -> [component]
        verdicts edit printing = fmap (manaYieldSlotOffends . edit) (manaOf printing)
    Spec.assertEqWith s "the real cards are accepted" (verdicts id priest <> verdicts id battery) [False, False]
    Spec.assertEqWith s "two permanents sacrificed are rejected" (verdicts (onComponents (sacrificing twice)) priest) [True]
    Spec.assertEqWith s "a second one-object choice beside the sacrifice is rejected" (verdicts (onComponents (sacrificing alongside)) priest) [True]
    Spec.assertEqWith s "the tapped creature's power is rejected" (verdicts (onYields (reading Binding.tappedPermanent) . onComponents (sacrificing (turning TapState.Tapped))) priest) [True]
    Spec.assertEqWith s "the untapped land's, Benthic Explorers' shape, is accepted" (verdicts (onYields (reading Binding.untappedPermanent) . onComponents (sacrificing (turning TapState.Untapped))) priest) [False]
    Spec.assertEqWith s "a discarded card's mana value is rejected" (verdicts (onYields (reading Binding.discardedCard)) priest) [True]
    Spec.assertEqWith s "counters removed X of are rejected" (verdicts (onComponents announced) battery) [True]
  -- See manaEffectSlotOffends. When a printed card trips this, its PR carries
  -- the payment's objects to those effects in
  -- Parun.Engine.Cost.tapForManaWith, proves it on that card and deletes the
  -- lint.
  Spec.it s "CR 405.6c no card writes a mana ability whose other effects read an object its cost paid" $ do
    ps <- S.allPrintings s corpus
    let abilities face = Face.activatedAbilities face <> grantedActivatedAbilities face
        offenders = filter (anyFace (any manaEffectSlotOffends . abilities) . Printing.card) ps
    Spec.assertEqWith s "no mana ability's other effects read a paid object" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint itself, on a doctored Springjack Pasture ("{T}, Sacrifice X
  -- Goats: Add X mana of any one color. You gain X life.") whose life reads
  -- Priest of Yawgmoth's "the sacrificed artifact's mana value".
  Spec.it s "the lint itself catches a mana ability whose other effects read an object its cost paid" $ do
    pasture <- S.printingOf s registry "Springjack Pasture"
    priest <- S.printingOf s registry "Priest of Yawgmoth"
    let manaOf printing = filter ManaAbility.isManaAbility (Face.activatedAbilities (S.combinedFace printing))
        sacrificedValue = case [ManaAddition.count addition | ability <- manaOf priest, Effect.AddMana addition <- Modal.allEffects (ActivatedAbility.modal ability)] of
          quantity : _ -> quantity
          [] -> Quantity.Type.Literal 0
        onClauses f ability =
          let modal = ActivatedAbility.modal ability
           in ability {ActivatedAbility.modal = modal {Modal.modes = fmap (\mode -> mode {Mode.clauses = fmap f (Mode.clauses mode)}) (Modal.modes modal)}}
        gaining quantity clause = clause {Clause.effects = fmap (\effect -> case effect of Effect.GainLife gain -> Effect.GainLife gain {PlayerQuantity.quantity = quantity}; _ -> effect) (Clause.effects clause)}
        gated clause = clause {Clause.condition = Just (Condition.Type.Compares (Compares.MkCompares sacrificedValue Comparison.AtLeast (Quantity.Type.Literal 1)))}
        verdicts edit printing = fmap (manaEffectSlotOffends . edit) (manaOf printing)
    Spec.assertEqWith s "the real cards are accepted, Priest of Yawgmoth's read in its yield" (verdicts id pasture <> verdicts id priest) [False, False, False]
    Spec.assertEqWith s "life equal to the sacrificed permanent's mana value is rejected" (verdicts (onClauses (gaining sacrificedValue)) pasture) [False, True]
    Spec.assertEqWith s "a clause condition reading it is rejected" (verdicts (onClauses gated) pasture) [True, True]
    Spec.assertEqWith s "the counters a cost removed, an amount, are accepted" (verdicts (onClauses (gaining (Quantity.Type.InSlot Binding.removedCounters))) pasture) [False, False]
  -- The granted half (namedGrantedActivationCriteria) on The Dominion Bracelet,
  -- whose one static ability grants "dominate" and the cost reduction naming
  -- it. Renaming either side dangles; so does splitting the grant across two
  -- static abilities, which only the same-grant check sees, since the face
  -- still declares and reads "dominate".
  Spec.it s "CR 601.2f a granted cost reduction names an ability the same grant declares" $ do
    ps <- S.allPrintings s corpus
    let bracelets = [face | p <- ps, S.nameOf (Printing.card p) == CardName.MkCardName (Text.pack "The Dominion Bracelet"), face <- Foldable.toList (Card.Type.faces (Printing.card p))]
        nobody = AbilityName.MkAbilityName (Text.pack "nobody")
        onGrants f face = face {Face.staticAbilities = fmap (\a -> a {StaticAbility.modifications = fmap f (StaticAbility.modifications a)}) (Face.staticAbilities face)}
        renameAbility m = case m of
          Modification.GainAbility (GrantedAbility.Activated ability) -> Modification.GainAbility (GrantedAbility.Activated ability {ActivatedAbility.name = Just nobody})
          _ -> m
        renameModifier m = case m of
          Modification.GainAbility (GrantedAbility.Player ability)
            | PlayerEffect.ModifyCost modifier <- PlayerStaticAbility.effect ability,
              CostSubject.Activations criteria <- CostModifier.subject modifier ->
                Modification.GainAbility (GrantedAbility.Player ability {PlayerStaticAbility.effect = PlayerEffect.ModifyCost modifier {CostModifier.subject = CostSubject.Activations criteria {ActivationCriteria.whichAbility = Just nobody}}})
          _ -> m
        isPlayerGrant m = case m of
          Modification.GainAbility (GrantedAbility.Player _) -> True
          _ -> False
        split face =
          face
            { Face.staticAbilities =
                concat
                  [ [a {StaticAbility.modifications = ms} | Just ms <- [NonEmpty.nonEmpty part]]
                  | a <- Face.staticAbilities face,
                    part <- [NonEmpty.filter (not . isPlayerGrant) (StaticAbility.modifications a), NonEmpty.filter isPlayerGrant (StaticAbility.modifications a)]
                  ]
            }
        verdict face = (abilityNameOffends face, abilityNameOffends (onGrants renameAbility face), abilityNameOffends (onGrants renameModifier face), abilityNameOffends (split face))
    Spec.assertEqWith s "the printed grant is accepted and each doctored one rejected" (fmap verdict bracelets) [(False, True, True, True)]
  -- CR 116.2d's join, the same shape one rule over: an ignore naming an ability
  -- its face does not declare as ignorable is a payment that suppresses
  -- nothing. Whether every named ability is read is the case above's question,
  -- since a removal may read an ignorable carrier's name too.
  --
  -- BOTH AXES in one namespace, because CR 116.2d's grant does not know which:
  -- Damping Engine names two player abilities, and Volrath's Curse names two
  -- combat restrictions and an activation prohibition, one sentence each.
  Spec.it s "CR 116.2d every ignore names an ignorable ability its face declares" $ do
    ps <- S.allPrintings s corpus
    let faceOffends face = not (Set.isSubsetOf (ignoredAbilityNames face) (declaredIgnorableAbilityNames face))
        offenders = filter (anyFace faceOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling ignore names" (fmap (S.nameOf . Printing.card) offenders) []
  -- Every slot a delayed ability READS must be one the arming card DEFINES:
  -- the reserved trigger-source slot, a token bound by a Create, the
  -- incarnation a MoveToZone bound at its destination (Meandering Towershell's
  -- exiled card), a TARGET the arming carrier declared (armingTargetSlots, which
  -- is CR 603.7c's captured environment -- Ray of Command's third sentence), an
  -- event slot a TRIGGERED arming ability's condition stamped into that same
  -- environment (armingEventSlots -- Ivory Gargoyle's delayed return reading CR
  -- 400.7e's `became`, or a delayed one's -- Dr. Leonard "Bones" McCoy's), a
  -- slot an ACTIVATED arming ability's own cost bound
  -- (armingCostSlots -- Calim, Djinn Emperor's discarded card), a slot an
  -- arming ability's resolution-time cost bound (armingPayGateSlots -- Bolg of
  -- the North's sacrificed creature), the permanent a spell that armed as it
  -- was cast becomes (armingCastSlots -- Spider Climb), or a CR 603.2
  -- event slot the entry's own condition binds as it fires
  -- (Event.eventBindingSlots -- False Cure's "that player ... they gained", which
  -- Event.delayedPending stamps on top of the captured environment exactly as
  -- eventTriggers does for an object's trigger). The
  -- `abilityBound` side is `cardResolutionEffects` for the
  -- reason the lint above takes it: the binding effect can live in the ability
  -- that arms, not only in a spell mode.
  --
  -- Through modalSlotsOffend, so a delayed ability with modes is read PER MODE
  -- (#570) -- and so a mode's own declared target slots count, which this lint
  -- omitted entirely. CR 603.3d puts a delayed ability on the stack "identical
  -- to the process for casting a spell listed in rules 601.2c-d", so a slot it
  -- declares really is announced; declaredTargetSlots already counts delayed
  -- abilities' target slots on the DECLARING side, and this is the matching read
  -- side. Since #1043 that comparison is the spell lint's EQUALITY, so a delayed
  -- ability declaring a slot no effect of its reads fails here too.
  --
  -- Over withMinted, each face against its OWN cardBound: a token arms from its
  -- own abilities and is minted with empty bindings (Resolve.Slots), so
  -- nothing the minting resolution bound is in scope.
  Spec.it s "every slot a delayed ability reads is bound by its card" $ do
    ps <- S.allPrintings s corpus
    let cardBound card = Set.insert Binding.triggerSource (Set.unions [armingTargetSlots card, armingEventSlots card, armingCostSlots card, armingPayGateSlots card, armingSpendSlots card, armingCastSlots card, Resolve.definedSlots (cardResolutionEffects card)])
        abilityOffends card ability =
          modalSlotsOffend
            (armedReads card)
            (Set.unions [cardBound card, Event.eventBindingSlots (TriggeredAbility.condition ability), Event.eventBindingSlotsSometimes (TriggeredAbility.condition ability)])
            (TriggeredAbility.modal ability)
        -- The CONDITION's own slot, which modalSlotsOffend never sees: it walks
        -- the ability's modes, and CR 603.7's slot-named condition sits beside
        -- them. A SUBSET rather than the equality above, and against cardBound
        -- alone: the condition is matched before CR 603.3d puts the ability on
        -- the stack, so a slot the ability's own mode DECLARES is not yet
        -- announced, and a slot Event.eventBindingSlots names is bound by the
        -- match rather than available to it.
        conditionOffends bound ability = not (Set.isSubsetOf (Set.fromList (triggerConditionSlots (TriggeredAbility.condition ability))) bound)
        cardOffends card = any (\ability -> abilityOffends card ability || conditionOffends (cardBound card) ability) (delayedDeclarations card)
        sweeps = anyFaceOrMinted cardOffends
        offenders = filter (sweeps . Printing.card) ps
        watching slot = modalTrigger (TriggerCondition.LosesControl (LostPermanent.Bound slot)) [lintMode [] []]
        armingSlot = SlotName.MkSlotName (Text.pack "target")
        strangerSlot = SlotName.MkSlotName (Text.pack "elsewhere")
        tokenTapping slot =
          (vanillaFace "Token" instantLine)
            { Face.delayedAbilities = Map.singleton (AbilityName.MkAbilityName (Text.pack "later")) (modalTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) [lintMode [Effect.Tap (Tap.plain (ObjectRef.InSlot slot))] []])
            }
    Spec.assertBool s (sweeps (mintingSpell createToken (tokenTapping strangerSlot))) "a token's delayed ability reading a slot the token never binds is caught"
    Spec.assertBool s (not (sweeps (mintingSpell createToken (tokenTapping Binding.triggerSource)))) "and one reading its own source is accepted"
    -- The condition half is vacuous as a corpus sweep: every slot-named
    -- condition in the pool names a slot its own spell declares (Ray of
    -- Command's target), so the sweep would pass under a predicate that always
    -- answered False. The REJECTING direction is therefore put to a hand-built
    -- pair differing in the slot name alone, the posture the batch-bound lint
    -- below takes.
    Spec.assertBool s (conditionOffends (Set.singleton armingSlot) (watching strangerSlot)) "a condition naming a slot the card never binds is caught"
    Spec.assertBool s (not (conditionOffends (Set.singleton armingSlot) (watching armingSlot))) "a condition naming the arming spell's target slot is left alone"
    -- armingEventSlots' delayed half, put to a pair differing in whether the
    -- delayed ability that dies-triggers arms the one reading its `became`.
    let returning = AbilityName.MkAbilityName (Text.pack "return")
        nesting arms =
          (vanillaFace "Nester" instantLine)
            { Face.delayedAbilities =
                Map.fromList
                  [ (AbilityName.MkAbilityName (Text.pack "dies"), modalTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) [lintMode [Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger returning Onset.Immediately Nothing Nothing Nothing) | arms] []]),
                    (returning, modalTrigger (TriggerCondition.LosesControl (LostPermanent.Bound Binding.triggerSource)) [lintMode [Effect.Tap (Tap.plain (ObjectRef.InSlot Binding.became))] []])
                  ]
            }
    Spec.assertBool s (not (cardOffends (nesting True))) "a delayed ability reading the event slot of the delayed ability arming it is accepted"
    Spec.assertBool s (cardOffends (nesting False)) "and one reading the event slot of a delayed ability arming nothing is caught"
    -- armingCostSlots' sacrifice half, put to The Guardian of Forever and the
    -- same card with its cost's sacrifice stripped.
    guardian <- S.combinedFace <$> S.printingOf s registry "The Guardian of Forever"
    let unsacrificed face = face {Face.activatedAbilities = fmap (\ability -> ability {ActivatedAbility.cost = (ActivatedAbility.cost ability) {Cost.Type.components = []}}) (Face.activatedAbilities face)}
    Spec.assertBool s (not (cardOffends guardian)) "a delayed ability reading the activation's sacrificed permanent is accepted"
    Spec.assertBool s (cardOffends (unsacrificed guardian)) "and the same read under a cost sacrificing nothing is caught"
    Spec.assertEqWith s "no dangling delayed-ability slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- A delayed ability may not DECLARE a target slot under a name its own card
  -- already DEFINES, because the two would land in one slot and the reader would
  -- have to pick. Parun.Engine.Engine.placeOne merges the ability's placement-time
  -- choices with the environment captured when it was armed, per FIELD, so a
  -- collision leaves one Binding carrying both a CR 601.2c target and a Create's
  -- minted group -- and Parun.Engine.Resolve.Slots.slotGroup answers with the group,
  -- silently discarding a target that CR 608.2b was owed a re-validation of.
  --
  -- Rejected rather than resolved by precedence: the card would be saying two
  -- different things under one name, which is a card-data mistake and not a rules
  -- question the engine should have an answer to. The neighbouring "every slot a
  -- delayed ability reads is bound by its card" lint could not catch it while it
  -- was a SUBSET check: both sides were on the available list, so a name
  -- appearing in both passed it twice over. Under #1043's equality it rejects the
  -- same shape as a side effect, whether or not the ability reads the name back
  -- -- a read is answered by the bound side and so cancels, and no read leaves the
  -- read side empty, so either way the declared slot goes unmatched. Kept anyway:
  -- it states the claim that is actually true of the data (a name may not be both
  -- declared and defined) rather than deriving it from a dataflow count, and it
  -- names the offending card outright.
  Spec.it s "no delayed ability declares a target slot under a name its card defines" $ do
    ps <- S.allPrintings s corpus
    let offenders = filter (anyFace shadowsDefinedSlot . Printing.card) ps
    Spec.assertEqWith s "no delayed ability shadows a defined slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above is vacuous on its own -- no card offends, and none would
  -- under a predicate that always answered False -- so both directions are put
  -- to a hand-built pair. The accepted one is Thatcher Revolt's exact shape,
  -- which must stay legal: reading a defined slot is the whole point, and only
  -- DECLARING one is the mistake.
  Spec.it s "the shadowing lint accepts a delayed ability that only reads the slot" $ do
    let tokens = SlotName.MkSlotName (Text.pack "tokens")
        reads_ = modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [lintMode [Effect.Sacrifice SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot tokens, SacrificeEffect.sacrificer = Sacrificer.EffectController, SacrificeEffect.sacrificed = Nothing, SacrificeEffect.permanents = Nothing}] []]
        declares = modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [lintMode [Effect.Sacrifice SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot tokens, SacrificeEffect.sacrificer = Sacrificer.EffectController, SacrificeEffect.sacrificed = Nothing, SacrificeEffect.permanents = Nothing}] [tokens]]
    Spec.assertBool s (not (shadowsSlots (Set.singleton tokens) [reads_])) "reading a Create's slot is legal"
    Spec.assertBool s (shadowsSlots (Set.singleton tokens) [declares]) "declaring a target slot under the same name is not"
  -- The pairing Parun.Types.Onset.FromNextTurnOf depends on and cannot enforce
  -- alone. See onsetOffends for why the onset and the condition's TurnScope
  -- are two halves of one printed "your next turn", and what goes wrong when a
  -- card supplies only one of them.
  Spec.it s "every delayed ability armed for YOUR next turn is controller-scoped" $ do
    ps <- S.allPrintings s corpus
    let offenders = filter (anyFace onsetOffends . Printing.card) ps
    Spec.assertEqWith s "no onset over a condition that admits another player's turn" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above is NOT vacuous -- Meandering Towershell is a real card with
  -- an onset, so the accepting direction is exercised by the pool -- but nothing
  -- committed offends it, so the REJECTING direction is proven here instead,
  -- against that same card misauthored on purpose. Never a card file: a card
  -- that offends a lint must not be loadable.
  Spec.it s "the lint itself catches an onset over an each-turn condition" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    let face = S.combinedFace towershell
        -- The Towershell's own condition with CR 603.2b's OTHER turn scope: "at
        -- the beginning of EACH declare attackers step", which an opponent's
        -- turn satisfies. Built rather than pattern-matched, so this fixture
        -- states the offence outright.
        eachTurn ability =
          ability
            { TriggeredAbility.condition =
                TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Combat CombatStep.DeclareAttackers) Nothing (TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.AnyPlayer)))
            }
        widened = face {Face.delayedAbilities = fmap eachTurn (Face.delayedAbilities face)}
        -- The other way a card can reach this: an onset naming an ability the
        -- card does not declare at all.
        dangling = face {Face.delayedAbilities = Map.empty}
        -- CR 508.3d, the other condition whose answer turns on a payload rather
        -- than on the constructor: CR 508.1 lets only the active player declare,
        -- so "whenever YOU attack" can only happen on the controller's turn and
        -- "whenever A PLAYER attacks" can happen on anyone's.
        attacksWith relation ability = ability {TriggeredAbility.condition = TriggerCondition.Attacks (AttackTrigger.Declaration (DeclarationTrigger.MkDeclarationTrigger relation Nothing TriggerFrequency.EveryTime Nothing))}
        youAttack = face {Face.delayedAbilities = fmap (attacksWith PlayerRelation.You) (Face.delayedAbilities face)}
        anyoneAttacks = face {Face.delayedAbilities = fmap (attacksWith PlayerRelation.AnyPlayer) (Face.delayedAbilities face)}
        -- CR 102.1: a damage trigger's turn scope, Fblthp, Impossibly Lost's
        -- "during your turn" against no turn named.
        damagedDuring relation ability =
          ability
            { TriggeredAbility.condition =
                TriggerCondition.PlayerIsDealtDamage
                  (PlayerIsDealtDamage.watching (DealtDamage.MkDealtDamage (PlayerScope.Related PlayerRelation.Opponent) Nothing Nothing)) {PlayerIsDealtDamage.scope = TurnScope.PlayersTurn (PlayerRef.Relative relation)}
            }
        yourTurnDamage = face {Face.delayedAbilities = fmap (damagedDuring PlayerRelation.You) (Face.delayedAbilities face)}
        anyTurnDamage = face {Face.delayedAbilities = fmap (damagedDuring PlayerRelation.AnyPlayer) (Face.delayedAbilities face)}
    Spec.assertBool s (not (onsetOffends face)) "the real card, your turn, is accepted"
    Spec.assertBool s (onsetOffends widened) "each turn under an onset is rejected"
    Spec.assertBool s (onsetOffends dangling) "and so is an onset naming no declared ability"
    Spec.assertBool s (not (onsetOffends youAttack)) "CR 508.3d \"whenever you attack\" is controller-scoped"
    Spec.assertBool s (onsetOffends anyoneAttacks) "and \"whenever a player attacks\" is not"
    Spec.assertBool s (not (onsetOffends yourTurnDamage)) "CR 102.1 damage \"during your turn\" is controller-scoped"
    Spec.assertBool s (onsetOffends anyTurnDamage) "and damage on any turn is not"
    -- Not a check that fires for every card: one with no onset at all has
    -- nothing for this to reject, whatever its delayed abilities are scoped to.
    tidalWave <- S.printingOf s registry "Tidal Wave"
    Spec.assertBool s (not (onsetOffends (S.combinedFace tidalWave))) "a card with no onset is not swept up"
  -- The same equality over a card's TRIGGERED abilities, which is where
  -- the condition-specific reserved slots live -- CR 400.7e's `became` and
  -- CR 702.70a's `thatPlayer`. See triggeredAbilityOffends for what answers a
  -- read there.
  Spec.it s "every slot a triggered ability reads is bound for its condition, and every slot it declares is read" $ do
    ps <- S.allPrintings s corpus
    let -- CR 400.7d: the permanent a spell becomes keeps the record its own
        -- cost bound (Binding.paidCostRecord), and every ability it triggers
        -- carries it -- so a face whose cost collects evidence may read the
        -- slot there (Vitu-Ghazi Inspector), and one whose cost waterbends
        -- (Katara, Seeking Revenge).
        inherited face =
          Set.unions
            [ if any collectsEvidenceAsCost (spellCostsOf face) then Set.singleton Binding.collectedEvidence else Set.empty,
              if any waterbendsAsCost (spellCostsOf face) then Set.singleton Binding.waterbendCost else Set.empty
            ]
        -- CR 107.3m: an enters-the-battlefield trigger's effects read the X
        -- announced for the spell (Engine.placeBorne's enteredX), so a face
        -- whose spell cost declares X may read it there (Pin Collection);
        -- CR 107.3i's cast trigger likewise (Genesis Hydra).
        enteredX face ability =
          if TriggeredAbility.condition ability `elem` [TriggerCondition.Enters EntersTrigger.defaultValue, TriggerCondition.SpellCast SpellCast.self] && any declaresVariable (spellCostsOf face)
            then Set.singleton Binding.variableX
            else Set.empty
        -- Per face over withMinted, so a minted face inherits only what its OWN
        -- costs bound.
        cardOffends face = any (\ability -> triggeredAbilityOffendsGiven (armedReads face) (inherited face <> enteredX face ability) ability) (Face.triggeredAbilities face)
        sweeps = anyFaceOrMinted cardOffends
        offenders = filter (sweeps . Printing.card) ps
        -- Rule 702.70a's shape, as a targetless read of "that player".
        tokenDrawing condition = (vanillaFace "Token" instantLine) {Face.triggeredAbilities = [oneEffectTrigger condition (Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1) Nothing Nothing))]}
    Spec.assertEqWith s "no dangling triggered-ability slot" (fmap (S.nameOf . Printing.card) offenders) []
    Spec.assertBool s (sweeps (mintingSpell createToken (tokenDrawing (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self))))) "a token's dies trigger reading thatPlayer is caught"
    Spec.assertBool s (not (sweeps (mintingSpell createToken (tokenDrawing Mint.dealsCombatDamageToAPlayer)))) "and its combat-damage trigger reading thatPlayer is accepted"
  -- The sweep above passes VACUOUSLY: no committed card misauthors the
  -- pairing, so the sweep proves nothing about the lint. Both directions are
  -- proven here instead, against a hand-built offender (never a card file --
  -- a misauthored card must not be loadable) and against the real pairing.
  --
  -- Both reserved event slots, because a classification that answered "every
  -- slot, always" would pass the offending half of either one alone.
  Spec.it s "the lint itself catches a reserved event slot the condition never binds" $ do
    roaches <- S.printingOf s registry "Endless Cockroaches"
    let -- Endless Cockroaches' own payload: "return it to its owner's hand".
        returnIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing)
        -- Rule 702.70a's shape, as a targetless read of "that player".
        thatPlayerDraws = Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1) Nothing Nothing)
    Spec.assertBool
      s
      (triggeredAbilityOffends (oneEffectTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) returnIt))
      "CR 400.7e became under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) returnIt)))
      "and under a dies trigger it is accepted"
    Spec.assertBool
      s
      (triggeredAbilityOffends (oneEffectTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) thatPlayerDraws))
      "CR 702.70a thatPlayer under a dies trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger Mint.dealsCombatDamageToAPlayer thatPlayerDraws)))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (not (any triggeredAbilityOffends (Face.triggeredAbilities (S.combinedFace roaches))))
      "the real card's dies trigger is accepted"
  -- The same equality, on the read that is NOT an effect's operand: CR 603.2's
  -- "that player" may be named by a target slot's own FILTER (Trygon Predator),
  -- which Resolve.modeSlots sees through Filter.boundSlots. Without that clause
  -- the pairing below would be invisible and a card could narrow a slot by a
  -- player its condition never binds -- a slot that then admits nothing.
  Spec.it s "the lint itself catches a reserved event slot named by a target filter" $ do
    trygon <- S.printingOf s registry "Trygon Predator"
    let target = SlotName.MkSlotName (Text.pack "target")
        narrowed =
          Mode.MkMode
            (Seq.singleton (Mint.mandatory (Seq.singleton (Effect.Tap (Tap.plain (ObjectRef.InSlot target))))))
            (Map.singleton target (TargetSlot.required Pool.Permanents (Just (Filter.Type.HeldByBound Holder.Controller Binding.triggerPlayer))))
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [narrowed]))
      "CR 603.2 thatPlayer in a filter under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger Mint.dealsCombatDamageToAPlayer [narrowed])))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (not (any triggeredAbilityOffends (Face.triggeredAbilities (S.combinedFace trygon))))
      "the real card's own trigger is accepted"
  -- CR 601.2c's exemption for a sibling MODE's slot (modalSlotsOffend's
  -- withoutExclusions) is a target filter's Not (IsBound _) conjunct and no
  -- other read: an effect or another filter shape reading that slot reads
  -- nothing where the sibling mode is not chosen.
  Spec.it s "the lint itself catches a mode reading a sibling mode's slot outside an exclusion" $ do
    shadrix <- S.printingOf s registry "Shadrix Silverquill"
    let creator = SlotName.MkSlotName (Text.pack "creator")
        drawer = SlotName.MkSlotName (Text.pack "drawer")
        -- The drawing mode, its "different player" exclusion kept.
        doctored f ability =
          let modal = TriggeredAbility.modal ability
           in ability {TriggeredAbility.modal = modal {Modal.modes = Seq.adjust' f 1 (Modal.modes modal)}}
        creatorDraws mode = mode {Mode.clauses = Mode.clauses mode Seq.|> Mint.mandatory (Seq.singleton (Effect.Draw (Draw.MkDraw (PlayerRef.InSlot creator) (Quantity.Type.Literal 1) Nothing Nothing)))}
        sameAsCreator mode = mode {Mode.targetSlots = Map.adjust (\slot -> slot {TargetSlot.filter = Just (Filter.Type.IsBound creator)}) drawer (Mode.targetSlots mode)}
        triggers = Face.triggeredAbilities (S.combinedFace shadrix)
    Spec.assertBool s (not (any triggeredAbilityOffends triggers)) "the real card's exclusions are accepted"
    Spec.assertBool s (any (triggeredAbilityOffends . doctored creatorDraws) triggers) "an effect reading the sibling's slot is rejected"
    Spec.assertBool s (any (triggeredAbilityOffends . doctored sameAsCreator) triggers) "and so is a filter reading it other than as an exclusion"
  -- CR 700.2a: a modal spell or activated ability whose modes exclude each
  -- other's targets would be offered with selections no announcement
  -- completes, Target.announceableSelections running for triggers alone.
  Spec.it s "no spell or activated ability reads a sibling mode's target slot" $ do
    ps <- S.allPrintings s corpus
    let offenders = filter (anyFaceOrMinted crossModeCardOffends . Printing.card) ps
    Spec.assertEqWith s "no spell or activation excludes a sibling mode's target" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint itself, on Shadrix Silverquill's modes moved onto its spell and
  -- onto an activated ability: the trigger is accepted, the moved modes not.
  Spec.it s "the lint itself catches a spell or activated ability reading a sibling mode's target slot" $ do
    shadrix <- S.printingOf s registry "Shadrix Silverquill"
    let face = S.combinedFace shadrix
        modals = fmap TriggeredAbility.modal (Face.triggeredAbilities face)
        activated modal = (modalActivated []) {ActivatedAbility.modal = modal}
    Spec.assertBool s (not (crossModeCardOffends face)) "the real card's trigger is accepted"
    Spec.assertBool s (any (\modal -> crossModeCardOffends face {Face.spell = modal}) modals) "its modes as a spell are rejected"
    Spec.assertBool s (any (\modal -> crossModeCardOffends face {Face.activatedAbilities = [activated modal]}) modals) "and as an activated ability"
  -- The same equality on the slot's THIRD read, beside its pool's and its
  -- filter's: CR 202.3's computed bound (Resolve.targetSlotSlots). A bound may
  -- name CR 603.2's "that player" through a PlayerRef buried INSIDE the number --
  -- "with mana value X or less, where X is the amount of life that player gained
  -- this turn" -- and QuantitySlot.slots does not report that read, only
  -- QuantitySlot.nestedRefs does. Without it the pairing is invisible and a card could
  -- bound a slot by a player its condition never binds, which is the dead bound
  -- the fold exists to catch.
  Spec.it s "the lint itself catches a computed bound naming a slot through a player" $ do
    let target = SlotName.MkSlotName (Text.pack "target")
        -- Celestine, the Living Saint's own bound, one reference over: hers reads
        -- the caster's life gain, this reads the trigger's player.
        bounded amount =
          Mode.MkMode
            (Seq.singleton (Mint.mandatory (Seq.singleton (Effect.Tap (Tap.plain (ObjectRef.InSlot target))))))
            (Map.singleton target (TargetSlot.withAmount amount (TargetSlot.required Pool.Permanents (Just (Filter.Type.Measures (Measures.MkMeasures Measure.ManaValue Comparison.AtMost Operand.EnclosingAmount))))))
        thatPlayer = Quantity.Type.LifeChanged (LifeTally.MkLifeTally (PlayerRef.InSlot Binding.triggerPlayer) LifeDirection.Gained WhichTurn.ThisTurn)
        you = Quantity.Type.LifeChanged (LifeTally.MkLifeTally (PlayerRef.Relative PlayerRelation.You) LifeDirection.Gained WhichTurn.ThisTurn)
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [bounded thatPlayer]))
      "CR 603.2 thatPlayer inside a bound under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger Mint.dealsCombatDamageToAPlayer [bounded thatPlayer])))
      "and under a combat-damage trigger it is accepted"
    -- The pair that differs in exactly one thing: the same bound on the same
    -- trigger, reading CR 109.5's caster instead of a slot, stays accepted -- so
    -- the lint reads the reference rather than rejecting every computed bound.
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [bounded you])))
      "where a bound naming no slot at all is accepted"
    -- And the OTHER read QuantitySlot.slots does not report: CR 400.7j's fold,
    -- which names a slot outright rather than through a reference. Asserted on the map
    -- itself, there being no trigger condition that binds an ordinary target slot
    -- for the lint to accept it against.
    let overBound =
          Quantity.Type.Count
            ( Count.Type.MkCount
                (Scope.OverBound BoundReading.AsBound target)
                (Filter.Type.And [])
                Aggregation.Members
            )
    Spec.assertEqWith
      s
      "a bound folding over the objects a slot names reports that slot"
      (Map.keysSet (Resolve.targetSlotSlots (TargetSlot.withAmount overBound (TargetSlot.required Pool.Permanents Nothing))))
      (Set.singleton target)
  -- A bound folding over a sibling slot is answered exactly past
  -- Target.subsetBudget only in Target.membersGauge's shape; any other (another
  -- aggregation, a filter or pool reading the sibling, a gauge that is itself a
  -- reader) is judged fillable there, so a mode no announcement completes stays
  -- choosable (CR 700.2a). No printed card writes one; the synthetics below are
  -- exempt, and a printed card tripping this builds the exact answer.
  Spec.it s "no card writes a sibling bound only the capped search answers" $ do
    ps <- S.allPrintings s corpus
    let exempt = Set.fromList (fmap (CardName.MkCardName . Text.pack) ["Synthetic Total Verdict"])
        offenders = Set.fromList (fmap (S.nameOf . Printing.card) (filter (anyFaceOrMinted siblingBoundCardOffends . Printing.card) ps))
    Spec.assertEqWith s "only the exempt synthetics write a sibling bound the capped search answers" offenders exempt
  -- The lint itself, on a doctored Synthetic Total Verdict: "tap any number of
  -- target creatures you control, then destroy target creature an opponent
  -- controls with mana value equal to" a fold over the tapped ones.
  Spec.it s "the lint itself catches a sibling bound only the capped search answers" $ do
    let gauge = SlotName.MkSlotName (Text.pack "gauge")
        victim = SlotName.MkSlotName (Text.pack "victim")
        verdict aggregation victimFilter =
          Mode.MkMode
            (Seq.singleton (Mint.mandatory (Seq.fromList [Effect.Tap (Tap.plain (ObjectRef.InSlot gauge)), Effect.Tap (Tap.plain (ObjectRef.InSlot victim))])))
            ( Map.fromList
                [ (gauge, TargetSlot.anyNumber Pool.Creatures Nothing),
                  ( victim,
                    TargetSlot.withAmount
                      (Quantity.Type.Count (Count.Type.MkCount (Scope.OverBound BoundReading.AsBound gauge) (Filter.Type.HasCardType CardType.Creature) aggregation))
                      (TargetSlot.required Pool.Creatures (Just (Filter.Type.And (Filter.Type.Measures (Measures.MkMeasures Measure.ManaValue Comparison.Exactly Operand.EnclosingAmount) : victimFilter))))
                  )
                ]
            )
    Spec.assertBool s (not (siblingBoundOffends (verdict Aggregation.Members []))) "a bare Members bound is accepted"
    Spec.assertBool s (siblingBoundOffends (verdict (Aggregation.Total (Quantity.Type.Literal 1)) [])) "a Total bound is rejected"
    Spec.assertBool s (siblingBoundOffends (verdict (Aggregation.Greatest (Quantity.Type.Literal 1)) [])) "a Greatest bound is rejected"
    Spec.assertBool s (siblingBoundOffends (verdict Aggregation.DistinctNames [])) "a DistinctNames bound is rejected"
    Spec.assertBool s (siblingBoundOffends (verdict Aggregation.Members [Filter.Type.Not (Filter.Type.IsBound gauge)])) "a Members bound whose slot's filter reads the gauge is rejected"
  -- castAimable prices an aiming exactly unless an apart slot, or an exclusion
  -- with a slot named between its two, sits beside a slot taking several
  -- targets, or a Differing slot taking several beside another; there it
  -- stops at Target.subsetBudget and offers the cast unpriced (CR 601.2h takes
  -- it back). No printed card writes any of them; Synthetic Total Verdict's
  -- gauge, read by a Total bound, is exempt.
  Spec.it s "no card writes a joint check only the capped walk prices" $ do
    ps <- S.allPrintings s corpus
    let exempt = Set.fromList (fmap (CardName.MkCardName . Text.pack) ["Synthetic Total Verdict"])
        offenders = Set.fromList (fmap (S.nameOf . Printing.card) (filter (anyFaceOrMinted cappedWalkCardOffends . Printing.card) ps))
    Spec.assertEqWith s "only the exempt synthetics write a joint check the capped walk prices" offenders exempt
  -- The lint itself, on a doctored Graceful Takedown: "any number of target
  -- enchanted creatures you control and up to one other target creature you
  -- control", the "other" reading its sibling one way or another.
  Spec.it s "the lint itself catches a joint check only the capped walk prices" $ do
    let enchanted = SlotName.MkSlotName (Text.pack "enchanted")
        middle = SlotName.MkSlotName (Text.pack "middle")
        other = SlotName.MkSlotName (Text.pack "other")
        takedown otherFilter extra =
          Mode.MkMode
            (Seq.singleton (Mint.mandatory (Seq.fromList [Effect.Tap (Tap.plain (ObjectRef.InSlot enchanted)), Effect.Tap (Tap.plain (ObjectRef.InSlot other))])))
            ( Map.fromList
                ( [ (enchanted, TargetSlot.anyNumber Pool.Creatures Nothing),
                    (other, TargetSlot.required Pool.Creatures (Just otherFilter))
                  ]
                    <> extra
                )
            )
    Spec.assertBool s (not (cappedWalkOffends (takedown (Filter.Type.Not (Filter.Type.IsBound enchanted)) []))) "an exclusion is accepted"
    Spec.assertBool s (cappedWalkOffends (takedown (Filter.Type.IsBound enchanted) [])) "any other read of a wide sibling is rejected"
    Spec.assertBool s (cappedWalkOffends (takedown (Filter.Type.Not (Filter.Type.IsBound enchanted)) [(middle, TargetSlot.required Pool.Creatures Nothing)])) "a slot named between the exclusion's two beside a wide slot is rejected"
    let apart = (TargetSlot.anyNumber Pool.Creatures Nothing) {TargetSlot.together = Just (Together.Differing DifferentIn.Controllers)}
        alone = Mode.MkMode (Seq.singleton (Mint.mandatory (Seq.singleton (Effect.Tap (Tap.plain (ObjectRef.InSlot enchanted)))))) (Map.singleton enchanted apart)
    Spec.assertBool s (not (cappedWalkOffends alone)) "a wide Differing slot alone is accepted"
    Spec.assertBool s (cappedWalkOffends (takedown (Filter.Type.Not (Filter.Type.IsBound enchanted)) [(enchanted, apart)])) "a wide Differing slot beside another is rejected"
  -- The slot's FOURTH read: a CR 115.1 chooser naming its seat through a
  -- binding (Curse of Inertia's "that attacking player ... of their choice").
  -- Unreported, a card could hand its announcement to a player its condition
  -- never binds, and the trigger would be removed every time (CR 603.3d).
  Spec.it s "the lint itself catches a target chooser naming a slot the condition never binds" $ do
    inertia <- S.printingOf s registry "Curse of Inertia"
    let target = SlotName.MkSlotName (Text.pack "target")
        chosen =
          Mode.MkMode
            (Seq.singleton (Mint.mandatory (Seq.singleton (Effect.Tap (Tap.plain (ObjectRef.InSlot target))))))
            (Map.singleton target (TargetSlot.chosenBy (PlayerRef.InSlot Binding.triggerPlayer) (TargetSlot.required Pool.Permanents Nothing)))
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [chosen]))
      "CR 603.2 thatPlayer as a chooser under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger Mint.dealsCombatDamageToAPlayer [chosen])))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (not (any triggeredAbilityOffends (Face.triggeredAbilities (S.combinedFace inertia))))
      "the real card's own trigger is accepted"
  -- The same equality on a read that is not an effect's operand, a target slot's
  -- pool, its filter or its bound: CR 701.46a's per-clause "if". CR 608.2c lets a
  -- later clause's gate read what an earlier one bound, so a gate is the one
  -- place a card may read a slot and perform nothing with it -- and until
  -- Resolve.modeSlots folded Clause.condition such a read was recovered by
  -- nobody, so a card could gate on a player its condition never binds and
  -- silently never fire.
  --
  -- BOTH carriers of a slot inside a condition, because either alone would pass
  -- if only the other were folded: a PlayerRef buried in one of the compared
  -- numbers, and the FILTER of a Count under one of them.
  Spec.it s "the lint itself catches a slot read only from a clause's condition" $ do
    let gated condition =
          Mode.MkMode
            (Seq.fromList [Mint.mandatory (Seq.singleton (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 1) Nothing Nothing))), Clause.MkClause Nothing (Just condition) Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 1) Nothing Nothing)))])
            Map.empty
        atLeastOne measured = Condition.Type.Compares (Compares.MkCompares measured Comparison.AtLeast (Quantity.Type.Literal 1))
        -- "if that player has gained life this turn", the reference nested in the
        -- number Resolve.slotsOf cannot reach from an effect's own field.
        throughRef = atLeastOne (Quantity.Type.LifeChanged (LifeTally.MkLifeTally (PlayerRef.InSlot Binding.triggerPlayer) LifeDirection.Gained WhichTurn.ThisTurn))
        -- "if that player controls a permanent", the same read one carrier over:
        -- the slot is named by the FILTER of a Count rather than by a PlayerRef.
        throughCountFilter =
          atLeastOne
            ( Quantity.Type.Count
                (Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.AnyPlayer))) (Filter.Type.HeldByBound Holder.Controller Binding.triggerPlayer) Aggregation.Members)
            )
        -- And the pair that differs in exactly one thing: the same condition
        -- reading CR 109.5's caster instead of a slot, which no trigger has to
        -- bind, so the lint reads the reference rather than rejecting every gate.
        throughNoSlot = atLeastOne (Quantity.Type.LifeChanged (LifeTally.MkLifeTally (PlayerRef.Relative PlayerRelation.You) LifeDirection.Gained WhichTurn.ThisTurn))
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [gated throughRef]))
      "CR 608.2c thatPlayer inside a condition's number under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger Mint.dealsCombatDamageToAPlayer [gated throughRef])))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [gated throughCountFilter]))
      "CR 608.2c thatPlayer inside a condition's count filter under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger Mint.dealsCombatDamageToAPlayer [gated throughCountFilter])))
      "and that one too is accepted under a combat-damage trigger"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [gated throughNoSlot])))
      "where a condition naming no slot at all is accepted"
  -- The same equality on a slot read only from inside an EFFECT's number, which
  -- is the half the condition boards above cannot prove: Resolve.slotsOf inspects
  -- a player reference only on an effect's own field, so a reference buried in a
  -- Quantity was recovered by nobody until quantitySlots folded
  -- QuantitySlot.nestedRefs. Dream Salvage writes it -- "draw cards equal to the
  -- number of cards target opponent discarded this turn" -- and the
  -- data/scenarios/zone-change/cr-701-9a-dream-salvage-*.json pair drives that
  -- card at gameplay level.
  Spec.it s "the lint itself catches a slot read only from inside a number" $ do
    let drawsTally ref =
          Mode.MkMode
            (Seq.singleton (Mint.mandatory (Seq.singleton (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Tallied TurnTally.CardsDiscarded ref) Nothing Nothing)))))
            Map.empty
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [drawsTally (PlayerRef.InSlot Binding.triggerPlayer)]))
      "CR 608.2h thatPlayer inside an effect's number under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger Mint.dealsCombatDamageToAPlayer [drawsTally (PlayerRef.InSlot Binding.triggerPlayer)])))
      "and under a combat-damage trigger it is accepted"
    -- The pair that differs in exactly one thing, the arm of the reference: a
    -- number reading CR 109.5's caster stays accepted, so the lint reads the
    -- reference rather than rejecting every tally.
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [drawsTally (PlayerRef.Relative PlayerRelation.You)])))
      "where a number naming no slot at all is accepted"
  -- The same equality on a slot read only from a damage look-back's recipient,
  -- which Children.scopeChildren hands to the player visitor: no Count names one
  -- without it, since the Scope visitor answers nothing for a look-back.
  Spec.it s "the lint itself catches a slot read only from a damage look-back's recipient" $ do
    let drawsSources ref =
          Mode.MkMode
            (Seq.singleton (Mint.mandatory (Seq.singleton (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Count (Count.Type.MkCount (Scope.InHistory (EventShape.SourceDealtDamage (SourceDealtDamage.MkSourceDealtDamage Nothing (Set.singleton (DamageRecipient.Player ref))))) (Filter.Type.And []) Aggregation.Members)) Nothing Nothing)))))
            Map.empty
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [drawsSources (PlayerRef.InSlot Binding.triggerPlayer)]))
      "CR 608.2h thatPlayer as a look-back's recipient under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [drawsSources (PlayerRef.Relative PlayerRelation.Opponent)])))
      "where a recipient naming no slot at all is accepted"
  -- The same equality on ChoosePlayer's excluded players (Dack Fayden, Helping
  -- Hand's "choose a different opponent"): read nowhere else, so a misspelt
  -- slot, or the loop's reserved one outside its ForEach, would exclude nobody.
  Spec.it s "the lint itself catches a dangling slot of a chosen player's exclusion" $ do
    dack <- S.combinedFace <$> S.printingOf s registry "Dack Fayden, Helping Hand"
    let misspelt = SlotName.MkSlotName (Text.pack "thoseChosenForOther")
        respell effect = case effect of
          Effect.ChoosePlayer choice -> Effect.ChoosePlayer choice {ChoosePlayer.excluding = Just misspelt}
          Effect.ForEach loop -> Effect.ForEach loop {ForEach.body = fmap respell (ForEach.body loop)}
          _ -> effect
        respellMode mode = mode {Mode.clauses = fmap (\clause -> clause {Clause.effects = fmap respell (Clause.effects clause)}) (Mode.clauses mode)}
        respellAbility ability = ability {TriggeredAbility.modal = (TriggeredAbility.modal ability) {Modal.modes = fmap respellMode (Modal.modes (TriggeredAbility.modal ability))}}
        unlooped = Effect.ChoosePlayer (ChoosePlayer.MkChoosePlayer (PlayerScope.Related PlayerRelation.Opponent) (SlotName.MkSlotName (Text.pack "opponent")) (Just Binding.chosenForOthers) False)
    Spec.assertBool s (not (any triggeredAbilityOffends (Face.triggeredAbilities dack))) "Dack Fayden, Helping Hand's exclusion inside its ForEach is accepted"
    Spec.assertBool s (any (triggeredAbilityOffends . respellAbility) (Face.triggeredAbilities dack)) "a misspelt exclusion slot is rejected"
    Spec.assertBool s (triggeredAbilityOffends (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) [lintMode [unlooped] []])) "the loop's reserved slot outside a ForEach is rejected"
  -- The same equality one field further out: CR 118.12's gate multiplier
  -- (Parun.Types.PayGate.perEach), which is a Quantity a clause carries outside
  -- its effects, so neither Resolve.slotsOf nor the payer field reaches it. A
  -- gate scaled per bound object whose payer is CR 109.5's "you" names the slot
  -- nowhere else, and until Resolve.modeSlots folded quantitySlots over this
  -- field an undeclared read passed the lint and then evaluated to nothing --
  -- zero copies of the cost, which CR 118.5 makes a payable {0}.
  --
  -- BOTH carriers again, because either alone would pass if only the other were
  -- folded: a PlayerRef nested in the Count's SCOPE, and a bound named by its
  -- FILTER.
  Spec.it s "the lint itself catches a slot read only from a clause's gate multiplier, and CR 603.3b sees it too" $ do
    let scaled quantity =
          Mode.MkMode
            ( Seq.singleton
                ( Clause.MkClause
                    Nothing
                    Nothing
                    Nothing
                    Optionality.Mandatory
                    ( Just
                        PayGate.MkPayGate
                          { PayGate.payer = PlayerRef.Relative PlayerRelation.You,
                            PayGate.cost = CostChoice.MkCostChoice (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [] NonEmpty.:| []),
                            PayGate.basis = Nothing,
                            PayGate.branch = PayBranch.IfNotPaid,
                            PayGate.obligation = PayObligation.Optional,
                            PayGate.perEach = Just quantity,
                            PayGate.x = Nothing,
                            PayGate.offeredAt = Nothing,
                            PayGate.marks = Nothing
                          }
                    )
                    (Seq.singleton (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 1) Nothing Nothing)))
                )
            )
            Map.empty
        counting scope filter_ = Quantity.Type.Count (Count.Type.MkCount scope filter_ Aggregation.Members)
        -- "for each permanent that player controls", the slot named by the zone
        -- the Count folds over.
        throughScope = counting (Scope.InZone (InZone.MkInZone Zone.Battlefield (PlayerRef.InSlot Binding.triggerPlayer))) (Filter.Type.And [])
        -- The same read one carrier over: the slot is named by the Count's
        -- FILTER rather than by its scope's PlayerRef.
        throughCountFilter = counting (Scope.InZone (InZone.MkInZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.AnyPlayer))) (Filter.Type.HeldByBound Holder.Controller Binding.triggerPlayer)
        -- And the board that differs in exactly one thing: the same fold naming
        -- no slot, so the lint reads the reference rather than rejecting every
        -- scaled gate.
        throughNoSlot = counting (Scope.InZone (InZone.MkInZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.AnyPlayer))) (Filter.Type.And [])
        rejected condition quantity = triggeredAbilityOffends (modalTrigger condition [scaled quantity])
        -- CR 603.3b's consumer of the same map: Engine.orderInert elides the
        -- ordering prompt for a batch of EQUAL entries only when the mode reads
        -- no slot at all, so a gate scaled per bound object was reported inert
        -- and the batch went on the stack in the engine's canonical order -- the
        -- engine making a player's choice, since the two entries' gates are
        -- measured against different bound objects. Asserted on the carrier
        -- rather than at gameplay level: no card in `data/cards/` writes a gate
        -- scaled per bound object, so no board raises the prompt.
        inert quantity = Engine.orderInert (modalTrigger Mint.dealsCombatDamageToAPlayer [scaled quantity])
        -- One tagged list over both consumers rather than an assertion apiece,
        -- FilterPositionLintSpec's shape: a carrier the fold stops at names
        -- itself instead of hiding behind whichever assertion runs first.
        rows =
          [ ("D4: a scope's player, unbound", rejected (TriggerCondition.Enters EntersTrigger.defaultValue) throughScope),
            ("D4: a scope's player, bound", rejected Mint.dealsCombatDamageToAPlayer throughScope),
            ("D4: a count's filter, unbound", rejected (TriggerCondition.Enters EntersTrigger.defaultValue) throughCountFilter),
            ("D4: a count's filter, bound", rejected Mint.dealsCombatDamageToAPlayer throughCountFilter),
            ("D4: no slot at all", rejected (TriggerCondition.Enters EntersTrigger.defaultValue) throughNoSlot),
            ("CR 603.3b: inert scaled per bound object", inert throughScope),
            ("CR 603.3b: inert scaled by a slotless fold", inert throughNoSlot)
          ]
    Spec.assertEqWith
      s
      "CR 118.12 a gate multiplier's slot read reaches both readers of Resolve.modeSlots"
      rows
      [ ("D4: a scope's player, unbound", True),
        ("D4: a scope's player, bound", False),
        ("D4: a count's filter, unbound", True),
        ("D4: a count's filter, bound", False),
        ("D4: no slot at all", False),
        ("CR 603.3b: inert scaled per bound object", False),
        ("CR 603.3b: inert scaled by a slotless fold", True)
      ]
  -- The same equality over a card's ACTIVATED abilities, the one carrier with no
  -- event slot answering a read at all: an activation is not an event. See
  -- activatedAbilityOffends for the whole of it.
  --
  -- Brothers of Fire is what this caught: its "and 1 damage to you" reads CR
  -- 109.5's slot from an ACTIVATED ability, which nothing bound until
  -- Activate.activateAbility started stamping it (#569).
  --
  -- A GRANTED ability is read too (CardSpec.grantedActivatedAbilities): the
  -- permanent it lands on activates it through the same Activate road. It is
  -- read as the projection hands it over, its granter baked in
  -- (Projection.bakeGranter), so Trusty Boomerang's name for its granter is no
  -- slot read, and named for it (Projection.namedForGranter), so Archery
  -- Training's read is bound; any two ids serve, as a lend.
  Spec.it s "every slot an activated ability reads is bound for its activation, and every slot it declares is read" $ do
    ps <- S.allPrintings s corpus
    let baked ability = (Projection.namedForGranter (ObjectId.MkObjectId 0) (ObjectId.MkObjectId 1) ability) {ActivatedAbility.modal = Projection.bakeGranter (ObjectId.MkObjectId 0) (ActivatedAbility.modal ability)}
        -- Each ability beside the face whose delayed abilities it can arm.
        faceAbilities face = fmap ((,) face) (Face.activatedAbilities face <> fmap baked (grantedActivatedAbilities face))
        -- Over withMinted, each ability against what its own activation binds:
        -- the Clue a card creates is activated as the Clue's.
        cardAbilities card = concatMap faceAbilities (withMinted (Card.combined card))
        abilitiesOf p = fmap ((,) (Face.name (S.combinedFace p))) (cardAbilities (Printing.card p))
        abilities = concatMap abilitiesOf ps
        readsAnySlot (_, ab) = not (all (Map.null . Resolve.slotsOf) (Modal.allEffects (ActivatedAbility.modal ab)))
        offends (face, ab) = activatedAbilityOffendsGiven (armedReads face) ab
        sweeps = any offends . cardAbilities
        tokenDrawing slot = (vanillaFace "Token" instantLine) {Face.activatedAbilities = [oneEffectActivated Nothing (Effect.Draw (Draw.MkDraw (PlayerRef.InSlot slot) (Quantity.Type.Literal 1) Nothing Nothing))]}
    Spec.assertBool s (sweeps (mintingSpell createToken (tokenDrawing Binding.triggerPlayer))) "a token's ability reading thatPlayer is caught"
    Spec.assertBool s (not (sweeps (mintingSpell createToken (tokenDrawing Binding.you)))) "and one reading you is accepted"
    -- Guards the sweep against passing vacuously, in both directions: an empty
    -- pool of abilities, and a pool in which none reads a slot at all (where
    -- every ability would pass on an empty read side whatever the lint said).
    Spec.assertBool s (not (null abilities)) "the pool has activated abilities"
    Spec.assertBool s (any (readsAnySlot . snd) abilities) "and one of them reads a slot"
    Spec.assertEqWith s "no dangling activated-ability slot" (fmap fst (filter (offends . snd) abilities)) []
  -- Unprinted: see granterUnreachedOffends.
  Spec.it s "no granted ability names its granter in a target filter or a quoted ability" $ do
    ps <- S.allPrintings s corpus
    Spec.assertEqWith s "no granter named where the bake cannot reach" (fmap (S.nameOf . Printing.card) (filter (anyFaceOrMinted granterUnreachedOffends . Printing.card) ps)) []
  -- Blazing Torch's grant, doctored to name its granter in each unreached
  -- position: "other than Blazing Torch" on its target, and a token quoting
  -- "Tap Blazing Torch".
  Spec.it s "the lint itself catches a granter named in a target filter or a quoted ability" $ do
    torch <- S.printingOf s registry "Blazing Torch"
    let face = S.combinedFace torch
        onGrant f = face {Face.staticAbilities = fmap (\a -> a {StaticAbility.modifications = fmap (grant f) (StaticAbility.modifications a)}) (Face.staticAbilities face)}
        grant f m = case m of
          Modification.GainAbility (GrantedAbility.Activated a) -> Modification.GainAbility (GrantedAbility.Activated a {ActivatedAbility.modal = f (ActivatedAbility.modal a)})
          _ -> m
        other = Filter.Type.Not (Filter.Type.IsBound Binding.grantingPermanent)
        narrowed modal = modal {Modal.modes = fmap (\m -> m {Mode.targetSlots = fmap (\t -> t {TargetSlot.filter = Just other}) (Mode.targetSlots m)}) (Modal.modes modal)}
        servo = (vanillaFace "Servo" instantLine) {Face.activatedAbilities = [oneEffectActivated Nothing (Effect.Tap (Tap.plain (ObjectRef.InSlot Binding.grantingPermanent)))]}
        quoting _ = Mint.oneMode (Seq.singleton (createToken servo))
    Spec.assertBool s (not (granterUnreachedOffends face)) "the real Blazing Torch, naming its granter in its cost and effect, is accepted"
    Spec.assertBool s (granterUnreachedOffends (onGrant narrowed)) "its granter in the target's filter is rejected"
    Spec.assertBool s (granterUnreachedOffends (onGrant quoting)) "its granter in a token's quoted ability is rejected"
  -- CR 601.2c's count, over every carrier at once: see modalCountsOffend.
  Spec.it s "every slot that may take more than one target is read where a set fits" $ do
    ps <- S.allPrintings s corpus
    let carriers p =
          let face = S.combinedFace p
           in fmap ((,) (Face.name face)) $
                Face.spell face
                  : fmap ActivatedAbility.modal (Face.activatedAbilities face)
                    <> fmap TriggeredAbility.modal (Face.triggeredAbilities face)
        modals = concatMap carriers ps
        takesSeveral (_, modal) =
          any (any (SlotCount.plural . TargetSlot.count) . Mode.targetSlots) (Modal.modes modal)
    -- The pool must actually contain one, or the sweep says nothing.
    Spec.assertBool s (any takesSeveral modals) "the pool has a slot that takes more than one target"
    Spec.assertEqWith s "no multi-target slot is read one at a time" (fmap fst (filter (modalCountsOffend . snd) modals)) []
  -- The rejecting direction, which the sweep above cannot show: a mode whose slot
  -- takes two targets and whose only reader is Effect.TurnFaceUp, a bare SlotName.
  Spec.it s "the lint itself catches a multi-target slot read one at a time" $ do
    let slot = SlotName.MkSlotName (Text.pack "creature")
        modeReading targetSlot readers =
          Mint.oneModeTargeting (Map.singleton slot targetSlot) (Seq.fromList readers)
        modeWith targetSlot reader = modeReading targetSlot [reader]
        two = TargetSlot.upTo 2 Pool.Creatures Nothing
        -- A NUMBER reading the same name: Quantity.InSlot asks for the slot's
        -- amount (Parun.Engine.Binding.amountOf), never for an object.
        amountReader = Effect.Bolster (Quantity.Type.InSlot slot)
        -- ONE number reading the same name both ways: the sum of that amount and
        -- an inner number aimed at the object the slot names.
        bothReader =
          Effect.Bolster
            ( Quantity.Type.Arithmetic
                ( Arithmetic.Plus
                    ( Plus.MkPlus
                        (Quantity.Type.InSlot slot)
                        (Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot slot (Quantity.Type.Literal 1)))
                    )
                )
            )
    Spec.assertBool
      s
      (modalCountsOffend (modeWith two (Effect.TurnFaceUp slot)))
      "a two-target slot read as one object offends"
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith two (Effect.Tap (Tap.plain (ObjectRef.InSlot slot))))))
      "and the same slot read through an ObjectRef does not"
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith (TargetSlot.required Pool.Creatures Nothing) (Effect.TurnFaceUp slot))))
      "nor does a one-target slot read as one object"
    -- An effect's own FILTER over the same two-target slot, the pair differing
    -- in the atom alone: IsBound takes the whole set the slot names
    -- (Binding.objectsBySlot), CR 208.1's OfBound operand wants one
    -- toughness and answers nothing for two (Filter.singularSlots).
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith two (Effect.AttachAll (AttachAll.MkAttachAll (ObjectRef.InSlot slot) (Filter.Type.IsBound slot))))))
      "a two-target slot read whole by a filter does not offend"
    Spec.assertBool
      s
      (modalCountsOffend (modeWith two (Effect.AttachAll (AttachAll.MkAttachAll (ObjectRef.InSlot slot) (Filter.Type.Measures (Measures.MkMeasures Measure.Toughness Comparison.LessThan (Operand.OfBound (BoundMeasure.MkBoundMeasure slot Measure.Toughness))))))))
      "but one a filter reads as one object does"
    -- CR 601.2c's "any number of target ...", which states no maximum to compare
    -- against: an unbounded slot is plural, so the same one-object reader offends.
    -- No card in the corpus makes this mistake, so this is the only observer the
    -- OFFENDING direction of TargetCount.plural's unbounded arm has; the
    -- permitting direction is Tinybones Joins Up, whose "any number of target
    -- players" the sweep above walks.
    Spec.assertBool
      s
      (modalCountsOffend (modeWith (TargetSlot.anyNumber Pool.Creatures Nothing) (Effect.TurnFaceUp slot)))
      "an unbounded slot read as one object offends too"
    -- The OTHER slot-naming arm of a number, on the offending board's own slot:
    -- an amount read is no arity claim, so the two-target slot is legal beside
    -- it. The pair differs from the first assertion in the READER alone -- same
    -- slot, same count -- so it proves the lint discriminates rather than
    -- rejecting every number that names a plural slot; see #2774.
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith two amountReader)))
      "and a two-target slot read as an amount does not offend"
    -- The half neither board above can prove: a classification that dropped the
    -- amount read from the arity map by dropping it from the map ALTOGETHER
    -- would pass both, and silently stop the D4 dataflow lint seeing the read.
    -- Asserted on the map itself, that lint reading exactly these keys.
    Spec.assertEqWith
      s
      "and the D4 lint still sees the amount read"
      (fmap (Map.keysSet . Resolve.modeSlots) (Foldable.toList (Modal.modes (modeWith two amountReader))))
      [Set.singleton slot]
    -- And the two reads of one slot, which the boards above cannot tell apart
    -- either: an amount read must not MASK an object read of the same name.
    -- Twice, because the two reads meet in two different joins -- inside one
    -- number (Resolve.quantitySlots' own left-biased union) and across two
    -- effects of one mode (Resolve.joinTwo's min) -- and each keeps the narrower
    -- answer on its own.
    Spec.assertBool
      s
      (modalCountsOffend (modeWith two bothReader))
      "and one number reading the slot both ways offends"
    Spec.assertBool
      s
      (modalCountsOffend (modeReading two [amountReader, Effect.TurnFaceUp slot]))
      "as does an object read beside an amount read of the same slot"
  -- The sweep above passes VACUOUSLY on the rejecting side: no committed
  -- activated ability reads a slot it is not given, so the REJECTING direction is
  -- proven here instead, against hand-built offenders and against the four real
  -- cards that between them exercise every part of the available side.
  --
  -- Every reserved slot an activation does NOT bind gets its own case, because a
  -- classification answering "every slot, always" would pass any one of them
  -- alone. CR 109.5's `you` is the case that runs the other way, and is asserted
  -- on BOTH lints: the rule defines the word for an activated ability and for a
  -- triggered one, so the same effect is accepted either way (#569). Leaving it
  -- off one lint's available side is what a card would then fail on, so the pair
  -- is what keeps the two halves of the rule from drifting apart.
  Spec.it s "the lint itself catches an activated ability reading a slot activation never binds" $ do
    longtuskCub <- S.printingOf s registry "Longtusk Cub"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    cinderElemental <- S.printingOf s registry "Cinder Elemental"
    brothers <- S.printingOf s registry "Brothers of Fire"
    let free = Just (ManaCost.MkManaCost [])
        variable = Just (ManaCost.MkManaCost [ManaSymbol.Variable])
        -- CR 109.5's "you", read through its reserved slot: an opcode naming the
        -- controller by Binding.you.
        youDiscards = Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard (PlayerRef.InSlot Binding.you) (Quantity.Type.Literal 1) Nothing))
        -- Endless Cockroaches' payload (CR 400.7e) and rule 702.70a's, the two
        -- event slots, neither of which an activation has an event to bind.
        returnIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing)
        thatPlayerDraws = Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1) Nothing Nothing)
        -- CR 113.7's source slot, which every activation DOES bind.
        tapSelf = Effect.Tap (Tap.plain (ObjectRef.InSlot Binding.triggerSource))
        -- An ordinary slot this ability neither declares nor mints.
        tapGhost = Effect.Tap (Tap.plain (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "ghost"))))
        -- CR 601.2b's announced value, read as a slot rather than as Quantity.X.
        drawX = Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.InSlot Binding.variableX) Nothing Nothing)
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated free youDiscards)))
      "CR 109.5 you is accepted: an activation binds the player who activated it"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) youDiscards)))
      "and the very same effect is accepted on a triggered ability"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free returnIt))
      "CR 400.7e became is rejected: an activation is not an event"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free thatPlayerDraws))
      "CR 702.70a thatPlayer is rejected for the same reason"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free tapGhost))
      "and so is an ordinary slot the ability never declares"
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated free tapSelf)))
      "CR 113.7 self is accepted, stamped for every activation"
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated variable drawX)))
      "CR 601.2b X is accepted when the activation cost prints {X}"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free drawX))
      "and rejected when it does not"
    -- The four real cards between them cover every part of the available side
    -- that a committed card reaches: CR 113.7's self, CR 601.2c's declared
    -- target, an ability whose cost carries CR 601.2b's {X}, and CR 109.5's
    -- `you`. Brothers of Fire is the last one's only producer, so dropping it
    -- from this list would leave that part of the available side asserted by
    -- the hand-built case alone.
    Spec.assertEqWith
      s
      "Longtusk Cub, Prodigal Sorcerer, Cinder Elemental and Brothers of Fire are all accepted"
      (fmap (any activatedAbilityOffends . Face.activatedAbilities . S.combinedFace) [longtuskCub, sorcerer, cinderElemental, brothers])
      [False, False, False, False]
  -- The PER-MODE half of all three read lints (#570), which no sweep above can
  -- reach: a one-mode ability cannot have a mode read another mode's slot at
  -- all, and all three multi-mode abilities in the pool have each mode reading
  -- only what that mode declares, so per-mode and the old union shape agree on
  -- every card committed today. Aether Channeler's is the one that comes closest
  -- to exercising the difference and the only non-synthetic one -- three modes,
  -- of which only the middle declares a slot -- and the other two (Synthetic
  -- Modal Activator's, Synthetic Modal Trigger's) declare one apiece. Proven
  -- here against hand-built offenders instead.
  --
  -- Both halves of the union, because closing one and leaving the other would
  -- pass this: the declared TARGET slots (CR 700.2c) and the slots an effect
  -- MINTS (Resolve.definedSlots).
  Spec.it s "the lint itself catches a mode reading a slot only another mode declares" $ do
    let creature = SlotName.MkSlotName (Text.pack "creature")
        victim = SlotName.MkSlotName (Text.pack "victim")
        exiled = SlotName.MkSlotName (Text.pack "exiled")
        tap slot = Effect.Tap (Tap.plain (ObjectRef.InSlot slot))
        -- Mode 0 declares `creature` and reads it; mode 1 reads it and declares
        -- nothing. Under ChooseExactly 1, choosing mode 1 alone stamps mode 1's
        -- target slots -- which is nothing -- so the read is unbound at runtime.
        crossDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap creature] []]
        -- The same two reads, each mode declaring the slot it reads.
        ownDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap victim] [victim]]
        -- Mode 0 MINTS `exiled` at a MoveToZone's destination; mode 1 reads it.
        -- The two never resolve together, so mode 1's read is dangling.
        exileIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot creature) Zone.Exile EntryRiders.defaultValue (Just exiled) Nothing LibraryPlacement.defaultValue Nothing)
        crossMinted = modalActivated [lintMode [exileIt] [creature], lintMode [tap exiled] []]
        ownMinted = modalActivated [lintMode [exileIt, tap exiled] [creature], lintMode [tap victim] [victim]]
    Spec.assertBool s (activatedAbilityOffends crossDeclared) "a mode reading a slot only another mode declares is rejected"
    Spec.assertBool s (not (activatedAbilityOffends ownDeclared)) "and each mode reading only what it declares is accepted"
    Spec.assertBool s (activatedAbilityOffends crossMinted) "a mode reading a slot only another mode mints is rejected"
    Spec.assertBool s (not (activatedAbilityOffends ownMinted)) "and a mode reading what it mints itself is accepted"
    -- The ABILITY-scoped side must still reach every mode, not just the first:
    -- CR 400.7e's `became` is bound by the condition for the whole ability, so a
    -- SECOND mode reading it is accepted, and a mode reading it under a
    -- condition that never binds it is rejected however late the mode sits.
    let returnBecame = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing)
        secondModeReads condition = modalTrigger condition [lintMode [] [], lintMode [returnBecame] []]
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (secondModeReads (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)))))
      "the condition's event slots reach a later mode too"
    Spec.assertBool
      s
      (triggeredAbilityOffends (secondModeReads (TriggerCondition.Enters EntersTrigger.defaultValue)))
      "and a later mode is still rejected when the condition binds nothing"
  -- The DECLARED-BUT-UNREAD half of the ability lints, which the spell lint has
  -- always had and the three ability ones acquired with #1043. Before that they
  -- were subset checks, so an ability announcing a target no effect of its reads
  -- passed -- and so did an Effect whose Resolve.slotsOf arm UNDER-REPORTED,
  -- since a forgotten read only shrinks the read side that the subset check let
  -- be small. Reverting Resolve.slotsOf's BecomeMonarch arm to Set.empty left the
  -- entire suite green when Denethor, Stone Seer landed (#1040); under the
  -- equality it fails, because Denethor's ability declares two slots and would
  -- then read one.
  --
  -- Every carrier gets a case: what makes this an ability-side gap is that the
  -- claim was stated for Face.spell alone, so proving it on one ability would not
  -- show it reaching the others.
  Spec.it s "the lint itself catches an ability declaring a slot no effect reads" $ do
    denethor <- S.printingOf s registry "Denethor, Stone Seer"
    let creature = SlotName.MkSlotName (Text.pack "creature")
        victim = SlotName.MkSlotName (Text.pack "victim")
        tap slot = Effect.Tap (Tap.plain (ObjectRef.InSlot slot))
        -- One mode declaring two slots and reading only one of them: Denethor's
        -- exact shape under a slotsOf arm that forgot a read.
        unread = [lintMode [tap creature] [creature, victim]]
        read_ = [lintMode [tap creature] [creature]]
        -- The delayed lint calls modalSlotsOffend itself, with the whole card's
        -- minted slots on the bound side; nothing here mints, so the bound side
        -- is CR 113.7's source alone.
        delayed modes = modalSlotsOffend (const Set.empty) (Set.singleton Binding.triggerSource) (TriggeredAbility.modal (modalTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) modes))
        -- The chosen graveyard card's SCOPE as the SOLE reader of a declared
        -- slot, which is Grasping Tentacles' second clause with its mill set
        -- aside. The pair differs in the scope alone, so it proves
        -- Resolve.objectRefSlots reports that read rather than the chooser's.
        takeFrom scope =
          Effect.MoveToZone
            ( MoveToZone.MkMoveToZone
                (ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.TheController scope (Filter.Type.HasCardType CardType.Artifact) HowMany.one Nothing))
                Zone.Battlefield
                EntryRiders.defaultValue
                Nothing
                Nothing
                LibraryPlacement.defaultValue
                Nothing
            )
        scoped = [lintMode [takeFrom (ZoneScope.Scoped (PlayerScope.Related PlayerRelation.You))] [victim]]
        inSlot = [lintMode [takeFrom (ZoneScope.InSlot victim)] [victim]]
    Spec.assertBool s (activatedAbilityOffends (modalActivated unread)) "an activated ability declaring an unread slot is rejected"
    Spec.assertBool s (not (activatedAbilityOffends (modalActivated read_))) "and reading everything it declares is accepted"
    Spec.assertBool s (triggeredAbilityOffends (modalTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) unread)) "a triggered ability declaring an unread slot is rejected"
    Spec.assertBool s (not (triggeredAbilityOffends (modalTrigger (TriggerCondition.Leaves (LeavesTrigger.dies LeavingPermanent.Self)) read_))) "and reading everything it declares is accepted"
    Spec.assertBool s (delayed unread) "a delayed ability declaring an unread slot is rejected"
    Spec.assertBool s (not (delayed read_)) "and reading everything it declares is accepted"
    -- The real card whose landing exposed the gap, accepted: its ability declares
    -- a player slot for the crown and an `any target` slot for the damage, and
    -- Resolve.slotsOf reports both.
    Spec.assertBool
      s
      (not (any activatedAbilityOffends (Face.activatedAbilities (S.combinedFace denethor))))
      "Denethor, Stone Seer's two-slot ability is accepted"
    Spec.assertBool s (not (activatedAbilityOffends (modalActivated inSlot))) "a slot read only by a chosen graveyard card's scope is accepted"
    Spec.assertBool s (activatedAbilityOffends (modalActivated scoped)) "and the same effect over a scope naming no slot leaves that slot unread"
  -- CR 603.7c: a declared slot that only the condition of a delayed ability the
  -- mode arms names is read (CardSpec.armedReads), on every carrier that can
  -- arm. Each pair differs in the delayed condition alone. Fatal Fissure is
  -- the spell; the ability carriers' cases are hand-built.
  Spec.it s "a slot only an armed delayed ability's condition names is read" $ do
    let chosen = SlotName.MkSlotName (Text.pack "chosen")
        later = AbilityName.MkAbilityName (Text.pack "later")
        arming = [lintMode [Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger later Onset.Immediately Nothing Nothing Nothing)] [chosen]]
        watching permanent = (vanillaFace "Arming" instantLine) {Face.delayedAbilities = Map.singleton later (modalTrigger (TriggerCondition.Leaves (LeavesTrigger.dies permanent)) [lintMode [] []])}
        named = watching (LeavingPermanent.Bound chosen)
        unnamed = watching LeavingPermanent.Self
        spell face = modalSlotsOffend (armedReads face) Set.empty (Modal.MkModal (Seq.fromList arming) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
        triggered face = triggeredAbilityOffendsGiven (armedReads face) Set.empty (modalTrigger (TriggerCondition.Enters EntersTrigger.defaultValue) arming)
        activated face = activatedAbilityOffendsGiven (armedReads face) (modalActivated arming)
    Spec.assertEqWith s "the condition naming the slot is accepted" (fmap ($ named) [spell, triggered, activated]) [False, False, False]
    Spec.assertEqWith s "and one naming no slot leaves it unread" (fmap ($ unnamed) [spell, triggered, activated]) [True, True, True]
  -- CR 400.1: every InZone Count over a shared zone (battlefield, stack,
  -- exile, command) must pair with PlayerRef.Relative AnyPlayer -- the type
  -- permits any PlayerRef there, but only Relative AnyPlayer is meaningful for a
  -- zone no player owns individually, see #161. A REGRESSION FENCE rather than
  -- the guard: the decoder refuses such a file, so an offender would abort the
  -- whole corpus load before this sweep ran. Parun.RegistrySpec's "CR 400.1 a
  -- corpus dividing a shared zone between players" is where that is proved.
  Spec.it s "every InZone Count over a shared zone pairs with Relative AnyPlayer" $ do
    ps <- S.allPrintings s corpus
    let offenders =
          filter
            (anyFace cardOffendsSharedZoneScope . Printing.card)
            ps
    Spec.assertEqWith s "no shared-zone scope with a ref other than Relative AnyPlayer" (fmap (S.nameOf . Printing.card) offenders) []
    -- The sweep above is vacuous on any Count position the traversal forgets, so
    -- the newest one is asserted positively: Sphere of Safety's CR 508.1h share
    -- counts "enchantments you control", the only Count a cost to attack can
    -- hold, and cardCounts must see it.
    sphere <- S.printingOf s registry "Sphere of Safety"
    Spec.assertEqWith
      s
      "a cost to attack's counted share is in the sweep"
      (fmap Count.Type.scope (cardCounts (S.combinedFace sphere)))
      [Scope.InZone (InZone.MkInZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.AnyPlayer))]
    -- And the newest position of all, planted rather than read off a card: CR
    -- 122.5's GIVER became an ObjectRef when the first side was widened to a
    -- group, so a library walk's depth there is a Count position this traversal
    -- has to reach. No printing can exercise it -- rule 122.5 moves counters
    -- between two things on the battlefield, so a giver naming a card in a
    -- library moves nothing -- and the arm reading the moved kinds alone kept
    -- compiling (#2729).
    let counted = Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.You))) (Filter.Type.HasCardType CardType.Creature) Aggregation.Members
        moving =
          Effect.MoveCounters
            ( MoveCounters.MkMoveCounters
                (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Count counted) Nothing LibraryPosition.Top))
                (MovedKinds.EveryOfKind CounterKind.PlusOnePlusOne)
                Nothing
                (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "recipient")))
            )
    Spec.assertEqWith s "a counter move's giver puts its depth's Count in the sweep" (effectCounts moving) [counted]
  -- CR 611.2b, swept over the pool. See chosenPlayerStoredOffends.
  Spec.it s "CR 611.2b no card writes a chosen player in a stored condition" $ do
    ps <- S.allPrintings s corpus
    let offenders = filter (anyFace chosenPlayerStoredOffends . Printing.card) ps
    Spec.assertBool s (any (anyFace (not . null . storedConditions) . Printing.card) ps) "the pool has a card storing a condition"
    Spec.assertEqWith s "no stored condition reads a chosen player" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against planted faces: the condition on each of
  -- the two carriers Condition.bakeBound bakes.
  Spec.it s "the lint itself catches a chosen player in a stored condition" $ do
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    hama <- S.printingOf s registry "Hama, the Bloodbender"
    let chosen = PlayerRef.ChosenPlayerOfBound Binding.triggerSource (PlayerChoice.Promised Gift.Card (KeywordInstance.Own 1))
        atMostTen = Condition.Type.Compares (Compares.MkCompares (Quantity.Type.LifeTotal chosen) Comparison.AtMost (Quantity.Type.Literal 10))
        planted effect = (vanillaFace "Planted" instantLine) {Face.spell = Mint.oneMode (Seq.singleton effect)}
        pump = Effect.ModifyTarget (ModifyTarget.MkModifyTarget (Duration.ForAsLongAs atMostTen) (Modification.GainKeyword Keyword.Flying) (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Nothing)
        permission = Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile Duration.UntilEndOfTurn (PlayerRef.Relative PlayerRelation.You) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled"))) ManaSpending.AsProduced Nothing (Just atMostTen) PermissionVerb.Play 0 Nothing TapState.Untapped TapState.Untapped Nothing [] GrantZone.Exile)
    Spec.assertBool s (not (anyFace chosenPlayerStoredOffends (Printing.card garland))) "the real Garland, Royal Kidnapper is accepted"
    Spec.assertBool s (not (anyFace chosenPlayerStoredOffends (Printing.card hama))) "the real Hama, the Bloodbender is accepted"
    Spec.assertBool s (chosenPlayerStoredOffends (planted pump)) "a for-as-long-as duration is rejected"
    Spec.assertBool s (chosenPlayerStoredOffends (planted permission)) "an exile play permission's condition is rejected"
  -- CR 109.5 / 602.1b, swept over the pool. See grantedActivatorOffends.
  Spec.it s "CR 602.1b no card grants an activated ability whose activator names other than you" $ do
    ps <- S.allPrintings s corpus
    let offenders = filter (anyFaceOrMinted grantedActivatorOffends . Printing.card) ps
    Spec.assertBool s (any (anyFace (not . null . grantedActivators) . Printing.card) ps) "the pool has a grant naming who may activate it"
    Spec.assertEqWith s "every granted activator is a resolving grant's you" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against faces planted on Martyrdom's own grant:
  -- its activator widened, and the same ability granted by a static ability.
  Spec.it s "the lint itself catches a granted activator naming other than you" $ do
    martyrdom <- S.printingOf s registry "Martyrdom"
    let face = S.combinedFace martyrdom
        creatures = Filter.Type.HasCardType CardType.Creature
    granted <- case grantedActivatedAbilities face of
      [ability] -> pure ability
      _ -> Spec.assertFailure s "Martyrdom grants one activated ability"
    let opponents = granted {ActivatedAbility.activator = Activator.Player (PlayerRef.Relative PlayerRelation.Opponent)}
        resolving = (vanillaFace "Planted" instantLine) {Face.spell = Mint.oneMode (Seq.singleton (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn (Modification.GainAbility (GrantedAbility.Activated opponents)) (ObjectRef.EachMatching creatures) Nothing)))}
        static = face {Face.staticAbilities = [StaticAbility.MkStaticAbility (Affected.Matching creatures) Nothing Set.empty Nothing Nothing (NonEmpty.singleton (Modification.GainAbility (GrantedAbility.Activated granted)))]}
    Spec.assertBool s (not (grantedActivatorOffends face)) "the real Martyrdom is accepted"
    Spec.assertBool s (grantedActivatorOffends resolving) "a resolving grant's activator naming opponents is rejected"
    Spec.assertBool s (grantedActivatorOffends static) "a static grant's activator naming you is rejected"
  -- CR 613.1f, swept over the pool. See resolvingTriggeredLendOffends.
  Spec.it s "CR 613.1f no card writes a resolving lend of triggered abilities" $ do
    ps <- S.allPrintings s corpus
    let offenders = filter (anyFaceOrMinted resolvingTriggeredLendOffends . Printing.card) ps
    Spec.assertBool s (any (anyFaceOrMinted (not . null . resolvingLends) . Printing.card) ps) "the pool has a resolving lend"
    Spec.assertEqWith s "no resolving lend lends triggered abilities" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint itself, on Quicksilver Elemental's resolving lend doctored to
  -- lend triggered abilities too. Koh, the Face Stealer's static lend of them
  -- is accepted.
  Spec.it s "the lint itself catches a resolving lend of triggered abilities" $ do
    elemental <- S.printingOf s registry "Quicksilver Elemental"
    koh <- S.printingOf s registry "Koh, the Face Stealer"
    let lendTriggered effect = case effect of
          Effect.ModifyTarget modify
            | Modification.GainCardAbilities lend <- ModifyTarget.modification modify ->
                Effect.ModifyTarget modify {ModifyTarget.modification = Modification.GainCardAbilities lend {CardAbilities.triggered = True}}
          _ -> effect
        doctor ability = ability {ActivatedAbility.modal = Identity.runIdentity (Children.modalChildren Children.modalLeaves {Children.modalEffect = Identity.Identity . lendTriggered} (ActivatedAbility.modal ability))}
        face = S.combinedFace elemental
    Spec.assertBool s (not (resolvingTriggeredLendOffends face)) "the real Quicksilver Elemental is accepted"
    Spec.assertBool s (not (anyFace resolvingTriggeredLendOffends (Printing.card koh))) "the real Koh, the Face Stealer is accepted"
    Spec.assertBool s (resolvingTriggeredLendOffends face {Face.activatedAbilities = fmap doctor (Face.activatedAbilities face)}) "a resolving lend of triggered abilities is rejected"

-- CR 613.1f: Parun.Engine.Resolve.Common.expandGrant lends a resolving
-- GainCardAbilities' activated abilities and keywords but no triggered
-- ability, which only a static lend carries (Koh, the Face Stealer). No
-- printing resolves a lend of triggered abilities, so this keeps it out of card
-- data.
resolvingTriggeredLendOffends :: Face.Face Card.Type.Card -> Bool
resolvingTriggeredLendOffends = any CardAbilities.triggered . resolvingLends

-- Every GainCardAbilities a resolution applies, off Parun.Engine.Children's
-- traversal, so an opcode gaining a Modification is swept without an arm here.
resolvingLends :: Face.Face Card.Type.Card -> [CardAbilities.CardAbilities]
resolvingLends face =
  let stored = Const.getConst . Children.effectChildren Children.effectLeaves {Children.onModification = \modification -> Const.Const [modification]}
   in [lend | Modification.GainCardAbilities lend <- concatMap stored (cardAuthoredEffects face)]

-- Unprinted: a granted ability naming its granter (Binding.grantingPermanent)
-- in a target slot's filter, which CR 601.2c judges at announcement before
-- Binding.setGranter binds the slot, or inside an ability or object it quotes,
-- where Projection.bakeGranter does not descend. No printing has either shape,
-- so this keeps both out of card data.
granterUnreachedOffends :: Face.Face Card.Type.Card -> Bool
granterUnreachedOffends face = any modalOffends ([ActivatedAbility.modal a | GrantedAbility.Activated a <- granted] <> [TriggeredAbility.modal t | GrantedAbility.Triggered t <- granted])
  where
    granted = grantedAbilities face
    modalOffends modal =
      any (any (Map.member Binding.grantingPermanent . Resolve.targetSlotSlots) . Mode.targetSlots) (Modal.modes modal)
        || any (any (any names . quoted) . effectWithNested) (Modal.allEffects modal)
    quoted effect =
      fmap ability (concatMap modificationAbilities (effectGrants effect))
        <> Const.getConst (Children.effectChildren Children.effectLeaves {Children.onCard = card, Children.onConjured = card, Children.onMelded = card, Children.onAbility = \a -> Const.Const [ability a]} effect)
    ability = Codec.encode (GrantedAbility.Codec.codec Card.Codec.codec)
    card c = Const.Const [Codec.encode Card.Codec.codec c]
    names = (> 0) . jsonAtoms (SlotName.unwrap Binding.grantingPermanent)

-- CR 611.2b / 702.174a: Parun.Engine.Quantity.bakePlayerRef leaves
-- PlayerRef.ChosenPlayerOfBound standing in a stored condition, where no slot
-- names the object afterwards, so the condition would answer no one and the
-- effect never start. No printing writes it, so this keeps it out of card data.
-- Counted against the ENCODED condition, so a nested position is not a blind
-- spot.
chosenPlayerStoredOffends :: Face.Face Card.Type.Card -> Bool
chosenPlayerStoredOffends =
  any ((/= 0) . jsonAtoms (Text.pack "ChosenPlayerOfBound") . Codec.encode Condition.Codec.codec) . storedConditions

-- Every condition a resolution stores, the two Parun.Engine.Condition.bakeBound
-- bakes: a "for as long as" duration's (Parun.Engine.Expiry.arm) and an exile
-- play permission's (Parun.Engine.Resolve.Arms.Zones).
storedConditions :: Face.Face Card.Type.Card -> [Condition.Type.Condition]
storedConditions card =
  let durations = Const.getConst . Children.effectChildren Children.effectLeaves {Children.onDuration = \duration -> Const.Const [duration]}
      held duration = case duration of
        Duration.ForAsLongAs condition -> [condition]
        _ -> []
      permitted effect = case effect of
        Effect.GrantPlayFromExile grant -> Maybe.maybeToList (GrantPlayFromExile.condition grant)
        _ -> []
   in concatMap (\effect -> concatMap held (durations effect) <> permitted effect) (cardAuthoredEffects card)

-- Unprinted: Parun.Engine.Resolve.Arms.Damage.modifyTarget fixes a granted
-- activated ability's Activator.Player (CR 109.5 / 602.1b: the grant's text,
-- read off the granting ability) only when it names one player as the grant
-- resolves, and a static ability's grant never. So the one shape admitted is
-- Martyrdom's: a resolving grant's "Only you", which names its controller.
grantedActivatorOffends :: Face.Face Card.Type.Card -> Bool
grantedActivatorOffends face =
  let staticGrant ability = [activated | Modification.GainAbility (GrantedAbility.Activated activated) <- NonEmpty.toList (StaticAbility.modifications ability)]
      fromStatic = concatMap staticGrant (Face.staticAbilities face <> grantedStaticAbilities face)
   in any (/= PlayerRef.Relative PlayerRelation.You) (grantedActivators face)
        || any (Maybe.isJust . activatorPlayer) fromStatic

-- The Activator.Player reference of every activated ability a face grants.
grantedActivators :: Face.Face Card.Type.Card -> [PlayerRef.PlayerRef]
grantedActivators = Maybe.mapMaybe activatorPlayer . grantedActivatedAbilities

activatorPlayer :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe PlayerRef.PlayerRef
activatorPlayer ability = case ActivatedAbility.activator ability of
  Activator.Player who -> Just who
  _ -> Nothing

-- Every AbilityName a face's layer-6 removals refer to. Both carriers of a
-- Modification are swept: the printed ones on Face.staticAbilities, and the
-- stored ones a resolution creates, each off Parun.Engine.Children's traversal,
-- so an opcode gaining a Modification is swept without an arm here.
namedRemovals :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
namedRemovals face =
  let stored = Const.getConst . Children.effectChildren Children.effectLeaves {Children.onModification = \modification -> Const.Const [modification]}
      printed ability = Foldable.toList (StaticAbility.modifications ability)
      removals modification = case modification of
        Modification.LoseNamedAbility name -> [name]
        _ -> []
   in Set.fromList
        ( concatMap removals (concatMap printed (Face.staticAbilities face))
            <> concatMap removals (concatMap stored (cardAuthoredEffects face))
        )

-- CR 613.1f / 607.2a / 116.2d / 601.2f: does a face name an ability it does
-- not declare, declare one nothing reads, or name a non-activated one as an
-- exile pile or a cost reduction's ability? The case "every named removal,
-- exile reference or cost reduction names an ability its face declares" asks
-- it of every printing.
abilityNameOffends :: Face.Face Card.Type.Card -> Bool
abilityNameOffends face =
  let declared = Set.union (declaredAbilityNames face) (declaredIgnorableAbilityNames face)
      activatedOnly = Set.union (namedExileReferences face) (namedActivationCriteria face)
      readNames = Set.unions [namedRemovals face, activatedOnly, ignoredAbilityNames face, namedGrantedActivationCriteria face]
      grantOffends ability =
        let granted = [activated | Modification.GainAbility (GrantedAbility.Activated activated) <- NonEmpty.toList (StaticAbility.modifications ability)]
         in not (Set.isSubsetOf (grantedActivationCriteria ability) (Set.fromList (Maybe.mapMaybe ActivatedAbility.name granted)))
   in not (Set.isSubsetOf (Set.union (namedRemovals face) activatedOnly) declared)
        || declared /= readNames
        || not (Set.isSubsetOf activatedOnly (Set.fromList (Maybe.mapMaybe ActivatedAbility.name (Face.activatedAbilities face))))
        || any grantOffends (Face.staticAbilities face)

-- Every AbilityName a static ability's GRANTED cost modifier narrows to through
-- ActivationCriteria.whichAbility: The Dominion Bracelet's "This ability costs
-- {X} less to activate", granted beside the ability it reduces.
grantedActivationCriteria :: StaticAbility.StaticAbility (GrantedAbility.GrantedAbility Card.Type.Card) -> Set.Set AbilityName.AbilityName
grantedActivationCriteria ability =
  Set.fromList
    [ name
    | Modification.GainAbility (GrantedAbility.Player granted) <- NonEmpty.toList (StaticAbility.modifications ability),
      PlayerEffect.ModifyCost modifier <- [PlayerStaticAbility.effect granted],
      CostSubject.Activations criteria <- [CostModifier.subject modifier],
      Just name <- [ActivationCriteria.whichAbility criteria]
    ]

-- grantedActivationCriteria over every static ability of a face.
namedGrantedActivationCriteria :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
namedGrantedActivationCriteria face = Set.unions (fmap grantedActivationCriteria (Face.staticAbilities face))

-- Every AbilityName a face narrows to through ActivationCriteria.whichAbility:
-- a printed cost modifier's, Sanctum of Tranquil Light's "this ability costs {1}
-- less", and an activated ability's clause condition's count, Initiates of the
-- Ebon Hand's "this ability has been activated four or more times".
namedActivationCriteria :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
namedActivationCriteria face =
  Set.fromList
    ( [ name
      | PlayerEffect.ModifyCost modifier <- fmap PlayerStaticAbility.effect (Face.playerAbilities face),
        CostSubject.Activations criteria <- [CostModifier.subject modifier],
        Just name <- [ActivationCriteria.whichAbility criteria]
      ]
        <> [ name
           | ability <- Face.activatedAbilities face,
             Condition.Type.Compares compares <- modalClauseConditions (ActivatedAbility.modal ability),
             Quantity.Type.ActivationsThisTurn tally <- [Compares.measured compares],
             Just name <- [ActivationCriteria.whichAbility (ActivationsThisTurn.criteria tally)]
           ]
    )

-- Every AbilityName a face's CR 607.2a references name: the exiling ability
-- each OfAbility pile reads, wherever one is written. Parun.CardSpec.cardFilters
-- lists every pile -- an ObjectRef's, a Quantity's, a permission pool's, a
-- copy domain's, an acquired-abilities grant's -- as the IsExiledWithSource
-- atom naming the same cards, so the typed fold over its filters
-- (Parun.FilterPositionLintSpec.filterAtomsOf) finds each.
namedExileReferences :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
namedExileReferences face =
  let named atom = case atom of
        Filter.Type.IsExiledWithSource (ExiledPile.OfAbility name) -> [name]
        _ -> []
   in Set.fromList (concatMap (filterAtomsOf named . snd) (cardFilters face))

-- Every AbilityName a face declares FOR A LAYER-6 REMOVAL OR AN EXILE REFERENCE
-- to name -- the other side of the join above. Every carrier of one: an
-- activated ability (Gliding Licid), a triggered ability (Synthetic Silenced
-- Sentinel), a printed replacement (Glittering Lion) and a static ability
-- (Synthetic Muted Captain). A HAND-KEPT union, so a new carrier added to
-- Parun.Types.AbilityName's readers must be added here too, or its cards' names
-- read as dangling.
--
-- PlayerStaticAbility.name is deliberately NOT here: it belongs to CR 116.2d's
-- join below, which has its own pair of sides.
declaredAbilityNames :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
declaredAbilityNames face =
  Set.fromList
    ( Maybe.mapMaybe ActivatedAbility.name (Face.activatedAbilities face)
        <> Maybe.mapMaybe TriggeredAbility.name (Face.triggeredAbilities face)
        <> Maybe.mapMaybe PrintedReplacement.name (Face.replacementEffects face)
        <> Maybe.mapMaybe StaticAbility.name (Face.staticAbilities face)
        <> concatMap grantedNames (concatMap (NonEmpty.toList . StaticAbility.modifications) (Face.staticAbilities face))
    )
  where
    -- A rule or player ability the face's static ability GRANTS carries its own
    -- name, which the same face's removal reads (Synthetic Tethering Charm); a
    -- granted activated ability's, which a granted cost modifier reads (The
    -- Dominion Bracelet).
    grantedNames m = case m of
      Modification.GainAbility (GrantedAbility.Activated ability) -> Maybe.maybeToList (ActivatedAbility.name ability)
      Modification.GainAbility (GrantedAbility.Rules rules) ->
        Maybe.mapMaybe RuleAbilities.Engine.nameOf (RuleAbilities.combatRestrictions rules)
          <> Maybe.mapMaybe ActivationProhibition.name (RuleAbilities.activationProhibitions rules)
      Modification.GainAbility (GrantedAbility.Player ability) -> Maybe.maybeToList (PlayerStaticAbility.name ability)
      _ -> []

-- CR 116.2d: every ability name a face's ignore grants refer to. One grant per
-- name, and every printed producer prints exactly one grant.
ignoredAbilityNames :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
ignoredAbilityNames face =
  Set.fromList (Maybe.mapMaybe (\action -> case action of SpecialAction.IgnoreThisUntilEndOfTurn name _ -> Just name; _ -> Nothing) (Face.specialActions face))

-- Every AbilityName a face's IGNORABLE abilities declare -- the other side of
-- that join. SEVERAL rows may carry one name, which is Damping Engine's one
-- sentence declaring two and Volrath's Curse's one sentence declaring three, so
-- this is a set rather than a list.
--
-- A HAND-KEPT union over the carriers CR 116.2d can reach, declaredAbilityNames'
-- posture: a fourth carrier given a name field must be added here too, or its
-- cards' names read as dangling. The two BOUNDING combat restrictions have no
-- name field to read (Parun.Engine.RuleAbilities.nameOf says why).
declaredIgnorableAbilityNames :: Face.Face Card.Type.Card -> Set.Set AbilityName.AbilityName
declaredIgnorableAbilityNames face =
  Set.fromList
    ( Maybe.mapMaybe PlayerStaticAbility.name (Face.playerAbilities face)
        <> Maybe.mapMaybe RuleAbilities.Engine.nameOf (Face.combatRestrictions face)
        <> Maybe.mapMaybe ActivationProhibition.name (Face.activationProhibitions face)
    )

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> [(FilePath, Either Text.Text Card.Type.Card)] -> n ()
spec s registry corpus = Spec.describe s "Parun.Engine.Card" $ do
  abilitySlotLintSpec s registry corpus
