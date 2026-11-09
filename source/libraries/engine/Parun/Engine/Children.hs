module Parun.Engine.Children where

import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLinked as CounterLinked
import qualified Parun.Types.DamagedParty as DamagedParty
import qualified Parun.Types.DealtDamageTo as DealtDamageTo
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.SlotArity as SlotArity
import qualified Parun.Types.SlotName as SlotName

-- | One visitor per kind of child a DSL tree holds. The traversals below are
-- the ONE place that matches each constructor positionally, so a new
-- constructor or field is a build error there and nowhere else; a structural
-- walk is a traversal run with the visitors it has an opinion about, plus an
-- override arm for each constructor it answers itself (#6808).
data Children keyword f = MkChildren
  { -- | An operand of And, Or, Not or Unwritten, judged of the same candidate.
    connective :: Filter.Filter keyword -> f (Filter.Filter keyword),
    -- | A Filter an atom or an anchor nests, judged of some other object (CR
    -- 109.2): a host, a target, a source, a counted permanent.
    nest :: Filter.Filter keyword -> f (Filter.Filter keyword),
    -- | A keyword an atom holds, bare or inside a counter kind (CR 122.1b) or
    -- a designator.
    keyword :: keyword -> f keyword,
    -- | A slot name, with the arity its atom reads it at.
    slot :: SlotArity.SlotArity -> SlotName.SlotName -> f SlotName.SlotName,
    -- | An object named outright.
    object :: ObjectId.ObjectId -> f ObjectId.ObjectId
  }

-- | Every child left as it is: the base a walk overrides.
leaves :: (Applicative f) => Children keyword f
leaves =
  MkChildren
    { connective = pure,
      nest = pure,
      keyword = pure,
      slot = const pure,
      object = pure
    }

-- | Every nested Filter, connective or nest alike, handed to one recursion.
descending :: (Applicative f) => (Filter.Filter keyword -> f (Filter.Filter keyword)) -> Children keyword f
descending recur = leaves {connective = recur, nest = recur}

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
