module Parun.Engine.Children where

import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.ActivationsThisTurn as ActivationsThisTurn
import qualified Parun.Types.AgainstExiledPile as AgainstExiledPile
import qualified Parun.Types.AgainstLastCardExiledWith as AgainstLastCardExiledWith
import qualified Parun.Types.AgainstSlot as AgainstSlot
import qualified Parun.Types.Aggregation as Aggregation
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.BoundMeasure as BoundMeasure
import qualified Parun.Types.CastFrom as CastFrom
import qualified Parun.Types.CommanderColors as CommanderColors
import qualified Parun.Types.CompletedDungeon as CompletedDungeon
import qualified Parun.Types.Count as Count
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLinked as CounterLinked
import qualified Parun.Types.DamageTally as DamageTally
import qualified Parun.Types.DamagedParty as DamagedParty
import qualified Parun.Types.DealtDamage as DealtDamage
import qualified Parun.Types.DealtDamageTo as DealtDamageTo
import qualified Parun.Types.Devotion as Devotion
import qualified Parun.Types.DraftNoteTally as DraftNoteTally
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.LifeTally as LifeTally
import qualified Parun.Types.ManaCount as ManaCount
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.PlayerCounterTally as PlayerCounterTally
import qualified Parun.Types.PlayerDesignationTally as PlayerDesignationTally
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Scope as Scope
import qualified Parun.Types.SlotArity as SlotArity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SpellTally as SpellTally
import qualified Parun.Types.VotesReceived as VotesReceived

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
