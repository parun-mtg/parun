module Parun.Codec.Filter where

import qualified Data.Maybe as Maybe
import qualified Data.Typeable as Typeable
import qualified Numeric.Natural as Natural
import qualified Parun.Codec.AbilityKind as AbilityKind
import qualified Parun.Codec.ActivationCriteria as ActivationCriteria
import qualified Parun.Codec.Actor as Actor
import qualified Parun.Codec.AloneAmong as AloneAmong
import qualified Parun.Codec.Anchor as Anchor
import qualified Parun.Codec.BlockRole as BlockRole
import qualified Parun.Codec.BlockStatus as BlockStatus
import qualified Parun.Codec.BoundReading as BoundReading
import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.CardType as CardType
import qualified Parun.Codec.ChoiceOrdinal as ChoiceOrdinal
import qualified Parun.Codec.Color as Color
import qualified Parun.Codec.Comparison as Comparison
import qualified Parun.Codec.ControlMargin as ControlMargin
import qualified Parun.Codec.ControllerRelation as ControllerRelation
import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.CounterLinked as CounterLinked
import qualified Parun.Codec.CounterMotion as CounterMotion
import qualified Parun.Codec.DamageKind as DamageKind
import qualified Parun.Codec.DealtDamageTo as DealtDamageTo
import qualified Parun.Codec.DeclaredRole as DeclaredRole
import qualified Parun.Codec.Designation as Designation
import qualified Parun.Codec.EnteredZone as EnteredZone
import qualified Parun.Codec.ExiledPile as ExiledPile
import qualified Parun.Codec.Expansion as Expansion
import qualified Parun.Codec.Holder as Holder
import qualified Parun.Codec.Hop as Hop
import qualified Parun.Codec.KeywordCostPart as KeywordCostPart
import qualified Parun.Codec.KeywordDesignator as KeywordDesignator
import qualified Parun.Codec.KeywordFamily as KeywordFamily
import qualified Parun.Codec.Layout as Layout
import qualified Parun.Codec.LibraryDeparture as LibraryDeparture
import qualified Parun.Codec.ManaSymbolKind as ManaSymbolKind
import qualified Parun.Codec.Measures as Measures
import qualified Parun.Codec.NameChooser as NameChooser
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.Operand as Operand
import qualified Parun.Codec.ParityOperand as ParityOperand
import qualified Parun.Codec.PermanentAction as PermanentAction
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.ProductionTag as ProductionTag
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.Codec.StickerKind as StickerKind
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.Codec.Supertype as Supertype
import qualified Parun.Codec.Trait as Trait
import qualified Parun.Codec.TurnRole as TurnRole
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ActivationCriteria as ActivationCriteria.Type
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.BoundReading as BoundReading
import qualified Parun.Types.ChoiceOrdinal as ChoiceOrdinal
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.ControlMargin as ControlMargin
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterMotion as CounterMotion
import qualified Parun.Types.DamageKind as DamageKind.Type
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.Hop as Hop
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Trait as Trait
import qualified Parun.Types.Zone as ZoneType

-- | Recursive, mirroring Quantity's toJson/fromJson: And/Or carry their
-- operands as a JSON Array, Not as a single nested object, and each atom
-- delegates to the leaf-enum codec for the characteristic it cases on.
--
-- The keyword codec is a PARAMETER: Parun.Codec.Keyword imports this module for
-- CR 702.29e's typecycling filter, so a direct reference to Parun.Codec.Keyword
-- here would close a module cycle. Every caller at 'Filter Keyword.Keyword'
-- passes 'Parun.Codec.Keyword.codec' -- which is itself defined partly as
-- 'codec Keyword.codec', tying the knot at the value level; 'Codec.schema'
-- breaks it at the schema level (Parun.JsonSchema.Define.define registers the
-- name before running the body). And/Or/Not recurse on 'codec keywordCodec'
-- itself for the same reason. Parun.Codec.KeywordFamily is called DIRECTLY
-- below and needs no parameter, mirroring the types it encodes: a family
-- carries no filter, so that module imports neither this one nor
-- Parun.Codec.Keyword.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Filter.Filter keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "HasCardType" CardType.codec Filter.HasCardType (\x -> case x of Filter.HasCardType y -> Just y; _ -> Nothing),
      Arm.payload "HasSupertype" Supertype.codec Filter.HasSupertype (\x -> case x of Filter.HasSupertype y -> Just y; _ -> Nothing),
      Arm.payload "HasColor" Color.codec Filter.HasColor (\x -> case x of Filter.HasColor y -> Just y; _ -> Nothing),
      Arm.payload "HasSubtype" Subtype.codec Filter.HasSubtype (\x -> case x of Filter.HasSubtype y -> Just y; _ -> Nothing),
      Arm.payload "HasName" CardName.codec Filter.HasName (\x -> case x of Filter.HasName y -> Just y; _ -> Nothing),
      Arm.payload "HasNameOriginallyPrintedIn" Expansion.codec Filter.HasNameOriginallyPrintedIn (\x -> case x of Filter.HasNameOriginallyPrintedIn y -> Just y; _ -> Nothing),
      Arm.payload "NameWordsAtLeast" Common.natural Filter.NameWordsAtLeast (\x -> case x of Filter.NameWordsAtLeast y -> Just y; _ -> Nothing),
      Arm.payload "HasKeyword" keywordCodec Filter.HasKeyword (\x -> case x of Filter.HasKeyword y -> Just y; _ -> Nothing),
      Arm.payload "HasKeywordFamily" KeywordFamily.codec Filter.HasKeywordFamily (\x -> case x of Filter.HasKeywordFamily y -> Just y; _ -> Nothing),
      Arm.payload "HasProtectionFrom" Color.codec Filter.HasProtectionFrom (\x -> case x of Filter.HasProtectionFrom y -> Just y; _ -> Nothing),
      Arm.payload "Measures" Measures.codec Filter.Measures (\x -> case x of Filter.Measures y -> Just y; _ -> Nothing),
      Arm.payload "ControlledByRole" TurnRole.codec Filter.ControlledByRole (\x -> case x of Filter.ControlledByRole y -> Just y; _ -> Nothing),
      Arm.payload "HeldByBound" heldByBound (uncurry Filter.HeldByBound) (\x -> case x of Filter.HeldByBound h y -> Just (h, y); _ -> Nothing),
      -- Runtime-only, and accepted here anyway: the codec must stay total, so a
      -- corpus lint keeps the pool honest instead (#199) -- the treatment
      -- Modification.SetController's baked PlayerId gets.
      Arm.payload "HeldByPlayer" heldByPlayer (uncurry Filter.HeldByPlayer) (\x -> case x of Filter.HeldByPlayer h y -> Just (h, y); _ -> Nothing),
      Arm.payload "HeldByRecipient" Holder.codec Filter.HeldByRecipient (\x -> case x of Filter.HeldByRecipient y -> Just y; _ -> Nothing),
      Arm.payload "Shares" (shares (codec keywordCodec)) (uncurry Filter.Shares) (\x -> case x of Filter.Shares t a -> Just (t, a); _ -> Nothing),
      Arm.payload "ManaValueParity" ParityOperand.codec Filter.ManaValueParity (\x -> case x of Filter.ManaValueParity y -> Just y; _ -> Nothing),
      Arm.nullary "ManaCostRepeatsSymbol" Filter.ManaCostRepeatsSymbol,
      Arm.payload "ManaCostHasSymbol" ManaSymbolKind.codec Filter.ManaCostHasSymbol (\x -> case x of Filter.ManaCostHasSymbol y -> Just y; _ -> Nothing),
      Arm.nullary "ControllerIsOwner" Filter.ControllerIsOwner,
      Arm.payload "HeldBy" heldBy (uncurry Filter.HeldBy) (\x -> case x of Filter.HeldBy h r -> Just (h, r); _ -> Nothing),
      Arm.payload "IsPlayer" PlayerRelation.codec Filter.IsPlayer (\x -> case x of Filter.IsPlayer y -> Just y; _ -> Nothing),
      Arm.payload "IsPlayerInRole" TurnRole.codec Filter.IsPlayerInRole (\x -> case x of Filter.IsPlayerInRole y -> Just y; _ -> Nothing),
      Arm.payload "IsControllerOfBound" SlotName.codec Filter.IsControllerOfBound (\x -> case x of Filter.IsControllerOfBound y -> Just y; _ -> Nothing),
      Arm.payload "Controls" (controls keywordCodec) (uncurry Filter.Controls) (\x -> case x of Filter.Controls n y -> Just (n, y); _ -> Nothing),
      -- Natural rather than Common.integer above, so a negative literal is
      -- rejected at decode instead of decoding into a vacuously true filter: a
      -- zone holds no negative number of cards.
      Arm.payload "CardsInZoneAtLeast" cardsInZone (uncurry Filter.CardsInZoneAtLeast) (\x -> case x of Filter.CardsInZoneAtLeast z n -> Just (z, n); _ -> Nothing),
      Arm.payload "HoldsBoundAtLeast" (holdsBound keywordCodec) (\(h, r, n, y, f) -> Filter.HoldsBoundAtLeast h r n y f) (\x -> case x of Filter.HoldsBoundAtLeast h r n y f -> Just (h, r, n, y, f); _ -> Nothing),
      Arm.payload "TappedForManaThisTurn" (codec keywordCodec) Filter.TappedForManaThisTurn (\x -> case x of Filter.TappedForManaThisTurn y -> Just y; _ -> Nothing),
      Arm.nullary "IsSource" Filter.IsSource,
      Arm.nullary "BecameSource" Filter.BecameSource,
      Arm.payload "IsObject" ObjectId.codec Filter.IsObject (\x -> case x of Filter.IsObject y -> Just y; _ -> Nothing),
      Arm.nullary "TargetsSource" Filter.TargetsSource,
      Arm.nullary "HasSingleTarget" Filter.HasSingleTarget,
      -- Recursive for Controls' reason: the payload describes the one
      -- TARGET, and a card author writes it exactly as they write any other filter.
      Arm.payload "TargetsOnlyOne" (codec keywordCodec) Filter.TargetsOnlyOne (\x -> case x of Filter.TargetsOnlyOne y -> Just y; _ -> Nothing),
      Arm.payload "TargetsMatching" (codec keywordCodec) Filter.TargetsMatching (\x -> case x of Filter.TargetsMatching y -> Just y; _ -> Nothing),
      Arm.payload "TargetsPlayer" PlayerRelation.codec Filter.TargetsPlayer (\x -> case x of Filter.TargetsPlayer y -> Just y; _ -> Nothing),
      Arm.payload "IsBound" SlotName.codec Filter.IsBound (\x -> case x of Filter.IsBound y -> Just y; _ -> Nothing),
      Arm.nullary "IsTarget" Filter.IsTarget,
      Arm.payload "ControllerRelatesToBound" relatesToBound (\(r, h, y) -> Filter.ControllerRelatesToBound r h y) (\x -> case x of Filter.ControllerRelatesToBound r h y -> Just (r, h, y); _ -> Nothing),
      Arm.payload "HasChosenName" NameChooser.codec Filter.HasChosenName (\x -> case x of Filter.HasChosenName y -> Just y; _ -> Nothing),
      Arm.nullary "HasChosenColor" Filter.HasChosenColor,
      Arm.payload "Noted" CardName.codec Filter.Noted (\x -> case x of Filter.Noted y -> Just y; _ -> Nothing),
      -- A bare tag is the first choice, a card's only one.
      Arm.optionalPayload "HasChosenSubtype" ChoiceOrdinal.codec (Filter.HasChosenSubtype . Maybe.fromMaybe ChoiceOrdinal.First) (\x -> case x of Filter.HasChosenSubtype ChoiceOrdinal.First -> Just Nothing; Filter.HasChosenSubtype ordinal -> Just (Just ordinal); _ -> Nothing),
      Arm.nullary "HasChosenCardType" Filter.HasChosenCardType,
      Arm.payload "IsExiledWithSource" ExiledPile.codec Filter.IsExiledWithSource (\x -> case x of Filter.IsExiledWithSource y -> Just y; _ -> Nothing),
      Arm.nullary "IsLastExiledWithSource" Filter.IsLastExiledWithSource,
      Arm.nullary "IsChosenObject" Filter.IsChosenObject,
      Arm.nullary "OfChosenPlayer" Filter.OfChosenPlayer,
      Arm.nullary "NameStartsLikeCarrierSticker" Filter.NameStartsLikeCarrierSticker,
      Arm.payload "OfRelatedPlayer" PlayerRelation.codec Filter.OfRelatedPlayer (\x -> case x of Filter.OfRelatedPlayer y -> Just y; _ -> Nothing),
      -- Runtime-only, HeldByPlayer's treatment above.
      Arm.payload "OfSpecificPlayer" PlayerId.codec Filter.OfSpecificPlayer (\x -> case x of Filter.OfSpecificPlayer y -> Just y; _ -> Nothing),
      Arm.payload "OfBoundPlayer" SlotName.codec Filter.OfBoundPlayer (\x -> case x of Filter.OfBoundPlayer y -> Just y; _ -> Nothing),
      Arm.optionalPayload "IsAttacking" AloneAmong.codec Filter.IsAttacking (\x -> case x of Filter.IsAttacking y -> Just y; _ -> Nothing),
      Arm.payload "IsAttackingPlayer" ControllerRelation.codec Filter.IsAttackingPlayer (\x -> case x of Filter.IsAttackingPlayer y -> Just y; _ -> Nothing),
      Arm.payload "IsAttackingPlaneswalker" PlayerRelation.codec Filter.IsAttackingPlaneswalker (\x -> case x of Filter.IsAttackingPlaneswalker y -> Just y; _ -> Nothing),
      Arm.payload "IsAttackingBattle" PlayerRelation.codec Filter.IsAttackingBattle (\x -> case x of Filter.IsAttackingBattle y -> Just y; _ -> Nothing),
      Arm.optionalPayload "DeclaredAttackedThisCombat" SlotName.codec Filter.DeclaredAttackedThisCombat (\x -> case x of Filter.DeclaredAttackedThisCombat y -> Just y; _ -> Nothing),
      Arm.optionalPayload "IsBlocking" (codec keywordCodec) Filter.IsBlocking (\x -> case x of Filter.IsBlocking y -> Just y; _ -> Nothing),
      Arm.payload "HasBlockStatus" BlockStatus.codec Filter.HasBlockStatus (\x -> case x of Filter.HasBlockStatus y -> Just y; _ -> Nothing),
      Arm.payload "DeclaredThisTurn" DeclaredRole.codec Filter.DeclaredThisTurn (\x -> case x of Filter.DeclaredThisTurn y -> Just y; _ -> Nothing),
      Arm.nullary "CouldAttackThisTurn" Filter.CouldAttackThisTurn,
      Arm.nullary "DeclaredAttackerThisCombat" Filter.DeclaredAttackerThisCombat,
      Arm.nullary "DeclaredBlockerThisCombat" Filter.DeclaredBlockerThisCombat,
      Arm.nullary "HadToAttackThisCombat" Filter.HadToAttackThisCombat,
      Arm.nullary "IsGoaded" Filter.IsGoaded,
      Arm.payload "LeftLibraryThisTurn" LibraryDeparture.codec Filter.LeftLibraryThisTurn (\x -> case x of Filter.LeftLibraryThisTurn y -> Just y; _ -> Nothing),
      Arm.payload "CrewedThisTurn" Actor.codec Filter.CrewedThisTurn (\x -> case x of Filter.CrewedThisTurn y -> Just y; _ -> Nothing),
      Arm.nullary "ConvokedSourceThisTurn" Filter.ConvokedSourceThisTurn,
      Arm.nullary "SaddledSourceThisTurn" Filter.SaddledSourceThisTurn,
      Arm.payload "CantPayKeywordCost" KeywordCostPart.codec Filter.CantPayKeywordCost (\x -> case x of Filter.CantPayKeywordCost y -> Just y; _ -> Nothing),
      Arm.optionalPayload "DealtDamageThisTurn" (dealtDamageThisTurn keywordCodec) (maybe (Filter.DealtDamageThisTurn Nothing Nothing False) (\(k, b, e) -> Filter.DealtDamageThisTurn k b e)) (\x -> case x of Filter.DealtDamageThisTurn Nothing Nothing False -> Just Nothing; Filter.DealtDamageThisTurn k b e -> Just (Just (k, b, e)); _ -> Nothing),
      Arm.nullary "LostLifeThisTurn" Filter.LostLifeThisTurn,
      Arm.nullary "ActivatedThisTurn" Filter.ActivatedThisTurn,
      Arm.payload "ActedThisTurn" PermanentAction.codec Filter.ActedThisTurn (\x -> case x of Filter.ActedThisTurn y -> Just y; _ -> Nothing),
      Arm.payload "DealtDamageTo" DealtDamageTo.codec Filter.DealtDamageTo (\x -> case x of Filter.DealtDamageTo y -> Just y; _ -> Nothing),
      Arm.payload "EnteredThisTurn" EnteredZone.codec Filter.EnteredThisTurn (\x -> case x of Filter.EnteredThisTurn y -> Just y; _ -> Nothing),
      Arm.payload "CountersMovedThisTurn" (countersMovedThisTurn keywordCodec) (uncurry Filter.CountersMovedThisTurn) (\x -> case x of Filter.CountersMovedThisTurn m k -> Just (m, k); _ -> Nothing),
      Arm.nullary "ControlledSinceTurnBegan" Filter.ControlledSinceTurnBegan,
      -- Recursive for Controls' reason: the payload describes the
      -- HOST, and a card author writes it exactly as they write any other filter.
      Arm.payload "AttachedTo" (codec keywordCodec) Filter.AttachedTo (\x -> case x of Filter.AttachedTo y -> Just y; _ -> Nothing),
      Arm.payload "BeforeMove" (codec keywordCodec) Filter.BeforeMove (\x -> case x of Filter.BeforeMove y -> Just y; _ -> Nothing),
      -- Recursive for the arm above's reason, the payload describing the ATTACHER
      -- rather than the host.
      Arm.payload "HasAttached" (codec keywordCodec) Filter.HasAttached (\x -> case x of Filter.HasAttached y -> Just y; _ -> Nothing),
      Arm.payload "IsAttachedTo" (Anchor.codec (codec keywordCodec)) Filter.IsAttachedTo (\x -> case x of Filter.IsAttachedTo y -> Just y; _ -> Nothing),
      Arm.nullary "IsEvaluated" Filter.IsEvaluated,
      Arm.nullary "IsHostOfSource" Filter.IsHostOfSource,
      Arm.nullary "EnteredWithSource" Filter.EnteredWithSource,
      Arm.payload "CounterLinkedToSource" (CounterLinked.codec keywordCodec) Filter.CounterLinkedToSource (\x -> case x of Filter.CounterLinkedToSource y -> Just y; _ -> Nothing),
      Arm.nullary "AttachedNoLaterThanSource" Filter.AttachedNoLaterThanSource,
      Arm.nullary "CanHostSubject" Filter.CanHostSubject,
      Arm.nullary "CanAttachToSubject" Filter.CanAttachToSubject,
      Arm.nullary "HasEntryHost" Filter.HasEntryHost,
      Arm.payload "HostOfSubjectHasCardType" CardType.codec Filter.HostOfSubjectHasCardType (\x -> case x of Filter.HostOfSubjectHasCardType y -> Just y; _ -> Nothing),
      Arm.nullary "IsCommander" Filter.IsCommander,
      Arm.nullary "IsToken" Filter.IsToken,
      Arm.nullary "IsCard" Filter.IsCard,
      -- No payload is every activated ability, the criteria all Nothing, which
      -- is what Squelch prints.
      Arm.optionalPayload "IsActivatedAbility" (ActivationCriteria.codec keywordCodec) (Filter.IsActivatedAbility . Maybe.fromMaybe everyActivation) (\x -> case x of Filter.IsActivatedAbility y -> Just (if y == everyActivation then Nothing else Just y); _ -> Nothing),
      Arm.nullary "IsAbility" Filter.IsAbility,
      Arm.nullary "IsEmblem" Filter.IsEmblem,
      -- Recursive for AttachedTo's reason, the payload describing the ability's
      -- SOURCE rather than the ability.
      Arm.payload "FromSource" (codec keywordCodec) Filter.FromSource (\x -> case x of Filter.FromSource y -> Just y; _ -> Nothing),
      Arm.nullary "IsTapped" Filter.IsTapped,
      Arm.nullary "IsFaceDown" Filter.IsFaceDown,
      -- Recursive for AttachedTo's reason, the payload describing the CARD
      -- representing the candidate rather than the candidate.
      Arm.payload "RepresentedByCard" (codec keywordCodec) Filter.RepresentedByCard (\x -> case x of Filter.RepresentedByCard y -> Just y; _ -> Nothing),
      Arm.nullary "IsExiledFaceDown" Filter.IsExiledFaceDown,
      Arm.nullary "IsWarped" Filter.IsWarped,
      Arm.nullary "Transformed" Filter.Transformed,
      Arm.nullary "IsDoubleFaced" Filter.IsDoubleFaced,
      Arm.nullary "IsRingBearer" Filter.IsRingBearer,
      Arm.nullary "IsPaired" Filter.IsPaired,
      Arm.nullary "IsPairedWithSource" Filter.IsPairedWithSource,
      Arm.payload "BlockPartnerOfSource" BlockRole.codec Filter.BlockPartnerOfSource (\x -> case x of Filter.BlockPartnerOfSource y -> Just y; _ -> Nothing),
      Arm.payload "HasDesignation" Designation.codec Filter.HasDesignation (\x -> case x of Filter.HasDesignation y -> Just y; _ -> Nothing),
      Arm.payload "HasCounters" (hasCounters keywordCodec) (\(c, o, k) -> Filter.HasCounters c o k) (\x -> case x of Filter.HasCounters c o k -> Just (c, o, k); _ -> Nothing),
      Arm.nullary "HasCountersOfAnyKind" Filter.HasCountersOfAnyKind,
      Arm.payload "HasSticker" StickerKind.codec Filter.HasSticker (\x -> case x of Filter.HasSticker y -> Just y; _ -> Nothing),
      Arm.nullary "Stickered" Filter.Stickered,
      Arm.optionalPayload "HasActivatedAbility" AbilityKind.codec Filter.HasActivatedAbility (\x -> case x of Filter.HasActivatedAbility y -> Just y; _ -> Nothing),
      Arm.nullary "HasAbility" Filter.HasAbility,
      Arm.payload "IsInZone" Zone.codec Filter.IsInZone (\x -> case x of Filter.IsInZone y -> Just y; _ -> Nothing),
      Arm.optionalPayload "WasCastFrom" Zone.codec Filter.WasCastFrom (\x -> case x of Filter.WasCastFrom y -> Just y; _ -> Nothing),
      Arm.payload "TagWasSpent" ProductionTag.codec Filter.TagWasSpent (\x -> case x of Filter.TagWasSpent y -> Just y; _ -> Nothing),
      Arm.nullary "PaidWithManaFromSource" Filter.PaidWithManaFromSource,
      Arm.payload "Paid" (KeywordDesignator.codec keywordCodec) Filter.Paid (\x -> case x of Filter.Paid y -> Just y; _ -> Nothing),
      Arm.nullary "IsPrototyped" Filter.IsPrototyped,
      Arm.nullary "IsPreparedSpell" Filter.IsPreparedSpell,
      Arm.nullary "IsModal" Filter.IsModal,
      Arm.payload "HasInsetFrame" Layout.codec Filter.HasInsetFrame (\x -> case x of Filter.HasInsetFrame y -> Just y; _ -> Nothing),
      Arm.payload "And" (Common.list (codec keywordCodec)) Filter.And (\x -> case x of Filter.And y -> Just y; _ -> Nothing),
      Arm.payload "Or" (Common.list (codec keywordCodec)) Filter.Or (\x -> case x of Filter.Or y -> Just y; _ -> Nothing),
      Arm.payload "Not" (codec keywordCodec) Filter.Not (\x -> case x of Filter.Not y -> Just y; _ -> Nothing),
      Arm.payload "Unwritten" (codec keywordCodec) Filter.Unwritten (\x -> case x of Filter.Unwritten y -> Just y; _ -> Nothing)
    ]

-- | IsActivatedAbility's payload when absent: no criterion narrows it.
everyActivation :: ActivationCriteria.Type.ActivationCriteria keyword
everyActivation = ActivationCriteria.Type.MkActivationCriteria Nothing Nothing Nothing Nothing

-- | CR 110.2's comparison keyed by name: the margin, and the filter
-- describing the permanents being counted -- recursive like Not, and for the
-- atom's own reason rather than the combinator's.
controls :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ControlMargin.ControlMargin, Filter.Filter keyword)
controls keywordCodec =
  Fields.object $
    (,)
      <$> Fields.required "margin" ControlMargin.codec fst
      <*> Fields.required "filter" (codec keywordCodec) snd

-- | Which damage counts: its kind, the filter its source matched, and whether
-- only damage with an excess.
dealtDamageThisTurn :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Maybe DamageKind.Type.DamageKind, Maybe (Filter.Filter keyword), Bool)
dealtDamageThisTurn keywordCodec =
  Fields.object $
    (,,)
      <$> Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) (\(k, _, _) -> k)
      <*> Fields.defaulted "by" Nothing (Common.maybe (codec keywordCodec)) (\(_, b, _) -> b)
      <*> Fields.defaulted "excess" False Common.boolean (\(_, _, e) -> e)

-- | Which holder, read off which incarnation, how many of the slot's objects
-- the candidate must hold, and which count.
holdsBound :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Holder.Holder, BoundReading.BoundReading, Natural.Natural, SlotName.SlotName, Filter.Filter keyword)
holdsBound keywordCodec =
  Fields.object $
    (,,,,)
      <$> Fields.required "holder" Holder.codec (\(h, _, _, _, _) -> h)
      <*> Fields.required "reading" BoundReading.codec (\(_, r, _, _, _) -> r)
      <*> Fields.required "atLeast" Common.natural (\(_, _, n, _, _) -> n)
      <*> Fields.required "slot" SlotName.codec (\(_, _, _, y, _) -> y)
      <*> Fields.required "filter" (codec keywordCodec) (\(_, _, _, _, f) -> f)

-- | The zone, and how many cards at least.
cardsInZone :: Codec.Codec (ZoneType.Zone, Natural.Natural)
cardsInZone =
  Fields.object $
    (,)
      <$> Fields.required "zone" Zone.codec fst
      <*> Fields.required "atLeast" Common.natural snd

-- | The counters' kind, and how their count compares; at least one when unsaid.
hasCounters :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Comparison.Comparison, Operand.Operand, CounterKind.CounterKind keyword)
hasCounters keywordCodec =
  Fields.object $
    (,,)
      <$> Fields.defaulted "comparison" Comparison.AtLeast Comparison.codec (\(c, _, _) -> c)
      <*> Fields.defaulted "operand" (Operand.Literal 1) Operand.codec (\(_, o, _) -> o)
      <*> Fields.required "kind" (CounterKind.codec keywordCodec) (\(_, _, k) -> k)

-- | Which way the counters moved, a put's putter related to the perspective,
-- and their kind.
countersMovedThisTurn :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CounterMotion.CounterMotion, Maybe (CounterKind.CounterKind keyword))
countersMovedThisTurn keywordCodec =
  Fields.object $
    (,)
      <$> Fields.required "motion" CounterMotion.codec fst
      <*> Fields.defaulted "kind" Nothing (Common.maybe (CounterKind.codec keywordCodec)) snd

-- | Which of the candidate's players, and how it relates to the perspective.
heldBy :: Codec.Codec (Holder.Holder, ControllerRelation.ControllerRelation)
heldBy =
  Fields.object $
    (,)
      <$> Fields.required "holder" Holder.codec fst
      <*> Fields.required "relation" ControllerRelation.bare snd

-- | CR 603.2's bound holder keyed by name: which of the candidate's players,
-- and the slot binding the player it must be.
heldByBound :: Codec.Codec (Holder.Holder, SlotName.SlotName)
heldByBound =
  Fields.object $
    (,)
      <$> Fields.required "holder" Holder.codec fst
      <*> Fields.required "slot" SlotName.codec snd

-- | The bound holder baked: which of the candidate's players, and the player it
-- must be.
heldByPlayer :: Codec.Codec (Holder.Holder, PlayerId.PlayerId)
heldByPlayer =
  Fields.object $
    (,)
      <$> Fields.required "holder" Holder.codec fst
      <*> Fields.required "player" PlayerId.codec snd

-- | What is shared, and with which object.
shares :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec (Filter.Filter keyword) -> Codec.Codec (Trait.Trait, Anchor.Anchor (Filter.Filter keyword))
shares filterCodec =
  Fields.object $
    (,)
      <$> Fields.required "trait" Trait.codec fst
      <*> Fields.required "anchor" (Anchor.codec filterCodec) snd

-- | CR 110.2's comparison across two objects keyed by name: the relation, the
-- hop, and the slot holding the object whose controller it is relative to.
relatesToBound :: Codec.Codec (PlayerRelation.PlayerRelation, Hop.Hop, SlotName.SlotName)
relatesToBound =
  Fields.object $
    (,,)
      <$> Fields.required "relation" PlayerRelation.codec (\(r, _, _) -> r)
      <*> Fields.required "hop" Hop.codec (\(_, h, _) -> h)
      <*> Fields.required "slot" SlotName.codec (\(_, _, y) -> y)

-- Exhaustive, not over Parun.Engine.Children: the codec is an interpreter, every atom a tag of its own.
tagOf :: Filter.Filter keyword -> String
tagOf x = case x of
  Filter.HasCardType {} -> "HasCardType"
  Filter.HasSupertype {} -> "HasSupertype"
  Filter.HasColor {} -> "HasColor"
  Filter.HasSubtype {} -> "HasSubtype"
  Filter.HasName {} -> "HasName"
  Filter.HasNameOriginallyPrintedIn {} -> "HasNameOriginallyPrintedIn"
  Filter.NameWordsAtLeast {} -> "NameWordsAtLeast"
  Filter.HasKeyword {} -> "HasKeyword"
  Filter.HasKeywordFamily {} -> "HasKeywordFamily"
  Filter.HasProtectionFrom {} -> "HasProtectionFrom"
  Filter.Measures {} -> "Measures"
  Filter.ControlledByRole {} -> "ControlledByRole"
  Filter.HeldByBound {} -> "HeldByBound"
  Filter.HeldByPlayer {} -> "HeldByPlayer"
  Filter.HeldByRecipient {} -> "HeldByRecipient"
  Filter.Shares {} -> "Shares"
  Filter.ManaValueParity {} -> "ManaValueParity"
  Filter.ManaCostRepeatsSymbol -> "ManaCostRepeatsSymbol"
  Filter.ManaCostHasSymbol {} -> "ManaCostHasSymbol"
  Filter.ControllerIsOwner {} -> "ControllerIsOwner"
  Filter.HeldBy {} -> "HeldBy"
  Filter.IsPlayer {} -> "IsPlayer"
  Filter.IsPlayerInRole {} -> "IsPlayerInRole"
  Filter.IsControllerOfBound {} -> "IsControllerOfBound"
  Filter.Controls {} -> "Controls"
  Filter.CardsInZoneAtLeast {} -> "CardsInZoneAtLeast"
  Filter.HoldsBoundAtLeast {} -> "HoldsBoundAtLeast"
  Filter.TappedForManaThisTurn {} -> "TappedForManaThisTurn"
  Filter.IsSource {} -> "IsSource"
  Filter.BecameSource {} -> "BecameSource"
  Filter.IsObject {} -> "IsObject"
  Filter.TargetsSource {} -> "TargetsSource"
  Filter.HasSingleTarget {} -> "HasSingleTarget"
  Filter.TargetsOnlyOne {} -> "TargetsOnlyOne"
  Filter.TargetsMatching {} -> "TargetsMatching"
  Filter.TargetsPlayer {} -> "TargetsPlayer"
  Filter.IsBound {} -> "IsBound"
  Filter.IsTarget {} -> "IsTarget"
  Filter.ControllerRelatesToBound {} -> "ControllerRelatesToBound"
  Filter.HasChosenName {} -> "HasChosenName"
  Filter.HasChosenColor {} -> "HasChosenColor"
  Filter.Noted {} -> "Noted"
  Filter.HasChosenSubtype {} -> "HasChosenSubtype"
  Filter.HasChosenCardType {} -> "HasChosenCardType"
  Filter.IsExiledWithSource {} -> "IsExiledWithSource"
  Filter.IsLastExiledWithSource -> "IsLastExiledWithSource"
  Filter.IsChosenObject -> "IsChosenObject"
  Filter.OfChosenPlayer {} -> "OfChosenPlayer"
  Filter.NameStartsLikeCarrierSticker {} -> "NameStartsLikeCarrierSticker"
  Filter.OfRelatedPlayer {} -> "OfRelatedPlayer"
  Filter.OfSpecificPlayer {} -> "OfSpecificPlayer"
  Filter.OfBoundPlayer {} -> "OfBoundPlayer"
  Filter.IsAttacking {} -> "IsAttacking"
  Filter.IsAttackingPlayer {} -> "IsAttackingPlayer"
  Filter.IsAttackingPlaneswalker {} -> "IsAttackingPlaneswalker"
  Filter.IsAttackingBattle {} -> "IsAttackingBattle"
  Filter.DeclaredAttackedThisCombat {} -> "DeclaredAttackedThisCombat"
  Filter.IsBlocking {} -> "IsBlocking"
  Filter.HasBlockStatus {} -> "HasBlockStatus"
  Filter.DeclaredThisTurn {} -> "DeclaredThisTurn"
  Filter.CouldAttackThisTurn {} -> "CouldAttackThisTurn"
  Filter.DeclaredAttackerThisCombat {} -> "DeclaredAttackerThisCombat"
  Filter.DeclaredBlockerThisCombat {} -> "DeclaredBlockerThisCombat"
  Filter.HadToAttackThisCombat {} -> "HadToAttackThisCombat"
  Filter.IsGoaded {} -> "IsGoaded"
  Filter.LeftLibraryThisTurn {} -> "LeftLibraryThisTurn"
  Filter.CrewedThisTurn {} -> "CrewedThisTurn"
  Filter.ConvokedSourceThisTurn {} -> "ConvokedSourceThisTurn"
  Filter.SaddledSourceThisTurn {} -> "SaddledSourceThisTurn"
  Filter.CantPayKeywordCost {} -> "CantPayKeywordCost"
  Filter.DealtDamageThisTurn {} -> "DealtDamageThisTurn"
  Filter.LostLifeThisTurn {} -> "LostLifeThisTurn"
  Filter.ActivatedThisTurn {} -> "ActivatedThisTurn"
  Filter.ActedThisTurn {} -> "ActedThisTurn"
  Filter.DealtDamageTo {} -> "DealtDamageTo"
  Filter.EnteredThisTurn {} -> "EnteredThisTurn"
  Filter.CountersMovedThisTurn {} -> "CountersMovedThisTurn"
  Filter.ControlledSinceTurnBegan {} -> "ControlledSinceTurnBegan"
  Filter.AttachedTo {} -> "AttachedTo"
  Filter.BeforeMove {} -> "BeforeMove"
  Filter.HasAttached {} -> "HasAttached"
  Filter.IsAttachedTo {} -> "IsAttachedTo"
  Filter.IsEvaluated {} -> "IsEvaluated"
  Filter.IsHostOfSource {} -> "IsHostOfSource"
  Filter.EnteredWithSource {} -> "EnteredWithSource"
  Filter.CounterLinkedToSource {} -> "CounterLinkedToSource"
  Filter.AttachedNoLaterThanSource {} -> "AttachedNoLaterThanSource"
  Filter.CanHostSubject {} -> "CanHostSubject"
  Filter.CanAttachToSubject {} -> "CanAttachToSubject"
  Filter.HasEntryHost {} -> "HasEntryHost"
  Filter.HostOfSubjectHasCardType {} -> "HostOfSubjectHasCardType"
  Filter.IsCommander {} -> "IsCommander"
  Filter.IsToken {} -> "IsToken"
  Filter.IsCard {} -> "IsCard"
  Filter.IsActivatedAbility {} -> "IsActivatedAbility"
  Filter.IsAbility {} -> "IsAbility"
  Filter.IsEmblem {} -> "IsEmblem"
  Filter.FromSource {} -> "FromSource"
  Filter.IsTapped {} -> "IsTapped"
  Filter.IsFaceDown {} -> "IsFaceDown"
  Filter.RepresentedByCard {} -> "RepresentedByCard"
  Filter.IsExiledFaceDown {} -> "IsExiledFaceDown"
  Filter.IsWarped {} -> "IsWarped"
  Filter.Transformed {} -> "Transformed"
  Filter.IsDoubleFaced {} -> "IsDoubleFaced"
  Filter.IsRingBearer {} -> "IsRingBearer"
  Filter.IsPaired {} -> "IsPaired"
  Filter.IsPairedWithSource {} -> "IsPairedWithSource"
  Filter.BlockPartnerOfSource {} -> "BlockPartnerOfSource"
  Filter.HasDesignation {} -> "HasDesignation"
  Filter.HasCounters {} -> "HasCounters"
  Filter.HasCountersOfAnyKind {} -> "HasCountersOfAnyKind"
  Filter.HasSticker {} -> "HasSticker"
  Filter.Stickered {} -> "Stickered"
  Filter.HasActivatedAbility {} -> "HasActivatedAbility"
  Filter.HasAbility {} -> "HasAbility"
  Filter.IsInZone {} -> "IsInZone"
  Filter.WasCastFrom {} -> "WasCastFrom"
  Filter.TagWasSpent {} -> "TagWasSpent"
  Filter.PaidWithManaFromSource {} -> "PaidWithManaFromSource"
  Filter.Paid {} -> "Paid"
  Filter.IsPrototyped {} -> "IsPrototyped"
  Filter.IsPreparedSpell {} -> "IsPreparedSpell"
  Filter.IsModal {} -> "IsModal"
  Filter.HasInsetFrame {} -> "HasInsetFrame"
  Filter.And {} -> "And"
  Filter.Or {} -> "Or"
  Filter.Not {} -> "Not"
  Filter.Unwritten {} -> "Unwritten"
