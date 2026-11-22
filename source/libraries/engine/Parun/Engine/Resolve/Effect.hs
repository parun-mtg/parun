-- Carrying out one effect (CR 608.2): applyOneEffect, the sole casing on
-- Effect at resolution, with the prevention riders, hand actions and mana
-- abilities that share its recursion. Split out of Parun.Engine.Resolve for
-- size; the spell and ability resolution that drives it stays there, and both
-- are imported by callers under the same Resolve alias. Each arm's body is a
-- function in Parun.Engine.Resolve.Arms, handed the recursion below that it
-- calls back into; the helpers that never recurse into resolving an effect are
-- Parun.Engine.Resolve.Common.
module Parun.Engine.Resolve.Effect where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.ActivationRestriction as ActivationRestriction
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Card as Card
import qualified Parun.Engine.Cast as Cast
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Condition as Condition
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Decide as Decide
import qualified Parun.Engine.Dice as Dice
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Event.Trigger as Trigger
import qualified Parun.Engine.Exile as Exile
import qualified Parun.Engine.Expiry as Expiry
import qualified Parun.Engine.Filter as Filter
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Loyalty as Loyalty
import qualified Parun.Engine.Modal as Modal
import qualified Parun.Engine.PlayerEffect as PlayerEffect
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection
import qualified Parun.Engine.Replacement as Replacement
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Resolve.Arms.Counters as Counters
import qualified Parun.Engine.Resolve.Arms.Damage as Damage
import qualified Parun.Engine.Resolve.Arms.Objects as Objects
import qualified Parun.Engine.Resolve.Arms.Zones as Zones
import qualified Parun.Engine.Resolve.Common as Common
import Parun.Engine.Resolve.Slots (boundSlots, resolvingAbilityName, slotBindings)
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.AbilityWasActivated as AbilityWasActivated
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivationRestriction as ActivationRestriction.Type
import qualified Parun.Types.ActiveCopyCast as ActiveCopyCast
import qualified Parun.Types.AppliedReduction as AppliedReduction
import qualified Parun.Types.AttackChoice as AttackChoice
import qualified Parun.Types.Binding as Binding.Type
import qualified Parun.Types.CandidateCost as CandidateCost
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CastObligation as CastObligation
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.CastRepetition as CastRepetition
import Parun.Types.ClauseIndex (ClauseIndex)
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.ControlDuration as ControlDuration
import qualified Parun.Types.CopyStackObject as CopyStackObject
import qualified Parun.Types.CopyTargets as CopyTargets
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CountedAct as CountedAct
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.DieResult as DieResult
import Parun.Types.Effect (Effect)
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.ExileLink as ExileLink
import qualified Parun.Types.ExilePlayPermission as ExilePlayPermission
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.FaceDownReason as FaceDownReason
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.Facing as Facing
import Parun.Types.Game (Game)
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.HandActionPerformer as HandActionPerformer
import qualified Parun.Types.Keyword as Keyword.Type
import qualified Parun.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.Modal as Modal.Type
import Parun.Types.ModeIndex (ModeIndex)
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PayObligation as PayObligation
import qualified Parun.Types.PayOffer as PayOffer
import qualified Parun.Types.PayerAnswer as PayerAnswer
import qualified Parun.Types.Payment as Payment
import qualified Parun.Types.PaymentMoment as PaymentMoment
import qualified Parun.Types.PaymentSubject as PaymentSubject
import qualified Parun.Types.PendingEntryEffect as PendingEntryEffect
import qualified Parun.Types.PendingOrigin as PendingOrigin
import qualified Parun.Types.PendingReplacementEffect as PendingReplacementEffect
import qualified Parun.Types.PendingTrigger as PendingTrigger
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PermissionVerb as PermissionVerb
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerAction as PlayerAction
import qualified Parun.Types.PlayerControl as PlayerControl
import Parun.Types.PlayerId (PlayerId)
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Prevention as Prevention
import qualified Parun.Types.PreventionRider as PreventionRider
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Quantity as Quantity.Type
import Parun.Types.Recipient (Recipient)
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RerollReach as RerollReach
import qualified Parun.Types.Response as Response
import Parun.Types.Result (Result)
import qualified Parun.Types.RollAdjustment as RollAdjustment
import qualified Parun.Types.RollReading as RollReading
import qualified Parun.Types.Scratch as Scratch
import qualified Parun.Types.Settle as Settle
import Parun.Types.SlotName (SlotName)
import qualified Parun.Types.Snapshots as Snapshots
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.Throw as Throw
import qualified Parun.Types.ThrowKind as ThrowKind
import qualified Parun.Types.ThrowResult as ThrowResult
import qualified Parun.Types.TotalCap as TotalCap
import qualified Parun.Types.TriggerSource as TriggerSource
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.UpToCasts as UpToCasts
import qualified Parun.Types.Zone as Zone

-- CR 608.2g: make the offer Effect.OfferCast carries, once or over again.
--
-- CR 608.2d's choice among the named cards is `offerCastOnce` below; this is
-- the repetition Parun.Types.CastRepetition names -- Fevered Suspicion's "you
-- may cast ANY NUMBER of spells from among those nonland cards", where Shell of
-- the Last Kappa's "cast A SPELL from among cards exiled with Shell of the Last
-- Kappa" stops after one. CR 608.2g's own "which may include casting other
-- spells this way" is what admits the repeat.
--
-- The card a round took is dropped from the next round's candidates rather than
-- the round re-reading the whole set: CR 400.7 has already made the cast card a
-- new object, so it would be dropped anyway, and dropping it HERE is what bounds
-- the recursion -- a round that somehow left its card where it was cannot offer
-- it for ever.
--
-- A DECLINE ENDS IT, and no reachable board can tell that from a decline that
-- skipped one card and went on: the caster names which card they are declining by
-- picking it at Prompt.ChooseOfferedCastSpell, so every card they wanted was
-- reachable before they said no.
--
-- Mandatory repeats too, and terminates for the same reason: each round casts a
-- different card, and a card the rules will not let them cast never reaches the
-- offer (question 4 below).
-- CR 707.12's copy is minted BEFORE the first round rather than at the cast, and
-- once for the whole offer rather than once per round: rule 707.12a's "an effect
-- that creates multiple copies and says a player 'may cast' those objects" makes
-- the creation one act and the casting a decision per object, so a repeated
-- offer asks about the same copies over again. A copy nobody casts is left where
-- it was made, for CR 704.5e to sweep at the next check (Parun.Engine.Sba).
--
-- Rod of Absorption's total is the repeat with a budget
-- (Parun.Types.TotalCap, Game.pickCounted's bound): each round offers only a
-- half whose weight fits what is left (CR 202.3d, read per face beside
-- CastOffer.restriction, so a split card's half is measured as the spell it
-- becomes -- unobserved, no board casting a split card this way), and the
-- cast spell's weight is spent from it.
-- `reading Nothing` reads the budget's Quantity once, before the first round
-- (CR 608.2h), and `reading (Just candidate)` a weight against that
-- candidate's view. UpTo's count is the same repeat with a cap on casts, read
-- once the same way, and Invoke Calamity's UpTo carries both.
--
-- Answers the spells the offer's casts put on the stack, for CR 400.7h's
-- OfferCast.slot.
offerCast :: Game Result -> Filter.Context -> (Maybe (ObjectId, Filter.View) -> Quantity.Type.Quantity -> Maybe Integer) -> [ObjectId] -> PlayerId -> CastObligation.CastObligation -> PermissionVerb.PermissionVerb -> Maybe PlayerId -> CastRepetition.CastRepetition -> Bool -> CastOffer.CastOffer -> Game [ObjectId]
offerCast runSubgame context reading named caster optionality verb retake repetition copied offer = do
  subjects <- if copied then Maybe.catMaybes <$> traverse (Common.castableCopy caster) named else pure named
  case repetition of
    CastRepetition.Once -> foldMap (Maybe.maybeToList . snd) <$> offerCastOnce runSubgame context (\_ _ -> True) subjects caster optionality verb retake offer
    CastRepetition.AnyNumber -> again Nothing Nothing subjects
    CastRepetition.WithinTotal total -> again (Just (budgetOf total)) Nothing subjects
    CastRepetition.UpTo (UpToCasts.MkUpToCasts casts total) -> again (fmap budgetOf total) (Just (Maybe.fromMaybe 0 (reading Nothing casts))) subjects
  where
    budgetOf (TotalCap.MkTotalCap weight most) = (weighs weight, Maybe.fromMaybe 0 (reading Nothing most))
    -- TotalCap.weight's read; a negative one is unobserved, mana value having
    -- none (CR 202.3).
    weighs weight oid view = Maybe.fromMaybe 0 (reading (Just (oid, view)) weight)
    again :: Maybe (ObjectId -> Filter.View -> Integer, Integer) -> Maybe Integer -> [ObjectId] -> Game [ObjectId]
    again _ (Just casts) _ | casts <= 0 = pure []
    again budget casts remaining = do
      let fits = maybe (\_ _ -> True) (\(weigh, room) oid view -> weigh oid view <= room) budget
      taken <- offerCastOnce runSubgame context fits remaining caster optionality verb retake offer
      case taken of
        Nothing -> pure []
        Just (oid, spell) -> do
          gs <- State.get
          let spent weigh = maybe 0 (\sid -> maybe 0 (weigh sid) (Projection.fullView gs sid)) spell
          (Maybe.maybeToList spell <>) <$> again (fmap (\(weigh, room) -> (weigh, room - spent weigh)) budget) (fmap (subtract 1) casts) (filter (/= oid) remaining)

-- CR 707.13 / 707.14: offer the cast of a copy of this printing created outside
-- the game (Event.mintOutside), bracketing `offerCast` with the mint and the
-- discard. An uncast copy is deleted as the offer returns: CR 109.1 and CR
-- 400.11c leave nothing able to observe it afterwards, and it bounds
-- Object.zone's placeholder to one resolution, in which no player gets priority
-- and no SBA is checked.
offerOutsideCopy :: Game Result -> Filter.Context -> PlayerId -> PrintingId.PrintingId -> CastOffer.CastOffer -> Game ()
offerOutsideCopy runSubgame context caster printingId offer = do
  copyId <- State.state (Event.mintOutside caster printingId)
  Monad.void (offerCast runSubgame context (\_ _ -> Nothing) [copyId] caster CastObligation.Optional PermissionVerb.Cast Nothing CastRepetition.Once False offer)
  State.modify' $ \g ->
    if Set.member copyId (GameState.outsideCopies g)
      then g {GameState.objects = Map.delete copyId (GameState.objects g), GameState.outsideCopies = Set.delete copyId (GameState.outsideCopies g)}
      else g

-- One round of the offer above: CR 608.2d's choice among the cards `named`
-- still holds, and the cast if it is taken. Answers the id of the card that was
-- cast, which is what tells the caller whether to go round again, beside the
-- spell CR 601.2a put on the stack, which a land play and CR 601.2e's reversal
-- lack.
--
-- The four questions, in the order the rules ask them:
--
--   1. IS THERE ANYTHING TO OFFER -- an id the reference named (CR 400.7) may no
--      longer resolve to an object (CR 603.7c).
--   2. WHICH FACE: CR 712.8a for the `transformed` rider, otherwise
--      Game.castableFacesOf (CR 709.3, CR 712.8b, CR 716.3, CR 721.3) plus CR
--      702.102a's fused face for a fuse card in the caster's hand, less the face
--      CR 702.162a's or CR 702.146a's alternative cost is the only road to, when
--      this offer states an alternative cost of its own (CR 118.9a).
--   3. WHAT IT COSTS (CR 118.9): `withoutPayingManaCost` or a stated
--      `payingInstead` (CR 702.94a); otherwise CR 601.2b's own candidates, and
--      how mana may be spent toward it (CR 118.14's `spending`).
--   4. MAY IT BE CAST AT ALL -- Cast.castableWhenOffered, asked BEFORE the
--      prompt so no cast is offered that the announcement would reverse, and
--      beside it whatever quality the offer's own CastOffer.restriction states of
--      the resulting spell (CR 702.85a).
--
-- Questions 3 and 4 are asked of EACH half of EACH named card separately (CR
-- 709.3a, CR 712.8c); where more than one survives, CR 608.2d's choice is put
-- to the caster before
-- the "may" below, since CR 118.8c's excuse is a property of the spell being
-- cast. At CastObligation.Mandatory the cast is not a decision, so
-- Prompt.OfferedCast is elided; question 4 is what a printed "if able" comes to
-- (CR 601.3, CR 609.3). CR 118.8c is the exception: `excused` turns the
-- mandatory branch back into a may, classified by Cost.statesHiddenQuality.
--
-- The caster is a parameter and not the resolving controller: CR 608.2g says "a
-- player". Everything above is a CLASSIFICATION carried by the opcode's
-- CastOffer and its CastObligation; nothing here asks which card is offered.
--
-- Under PermissionVerb.Play a named land is offered too, as CR 305.2a's land
-- play during a resolution (CR 601.1a's "play"), under the special action's
-- per-player limits (Cast.landDropOpen: CR 305.2b, 305.3) and CR 101.2's
-- prohibitions, and not CR 305.1's phase or empty stack, which an instruction lifts.
-- Parun.GameSpec's "CR 305.2a gameplay: Word of Command makes bob play a land
-- on his turn" proves it.
--
-- `retake` is CR 724.2's second span: the player who controls the caster while
-- the spell this cast makes resolves (Word of Command). Keyed to the spell's
-- own id, which is the one object CR 608.2g puts on top of the stack.
offerCastOnce :: Game Result -> Filter.Context -> (ObjectId -> Filter.View -> Bool) -> [ObjectId] -> PlayerId -> CastObligation.CastObligation -> PermissionVerb.PermissionVerb -> Maybe PlayerId -> CastOffer.CastOffer -> Game (Maybe (ObjectId, Maybe ObjectId))
offerCastOnce runSubgame context fits named caster optionality verb retake offer = do
  gs <- State.get
  let -- Whether this offer states CR 118.9's alternative cost, in either of the
      -- two wordings `applied` below reads. NOT `transformed`, which is CR
      -- 712.8a's rider about which face goes on the stack and says nothing about
      -- payment.
      alternative = CastOffer.withoutPayingManaCost offer || Maybe.isJust (CastOffer.payingInstead offer)
      -- CR 712.8a for the transformed rider; CR 709.3, CR 712.8b, CR 716.3 and
      -- CR 721.3 otherwise, via Game.castableFacesOf. Nothing for a card with no back face,
      -- CR 712.8a's cast having no face to put on the stack: an offer that cannot
      -- be made is not made.
      --
      -- Less Card.convertedFace under an alternative, which is CR 118.9a: a spell
      -- takes one alternative cost, and that face is on the list only because CR
      -- 702.162a's or CR 702.146a's alternative cost put it there, so an offer
      -- spending the spell's one alternative on something else cannot also reach
      -- it. CR 712.8's default -- the front face -- is then the whole answer.
      --
      -- Unobserved for rule 702.146a's half: no board in the pool offers a cast
      -- of a disturb card under an alternative cost of the offer's own, so only
      -- rule 702.162a's limb is driven (Parun.InvestigateSpec's "CR 118.9a a free
      -- offer does not also offer the converted face").
      --
      -- Game.castableFacesOf carries the HALVES and never Card.fusedFace, so rule
      -- 702.102a's third offer is added here, beside them and never instead of
      -- them: "if a player casts a split card with fuse FROM THEIR HAND, the
      -- player may choose to cast both halves of that split card rather than
      -- choose one half". CR 608.2g's offer is a permission about a zone, so it
      -- reaches that cast exactly as the ordinary action does; which of the three
      -- is taken stays the caster's choice below.
      --
      -- THE ZONE is the rule's own -- fuse "applies while the card with fuse is in
      -- a player's hand" -- and so is the SEAT: "from their hand", which CR 400.3
      -- makes the owner's, the seat Game.zoneMembers indexes a hand by. So an
      -- offer letting a player cast out of somebody ELSE'S hand (Sen Triplets,
      -- see #2169) offers that card's halves alone -- the rule's own narrowing
      -- and not an elision. Parun.Engine.Cast.castableSpells'
      -- fusedProposals is this same offer on the ordinary-action road, and the two
      -- read the same zone for the same reason.
      --
      -- FACE UP alone, that road's other narrowing: `faces` below offers a
      -- face-down cast its one proposal before this is asked.
      fusedFaces oid
        | elem oid (Game.zoneMembers Zone.Hand caster gs) = Maybe.maybeToList (Cast.fusedFaceOf oid gs)
        | otherwise = []
      -- CR 708.4: CastOffer.faceDown's cast is turned face down before it is put
      -- onto the stack, as CR 708.2a's 2/2 under the offer's own reason and with
      -- the events the offer turns it face up before; every other offer is cast
      -- face up.
      facing = case CastOffer.faceDown offer of
        Nothing -> Facing.FaceUp
        Just events -> Facing.FaceDown FaceDownState.MkFaceDownState {FaceDownState.reason = FaceDownReason.CastFaceDown, FaceDownState.listed = FaceDownCharacteristics.defaultValue, FaceDownState.turnsUpBefore = events}
      faces oid card
        -- A face-down spell has no halves to choose among (CR 708.2 leaves it
        -- the listed characteristics alone), so the card is proposed once,
        -- under its front face's name -- the face it shows when turned up.
        | Facing.isFaceDown facing = Just [Card.frontFace card]
        | CastOffer.transformed offer = fmap pure (Card.backFace card)
        | alternative = Just (filter (\face -> fmap Face.name (Card.convertedFace card) /= Just (Face.name face)) (Game.castableFacesOfId oid gs) <> fusedFaces oid)
        | otherwise = Just (Cast.castableFacesFor oid gs <> fusedFaces oid)
      -- One proposal per half, gated on its own (CR 709.3a, CR 712.8c), which
      -- is why the whole tuple is built per face rather than once per card.
      --
      -- The excuse below being the HALF's and not the card's is what CR 709.3a
      -- makes it, and Parun.InvestigateSpec's "CR 118.8c the excuse is the half's,
      -- so one half is offered and the other forced" is the board that proves it:
      -- one mandatory offer of a split card whose front half states a hidden-zone
      -- additional cost and whose back half states none.
      proposal oid face =
        let name = Face.name face
            -- CR 118.9a: at most ONE alternative cost, so the applied one
            -- replaces the candidates rather than joining them; the two riders
            -- are asked in order, free first. CR 118.9d in both cases -- an
            -- alternative replaces only the MANA cost, so the face's additional
            -- costs ride along.
            --
            -- CR 601.2b's tag rides along: an offer stating which keyword
            -- ability is behind its cost (rule 702.62a's third ability, rule
            -- 702.35a's second and rule 702.88a's delayed one) hands that keyword to
            -- the candidate, so the cast records it as Object.castUsing exactly as a
            -- printed alternative does.
            -- CastOffer.offeredBy is Nothing for every other offer, which is
            -- Cost.untagged's own value.
            --
            -- One offer can still be SEVERAL candidates, and CR 118.8's choice
            -- costs are why: the face's additional costs ride the applied cost,
            -- and one of them may offer the caster a choice of payments, which
            -- Cost.choiceVariants expands here exactly as it does for a cast the
            -- board itself offers. CR 702.48a's offering and an effect's optional
            -- addition are the others, which Cost.withOffering and
            -- Cost.withOptionalAdditions add the same way.
            appliedOne
              | CastOffer.withoutPayingManaCost offer = Just (CandidateCost.plain (CastOffer.offeredBy offer) (Cost.withoutPayingManaCost priced))
              | otherwise = fmap (\c -> CandidateCost.plain (CastOffer.offeredBy offer) (c {Cost.Type.components = Cost.Type.components c <> Face.additionalCosts priced})) (CastOffer.payingInstead offer)
            applied = concatMap (Cost.withOptionalAdditions caster oid proposed) . concatMap (Cost.withOffering caster oid proposed) $ concatMap (Cost.choiceVariants priced) (Maybe.maybeToList appliedOne)
            -- CR 708.4: a cast proposed face down is measured against the
            -- face-down characteristics, so it is the stamped state's face that
            -- prices it and carries the additional costs -- none, CR 708.2a's 2/2
            -- printing no text.
            proposed = Cast.asProposed oid name facing gs
            --
            -- CR 113.6d: less the cost abilities a layer-6 wipe took from the
            -- spell (Projection.wipedFace).
            priced = Projection.wipedFace (Cost.asSpellProjected caster oid proposed) (if Facing.isFaceDown facing then Maybe.fromMaybe face (Cast.proposedFace oid name proposed) else face)
            -- CR 601.2f: the offer's own reduction (CastOffer.reduction) rides
            -- every candidate, the printed cost and an alternative alike, so it
            -- applies after any increase. The cast is then handed these
            -- candidates rather than finding its own, which are the same list.
            lessened candidate = candidate {CandidateCost.reductions = CandidateCost.reductions candidate <> [AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost [ManaSymbol.Generic (CastOffer.reduction offer)]) 0 Nothing]}
            candidates
              | CastOffer.reduction offer == 0 = if null applied then Cost.candidateCostsGiven True caster name oid proposed else applied
              | otherwise = fmap lessened (if null applied then Cost.candidateCostsGiven True caster name oid proposed else applied)
            announced = if CastOffer.reduction offer == 0 then applied else candidates
            -- CR 712.1 asks of the card, which the printed face's view cannot.
            faceView = (Projection.viewOfCard face) {Filter.doubleFaced = Game.isDoubleFacedCard oid gs}
         in -- CR 702.85a's second condition, asked of THIS HALF: the offer may
            -- state a quality the resulting spell must have, and CR 709.3a makes
            -- that a per-half question like the two above. Against the face's own
            -- view and the resolution's context, so a source-comparing atom reads
            -- the same source the reference did. An offer stating none narrows
            -- nothing. `fits` is the repetition's total asking the same half
            -- (CastRepetition.WithinTotal, or UpTo with a total).
            --
            -- Also CR 406.3b's quality half for a card exiled face down: the
            -- face's view is the spell it becomes, where the reference's walk
            -- saw no characteristics (CR 406.3a). The scenario
            -- the-dragon-kami-reborn-hatches-a-face-down-creature-card-and-not-an-instant-when-its-egg-dies
            -- proves it. Its look half asks Exile.mayLookAt of the caster, so
            -- only an offer stating a quality refuses a card it may not look at;
            -- the scenario
            -- cr-406-3b-an-offer-stating-a-quality-refuses-a-face-down-card-its-caster-may-not-look-at
            -- proves it.
            if maybe True (\restriction -> Exile.mayLookAt caster oid gs && Filter.matches context faceView restriction) (CastOffer.restriction offer)
              && fits oid faceView
              && Cast.castableWhenOffered (CastOffer.spending offer) caster oid name candidates proposed
              then
                -- CR 118.8c, read off the same candidates the cast will be
                -- announced with: CR 118.9d keeps the face's additional costs on
                -- an alternative, so every candidate already carries them. A
                -- cost APPLIED from another effect (CR 118.8) is appended by
                -- Cost.plusComponents, the funnel Cast.payableCostAt measures
                -- through. Parun.InvestigateSpec's "CR 118.8c an additional cost
                -- another effect applies excuses the cast too" proves it.
                let excused = any ((\cost -> Cost.statesHiddenQuality (Cost.plusComponents (Cost.spellAdjustments (Cost.announcedRecords cost) Map.empty caster oid proposed) cost)) . CandidateCost.cost) candidates
                 in Just (oid, name, announced, excused)
              else Nothing
      -- EVERY object the reference names, each contributing one entry per
      -- castable half (CR 709.3a) -- Shell of the Last Kappa's whole exiled pile
      -- as readily as Tinybones, the Pickpocket's one target. The caller's
      -- objectRefObjects is what makes the two the same read.
      offers =
        concatMap
          ( \oid -> Maybe.fromMaybe [] $ do
              card <- Game.cardOf oid gs
              fmap (Maybe.mapMaybe (proposal oid)) (faces oid card)
          )
          named
      -- One entry per land face (CR 712.9), beside the spells above, which
      -- hold no land face (CR 305.9, Cast.printedRestrictionsOk), so no pair is
      -- offered both ways.
      landOffers
        | verb == PermissionVerb.Play && Cast.landDropOpen caster gs =
            [ (oid, mName, Face.name face)
            | oid <- named,
              -- CR 406.3a: judged on the card turned up, as Cast.asProposed does
              -- for a cast, so a land exiled face down shows its name.
              not (PlayerEffect.prohibitsPlayingLand caster oid (Cast.turnedUpForPlay oid Facing.FaceUp gs)),
              Just obj <- [Game.lookupLayerOne oid gs],
              Just card <- [Game.cardOf oid gs],
              (mName, face) <- Game.landFacesOf obj card
            ]
        | otherwise = []
      everything = fmap Left landOffers <> fmap Right offers
      keyOf entry = case entry of
        Left (oid, _, name) -> (oid, name)
        Right (oid, name, _, _) -> (oid, name)
  -- No survivor is no offer; one survivor is one outcome, so CR 608.2d's choice
  -- is elided there rather than asked.
  chosen <- case everything of
    [] -> pure Nothing
    [sole] -> pure (Just sole)
    first : rest -> do
      let decider = Decide.deciderFor caster gs
      picked <- Game.choose (Prompt.ChooseOfferedCastSpell decider caster (fmap keyOf (first NonEmpty.:| rest)))
      -- Reject-not-repair: a pair the offer did not include is no cast at all.
      -- The PAIR and not the name alone, for castWhileSearching's reason: one
      -- card's half must not answer another card's.
      pure (List.find ((== picked) . keyOf) everything)
  let -- The SAME prompt on both paths: CR 118.8c creates no new decision. A land
      -- play's "may" is this prompt too.
      mayDo oid name act = do
        let decider = Decide.deciderFor caster gs
        decision <- Game.choose (Prompt.OfferedCast decider caster oid name)
        case decision of
          OptionalDecision.Declines -> pure Nothing
          OptionalDecision.Exercises -> act
  case chosen of
    Nothing -> pure Nothing
    Just (Left (oid, mName, name)) -> do
      let play = do
            Cast.playLand (settleWith runSubgame) True caster oid mName
            pure (Just (oid, Nothing))
      case optionality of
        CastObligation.Mandatory -> play
        CastObligation.Optional -> mayDo oid name play
    Just (Right (oid, name, applied, excused)) -> do
      let cast = do
            stackBefore <- State.gets (TurnRegisters.stack . GameState.turn)
            Cast.castSpellWith (performManaAbility runSubgame) False True applied (CastOffer.spending offer) caster oid name facing
            -- CR 608.2g: the spell is the new topmost object. A cast that was
            -- reversed put nothing new on top.
            stackAfter <- State.gets (TurnRegisters.stack . GameState.turn)
            let spell = case stackAfter of
                  sid : _ | notElem sid stackBefore -> Just sid
                  _ -> Nothing
            -- CR 724.2's second span, dormant until the spell resolves
            -- (Parun.Engine.Stack.resolveTopWith). A reversed cast waits on
            -- nothing.
            case (retake, spell) of
              (Just controller, Just sid) -> do
                stamp <- State.state Game.freshTimestamp
                State.modify' $
                  Common.pushControl
                    caster
                    PlayerControl.MkPlayerControl
                      { PlayerControl.decider = Decider.MkDecider controller,
                        PlayerControl.duration = ControlDuration.WhileResolving sid,
                        -- CR 724.7: Word of Command's mana clause is
                        -- "while doing so", the playing of the card.
                        PlayerControl.manaFromLandsOnly = False,
                        PlayerControl.timestamp = stamp
                      }
              _ -> pure ()
            pure (Just (oid, spell))
      case optionality of
        CastObligation.Mandatory | not excused -> cast
        CastObligation.Mandatory -> mayDo oid name cast
        CastObligation.Optional -> mayDo oid name cast

-- CR 609.1: `body` as the effect of the spell or ability resolving as
-- `resolving`. The cause is read BEFORE the body runs, while the resolving
-- ability is surely still an object to read.
caused :: ObjectId -> ObjectId -> PlayerId -> Game a -> Game a
caused resolving source controller body = do
  cause <- State.gets (Common.causeOf resolving source controller)
  Event.causedBy (Just cause) body

-- One effect, applied, wrapped in the window CR 607.2a's link is filed from:
-- what was in exile before, and what is in it after.
--
-- CR 609.1: and performed as the effect of the spell or ability resolving
-- (Event.causedBy, Common.causeOf), the one door every instruction of a spell, an
-- ability, a mana ability and a replacement's own effects goes through, so the
-- moves it makes record their cause. Sacred Ground's scenarios prove it.
applyEffectWith :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
applyEffectWith runSubgame resolving source controller legal chosen effect = caused resolving source controller $ do
  before <- State.gets GameState.exile
  logged <- State.gets (Seq.length . (TurnRegisters.events . GameState.turn))
  -- Read BEFORE the effect runs, while the resolving ability is surely still an
  -- object to read.
  ability <- State.gets (resolvingAbilityName resolving)
  acquisition <- State.gets (\g -> Game.readingAcquisition resolving source g source)
  applyOneEffect runSubgame resolving source controller legal chosen effect
  State.modify' (Common.recordExiledWith ExileLink.MkExileLink {ExileLink.source = source, ExileLink.ability = ability, ExileLink.acquisition = acquisition, ExileLink.chosen = False, ExileLink.controller = Just controller} before)
  State.modify' (Common.recordCounteredWith source acquisition logged)
  State.modify' (Common.recordExilePile before)
  -- CR 614.1c: the as-enters effects of what this instruction put onto the
  -- battlefield, before the next instruction reads the board. After the window
  -- above, so what they exile is filed by their own windows, not this one's.
  runEntryEffects runSubgame
  -- CR 614.1a: what a replacement put in a lost game's place (Lich's Mirror),
  -- before the next instruction reads the board. A fence rather than a proof:
  -- no test loses a player the game mid-resolution and reads the board after,
  -- so Engine.performSettle's drain answers every test the same without it.
  runReplacementEffects runSubgame
  -- CR 611.2b: a "for as long as" duration this instruction ended, ended
  -- before the next one reads the board, and before the sample below, so the
  -- control it handed back is observed; one this resolution stored that has
  -- yet to begin, begun or held off the board likewise.
  Monad.void (Expiry.sweepConditional (Just resolving))
  -- CR 603.2 / 603.10d: a control change this instruction made, observed before
  -- the next one can make another.
  Monad.void Event.sampleControl
  -- CR 101.4 / 611.3a: a Topsy Turvy this instruction put onto or took off the
  -- battlefield, observed before the next one reads the turn order.
  State.modify' PlayerEffect.sampleTurnOrder
  -- CR 401.6: a revealed top card this instruction covered, re-minted before
  -- the next one can uncover it (Event.settleRevealedTops).
  State.modify' Event.settleRevealedTops
  -- CR 603.4: the board this instruction's events left, before the next one
  -- changes it. data/scenarios/trigger's Slaughterhouse Bouncer scenario proves
  -- it.
  State.modify' Trigger.fileBoards

-- One effect, applied. `runSubgame` is the injected nested-game runner; only
-- the PlaySubgame arm consults it.
--
-- `controller` is the controller of the resolving spell or ability, never the
-- effect `source`, which for an ability may already have been sacrificed as a
-- cost. Every arm evaluating a Quantity views through
-- `effectViewOf source legal gs` (CR 608.2h).
--
-- `resolving` is the object ON THE STACK -- the spell, or the ABILITY object --
-- where every slot this fold defines is bound, where CR 603.7c's captured
-- environment is read back, and where CR 601.2b's announced X lives. Not
-- `source`: for an ability the two differ (CR 113.7a), and that permanent can be
-- gone before a later effect of the same list runs.
-- Exhaustive, not over Parun.Engine.Children: an interpreter, every arm a call into its own Parun.Engine.Resolve.Arms function.
applyOneEffect :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
applyOneEffect runSubgame resolving source controller legal chosen effect = case effect of
  Effect.DealDamage payload -> Damage.dealDamage runPreventionRiders settleWith runSubgame resolving source controller legal payload
  Effect.Fight payload -> Damage.fight runPreventionRiders settleWith runSubgame resolving source controller legal payload
  Effect.ModifyTarget payload -> Damage.modifyTarget resolving source controller legal chosen payload
  Effect.ChangeText payload -> Damage.changeText resolving source controller legal payload
  Effect.AddMana payload -> Damage.addMana resolving source controller legal payload
  Effect.ActivateManaAbilities payload -> Damage.activateManaAbilities performManaAbility runSubgame resolving source controller legal payload
  Effect.MoveMana payload -> Damage.moveMana (settleWith runSubgame) resolving source controller legal payload
  Effect.Search payload -> Damage.search (settleWith runSubgame) performManaAbility runSubgame resolving source controller legal payload
  Effect.ExileAllGraveyards -> Damage.exileAllGraveyards (settleWith runSubgame) source controller
  Effect.ExileHandThenDraw -> Damage.exileHandThenDraw (settleWith runSubgame) controller
  Effect.Note kind -> Damage.noteFor resolving source controller kind
  Effect.RestartGame payload -> Damage.restartGame performHandAction runSubgame resolving source controller legal payload
  Effect.PlaySubgame payload -> Damage.playSubgame runSubgame resolving payload
  Effect.ChoosePlayer payload -> Damage.choosePlayer resolving source controller payload
  Effect.ChoosePlayerAtRandom payload -> Damage.choosePlayerAtRandom resolving source controller payload
  Effect.ChoosePermanents payload -> Damage.choosePermanents resolving source controller legal payload
  Effect.ChooseGroup payload -> Damage.chooseGroup resolving source controller legal payload
  Effect.RollDie payload -> Damage.rollDieArm throwDice runSubgame resolving source controller legal payload
  Effect.Rethrow -> Damage.rethrow throwDice runSubgame resolving controller
  Effect.RerollStoredResults payload -> Damage.rerollStoredResults throwDice runSubgame resolving source controller legal payload
  Effect.FlipCoin payload -> Damage.flipCoinArm settleWith runSubgame resolving source controller legal payload
  Effect.Guess payload -> Zones.guess resolving source controller legal chosen payload
  Effect.ControlPlayerNext payload -> Damage.controlPlayerNext controller legal payload
  Effect.ControlPlayerThisResolution payload -> Damage.controlPlayerThisResolution controller legal payload
  Effect.Destroy payload -> Damage.destroy (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.Sacrifice payload -> Damage.sacrifice (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.TurnFaceDown payload -> Damage.turnFaceDown resolving source controller legal payload
  Effect.TurnFaceUp payload -> Damage.turnFaceUp resolving source controller legal payload
  Effect.RemoveFromCombat payload -> Damage.removeFromCombat resolving source controller legal payload
  Effect.BecomesBlocked payload -> Damage.becomesBlocked legal payload
  Effect.BlockAttacker payload -> Damage.blockAttacker resolving source controller legal payload
  Effect.JoinAttack payload -> Damage.joinAttack resolving source controller legal payload
  Effect.ReselectAttack payload -> Damage.reselectAttack resolving source controller legal payload
  Effect.SwitchBlockers payload -> Damage.switchBlockers legal payload
  Effect.ExchangeBlocks payload -> Damage.exchangeBlocks legal payload
  Effect.MoveToZone payload -> Zones.moveToZone (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.ExchangeZones payload -> Zones.exchangeZones (settleWith runSubgame) resolving source controller legal payload
  Effect.ExchangeCardZones payload -> Zones.exchangeCardZones (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.ShuffleIntoLibrary payload -> Zones.shuffleIntoLibrary (settleWith runSubgame) resolving source controller legal payload
  Effect.SetOwner payload -> Zones.setOwner resolving source controller legal payload
  Effect.ExchangeOwnership payload -> Zones.exchangeOwnership resolving source controller legal payload
  Effect.Ante payload -> Zones.ante (settleWith runSubgame) resolving source controller legal payload
  Effect.PutSticker payload -> Zones.putSticker resolving source controller legal chosen payload
  Effect.Shuffle payload -> Zones.shuffle resolving source controller legal payload
  Effect.OfferCast payload -> Zones.offerCastArm offerCast runSubgame resolving source controller legal chosen payload
  Effect.OfferNamedCopy payload -> Zones.offerNamedCopy offerOutsideCopy runSubgame resolving source controller legal payload
  Effect.OfferNotedCopy payload -> Zones.offerNotedCopy offerOutsideCopy runSubgame resolving source controller legal payload
  Effect.GrantPlayFromExile payload -> Zones.grantPlayFromExile resolving source controller legal payload
  Effect.GrantCopyCast payload -> Zones.grantCopyCast resolving source controller legal payload
  Effect.GrantLookAtExiled payload -> Zones.grantLookAtExiled resolving source controller legal payload
  Effect.MakePlotted payload -> Zones.makePlotted resolving source controller legal payload
  Effect.MakeForetold payload -> Zones.makeForetold resolving source controller legal payload
  Effect.MakeWarped payload -> Zones.makeWarped resolving source controller legal payload
  Effect.Heal payload -> Zones.healArm resolving source controller legal payload
  Effect.ForEach payload -> Zones.forEach (settleWith runSubgame) applyEffectWith loopOffers runSubgame resolving source controller legal chosen payload
  Effect.ForEachNumber payload -> Zones.forEachNumber applyLoopBody runSubgame resolving source controller legal chosen payload
  Effect.Repeat payload -> Zones.repeatArm applyLoopBody runSubgame resolving source controller legal chosen payload
  Effect.RepeatIf payload -> Zones.repeatIf applyLoopBody runSubgame resolving source controller legal chosen payload
  Effect.Discover payload -> Zones.discover applyLoopBody runSubgame resolving source controller legal chosen payload
  Effect.Draw payload -> Zones.draw (settleWith runSubgame) resolving source controller legal payload
  Effect.Mill payload -> Zones.mill (settleWith runSubgame) resolving source controller legal payload
  Effect.Reveal payload -> Zones.reveal resolving source controller legal chosen payload
  Effect.LookAt payload -> Zones.lookAt resolving source controller legal chosen payload
  Effect.ArrangeInLibrary payload -> Zones.arrangeInLibrary resolving source controller legal chosen payload
  Effect.Scry payload -> Zones.scry (settleWith runSubgame) resolving source controller legal payload
  Effect.Surveil payload -> Zones.surveil (settleWith runSubgame) resolving source controller legal payload
  Effect.Fateseal payload -> Zones.fateseal resolving source controller legal payload
  Effect.Clash payload -> Zones.clash resolving source controller payload
  Effect.AsYouCascade payload -> Zones.asYouCascade applyOneEffect runSubgame resolving source controller legal chosen payload
  Effect.Explore payload -> Zones.explore (settleWith runSubgame) resolving source controller legal payload
  Effect.Connive payload -> Zones.connive (settleWith runSubgame) resolving source controller legal payload
  Effect.Discard payload -> Zones.discard (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.LoseLife payload -> Zones.loseLife (settleWith runSubgame) resolving source controller legal payload
  Effect.GainLife payload -> Zones.gainLife (settleWith runSubgame) resolving source controller legal payload
  Effect.ExchangeLifeTotals payload -> Zones.exchangeLifeTotals (settleWith runSubgame) resolving source controller legal payload
  Effect.ExchangeValues payload -> Zones.exchangeValues (settleWith runSubgame) resolving source controller legal payload
  Effect.SetLifeTotal payload -> Zones.setLifeTotal (settleWith runSubgame) resolving source controller legal payload
  Effect.LoseGame payload -> Zones.loseGame resolving source controller legal payload
  Effect.WinGame payload -> Zones.winGame resolving source controller legal payload
  Effect.DrawGame -> Zones.drawGame source controller
  Effect.RedistributeLifeTotals -> Zones.redistributeLifeTotals (settleWith runSubgame) source controller
  Effect.IncreaseSpeed payload -> Zones.increaseSpeed resolving source controller legal payload
  Effect.DecreaseSpeed payload -> Zones.decreaseSpeed resolving source controller legal payload
  Effect.PlayerSacrifices payload -> Zones.playerSacrifices (settleWith runSubgame) resolving source controller legal payload
  Effect.Vote payload -> Zones.vote resolving source controller legal payload
  Effect.Bid payload -> Zones.bid resolving source controller legal payload
  Effect.Create payload -> Objects.create (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.Conjure payload -> Objects.conjure (settleWith runSubgame) resolving source controller legal payload
  Effect.CreateCopy payload -> Objects.createCopy (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.BecomeCopy payload -> Objects.becomeCopy resolving source controller legal chosen payload
  Effect.CopyStackObject payload -> Objects.copyStackObject resolving source controller legal payload
  Effect.SetX payload -> Objects.setX resolving source controller legal payload
  Effect.ChooseNewTargets payload -> Objects.chooseNewTargets resolving source controller legal payload
  Effect.ChangeTargets payload -> Objects.changeTargets resolving source controller legal payload
  Effect.ArmDelayedTrigger payload -> Objects.armDelayedTrigger resolving source controller legal payload
  Effect.Replace payload -> Objects.replace resolving source controller legal payload
  Effect.PreventNextDamage payload -> Objects.preventNextDamage resolving source controller legal chosen payload
  Effect.LaterPayment payload -> Objects.laterPayment resolving source controller legal payload
  Effect.PreventAllDamage payload -> Objects.preventAllDamage resolving source controller legal chosen payload
  Effect.NextDamageInstance payload -> Objects.nextDamageInstance resolving source controller legal chosen payload
  Effect.RedirectDamage payload -> Objects.redirectDamage resolving source controller legal payload
  Effect.SkipPhase payload -> Objects.skipPhase resolving source controller legal payload
  Effect.AffectPlayers payload -> Objects.affectPlayers resolving source controller legal payload
  Effect.RequireBlock payload -> Objects.requireBlock resolving source controller legal payload
  Effect.Prohibit payload -> Objects.prohibit resolving source controller legal payload
  Effect.ForbidAttack payload -> Objects.forbidAttack resolving source controller legal payload
  Effect.ForbidBeingBlocked payload -> Objects.forbidBeingBlocked resolving source controller legal payload
  Effect.RequireAttack payload -> Objects.requireAttack resolving source controller legal payload
  Effect.CreateEmblem payload -> Objects.createEmblem resolving source controller legal payload
  Effect.BecomeMonarch payload -> Objects.becomeMonarch resolving source controller legal payload
  Effect.TakeTheInitiative payload -> Objects.takeTheInitiative resolving source controller legal payload
  Effect.Designate payload -> Objects.designate resolving source controller legal payload
  Effect.SetClassLevel payload -> Objects.setClassLevel legal payload
  Effect.Undesignate payload -> Objects.undesignate resolving source controller legal payload
  Effect.SetHalfLocked payload -> Objects.setHalfLocked resolving source controller legal payload
  Effect.BecomeProtector payload -> Objects.becomeProtector controller legal payload
  Effect.CounterAndMark payload -> Objects.counterAndMark controller legal payload
  Effect.Mentor payload -> Objects.mentor source controller legal payload
  Effect.Exploit -> Objects.exploit (settleWith runSubgame) resolving source controller legal
  Effect.GiveGift -> Objects.giveGift controller
  Effect.ItBecomes payload -> Objects.itBecomes payload
  Effect.Attach payload -> Objects.attach resolving source legal payload
  Effect.AttachAsThoughCreature payload -> Objects.attachAsThoughCreature source legal payload
  Effect.AttachTarget payload -> Objects.attachTarget resolving source controller legal payload
  Effect.AttachTargetToEach payload -> Objects.attachTargetToEach resolving source controller legal payload
  Effect.AttachBound payload -> Objects.attachBound resolving source controller legal payload
  Effect.AttachAll payload -> Objects.attachAll resolving source controller legal chosen payload
  Effect.Unattach payload -> Objects.unattach resolving source controller legal chosen payload
  Effect.ExileHaunting payload -> Objects.exileHaunting (settleWith runSubgame) resolving legal payload
  Effect.Counter payload -> Counters.counter (settleWith runSubgame) resolving source controller legal payload
  Effect.PutCounters payload -> Counters.putCounters resolving source controller legal chosen payload
  Effect.Divide payload -> Counters.divide resolving source controller legal payload
  Effect.PutCountersFrom payload -> Counters.putCountersFrom resolving source controller legal payload
  Effect.PutCounterOfEachKind payload -> Counters.putCounterOfEachKind resolving source controller legal chosen payload
  Effect.ChooseCardName payload -> Counters.chooseCardName resolving source controller legal payload
  Effect.ChooseQuality payload -> Counters.chooseQuality resolving source controller payload
  Effect.FromOutsideTheGame payload -> Counters.fromOutsideTheGame source controller payload
  Effect.ExileThisSpell -> Counters.exileThisSpell (settleWith runSubgame) resolving
  Effect.RemoveCounters payload -> Counters.removeCounters resolving source controller legal payload
  Effect.RemoveCountersAmong payload -> Counters.removeCountersAmong resolving source controller legal payload
  Effect.MoveCounters payload -> Counters.moveCounters resolving source controller legal payload
  Effect.Proliferate payload -> Counters.proliferate source controller legal payload
  Effect.Bolster payload -> Counters.bolster resolving source controller legal payload
  Effect.Earthbend payload -> Counters.earthbendArm applyEffectWith runSubgame resolving source controller legal chosen payload
  Effect.Airbend payload -> Counters.airbend applyEffectWith runSubgame resolving source controller legal chosen payload
  Effect.Assimilate payload -> Counters.assimilate applyEffectWith runSubgame resolving source controller legal chosen payload
  Effect.Firebend payload -> Counters.firebend applyEffectWith runSubgame resolving source controller legal chosen payload
  Effect.Amass payload -> Counters.amass (settleWith runSubgame) resolving source controller legal payload
  Effect.Blight payload -> Counters.blight resolving source controller legal payload
  Effect.TemptWithTheRing -> Counters.temptWithTheRing controller
  Effect.OpenAttraction -> Counters.openAttraction (settleWith runSubgame) controller
  Effect.RollToVisit -> rollToVisit runSubgame controller
  Effect.Assemble payload -> Counters.assemble (settleWith runSubgame) resolving source controller legal payload
  Effect.Reassemble payload -> Counters.reassemble applyEffectWith runSubgame resolving source controller legal chosen payload
  Effect.Crank -> Counters.crank controller
  Effect.Planeswalk ref -> Counters.planeswalk resolving source controller legal ref
  Effect.ChaosEnsues ref -> Counters.chaosEnsues resolving source controller legal ref
  Effect.Abandon -> Counters.abandon source
  Effect.SetInMotion ref -> Counters.setInMotion resolving source controller legal ref
  Effect.ClaimPrize -> Counters.claimPrize controller
  Effect.Forage -> Counters.forage (settleWith runSubgame) resolving controller
  Effect.Populate -> Counters.populate (settleWith runSubgame) resolving controller
  Effect.TimeTravel -> Counters.timeTravel resolving controller
  Effect.Recruit -> Counters.recruit (settleWith runSubgame) controller
  Effect.Learn -> Counters.learn (settleWith runSubgame) resolving source controller
  Effect.Cloak payload -> Counters.cloak (settleWith runSubgame) resolving source controller legal chosen payload
  Effect.ManifestDread payload -> Counters.manifestDread (settleWith runSubgame) resolving source controller legal payload
  Effect.Venture payload -> Counters.venture controller payload
  Effect.GainPlayerCounters payload -> Counters.gainPlayerCounters resolving source controller legal payload
  Effect.RemovePlayerCounters payload -> Counters.removePlayerCounters resolving source controller legal payload
  Effect.PayAnyEnergy payload -> Counters.payAnyEnergy resolving source controller payload
  Effect.ChooseNumber payload -> Counters.chooseNumber resolving source controller legal payload
  Effect.BindAmount payload -> Counters.bindAmount resolving source controller legal payload
  Effect.BindPlayers payload -> Counters.bindPlayers resolving source controller legal payload
  Effect.BindLeaders payload -> Counters.bindLeaders resolving source controller legal payload
  Effect.Tap payload -> Counters.tap resolving source controller legal payload
  Effect.Untap payload -> Counters.untap resolving source controller legal chosen payload
  Effect.Detain payload -> Counters.detain resolving source controller legal payload
  Effect.Goad payload -> Counters.goad resolving source controller legal payload
  Effect.Pair payload -> Counters.pair resolving source controller legal chosen payload
  Effect.DoesNotUntapNext payload -> Counters.doesNotUntapNext resolving source controller legal payload
  Effect.Transform payload -> Counters.transform resolving source controller legal chosen payload
  Effect.Convert payload -> Counters.convert resolving source controller legal chosen payload
  Effect.Flip payload -> Counters.flipArm resolving source controller legal payload
  Effect.Meld payload -> Counters.meld (settleWith runSubgame) resolving source controller legal payload
  Effect.PhaseOut payload -> Counters.phaseOut resolving source controller legal payload
  Effect.AddPhases payload -> Counters.addPhases controller payload
  Effect.AddStep payload -> Counters.addStep payload
  Effect.EndTurn -> Counters.endTurn runDamageRewriteEffect runReplacementEffects runSubgame
  Effect.EndCombatPhase -> Counters.endCombatPhase runDamageRewriteEffect runReplacementEffects runSubgame
  Effect.GainControl payload -> Counters.gainControl resolving source controller legal payload
  Effect.ExchangeControl payload -> Counters.exchangeControl resolving source controller legal payload
  Effect.TakeExtraTurn payload -> Counters.takeExtraTurnArm resolving source controller legal payload
  Effect.ReverseTurnOrder -> Counters.reverseTurnOrder

-- The no-subgame executor, for a test that drives one effect directly: a
-- PlaySubgame resolves as a draw here (see noSubgame).
applyEffect :: ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
applyEffect = applyEffectWith Common.noSubgame

-- CR 615.5: run the additional effect of every prevention that has just been
-- applied. Drains StoredEffects.pendingPreventionRiders, which Parun.Engine.Damage
-- filled -- that module is below this one and cannot run a card's effects. Every
-- caller runs it before the board can be observed and before the next
-- state-based action check, and that ordering IS the rule: a 1/1 dealt 6 through
-- Test of Faith ends as a 4/4 rather than dying to CR 704.5g first. Emptied
-- BEFORE the riders run, so a rider whose own damage is prevented appends to a
-- fresh queue instead of being re-run here.
runPreventionRiders :: Game Result -> Game ()
runPreventionRiders runSubgame = do
  queued <- State.gets (StoredEffects.pendingPreventionRiders . GameState.stored)
  State.modify' (\gs -> gs {GameState.stored = (GameState.stored gs) {StoredEffects.pendingPreventionRiders = Seq.empty}})
  Foldable.traverse_ (runPreventionRider runSubgame) queued

-- One queued prevention's additional effect; nothing runs unless the prevention
-- carries a rider.
--
-- Run ONCE PER SOURCE the application covered, ascending by id, each run with
-- that source bound under the reserved Binding.preventedDamageSource and that
-- source's own amount: Comeuppance deals each attacker the damage prevented from
-- it, and Deflecting Palm's "that source's controller" reads the one source
-- bound. CR 615.5's "amount of damage that was prevented" is the SUM over the
-- recipients this application covered from that source (two recipients of one
-- source: one lot of 3, not a 2 and a 1). Two sources run twice (docs/decisions.md,
-- "How many times a prevention's additional effect runs"). The amount is a Quantity.InSlot read of the reserved
-- Binding.eventAmount slot, published through Scratch.ambientAmounts rather
-- than bound onto an object, because the shielded recipient may be a PLAYER.
-- Restored rather than cleared, so this cannot clobber an outer amount.
--
-- `resolving` and `source` are both the rider's own source (CR 113.7). Every slot
-- the rider names is treated as a LEGAL target, CR 608.2b having been applied
-- when the installing spell resolved.
runPreventionRider :: Game Result -> Prevention.Prevention -> Game ()
runPreventionRider runSubgame prevention = Foldable.for_ (Prevention.rider prevention) $ \rider ->
  Foldable.for_ (Map.toAscList (Prevention.amounts prevention)) $ \(damager, share) -> do
    was <- State.gets (Scratch.ambientAmounts . GameState.scratch)
    State.modify' (\gs -> gs {GameState.scratch = (GameState.scratch gs) {Scratch.ambientAmounts = Map.insert Binding.eventAmount (sum share) was}})
    let targets = Map.insert Binding.preventedDamageSource (Set.singleton (Recipient.ToObject damager)) (PreventionRider.targets rider)
        src = PreventionRider.source rider
    Monad.void (runCarrying runSubgame src (PreventionRider.controller rider) Map.empty targets (PreventionRider.effects rider))
    State.modify' (\gs -> gs {GameState.scratch = (GameState.scratch gs) {Scratch.ambientAmounts = was}})

-- CR 116.2c: what a paid Parun.Engine.LaterPayment row runs, in the environment
-- the resolution baked, runPreventionRider's way: `source` is both resolving and
-- source (CR 113.7), and every slot is treated as a legal target, CR 608.2b having
-- been applied when the row was stored.
runLaterPayment :: Game Result -> PreventionRider.PreventionRider -> Game ()
runLaterPayment runSubgame payoff =
  Monad.void (runCarrying runSubgame (PreventionRider.source payoff) (PreventionRider.controller payoff) Map.empty (PreventionRider.targets payoff) (PreventionRider.effects payoff))

-- CR 707.10: a StoredEffects.copyCasts row's "if you cast a spell this way,
-- copy this ability if ...", run as the spell it allowed becomes cast (CR
-- 601.2i), and no delayed triggered ability, the sentence having no "when" or
-- "at" (CR 603.7). The ability is the one that stored the row, by its stack id, which
-- Game.cease archived as it left the stack, so it is copied off
-- Snapshots.stackArchive as CR 702.50a's epic copies its spell; the holder
-- copies it, and the condition is asked of the row's source then.
-- runLaterPayment's environment: `source` is resolving and source, and the
-- ability is the one slot. Discord, Lord of Disharmony's scenarios prove it.
recopyAbility :: Game Result -> ActiveCopyCast.ActiveCopyCast -> Game ()
recopyAbility runSubgame row = Monad.forM_ (ActiveCopyCast.recopy row) $ \condition -> do
  gs <- State.get
  let permission = ActiveCopyCast.permission row
      source = ExilePlayPermission.source permission
      holder = ExilePlayPermission.player permission
      copyIt =
        CopyStackObject.MkCopyStackObject
          { CopyStackObject.ref = ObjectRef.InSlot Binding.thisAbility,
            CopyStackObject.targets = CopyTargets.Copied,
            CopyStackObject.quantity = Quantity.Type.Literal 1,
            CopyStackObject.copier = PlayerRef.Relative PlayerRelation.You,
            CopyStackObject.exceptions = [],
            CopyStackObject.slot = Nothing
          }
  Monad.when (Condition.holds (Projection.fullView gs) (Projection.sourceContext gs (Just holder) source) gs source condition) $
    Monad.void (runCarrying runSubgame source holder Map.empty (Map.singleton Binding.thisAbility (Set.singleton (Recipient.ToObject (ActiveCopyCast.ability row)))) [Effect.CopyStackObject copyIt])

-- CR 614.1c: run the effects of every as-enters rewrite that has applied and not
-- run yet. Drains StoredEffects.pendingEntryEffects, which Parun.Engine.Event filled
-- -- runPreventionRiders above in every structural respect and for the same
-- reason. Emptied before the effects run.
--
-- Three callers. Settle.entryEffects runs it after each member of a batch
-- entering the battlefield, so the next member's entry applies after it (CR
-- 616.1g's Voice of All example, Event.orderEntries). applyEffectWith runs it as each instruction
-- finishes, so a later instruction of the same resolution reads the board the
-- effects left (CR 608.2h) -- Parun.FaceDownSpec's "CR 614.1c / 608.2h the
-- reanimated Ixidron's power is read after its own sweep" proves it.
-- Parun.Engine.Engine.performSettle runs it for every other road onto the
-- battlefield (a permanent spell, a land play), before the SBA pass and the
-- trigger scan.
runEntryEffects :: Game Result -> Game ()
runEntryEffects runSubgame = do
  queued <- State.gets (StoredEffects.pendingEntryEffects . GameState.stored)
  State.modify' (\gs -> gs {GameState.stored = (GameState.stored gs) {StoredEffects.pendingEntryEffects = Seq.empty}})
  -- CR 603.4 / 614.12: the effects are part of the entry, so a board their own
  -- instructions file (Trigger.fileBoards) is dropped, the entry not yet over:
  -- its siblings are still held off the battlefield. The filing after the drain
  -- stands. data/scenarios/trigger's Battering Krasis scenario proves it.
  filed <- State.gets (Snapshots.boardAfterEvent . GameState.snapshots)
  Foldable.traverse_ (runEntryEffect runSubgame) queued
  State.modify' (\gs -> gs {GameState.snapshots = (GameState.snapshots gs) {Snapshots.boardAfterEvent = filed}})

-- One entered permanent's as-enters effects, in printed order.
--
-- `resolving` and `source` are both the permanent (CR 113.7), runPreventionRider's
-- posture. The slot maps hold Binding.triggerSource alone, the permanent
-- (Knight in _____ Armor's "put a name sticker on it"), because a static
-- ability targets nothing (CR 115.10a).
--
-- CR 107.3m: an X these effects read is the one announced for the spell that
-- became the permanent (Neverwinter Hydra's "roll X d6"), bound on the permanent
-- only while they run, since that clause makes the permanent's own X 0.
runEntryEffect :: Game Result -> PendingEntryEffect.PendingEntryEffect -> Game ()
runEntryEffect runSubgame pending = do
  let oid = PendingEntryEffect.object pending
      setX value gs = gs {GameState.objects = Map.adjust (\o -> o {Object.bindings = Map.alter (const value) Binding.variableX (Object.bindings o)}) oid (GameState.objects gs)}
  entering <- State.gets (Game.lookupObject oid)
  let announced = entering >>= Object.announcedX
      before = entering >>= Map.lookup Binding.variableX . Object.bindings
  Foldable.for_ announced $ \n -> State.modify' (setX (Just (Binding.toAmount n)))
  -- CR 614.12: what entered in the same event is held off the battlefield while
  -- the effects run, as it was when the rewrite applied (enteredBeside). CR
  -- 614.12a: the permanent itself is a subject with no origin to restore, so a
  -- choice among permanents can't name it (Slots.chosenCandidates).
  outer <- State.gets (Scratch.enteringBeside . GameState.scratch)
  outerSubjects <- State.gets (Scratch.enteringSubjects . GameState.scratch)
  beside <- State.gets (Common.enteredBeside oid)
  State.modify' (\gs -> gs {GameState.scratch = (GameState.scratch gs) {Scratch.enteringBeside = Set.union outer beside, Scratch.enteringSubjects = Map.insert oid Nothing outerSubjects}})
  Monad.void (runCarrying runSubgame oid (PendingEntryEffect.controller pending) (Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject oid))) Map.empty (PendingEntryEffect.effects pending))
  State.modify' (\gs -> gs {GameState.scratch = (GameState.scratch gs) {Scratch.enteringBeside = outer, Scratch.enteringSubjects = outerSubjects}})
  Foldable.for_ announced $ \_ -> State.modify' (setX before)
  -- CR 508.4: EntryRewrite.EntersAttacking, whose guards (CR 506.3) and
  -- choice are Combat's, the road Create's attacking rider takes. An effect's
  -- own rider (a mobilize token, a ninja) cleared the flag, met or failed (CR
  -- 508.4e); skipped for a creature a second row already put in combat, since
  -- it enters attacking once.
  attacking <- State.gets (Map.member oid . Combat.Type.attackers . TurnRegisters.combat . GameState.turn)
  Monad.when (PendingEntryEffect.attacking pending && not attacking) (Combat.putOntoBattlefieldAttacking AttackChoice.Any oid)
  State.modify' (Trigger.resampleEntry oid)

-- CR 614.1a: run the effects a replacement put in an event's place -- Kill-Suit
-- Cultist's destruction, Lich's Mirror's reset, Forbidden Crypt's loss. Drains
-- StoredEffects.pendingReplacementEffects, which Parun.Engine.Event filled --
-- runPreventionRiders above in every structural respect and for the same reason.
-- Emptied before the effects run, so an effect whose own damage is replaced
-- appends to a fresh queue instead of being re-run here.
--
-- For damage, every caller runs it BEFORE runPreventionRiders and before the
-- next state-based action check: the replaced event and the rest of its batch
-- were simultaneous (CR 608.2f, 614.6), so what happens instead of it lands
-- before CR 704.5g reads the board. A destruction's drains inside its own
-- event instead (Event.destroyIn, Sba.checkOnce). For a loss of the game or a
-- draw, it runs
-- after each draw of a sequence, an instruction's or a replacement's (Event.drawSeries, CR
-- 614.11a), after each resolved instruction and in every settle before
-- priority; a loss a
-- state-based action check meets runs inside that check instead (Sba.performStateBasedActions' `instead`, runDamageRewriteEffect).
-- The callers StoredEffects.pendingReplacementEffects lists keep it empty at every
-- priority window.
runReplacementEffects :: Game Result -> Game ()
runReplacementEffects runSubgame = do
  queued <- State.gets (StoredEffects.pendingReplacementEffects . GameState.stored)
  State.modify' (\gs -> gs {GameState.stored = (GameState.stored gs) {StoredEffects.pendingReplacementEffects = Seq.empty}})
  Foldable.traverse_ (runDamageRewriteEffect runSubgame) queued

-- The Parun.Types.Settle every road below Resolve takes from a resolution:
-- runReplacementEffects, runEntryEffects, throwRecorded and reflipWindow around
-- CR 730.1a's subgame runner.
settleWith :: Game Result -> Settle.Settle
settleWith runSubgame =
  Settle.MkSettle
    { Settle.queued = runReplacementEffects runSubgame,
      Settle.entryEffects = runEntryEffects runSubgame,
      Settle.throwRecorded = Just (throwRecorded runSubgame),
      Settle.reflipWindow = reflipWindow runSubgame
    }

-- One replaced damage event's effects, in printed order.
--
-- `resolving` and `source` are both the row's own source (CR 113.7),
-- runPreventionRider's posture. Every slot the row carries is treated as a LEGAL
-- target, CR 608.2b having been applied when the installing ability resolved and
-- never again (Harm's Way's 2009-10-01 ruling); Words of War's scenario
-- "still-damages-a-creature-that-gained-hexproof" proves it.
-- An effect (CR 609.1) even when drained inside CR 118.12's cost, so the
-- paying flag is cleared (Event.resolvingDiscardCause).
--
-- A ZoneChangeR rider's (PendingOrigin.ZoneChangeRider) also gets CR
-- 400.7's new object under Binding.became, found off the turn's log now that
-- the move has happened: what the departed permanent became in a public zone,
-- none where it went to a hidden one (CR 400.7e's posture). An "as this
-- transforms" row's (PendingOrigin.AsTransforms) is part of the transform, so
-- the transform's event is sampled again after it (Event.resampleTransformed).
runDamageRewriteEffect :: Game Result -> PendingReplacementEffect.PendingReplacementEffect -> Game ()
runDamageRewriteEffect runSubgame pending = do
  gs <- State.get
  let carried = PendingReplacementEffect.targets pending
      departed = [oid | Recipient.ToObject oid <- Foldable.toList (Map.findWithDefault Set.empty Binding.departedPermanent carried)]
      arrivals = Set.fromList (Recipient.ToObject <$> concatMap (`Game.publicArrivalsOf` gs) departed)
      targets
        | PendingReplacementEffect.origin pending == PendingOrigin.ZoneChangeRider && not (Set.null arrivals) = Map.insert Binding.became arrivals carried
        | otherwise = carried
      src = PendingReplacementEffect.source pending
      outer = Scratch.replacementAcquisitions (GameState.scratch gs)
      frozen acquisitions g = g {GameState.scratch = (GameState.scratch g) {Scratch.replacementAcquisitions = acquisitions}}
  -- CR 607.5: the effects read the source's links as the abilities it had as
  -- they began, so Olag, Ludevic's Hubris's counters, put after it becomes a
  -- copy, still count the cards exiled with it. data/scenarios/
  -- ludevic-necrogenius-transforms-into-olag-a-copy-of-a-chosen-exiled-creature-card.json
  -- is the proof.
  State.modify' (frozen (Map.insert src (Game.acquisitionOf src gs) outer))
  Monad.void (Event.payingOnResolution False (runCarrying runSubgame src (PendingReplacementEffect.controller pending) Map.empty targets (PendingReplacementEffect.effects pending)))
  State.modify' (frozen outer)
  Monad.when (PendingReplacementEffect.origin pending == PendingOrigin.AsTransforms) (State.modify' (Event.resampleTransformed src))

-- CR 103.5b / CR 103.6: perform the effects of an action a card grants from a
-- player's hand. Parun.Engine.Mulligan's window loops reach this through the
-- HandActionPerformer parameter (see Parun.Types.HandActionPerformer).
--
-- The action does not use the stack, so there is nothing to put on it and no
-- modes to bind.
performHandAction :: Game Result -> HandActionPerformer.HandActionPerformer
performHandAction runSubgame source player =
  Monad.mapM_
    ( applyEffectWith
        runSubgame
        source
        source
        player
        -- CR 115.1: the reserved self slot is NOT a target, so there is no CR
        -- 608.2b legality question -- the card is in the acting player's hand by
        -- construction. Binding it is how "this card" is expressible with no
        -- self-variant opcode.
        (Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject source)))
        (Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject source)))
    )

-- | CR 701.52a: roll a six-sided die to visit this player's Attractions. A die
-- roll like any other (CR 706), then the result the Visit triggers read.
rollToVisit :: Game Result -> PlayerId -> Game ()
rollToVisit runSubgame pid = do
  (results, throwers, rolled) <- throwDice runSubgame pid 6 1 0 (pure 0)
  Monad.unless (null rolled) (Common.recordRoll pid throwers rolled)
  -- CR 706.6 can throw a second die and ignore the lower; one is left.
  Foldable.for_ (Maybe.listToMaybe results) $ \result ->
    State.modify' (Event.recordEvent (GameEvent.RolledToVisit DieResult.MkDieResult {DieResult.roller = pid, DieResult.result = result, DieResult.reading = RollReading.FinalResult}))

-- | CR 706.1-706.6: throw `named` dice with `sides` sides for `controller`,
-- through every modification rule 706 names -- CR 614.1a's replacements, CR
-- 706.2b's rerolls and adjustments, CR 706.6's ignored rolls -- and answer each
-- kept die's final result beside the player who threw its final natural result
-- and the natural result of the die's first roll, with its thrower, none where
-- a coin gave it; each reroll's is recorded with the reroll (`throwSettled`).
-- `ignore` is the instruction's own CR 706.6 ignore (Parun.Types.RollDie's), and
-- `perDie` its own modifier (CR 706.2). Shared by
-- Effect.RollDie's arm and CR 701.52a's roll to visit, which records nothing
-- here: each caller records its own events.
--
-- Answers what the INSTRUCTION reads, the throwers, and what was actually
-- ROLLED, which is what a caller records. They part where Pokey, the Scallywagg
-- replaced the roll (CR 614.6): a coin flipped instead reads as 1 or 20 and
-- rolls nothing, and a roll standing for that flip (Pokey's other half, applied
-- in the same loop) is recorded as rolled and read back through the face it is
-- -- nothing where it was neither.
throwDice :: Game Result -> PlayerId -> Natural -> Natural -> Natural -> Game Integer -> Game ([Natural], [(PlayerId, [(PlayerId, Natural)])], [Natural])
throwDice runSubgame controller sides named ignore perDie = do
  settled <- Event.proposeDiceRoll (reflipWindow runSubgame) controller sides named ignore
  case settled of
    Left flipped -> do
      Foldable.for_ flipped (State.modify' . Event.recordEvent . snd)
      pure (fmap fst flipped, fmap (const (controller, [])) flipped, [])
    Right roll -> do
      -- A roll standing for Pokey's flip throws with no modifier of the
      -- instruction's own, throwRecorded's reason: the flip named none, and
      -- the instruction sees only the 1 or 20 the face is read as.
      (results, throwers) <- throwSettled runSubgame roll (if Throw.instead roll then pure 0 else perDie)
      let readBack = if Throw.instead roll then Maybe.mapMaybe (fmap Event.resultOfFace . Event.faceOfResult) results else results
      pure (readBack, throwers, results)

-- | CR 706.1: a roll settled below Resolve, thrown through `throwSettled` and
-- recorded -- the CR 706 road Parun.Engine.Event.flipOneCoin and
-- Parun.Engine.Planechase.roll cannot reach. Pokey, the Scallywagg's roll
-- instead of a coin flip, and the planar die (CR 901.9). No modifier of the
-- instruction's own: neither names one.
throwRecorded :: Game Result -> Throw.Throw -> Game [Natural]
throwRecorded runSubgame roll = do
  (results, throwers) <- throwSettled runSubgame roll (pure 0)
  Monad.unless (null results) $ case Throw.kind roll of
    -- CR 901.9d: a roll of dice, but no number came up for "whenever you roll
    -- a 6" to read.
    ThrowKind.PlanarDie -> State.modify' (Event.recordEvent (GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.RollDice (Throw.thrower roll))))
    _ -> Common.recordRoll (Throw.thrower roll) throwers results
  pure results

-- | `throwDice` once CR 614.1a's loop has settled the roll: how many dice to
-- throw, which to keep, and how many of the lowest CR 706.6 then ignores
-- (Parun.Types.Throw).
throwSettled :: Game Result -> Throw.Throw -> Game Integer -> Game ([Natural], [(PlayerId, [(PlayerId, Natural)])])
throwSettled runSubgame roll perDie = do
  let controller = Throw.thrower roll
      -- Unreachable for coins: both funnels settle a flip themselves
      -- (Event.settleCoins), so a thrower is handed dice alone.
      sides = case Throw.kind roll of
        ThrowKind.Dice n -> n
        ThrowKind.Coins -> 1
        -- CR 901.3a, Parun.Engine.Planechase.faceOf's numbering.
        ThrowKind.PlanarDie -> 6
      -- CR 901.9d: an effect reading a die's number ignores the planar die, so
      -- only a reroll that names no number reaches it (Wall of Fortune, Monitor
      -- Monitor), and an adjustment changes nothing (Night Shift of the Living
      -- Dead's ruling: the life is paid, the result stands).
      numbered = Throw.kind roll /= ThrowKind.PlanarDie
      -- CR 706.1a's outcomes "numbered from 1 to N", BOTH ends included: the
      -- answer filtered back to a face the die could show. Its own binding
      -- because a reroll runs it a second time.
      faceOf rolled = if rolled >= 1 && rolled <= sides then rolled else 1
      -- CR 706.2b's FIRST step, taken before the instruction's own modifier
      -- is added below: a source other than this instruction offers to throw
      -- the same die again and take the new number as the natural result
      -- (Clam-I-Am). CR 706.2b's "rerolling that die", so `sides` is unchanged and
      -- the second answer is filtered back against the same range.
      --
      -- Over every die thrown at once: the roller picks which die's offer is
      -- put next, from all of them (CR 706.2b's "chooses one to apply"), so a
      -- 3 can be rerolled before the 2 beside it is decided on. A die a reroll
      -- throws rejoins them, open to the offers again, since its gate is on the
      -- natural result: a rerolled 3 that comes up 3 again is a 3 the Clam
      -- sees, and rule 706.2 puts no limit on how many times a roll is
      -- modified. Terminating because every round costs some player a
      -- Prompt.Rethrow they may decline. The scenario
      -- cr-706-2b-the-roller-rerolls-a-later-die-first proves it.
      --
      -- The offers are re-read each round, rather than captured before the
      -- first die: a reroll can move the board a later offer is read off.
      --
      -- A reroll IS a roll, and records CR 706.1's event again under the
      -- player who throws it -- Pippa, Duchess of Dice's ruling, which has a
      -- reroll trigger "whenever you roll a die". The discarded number is not
      -- CR 706.6's ignored roll: it happened, and its event stands -- unless
      -- the die it belongs to is ignored later (`recordRerolls`), which is
      -- why it is recorded late, over the board it came up on.
      --
      -- Answers each die's natural result with the player who THREW it: the
      -- roller, or whoever activated a reroll in the window below
      -- (`windowCandidates`), the face of the die's first roll with its
      -- thrower, none where a coin gave it, and the die's rerolls not yet
      -- recorded, each with its own natural result (`rethrow`).
      -- Each roll is read at the face it came up: CR 706.2's natural result
      -- is "before any modifiers", and a reroll is a modifier (CR 706.2b)
      -- and a roll of its own (docs/decisions.md, "Whether a natural 20
      -- rerolled away was rolled").
      -- The static offers and those activations compete in the one step, so
      -- both are candidates for the roller's pick.
      --
      -- A die still open is Right: whether an offer gated on a number reaches
      -- it (False for a natural result a coin gave: no die came up that
      -- number, so "if you roll a 3" is not offered, where an offer reaching
      -- any die the player rolled is), its thrower and natural result, the
      -- candidates declined over it, and what builds its answer from what the
      -- offers leave. A die done with the offers is Left.
      opened byNumber thrower natural finish = Right (byNumber, thrower, natural, [], finish . prefixed byNumber thrower natural)
      prefixed byNumber thrower natural (by, number, faces, pending) = (by, number, [(thrower, natural) | byNumber] <> faces, pending)
      closed slot = case slot of
        Left die -> die
        Right (_, thrower, natural, _, finish) -> finish (thrower, natural, [], [])
      -- The standing offers over one open die, each tagged with the die's
      -- place and natural result. A declined candidate is not asked again
      -- over the same die, nor is another free one to the same payer
      -- (`distinct`'s reason).
      candidatesOver (index, slot) = case slot of
        Left _ -> pure []
        Right (byNumber, _, natural, declined, _) -> do
          gs <- State.get
          modifiers <- Dice.modifiersFor controller
          activations <- duringRoll natural (State.gets windowCandidates)
          let gated (_, offer) = byNumber || Maybe.isNothing (ModifiedRoll.natural offer)
              free = either (Maybe.isNothing . ModifiedRoll.cost) (const False)
              fresh candidate@(payer, _, kind) = notElem candidate declined && not (free kind && any (\(asked, _, earlier) -> asked == payer && free earlier) declined)
              static = distinct Set.empty (filter (\(payer, stated, offer) -> fresh (payer, stated, Left offer)) (fmap (offerOf gs) (filter gated (Dice.rerollOffers RerollReach.ThatDie sides (Just natural) modifiers))))
              offers = fmap (\(payer, stated, offer) -> (payer, stated, Left offer)) static <> filter fresh activations
          standing <- duringRoll natural (State.gets (\now -> filter (live now) offers))
          pure (fmap (\(payer, stated, kind) -> (payer, stated, (index, natural, kind))) standing)
      -- CR 109.5's "you" on the modifier, which is the player its "may" and
      -- its CR 706.2a cost belong to -- Clam-I-Am's own controller, who is
      -- also the roller, and Wall of Fortune's, who need not be. The ROLLER
      -- where the carrier names no object, which CR 611.2a's stored form would
      -- be and no printing of this family is.
      offerOf gs (stated, offer) = (payerOf gs stated, stated, offer)
      payerOf gs stated = Maybe.fromMaybe controller (stated >>= \oid -> Projection.controllerOf oid gs)
      -- Two FREE offers to the same player are the same question asked twice:
      -- the answers are indistinguishable, and either accepted throws the same
      -- die. So two Clam-I-Ams ask once. Where CR 706.2a's cost is stated the
      -- offers stand apart -- what is paid, and the permanent it is paid
      -- against, are things the payer can tell apart -- so each is asked.
      distinct seen candidates = case candidates of
        [] -> []
        candidate@(payer, _, offer) : rest
          | Maybe.isJust (ModifiedRoll.cost offer) -> candidate : distinct seen rest
          | Set.member payer seen -> distinct seen rest
          | otherwise -> candidate : distinct (Set.insert payer seen) rest
      -- CR 706.2b's pick: the roller chooses which standing offer, over which
      -- die, is put next (Dice.pickModifier), and its payer decides whether to
      -- take it -- the modifier's own "may" (CR 706.2a). A declined offer
      -- leaves the rest to be re-checked against the board as it then stands.
      offeringAll slots = do
        candidates <- fmap concat (traverse candidatesOver (zip [0 :: Natural ..] slots))
        picked <- Dice.pickModifier controller (\(index, natural, _) -> Just (index, natural)) candidates
        case (picked, picked >>= \((_, _, (index, _, _)), _) -> Maybe.listToMaybe (List.genericDrop index slots)) of
          (Just ((payer, stated, (index, natural, kind)), _), Just (Right (byNumber, thrower, _, declined, finish))) -> do
            outcome <- takeOffer (shownDice natural slots) index thrower natural payer stated kind
            let slot = case outcome of
                  Nothing -> Right (byNumber, thrower, natural, (payer, stated, kind) : declined, finish)
                  Just (Left die) -> Left (finish die)
                  Just (Right (byNumber', thrower', natural', declined', finish')) -> Right (byNumber', thrower', natural', declined', finish . finish')
            offeringAll (zipWith (\at held -> if at == index then slot else held) [0 :: Natural ..] slots)
          -- Nothing picked; a candidate naming no open die is unreachable.
          _ -> pure (fmap closed slots)
      -- What every die of the roll shows now, in roll order, for the seat
      -- deciding on one of them (Night Shift of the Living Dead's ruling).
      -- Defensive: no slot at all shows the die the offer reaches.
      shownDice natural slots = Maybe.fromMaybe (ThrowResult.DieCameUp natural NonEmpty.:| []) (NonEmpty.nonEmpty (fmap (ThrowResult.DieCameUp . either (\(_, number, _, _) -> number) (\(_, _, number, _, _) -> number)) slots))
      -- One offer put to its payer over one die, the `index`th of `dice`:
      -- Nothing where it is declined, or the die the reroll threw.
      takeOffer dice index thrower natural payer stated kind = do
        let mCost = either ModifiedRoll.cost (Just . ActivatedAbility.cost . snd) kind
            ask = do
              gs <- State.get
              answer <- Game.choose (Prompt.Rethrow (Decide.deciderFor payer gs) payer dice index mCost)
              pure $ case answer of
                OptionalDecision.Exercises -> Just ()
                OptionalDecision.Declines -> Nothing
        case kind of
          Left offer -> do
            taken <- askPaying Set.empty thrower payer stated mCost ask
            case taken of
              Nothing -> pure Nothing
              -- The roller throws it: Clam-I-Am's "you may reroll
              -- it", Wall of Fortune's "have any player reroll a die
              -- that player rolled".
              Just () -> do
                State.modify' (Dice.spendLimit stated offer)
                fmap Just (rethrowOne controller)
          Right (oid, ability) -> do
            answer <- ask
            case answer of
              Nothing -> pure Nothing
              Just () -> do
                requested <- duringRoll natural $ do
                  activated <- activateWhileRolling runSubgame payer oid ability
                  after <- State.get
                  pure (activated && Scratch.rethrowRequested (GameState.scratch after))
                if requested
                  then fmap Just (rethrowOne payer)
                  else pure Nothing
      -- CR 706.2b's reroll is a roll: Pippa, Duchess of Dice's ruling has it
      -- trigger "whenever you roll a die", so it is CR 706.1's event and goes
      -- through CR 614's loop, Event.proposeDiceRoll, as ONE roll of `count`
      -- dice of the same size: the dice one modifier rerolls at once (Monitor
      -- Monitor's ruling) are one roll, so Barbarian Class adds one die to
      -- the lot and ignores the single lowest (CR 614.1a, 706.6). Pokey, the
      -- Scallywagg may flip a coin for a d20, whose 1 or 20 is the die's new
      -- natural result. Each die it throws is offered its own rerolls, before
      -- CR 706.6 drops any, so the numbers that come back have had every offer
      -- already. The scenario
      -- cr-706-2b-barbarian-class-adds-one-die-to-a-reroll-of-several-dice
      -- proves it.
      --
      -- The die a coin stood in for is still one the player rolled, so it is
      -- offered again -- Wall of Fortune's ruling, "you can tap another
      -- untapped Wall ... to have them reroll again" -- but only by an offer
      -- that names no number: the coin "counts as flipping a coin, not rolling
      -- a die" (Pokey), so no die came up that 1 or 20.
      --
      -- Recorded as the rerolling player's roll: Pippa's ruling gives a reroll
      -- of another player's die to the rerolling player's "whenever you roll
      -- a die".
      --
      -- A die the CR 706.6 ignore would take may still be rerolled first:
      -- docs/decisions.md, "Whether a reroll reaches a die before the lowest roll is
      -- ignored".
      --
      -- Answers the `count` dice, in the order thrown, each carrying its
      -- rerolls' events, this one's last: `key` names this one where several
      -- dice share it (`rerollingDice`), and none where a coin stood in for
      -- it. Recorded once the roll's own ignore is done (`recordRerolls`).
      --
      -- A die is answered still open to its offers (`offeringAll`), for the
      -- caller to put beside the roll's other dice, unless the reroll itself
      -- keeps or ignores some of what it threw: those offers come first.
      --
      -- Not implemented: offering such a reroll's kept die again once the
      -- roller has turned to another die (#9426).
      rethrow by key count = do
        -- The planar die past CR 614.1a's loop: every printed row reads a
        -- number (Parun.Engine.Replacement's DieRollR arm).
        settled <-
          if numbered
            then Event.proposeDiceRoll (reflipWindow runSubgame) by sides count 0
            else pure (Right roll {Throw.thrower = by, Throw.units = Seq.replicate (Natural.toIntSaturating count) 1, Throw.ignored = 0})
        case settled of
          -- Each coin's event rides with the die it stood in for, ahead of
          -- that die's later rerolls: a die CR 706.6 ignores never happened,
          -- its rerolls included, so neither did the flip standing in for one
          -- (docs/decisions.md, "Whether a coin flipped for an ignored die's
          -- reroll was flipped").
          Left flipped -> do
            landed <- State.gets Trigger.battlefieldCandidates
            pure (fmap (\(result, event) -> opened False by result (\(thrower, natural, rolls, pending) -> (thrower, natural, rolls, foldMap (\e -> [(Nothing, Left e, landed)]) event <> pending))) (padded (1, Nothing) (fmap (fmap Just) flipped)))
          Right again
            -- One die per unit and none ignored: nothing the reroll decides
            -- waits on its dice's offers, so they join the roll's.
            | all (== 1) (Throw.units again) && Throw.ignored again == 0 && not (Throw.instead again) -> do
                faces <- traverse (const (fmap faceOf (Game.ask (Prompt.RollDie sides)))) (Foldable.toList (Throw.units again))
                landed <- State.gets Trigger.battlefieldCandidates
                pure (padded (Left (by, 1, [], [])) (fmap (\face -> opened numbered (Throw.thrower again) face (rerolled again (Just (face, landed)))) faces))
          Right again -> do
            kept <- naturals False again (pure 0)
            -- `count` kept: every die a row adds comes with an ignore. A d20
            -- rolled for a coin flipped for the reroll reads back as that
            -- coin's 1 or 20.
            --
            -- Each kept die carries the face it came up, before any reroll of
            -- it: a later reroll modifies the original roll, not this one
            -- (docs/decisions.md, "Which result a reroll's trigger reads").
            -- A d20 rolled for that coin keeps its own face, the one "a
            -- natural 20" reads (Pokey, the Scallywagg's rulings).
            --
            -- The reroll's natural result rides beside its event, over the
            -- board it came up on, rather than with the die's first roll.
            pure (padded (Left (by, 1, [], [])) (fmap (\((total, thrower, faces, pending), face) -> Left (rerolled again face (thrower, Integer.toNaturalSaturating total, faces, pending))) kept))
        where
          -- Defensive: a short answer is filled with a die of 1.
          padded filler xs = take (Natural.toIntSaturating count) (xs <> repeat filler)
          rerolled again face (thrower, natural, faces, pending) = (thrower, readBack again natural, [], pending <> foldMap (\(came, landed) -> (key, Right (by, came), landed) : fmap (\(who, shown) -> (Nothing, Left (Common.naturalResult who shown), landed)) faces) face)
      -- CR 706.1's event for each reroll a kept die carries, once the roll's
      -- own CR 706.6 ignore is done: a die it ignores "never happened", so
      -- neither did its rerolls, and "no abilities trigger because of the
      -- ignored roll" (docs/decisions.md, "Whether a reroll reaches a die
      -- before the lowest roll is ignored").
      --
      -- A reroll rerolls its dice at once, so "whenever you roll one or more
      -- dice" triggers once for it (CR 603.2): one event per `key`, its number
      -- the highest among the kept dice carrying it, as recordRoll's is. Under
      -- the rerolling player (Pippa, Duchess of Dice's ruling). The planar
      -- die's has no number (CR 901.9d).
      --
      -- Checked against the board the reroll came up on (CR 603.10), which
      -- every die it threw carries (`naturals`), not the board a later
      -- payment left: a Brazen Dwarf sacrificed to pay for a later reroll
      -- still triggers for this one. The scenario
      -- cr-603-10-a-reroll-triggers-a-brazen-dwarf-sacrificed-for-a-later-reroll
      -- proves it.
      --
      -- A coin flipped instead of a reroll (Left) is that coin's own event, and
      -- a reroll's natural result (Left) is its own per die, uncounted.
      recordRerolls pending = Foldable.for_ (groups Set.empty pending) $ \(event, landed) ->
        State.modify' (Event.recordEventOver landed event)
        where
          rolled by number =
            if numbered
              then GameEvent.CountedAct (CountedAct.MkCountedAct PlayerAction.RollDice by number)
              else GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.RollDice by)
          groups seen entries = case entries of
            [] -> []
            (_, Left event, landed) : rest -> (event, landed) : groups seen rest
            (Nothing, Right (by, face), landed) : rest -> (rolled by face, landed) : groups seen rest
            (Just key, Right (by, face), landed) : rest
              | Set.member key seen -> groups seen rest
              | otherwise -> (rolled by (Foldable.foldl' max face [other | (Just k, Right (_, other), _) <- rest, k == key]), landed) : groups (Set.insert key seen) rest
      rethrowOne by = fmap (Maybe.fromMaybe (Left (by, 1, [], [])) . Maybe.listToMaybe) (rethrow by Nothing (1 :: Natural))
      readBack again natural = if Throw.instead again then maybe natural Event.resultOfFace (Event.faceOfResult natural) else natural
      -- Whether an offer can still go through: a static offer's budget admits
      -- it and its cost is payable, an activation passes the gate a priority
      -- activation takes.
      live gs (payer, stated, kind) = case kind of
        Left offer -> Dice.withinLimit gs payer stated offer && payable gs payer stated (ModifiedRoll.cost offer)
        Right (oid, ability) -> Activatable.activatable payer oid ability gs
      -- CR 118.3, Prompt.ChooseToPay's posture for CR 118.12: a cost the
      -- payer has not the resources to pay fully is not offered. A stated
      -- cost with no object behind it cannot be paid at all -- every
      -- component is paid against the permanent that printed it -- and is
      -- skipped for that reason.
      payable gs payer stated mCost = case (stated, mCost) of
        (_, Nothing) -> True
        (Just oid, Just cost) -> Cost.canPay PaymentSubject.ForNeither payer oid cost gs
        (Nothing, Just _) -> False
      -- CR 706.2b's SECOND step, once every die is up and every reroll done:
      -- a source other than this instruction offers to move one result up or
      -- down (Night Shift of the Living Dead). After the rerolls, which is
      -- the rule's order, so a result an adjustment moved onto Clam-I-Am's 3
      -- is never offered a reroll. Every result is shown before the choice,
      -- the printed ruling's reading of "after you roll a die", and the answer
      -- names the die.
      --
      -- CR 706.2b's pick, `offeringAll`'s: the roller chooses which standing offer
      -- is put next. An offer TAKEN is spent, since a modifier applies to a
      -- roll once. One declined, or not standing, waits out the rest of this
      -- look and is offered again once another modifier applies, over the
      -- number that left (`waiting`). The budget and the cost are re-read
      -- before each question.
      adjusting results = do
        modifiers <- Dice.modifiersFor controller
        gs <- State.get
        adjustingThrough (fmap (\(stated, offer, amount) -> (payerOf gs stated, stated, (offer, amount))) (Dice.adjustOffers sides modifiers)) [] results
      adjustingThrough offers waiting results = case NonEmpty.nonEmpty results of
        Nothing -> pure results
        Just shown -> do
          gs <- State.get
          let standing (payer, stated, (offer, _)) = Dice.withinLimit gs payer stated offer && payable gs payer stated (ModifiedRoll.cost offer)
              (up, idle) = List.partition standing offers
          picked <- Dice.pickModifier controller (const Nothing) up
          case picked of
            Nothing -> pure results
            Just (chosen@(payer, stated, (offer, amount)), rest) -> do
              let mCost = ModifiedRoll.cost offer
                  passed = adjustingThrough rest (chosen : idle <> waiting) results
              taken <- askPaying Set.empty controller payer stated mCost (State.get >>= \now -> Game.choose (Prompt.AdjustDieRoll (Decide.deciderFor payer now) payer shown amount mCost))
              case taken of
                Nothing -> passed
                Just (index, direction) -> do
                  State.modify' (Dice.spendLimit stated offer)
                  -- FILTERED, NOT TRUSTED: an index past the end shifts
                  -- the first die.
                  let at = if index < List.genericLength results then index else 0
                      shift n
                        | not numbered = n
                        | otherwise = case direction of
                            RollAdjustment.Increase -> n + toInteger amount
                            RollAdjustment.Decrease -> n - toInteger amount
                  adjustingThrough (rest <> idle <> waiting) [] (zipWith (\i n -> if i == at then shift n else n) [0 :: Natural ..] results)
      -- CR 706.2a's cost, charged between the offer and the modifier's
      -- application: a declined payment leaves the number standing. `ask`
      -- puts the offer, Nothing declining it, and the answer taken is paid
      -- for; `failed` holds the answers already paid for in vain. CR 734.1 / 118.12a: a failed payment is reversed, which returns
      -- the payer to the choice, so the offer is asked again; the same answer
      -- paid the same way failing twice stands as declined (Common.payAgain);
      -- the scenario "CR 734.1 a Monitor Monitor payment that fails is asked
      -- again" proves it.
      --
      -- DuringResolution, payGatePaidBy's moment and for its reason -- the
      -- roll is a resolution being followed -- so the payment is CR 734.1's
      -- own reversible action and the CR 605.3a window it opens is the
      -- payer's to keep.
      --
      -- CR 706.2a's second sentence gives the ROLLER a chance to activate mana
      -- abilities before a modifier with a mana cost applies, beside the
      -- payer's own CR 605.3a window: `rollerWindow`, run ahead of the payment
      -- (docs/synthetic-readings.md, "When the roller's mana window opens for a
      -- modifier another player pays for"). Where the roller pays, the
      -- payment's window is that chance. The scenario "CR 706.2a the roller
      -- gets a mana window before another player's mana-cost reroll" proves it.
      --
      -- payGateCost's payment, read against no slots and with the bound slots
      -- dropped, which is not an elision: the cost is printed on the
      -- MODIFIER's object, not on the one resolving, so no text of the
      -- resolving object can name what it took, and the modifier's own change
      -- to the die reads no slot. Proved by Parun.DiceSpec's "CR 701.67a a
      -- modifier's waterbend cost is weighed" case.
      askPaying failed roller payer stated mCost ask = do
        answer <- ask
        case (answer, stated, mCost) of
          (Just taken, Just oid, Just cost) -> do
            rollerWindow runSubgame roller payer cost
            (paid, answered) <- payGateCost runSubgame Seq.empty Map.empty payer oid cost
            Common.payAgain failed (taken, answered) paid (fmap (const taken)) (\failed' -> askPaying failed' roller payer stated mCost ask)
          _ -> pure answer
      duringRoll natural = Common.withThrowing (ThrowResult.DieCameUp natural)
      windowCandidates = fmap (\(pid, oid, ability) -> (pid, Just oid, Right (oid, ability))) . windowAbilities
      -- CR 706.2b's first step over the roll as a whole, once each of its dice
      -- has had the offers that reach that die alone: a modifier reaching any
      -- number of the dice its payer rolled at the same time, for one payment
      -- (Monitor Monitor, Parun.Types.RerollReach). Its payer picks the dice
      -- (Prompt.RerollDice), and the picked dice are rerolled as one roll
      -- (`rethrow`), each with its own offers, under one event
      -- (`recordRerolls`), the new dice taking the picked places in order,
      -- each keeping the rerolls the die it replaces carried.
      -- The offers are re-read after each one taken, Night Shift's budget
      -- (Dice.withinLimit) keeping a spent one out, so a second Monitor
      -- Monitor sees the dice the first left.
      --
      -- Only at the roll's top: a die `rethrow` throws is one of this roll's
      -- dice, so it is offered here, with the others, rather than alone.
      --
      -- `next` is the `key` the next reroll's event takes.
      rerollingDice next dice = do
        modifiers <- Dice.modifiersFor controller
        gs <- State.get
        rerollingDiceThrough next (fmap (offerOf gs) (Dice.rerollOffers RerollReach.AnyOfTheDice sides Nothing modifiers)) dice
      rerollingDiceThrough next offers dice = do
        gs <- State.get
        -- "Dice you rolled": the dice whose natural result its payer threw.
        let reachable payer = [i | (i, (thrower, _, _, _)) <- zip [0 :: Natural ..] dice, thrower == payer]
            standing (payer, stated, offer) = not (null (reachable payer)) && Dice.withinLimit gs payer stated offer && payable gs payer stated (ModifiedRoll.cost offer)
        picked <- Dice.pickModifier controller (const Nothing) (filter standing offers)
        case picked of
          Nothing -> pure dice
          Just ((payer, stated, offer), rest) -> case NonEmpty.nonEmpty (reachable payer) of
            -- Unreachable: `standing` kept only an offer with a die to reach.
            Nothing -> rerollingDiceThrough next rest dice
            Just indices -> do
              let mCost = ModifiedRoll.cost offer
                  shown = fmap (\i -> maybe 1 (\(_, natural, _, _) -> natural) (Maybe.listToMaybe (List.genericDrop i dice))) indices
              taken <- askPaying Set.empty payer payer stated mCost $ do
                now <- State.get
                chosen <- Game.choose (Prompt.RerollDice (Decide.deciderFor payer now) payer shown mCost)
                -- FILTERED, NOT TRUSTED: an index past the end is dropped.
                let kept = Set.filter (< List.genericLength (NonEmpty.toList indices)) chosen
                pure (if Set.null kept then Nothing else Just kept)
              case taken of
                Nothing -> rerollingDiceThrough next rest dice
                Just chosen -> do
                  State.modify' (Dice.spendLimit stated offer)
                  let targets = Set.map (List.genericIndex (NonEmpty.toList indices)) chosen
                  again <- rethrow payer (Just next) (Natural.length targets) >>= offeringAll
                  let placed fresh held = case held of
                        [] -> []
                        (i, die@(_, _, rolled, carried)) : others
                          | Set.member i targets, (thrower, natural, faces, pending) : later <- fresh -> (thrower, natural, rolled <> faces, carried <> pending) : placed later others
                          | otherwise -> die : placed fresh others
                  rerollingDice (next + 1) (placed again (zip [0 :: Natural ..] dice))
      -- The roll's units again, `flat` cut back to their sizes.
      regroup units flat = case units of
        [] -> []
        unit : rest -> let (here, after) = List.splitAt (length unit) flat in here : regroup rest after
      -- CR 706.6: every die is thrown and every face asked for -- an ignored
      -- roll HAPPENED and is only then treated as never having happened -- so
      -- the ignore runs on the results and not on the count -- and could not
      -- run any earlier in any case, the lowest roll not being known until
      -- every die has come up.
      --
      -- A unit of several dice is Krark's Thumb's "flip two coins and ignore
      -- one" carried across by Pokey (Parun.Types.Throw): one of them is kept,
      -- chosen once every die is up, the Thumb's ruling's order -- "you will
      -- know the results of all simultaneous flips before choosing which to
      -- ignore". Through Prompt.ChooseDieResult, CR 706.4's question of which
      -- result to use; elided where they agree, since no board can tell the
      -- answers apart: the same number thrown by the same player with the same
      -- face. Each option is shown with its thrower and the natural results
      -- and coins it carries (`asOption`), which is what tells apart options
      -- reading the same number, Dice.ignoreLowest's posture.
      -- FILTERED, NOT TRUSTED: an index past the end keeps the first die.
      --
      -- Each die keeps the player who threw its final natural result, which a
      -- Goblin Bookie reroll can make another seat's (Pippa's ruling), so
      -- dice tied for CR 706.6's lowest but thrown by different players are
      -- asked about (Dice.ignoreLowest).
      --
      -- `atTop` is whether this is the roll itself, which takes the offers over
      -- several dice (rerollingDice), or a die `rethrow` rerolls.
      --
      -- Answers, beside each die kept, the face it was thrown with before any
      -- reroll of it and the board once every face was up, for a reroll's
      -- event (`rethrow`); Nothing at the top, whose event is the roll's
      -- once every modifier has applied (docs/decisions.md, "Which board a
      -- roll's trigger is checked against"). Carried through the choice and
      -- the ignore with the die's rerolls not yet recorded, so none is read
      -- for a die not kept (CR 706.6), and two dice differing in any are told
      -- apart there.
      --
      -- Every die's face is asked for before any die is offered a reroll:
      -- the dice are thrown at once, and Night Shift of the Living Dead's
      -- ruling has the roller "see all the results before deciding", which
      -- Prompt.Rethrow shows them.
      naturals atTop settled modifierOf = do
        thrownFaces <- traverse (\n -> traverse (const (fmap faceOf (Game.ask (Prompt.RollDie sides)))) [1 .. n]) (Foldable.toList (Throw.units settled))
        landed <- State.gets Trigger.battlefieldCandidates
        done <- offeringAll (fmap (\face -> opened numbered (Throw.thrower settled) face id) (concat thrownFaces))
        let thrown = regroup thrownFaces done
            faces = fmap (fmap (\face -> if atTop then Nothing else Just (face, landed))) thrownFaces
        rerolled <- if atTop then fmap (regroup thrown) (rerollingDice (0 :: Natural) (concat thrown)) else pure thrown
        -- After the rerolls above, which is CR 706.2b's order: rerolls are
        -- considered first, and what they leave is the natural result the
        -- instruction's own modifier adds to.
        --
        -- Left unclamped: CR 706.2's result is the number after EVERY
        -- modifier, so a shift from another source applies to this sum as it
        -- stands, negative or not, and only the final figure is clamped below.
        units <- traverse (traverse (\((thrower, natural, rolls, pending), face) -> fmap (\modifier -> ((toInteger natural + modifier, thrower, rolls, pending), face)) modifierOf)) (zipWith zip rerolled faces)
        picked <- Monad.forM units $ \dice -> case NonEmpty.nonEmpty dice of
          -- Unreachable: a unit starts at one die and only a scaling that
          -- cannot reach zero resizes it. Defensive: a die of 1.
          Nothing -> pure ((1, Throw.thrower settled, [], []), Nothing)
          Just offered
            | all (== NonEmpty.head offered) (NonEmpty.tail offered) -> pure (NonEmpty.head offered)
            | otherwise -> do
                gs <- State.get
                answer <- Game.choose (Prompt.ChooseDieResult (Decide.deciderFor (Throw.thrower settled) gs) (Throw.thrower settled) Nothing (fmap (asOption (Throw.thrower settled)) offered))
                pure (Replacement.at dice answer (NonEmpty.head offered))
        Dice.ignoreLowest (Throw.thrower settled) (\((total, _, _, _), _) -> total) (\((_, thrower, rolls, pending), face) -> (thrower, rolls, pending, face)) (asOption (Throw.thrower settled)) (Throw.ignored settled) picked
      -- One die as Prompt.ChooseDieResult shows it: its number, its thrower,
      -- and as natural-result events the face it was thrown with by `by`, the
      -- results it came up and the rerolls it carries, beside a coin's event.
      asOption by ((total, thrower, rolls, pending), face) =
        ( Integer.toNaturalSaturating total,
          thrower,
          foldMap (\(came, _) -> [Common.naturalResult by came]) face
            <> fmap (uncurry Common.naturalResult) rolls
            <> fmap (\(_, entry, _) -> either id (uncurry Common.naturalResult) entry) pending
        )
  kept <- naturals True roll perDie
  recordRerolls (foldMap (\((_, _, _, pending), _) -> pending) kept)
  -- CR 706.6 before the second step: an ignored roll is one "no effects apply
  -- to", so it is not offered for an adjustment.
  --
  -- CR 107.1b once, on the final figure, after every modifier: The Deck of
  -- Many Things' natural 3, four cards in hand and a shift up is a 0, not a 1
  -- (Parun.DiceSpec's "CR 706.2 a shift applies to the unclamped sum").
  results <- fmap (fmap Integer.toNaturalSaturating) (adjusting (fmap (\((total, _, _, _), _) -> total) kept))
  pure (results, fmap (\((_, thrower, faces, _), _) -> (thrower, faces)) kept)

-- What Parun.Engine.Cost reaches through the Parun.Types.ManaAbilityPerformer
-- parameter: CR 405.6c's other effects of the activated mana ability being paid,
-- CR 605.4a's triggered mana ability, CR 118.12's resolution cost, and the CR
-- 706 roll Pokey, the Scallywagg makes for a cost's coin flip and the planar
-- die's (throwRecorded),
-- and CR 614.11a's settle for a life payment's draws.
--
-- `runSubgame` is CR 730.1a's runner for a subgame either one starts. Only the
-- triggered half can: CR 605.1a's library clause (ManaAbility.movesLibraryCard)
-- keeps an activated ability that plays one off the mana-ability road, and CR
-- 605.1b has no such clause. Parun.GameSpec's "CR 605.4a / 730.1b gameplay: a
-- triggered mana ability's subgame winner draws a card" proves the triggered road.
performManaAbility :: Game Result -> ManaAbilityPerformer.ManaAbilityPerformer
performManaAbility runSubgame =
  ManaAbilityPerformer.MkManaAbilityPerformer
    { ManaAbilityPerformer.effects = performManaAbilityEffects runSubgame,
      ManaAbilityPerformer.triggered = performTriggeredManaAbility runSubgame,
      ManaAbilityPerformer.payGate = performManaPayGate runSubgame,
      ManaAbilityPerformer.settle = settleWith runSubgame
    }

-- CR 118.12 on a mana ability's clause, which CR 605.3b gives no stack object:
-- the SOURCE stands in for the resolving one, performManaAbilityEffects'
-- posture. Offered and answered as Parun.Engine.Resolve.payGateAdmits offers a
-- resolving clause's gate, and reusing an answer already recorded for the
-- ordinal a sharing clause names. The seats selected are not bound under
-- Binding.gatePlayers: there is no ability object to bind them on, and no mana
-- ability prints a "they" (MTGJSON's dump of 2026-08-23: Rhystic Cave is the one
-- mana ability with a resolution cost).
performManaPayGate :: Game Result -> ObjectId -> PlayerId -> ClauseIndex.ClauseIndex -> PayGate.PayGate -> Map.Map ClauseIndex.ClauseIndex (Map.Map PlayerId Bool) -> Game (Bool, Map.Map ClauseIndex.ClauseIndex (Map.Map PlayerId Bool))
performManaPayGate runSubgame source controller cIdx gate answers = do
  let offerAt = Maybe.fromMaybe cIdx (PayGate.offeredAt gate)
  asked <- maybe (payGatePaid runSubgame source source controller (ModeIndex.MkModeIndex 0) cIdx Map.empty Nothing Set.empty gate) pure (Map.lookup offerAt answers)
  pure (not (Set.null (Common.branchSelects (PayGate.branch gate) asked)), Map.insert offerAt asked answers)

-- CR 605.4a: apply one triggered mana ability where it stands. CR 605.1b's
-- classification and CR 603.4's intervening "if" are already spent by
-- Parun.Engine.Cost.applyManaTriggers, which is the only caller; this is the
-- resolution half, and it lives here because Parun.Engine.Cost cannot reach
-- applyEffect.
--
-- No stack object, exactly as for the activated mana ability above, so the
-- SOURCE stands in for the resolving one and the slots are stamped rather than
-- read off an object. CR 605.1b leaves no targets to bind, so what the event
-- bound (Parun.Engine.Event.Binding.eventBindings) is the whole environment --
-- Binding.manaSource, which is how Wild Growth's "its controller" names the
-- land rather than the Aura, and Binding.triggerPlayer, Mana Flare's "that
-- player". What one effect binds, a later one reads
-- (runCarrying): Synthetic Wellspring of Legends' subgame winner.
--
-- A SOURCELESS pending trigger cannot arrive: no inherent ability the rulebook
-- states adds mana, and Parun.Engine.Cost gathers only from an object's
-- conditions. Answered by doing nothing rather than by a partial pattern.
--
-- Unprinted: a modal triggered mana ability, which would want the CR 700.2b
-- prompt Engine.placeBorne raises where CR 605.4a leaves no stack object; only
-- a FORCED selection is applied. Parun.CardSpec's "CR 605.1b no card writes a
-- trigger limit or a mode choice on a triggered mana ability" keeps it out of
-- card data.
performTriggeredManaAbility :: Game Result -> PendingTrigger.PendingTrigger -> Game ()
performTriggeredManaAbility runSubgame pending = case PendingTrigger.source pending of
  TriggerSource.Sourceless -> pure ()
  TriggerSource.OfObject source -> do
    let controller = PendingTrigger.controller pending
        modal = TriggeredAbility.modal (PendingTrigger.ability pending)
        -- Every PRINTED mode is fillable: CR 605.1b's no-target clause leaves
        -- nothing for Target.fillableModes to reject, which is the gate
        -- Engine.placeBorne applies instead. Zipped against the modes themselves
        -- rather than counted through Modal.modeCount, whose Natural predecessor
        -- would underflow on a modeless ability.
        every = Set.fromList (fmap fst (zip (fmap ModeIndex.MkModeIndex [0 ..]) (Modal.modeEffects modal)))
        bound =
          Map.union
            (Binding.targetsOf (PendingTrigger.bindings pending))
            ( Map.fromList
                [ (Binding.triggerSource, Set.singleton (Recipient.ToObject source)),
                  (Binding.you, Set.singleton (Recipient.ToPlayer controller))
                ]
            )
    gs <- State.get
    -- CR 113.7a / 613.1f: the arms carry what their names mean to this
    -- ability, as performManaAbilityEffects' do. A regression fence: `jq`
    -- over data/cards/ for a triggered ability holding both AddMana and
    -- ArmDelayedTrigger, 2026-11-13, finds only Kari Zev, Crew of Two's
    -- attack trigger, no mana ability; a Mana Flare that drew at the next
    -- upkeep would refute it.
    let carrying = Projection.carryingDelayed (TriggeredAbility.name (PendingTrigger.ability pending)) source gs
    case Modal.forcedSelection every (Modal.Type.selection modal) of
      Nothing -> pure ()
      Just selection -> Monad.void (runCarrying runSubgame source controller bound Map.empty (fmap carrying (Modal.modesEffects selection modal)))

-- CR 405.6c: run some non-mana effects of a mana ability, as
-- Parun.Engine.Cost.tapForManaWith reaches them in printed order.
--
-- CR 605.3b gives the ability no stack object, so the SOURCE stands in for the
-- resolving one, performHandAction's posture.
--
-- CR 608.2c / 608.2d: what one effect binds, a later one reads -- Spectral
-- Searchlight's "Choose a player. That player adds". The ability has no object
-- to hold its slots, so they are threaded: each effect reads the slots handed in
-- and the ones its predecessors bound, and the answer is all of them. What an
-- effect binds lands on the SOURCE (it is the resolving id here), so each slot
-- the effect defines (boundSlots) is cleared there first and read back after:
-- a slot an earlier activation left behind is never mistaken for this one's.
--
-- The amounts its cost's payment bound arrive on Scratch.ambientAmounts
-- (Cost.tapForManaWith); CR 605.1a leaves it no targets to have bound.
--
-- Unprinted: an object of the ability's own to carry the OBJECTS that payment
-- bound; a read of one would fall through to the source PERMANENT's bindings.
-- Parun.AbilitySlotLintSpec's "CR 405.6c no card writes a mana ability whose
-- other effects read an object its cost paid" keeps that read out of card data.
--
-- CR 113.7a / 613.1f: nor has it an object to freeze its source's delayed
-- abilities on, so each arm carries the one its name means to the ability
-- named `reader` (Projection.carryingDelayed). The scenario
-- myr-welder-s-acquired-mana-ability-arms-the-delayed-trigger-of-its-card
-- proves it.
performManaAbilityEffects :: Game Result -> ObjectId -> Maybe AbilityName.AbilityName -> PlayerId -> Map.Map SlotName (Set Recipient) -> [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Game (Map.Map SlotName (Set Recipient))
performManaAbilityEffects runSubgame source reader controller bound effects = do
  gs <- State.get
  -- CR 109.5's "you" is the player who activated the ability, and the reserved
  -- self slot is CR 113.7's source. Both are bound here rather than read off an
  -- object, because there is no ability object carrying them:
  -- Parun.Engine.Activate.activateAbility stamps them for every ability that does
  -- go on the stack.
  runCarrying
    runSubgame
    source
    controller
    ( Map.fromList
        [ (Binding.triggerSource, Set.singleton (Recipient.ToObject source)),
          (Binding.you, Set.singleton (Recipient.ToPlayer controller))
        ]
    )
    bound
    (fmap (Projection.carryingDelayed reader source gs) effects)

-- CR 608.2c: run instructions that have no stack object in printed order, with
-- `holder` both resolving and source (CR 113.7), carrying what each one binds
-- onto `holder` into the slot maps of the next -- the re-read resolveSpellWith
-- makes off the stack object before each effect. PlaySubgame's winner is what a
-- following Draw reads (Synthetic Threshold of Legends). `fixed` is unioned in
-- under every instruction; the slots an instruction defines are cleared off
-- `holder` first, so a stale value cannot stand in for one it never bound.
runCarrying :: (Foldable t) => Game Result -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> t (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Game (Map.Map SlotName (Set Recipient))
runCarrying runSubgame holder controller fixed =
  let holderBindings gs = case Game.lookupObject holder gs of
        Just obj -> Object.bindings obj
        Nothing -> Map.findWithDefault Map.empty holder (GameState.detachedBindings gs)
      run bound effect = do
        let defined = boundSlots effect
            env = Map.union bound fixed
        State.modify' $ \gs ->
          (Common.overHolderBindings holder (`Map.withoutKeys` defined) gs)
            { GameState.detachedBindings = Map.adjust (`Map.withoutKeys` defined) holder (GameState.detachedBindings gs)
            }
        applyEffectWith runSubgame holder holder controller env env effect
        after <- State.get
        pure (Map.union (Map.restrictKeys (Binding.targetsOf (holderBindings after)) defined) bound)
   in Monad.foldM run

-- "Activate only any time it makes sense" reaches a die being rolled: Pippa,
-- Duchess of Dice's rulings of 2020-02-29, on the same words ("any die roll,
-- not just yours"; "the result still counts"), and a coin just flipped the same
-- way. No priority falls inside a roll or a flip (CR 117.1, 117.3b), so each
-- ability ActivationRestriction.DuringThrow marks is offered through the gate a
-- priority activation takes (CR 602.5a; Activatable.activatable) and resolves at
-- once, as the ruling needs it to reach the die. CR 706.2b's pick is the
-- roller's, throwSettled's `offeringAll` and reflipWindow's; the order below is
-- only where that pick starts. Parun.CardSpec keeps such an ability to an
-- untargeted Effect.Rethrow, which is what lets Prompt.Rethrow ask for it.
--
-- In APNAP order, and within a player in object order, behind the static
-- offers: the order the roller's pick starts from.
windowAbilities :: GameState.GameState -> [(PlayerId, ObjectId, ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))]
windowAbilities opened =
  [ (pid, oid, ability)
  | pid <- Game.apnapOrder opened,
    oid <- Activatable.activationSources pid opened,
    ability <- Activatable.abilitiesFor oid opened,
    elem ActivationRestriction.Type.DuringThrow (ActivatedAbility.restrictions ability)
  ]

-- | CR 705.1's coin, just flipped and not yet read: the window Goblin Bookie's
-- "reflip any coin" is activated in, answering whether the coin is to be
-- flipped again (Parun.Engine.Event.settleCoins does it). The flipper's pick
-- among the standing activations, throwSettled's `offeringAll` without its static
-- offers, since no printing offers a reflip but this window's.
--
-- Not implemented: flipping every coin of a unit before any reflip is offered,
-- and showing the deciding seat the other coins (#9437).
reflipWindow :: Game Result -> PlayerId -> CoinFace.CoinFace -> Game Bool
reflipWindow runSubgame flipper face = do
  let landed = ThrowResult.CoinCameUp face
      offering offers = do
        standing <- Common.withThrowing landed (State.gets (\gs -> filter (\(payer, _, (oid, ability)) -> Activatable.activatable payer oid ability gs) offers))
        picked <- Dice.pickModifier flipper (const Nothing) standing
        case picked of
          Nothing -> pure False
          Just ((payer, _, (oid, ability)), rest) -> do
            gs <- State.get
            answer <- Game.choose (Prompt.Rethrow (Decide.deciderFor payer gs) payer (landed NonEmpty.:| []) 0 (Just (ActivatedAbility.cost ability)))
            case answer of
              OptionalDecision.Declines -> offering rest
              OptionalDecision.Exercises -> do
                requested <- Common.withThrowing landed $ do
                  activated <- activateWhileRolling runSubgame payer oid ability
                  after <- State.get
                  pure (activated && Scratch.rethrowRequested (GameState.scratch after))
                if requested then pure True else offering rest
  offers <- State.gets (fmap (\(pid, oid, ability) -> (pid, Just oid, (oid, ability))) . windowAbilities)
  offering offers

-- | Action.RerollShownDie: `payer` takes the reroll the modifier `stated`
-- states on the die permanent `die` (Dice.shownDieRerolls), paying CR 706.2a's
-- cost through payGateCost, and the die's roller rerolls it
-- (Damage.rerollShownDie). A payment that fails leaves the die as it was.
rerollShownDieWith :: Game Result -> PlayerId -> ObjectId -> ObjectId -> Game ()
rerollShownDieWith runSubgame payer stated die = do
  gs <- State.get
  Foldable.for_ (List.find (\(s, _, d, _) -> s == stated && d == die) (Dice.shownDieRerolls gs)) $ \(_, offer, _, roller) -> do
    paid <- case ModifiedRoll.cost offer of
      Nothing -> pure True
      Just cost -> do
        rollerWindow runSubgame roller payer cost
        fmap (Maybe.isJust . fst) (payGateCost runSubgame Seq.empty Map.empty payer stated cost)
    Monad.when paid $ do
      State.modify' (Dice.spendLimit (Just stated) offer)
      Damage.rerollShownDie throwDice runSubgame roller die

-- CR 706.2a's second sentence: a modifier with a mana cost gives the player who
-- ROLLED a chance to activate mana abilities before it applies. A
-- `Cost.payManaWindow` over an EMPTY cost, `Cost.offerAssist`'s posture: a
-- chance to activate and nothing to pay. None where the roller is the payer,
-- whose payment opens their CR 605.3a window anyway, nor where the cost needs no
-- mana (Wall of Fortune's).
rollerWindow :: Game Result -> PlayerId -> PlayerId -> Cost.Type.Cost Keyword.Type.Keyword -> Game ()
rollerWindow runSubgame roller payer cost =
  Monad.when (roller /= payer && maybe False (not . Cost.requiresNoMana) (Cost.Type.mana cost)) $
    Monad.void (Cost.payManaWindow (performManaAbility runSubgame) Set.empty Nothing PaymentSubject.ForNeither ManaSpending.AsProduced roller (\mc -> pure (mc, [])) (ManaCost.MkManaCost []))

-- Activate an ability inside throwDice's or reflipWindow's window and resolve
-- it at once, answering whether it was paid for. The payment is Parun.Engine.Activate's
-- activateAbility without the stack object: CR 602.2b's cost, with CR 601.2f's
-- adjustments, charged against the activator's own mana window. Parun.CardSpec
-- keeps these abilities to one untargeted mode and no X, so there is no mode,
-- target or X to announce.
--
-- The effects then run as performManaAbilityEffects runs a mana ability's, the
-- source standing in for the ability object.
--
-- A window opened by an entry's flip (Molten Sentry) falls inside runEntry's
-- loop, so the whole activation runs with the entering permanents held off the
-- battlefield (Common.withEnteringHeldOff, CR 614.12a).
activateWhileRolling :: Game Result -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game Bool
activateWhileRolling runSubgame pid oid ability = Common.withEnteringHeldOff . Event.activating $ do
  before <- State.get
  -- CR 602.2b: recorded with no stack object to name, and under way as such.
  let activation = AbilityWasActivated.MkAbilityWasActivated {AbilityWasActivated.player = pid, AbilityWasActivated.ability = Nothing, AbilityWasActivated.source = oid, AbilityWasActivated.facts = Game.abilityFacts AbilityKind.NonManaAbility (Just ability), AbilityWasActivated.activationCost = Cost.Type.mana (ActivatedAbility.cost ability)}
  State.modify' (Event.nowActivating activation)
  let stamp = ActivatedAbility.keyword ability
      loyalty = Loyalty.loyaltyKindOf (ActivatedAbility.cost ability)
      gathered = Cost.activationAdjustments Set.empty stamp (ActivatedAbility.name ability) AbilityKind.NonManaAbility loyalty pid oid before
      totalled = Cost.plusComponents gathered (ActivatedAbility.cost ability)
      spendable = ActivationRestriction.spendable oid (ActivatedAbility.restrictions ability) before
  (announced, _) <- Cost.announce (PaymentSubject.Activating oid AbilityKind.NonManaAbility (ActivatedAbility.name ability) stamp spendable) ManaSpending.AsProduced pid oid (Cost.substitutedManas (Cost.statedSubstitutions (Cost.Type.components totalled) Map.empty pid oid before) (Cost.totalManas gathered)) totalled
  adjustments <- Cost.announceReductions pid oid before announced gathered
  (payment, _) <- Cost.paySubstituting (const False) (performManaAbility runSubgame) before [] PaymentMoment.OutsideResolution (PaymentSubject.Activating oid AbilityKind.NonManaAbility (ActivatedAbility.name ability) stamp spendable) Nothing ManaSpending.AsProduced pid oid (Cost.announceSubstitutions Cost.statedSubstitutions pid oid) (Cost.totalWith adjustments announced)
  case payment of
    -- CR 734.1: the payment reversed the activation back to `before` itself.
    Payment.Unpaid -> pure False
    -- CR 602.2b: the activation is recorded with no stack object to name, so
    -- a trigger copying "that ability" copies nothing (docs/decisions.md,
    -- "Whether copying an activation that never reached the stack copies
    -- anything").
    Payment.Paid _ -> do
      State.modify' (ActivationRestriction.recordActivation oid ability)
      State.modify' (ActivationRestriction.logActivation before pid oid (Just ability) AbilityKind.NonManaAbility Set.empty)
      State.modify' (Event.recordEvent (GameEvent.AbilityActivated activation))
      let modal = ActivatedAbility.modal ability
          every = Set.fromList (fmap fst (zip (fmap ModeIndex.MkModeIndex [0 ..]) (Modal.modeEffects modal)))
          bound =
            Map.fromList
              [ (Binding.triggerSource, Set.singleton (Recipient.ToObject oid)),
                (Binding.you, Set.singleton (Recipient.ToPlayer pid))
              ]
      Foldable.for_ (Modal.forcedSelection every (Modal.Type.selection modal)) $ \selection ->
        Monad.mapM_ (applyEffectWith runSubgame oid oid pid bound bound) (Modal.modesEffects selection modal)
      pure True

-- CR 608.2c: a loop body's instructions in written order, each read against what
-- the loop's earlier instructions bound -- a clause re-reads its bindings per
-- instruction (Parun.Engine.Resolve's applyOne), and a body is read the same way
-- over the slots the loop defines, Effect.ForEach's `defined`. The maps handed
-- down win, being CR 608.2b's re-validated targets. Parun.ZoneChangeSpec's Ad
-- Nauseam group proves it: the loss reads the card the body's own move put into
-- the hand.
applyLoopBody :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Set SlotName -> Seq.Seq (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Game ()
applyLoopBody runSubgame resolving source controller legal chosen bodyDefined body =
  let applyOne effect = do
        defined <- State.gets (\gs -> Map.restrictKeys (Binding.targetsOf (slotBindings resolving gs)) bodyDefined)
        applyEffectWith runSubgame resolving source controller (Map.union legal defined) (Map.union chosen defined) effect
   in Common.applyClauseEffects resolving source applyOne (Foldable.toList body)

-- The offer itself: who was offered this gate's cost, and which of them paid?
-- CR 118.12's MANDATORY limb is not offered, and that is the rule rather than an
-- elision -- it asks whether the player "started to pay", so a mandatory cost the
-- payer can afford leaves nothing to choose, and CR 118.3 is asked first so an
-- unpayable one takes the "can't" branch with no prompt either.
--
-- Narrowed by `announcedOnly` to the seats that announced this branch of a CR
-- 608.2d pair, where the clause is one: the offer belongs to the players who
-- took it, so Worms of the Earth's sacrifice is offered to nobody who announced
-- its damage instead.
--
-- CR 101.4's APNAP order over the players the reference names, which is what
-- `apnapDecidersOf` imposes, each told which option and objects the payers
-- before them paid with (CR 101.4b, Common.payerAnswerOf), at ChooseCost and
-- ChooseToPay and in the ChooseSacrifices and ChooseDiscard their own payment
-- raises (Common.takenBy). Nothing a payer chose has happened when the next is
-- asked, since each rehearsal is undone (CR 101.4a's hidden card stays
-- hidden). Parun.ResolveSpec's "CR 101.4b a later opponent is told" and "CR
-- 101.4b a later payer's sacrifice prompt" cases prove it. Only then does every payment
-- happen, made again from its answers, and two or more are ONE event: the
-- "then the actions happen simultaneously" of CR 101.4 and Perforating
-- Artist's ruling, which CR 603.10a's look-back reads. The scenarios
-- named perforating-artist prove both. A payment the others have since made fail
-- asks its payer again (payAgain), a fence: no pool cost depends on another
-- payer's permanent or mana ability, as is a lone payment's staying outside the
-- bracket.
--
-- The mana windows are not in the event: CR 605.3a's activations come before
-- the payment they are for (CR 601.2g's order), so every payer's window
-- closes, in APNAP order, before any payment is made (Cost.payReadingThen), and
-- a creature Ashnod's Altar sacrificed for mana dies apart from another
-- payer's. The scenario
-- cr-605-3a-each-altar-sacrifice-paying-for-rhystic-tutor-is-its-own-event
-- proves it.
--
-- A player the reference names who has LEFT the game stays in this list and is
-- answered unpaid by payGatePaidBy, CR 800.4f.
--
-- CR 118.12 / 118.12a: the payment is caused by the spell or ability resolving
-- (Event.causedBy; Tamiyo, Collector of Tales' ruling of 2019-05-03). The
-- scenario ajanis-last-stand-makes-an-avatar-discarded-to-an-opponents-painful-quandary
-- proves it.
payGatePaid :: Game Result -> ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> Set PlayerId -> PayGate.PayGate -> Game (Map.Map PlayerId Bool)
payGatePaid runSubgame resolving source controller idx cIdx legal announced committed gate = caused resolving source controller $ do
  gs <- State.get
  -- A COMMITTED payer (Parun.Engine.Resolve.chosenBranch) already chose to pay
  -- when they announced the branch, so their offer is CR 118.12's mandatory
  -- limb: paid when CR 118.3 allows, and not asked.
  let offerTo payer = if Set.member payer committed then gate {PayGate.obligation = PayObligation.Mandatory} else gate
      rehearse earlier payer = payGatePaidBy runSubgame resolving source controller (PayOffer.AtClause idx cIdx) (fmap (fmap (Common.payerAnswerOf gs)) earlier) legal payer (offerTo payer)
  rehearsed <-
    Monad.foldM
      ( \earlier payer -> do
          plan <- rehearse earlier payer
          State.put gs
          pure (earlier Seq.|> (payer, plan))
      )
      Seq.empty
      (Common.announcedOnly announced (Common.apnapDecidersOf (PayGate.payer gate) legal resolving source controller gs))
  -- CR 605.3a's activations ahead of the one event (CR 601.2g): each payer's
  -- last attempt is replayed up to its mana window's close, and its earlier,
  -- failed ones whole, in APNAP order, before any payment is made.
  settles <-
    Monad.forM rehearsed $ \(payer, (attempts, paid)) -> case Seq.viewr attempts of
      Seq.EmptyR -> pure (payer, paid, pure Nothing)
      failed Seq.:> (slots, cost, answers) -> do
        let taken = Common.takenBy (fmap (fmap (Common.payerAnswerOf gs)) (Seq.takeWhileL ((/= payer) . fst) rehearsed))
        Foldable.for_ failed (\(slots', cost', answers') -> Replay.replaying answers' (payGateCost runSubgame taken slots' payer source cost'))
        (settle, unused) <- Replay.replayingRest answers (payGateCostThen runSubgame taken slots payer source cost)
        pure (payer, paid, Replay.replaying unused settle)
  let together = if length (Seq.filter (Maybe.isJust . snd . snd) rehearsed) > 1 then Event.simultaneously else id
  answered <-
    together . Monad.forM settles $ \(payer, paid, settle) -> do
      replayed <- settle
      asked <- case (paid, replayed) of
        (Just _, Nothing) -> fmap snd (rehearse (Seq.takeWhileL ((/= payer) . fst) rehearsed) payer)
        _ -> pure replayed
      pure (payer, asked)
  State.modify' (Common.foldPaid resolving (foldr Binding.mergePaid Map.empty (Maybe.mapMaybe snd (Foldable.toList answered))))
  -- PayGate.marks: the source performs the act once anybody has paid (CR
  -- 702.30a's echo cost, Shah of Naar Isle).
  Foldable.for_ (PayGate.marks gate) $ \action ->
    Monad.when (any (Maybe.isJust . snd) answered) (State.modify' (Event.recordEvent (GameEvent.PermanentActed (PermanentActed.MkPermanentActed action source Nothing))))
  pure (Map.fromList [(payer, Maybe.isJust paid) | (payer, paid) <- Foldable.toList answered])

-- CR 118.12a over CR 608.2f's loop: the members whose ForEach.payGate branch
-- selects anybody, in the loop's own order. Each member is its own offer, read
-- through the loop's slot, so a payment for one member buys nothing for
-- another. CR 101.4: every payer answers all of their offers, one per member,
-- before the next payer in APNAP order answers theirs -- Killing Wave's and
-- Whirlwind Denial's rulings -- and is told what the payers before them
-- answered for THAT member (CR 101.4b). Affordability is of everything the
-- payer has agreed to so far (CR 118.3), plus the life their teammates sharing
-- a total have agreed to pay (CR 810.9b): the scenario "CR 810.9b Killing Wave
-- caps two teammates' payments at the team's life total" proves it.
--
-- Only then does anybody pay, and each payer pays everything they agreed to as
-- ONE payment (Cost.together), which is those rulings' "then pays that amount"
-- and CR 101.4's "Then the actions happen simultaneously": Killing Wave paid for
-- three creatures is one loss of 3X life, not three. A payer whose combined
-- payment fails has paid for none of them, and is asked again (CR 734.1,
-- payAgain): the scenario "CR 734.1 a Spirit owner whose payment for Cut the
-- Tethers fails is asked again" proves it. No body runs until every payment
-- is made, and what the payments bound is folded on first (foldPaid). Proved
-- by Parun.ResolveSpec's "an offer per member" group, whose Exquisite Blood case
-- counts the life-loss events.
loopOffers :: Game Result -> ObjectId -> ObjectId -> PlayerId -> SlotName -> Map.Map SlotName (Set Recipient) -> [Recipient] -> PayGate.PayGate -> Game [Recipient]
loopOffers runSubgame resolving source controller slot legal members gate = do
  gs <- State.get
  let legalFor member = Map.insert slot (Set.singleton member) legal
      payersOf member = Common.apnapDecidersOf (PayGate.payer gate) (legalFor member) resolving source controller gs
      order = Game.apnapOrder gs
      offers = [(payer, member) | payer <- order, member <- members, elem payer (payersOf member)]
      agreedBy payer answers = [taken | (p, _, Just taken) <- Foldable.toList answers, p == payer]
      agreeOne acc payer member = do
        (slots, options) <- State.gets (Common.gateCostOf resolving source controller (legalFor member) gate)
        let earlier = Seq.fromList [(p, maybe PayerAnswer.Declined (\(_, cost) -> PayerAnswer.Paid cost Set.empty) taken) | (p, m, taken) <- Foldable.toList acc, m == member]
            teammates = [Cost.lifePart cost | (p, _, Just (_, cost)) <- Foldable.toList acc, p /= payer, elem p (Game.lifeSharers payer gs)]
            owed = fmap Cost.together (NonEmpty.nonEmpty (fmap snd (agreedBy payer acc) <> teammates))
        agreed <- Common.payGateAgreed resolving source (PayOffer.ForMember member) earlier payer gate slots owed options
        pure (payer, member, fmap ((,) slots) agreed)
      -- One payer's offers answered again, in place, each against what was
      -- answered before it; later payers' answers stand, CR 734.1 reversing
      -- this payer's action alone.
      reagree payer answers =
        Monad.foldM
          ( \sofar i -> case Seq.lookup i sofar of
              Just (p, member, _) | p == payer -> fmap (\entry -> Seq.update i entry sofar) (agreeOne (Seq.take i sofar) p member)
              _ -> pure sofar
          )
          answers
          [0 .. Seq.length answers - 1]
      -- Each payer pays everything they agreed to as one payment; a failed one
      -- asks that payer again (payAgain). Its prompts name what the payers
      -- before it took, read off the board before any payment (CR 101.4b).
      payFor (done, answers) payer =
        let attempt failed current = case NonEmpty.nonEmpty (agreedBy payer current) of
              Nothing -> pure (Just (done, current))
              Just taken -> do
                let earlier = Seq.fromList [(p, Common.publicObjectsOf gs bound) | p <- takeWhile (/= payer) order, Just bound <- [Map.lookup p done]]
                (bound, answered) <- payGateCost runSubgame earlier (fst (NonEmpty.head taken)) payer source (Cost.together (fmap snd taken))
                Common.payAgain failed (fst (NonEmpty.head taken), NonEmpty.toList (fmap snd taken), answered) (fmap (\b -> (Map.insert payer b done, current)) bound) id (\failed' -> reagree payer current >>= attempt failed')
         in fmap (Maybe.fromMaybe (done, answers)) (attempt Set.empty answers)
  asked <- Monad.foldM (\acc (payer, member) -> fmap (acc Seq.|>) (agreeOne acc payer member)) Seq.empty offers
  (paid, answered) <- Monad.foldM payFor (Map.empty, asked) order
  State.modify' (Common.foldPaid resolving (foldr Binding.mergePaid Map.empty paid))
  let askedAbout member = Map.fromList [(p, Maybe.isJust taken && Map.member p paid) | (p, m, taken) <- Foldable.toList answered, m == member]
  pure (filter (not . Set.null . Common.branchSelects (PayGate.branch gate) . askedAbout) members)

-- One player's answer to one gate. The cost is the PRINTED one with CR 107.3's X
-- resolved (`announcedXOn`, or PayGate.x's chosen value) and then multiplied by the gate's "for each"
-- (PayGate.perEach), and that pair of rewrites is what every reader below
-- sees -- CR 118.3's affordability test, the prompt the payer is shown, and the
-- payment itself -- so none of them can disagree about what is owed.
--
-- The multiplier is read HERE rather than once for the whole gate, so a payer
-- asked again (payAgain) is measured against the board they face.
--
-- Measured against the RESOLUTION, not against the payer: the context is the
-- resolving controller's (`effectContext`, never Filter.contextFor), so
-- Rakshasa's Disdain's "for each card in your graveyard" counts the
-- graveyard of the player who cast it while the payer is the targeted spell's
-- controller. The quantity is evaluated against the ability's SOURCE through
-- `effectViewOf`, so CR 113.7a's last known record answers for a source that has
-- already left, with the announcement id CR 601.2b stamped on the RESOLVING
-- object -- Resolve.Slots' TopOfLibrary depth is the same pair. An unevaluable
-- or negative count is zero copies (CR 107.2, 107.1b). Whether such an ability should
-- be offering anything at all is its own text's business: rule 702.24a's
-- intervening "if" is what stops it (CR 603.4), proved at
-- Parun.KeywordTriggerSpec's "a Unicorn murdered in response".
--
-- CR 800.4f, asked BEFORE CR 118.3 and before the offer: a payer who has left
-- the game does not pay, and is not asked whether to. That is an answer of False
-- and not a removal from payGatePaid's map, because CR 118.12a's rewriting makes
-- "unless" mean "if they don't, [do something]" and an unpaid cost is exactly
-- what selects that limb -- rule 702.21a's ward counters the spell.
--
-- It cannot ride the CR 118.3 test beneath it. A departed player keeps their
-- CR 102.1 row, so an affordability question asked of their life total or their
-- last known board can still answer yes. Ward is the reachable case: rule
-- 702.21a targets nothing, so CR 608.2b never empties Binding.targetingObject
-- and the trigger resolves after the targeter has gone, with
-- PlayerRef.HolderOfBound naming them through CR 608.2h.
-- Parun.DepartureSpec's "CR 800.4f a departed player is not offered a ward cost,
-- and does not pay it" proves this arm.
--
-- CR 800.4g is the same situation for a choice that is NOT a payment, and the
-- opposite answer: the controller of the object picks another player to make it,
-- which Parun.Engine.Resolve.Common.askedChooser does. Not here -- a cost is the
-- whole of what this function asks about.
--
-- The answer is each attempt at the payment, the slots and cost it was made
-- against and how it was answered (payGateCost's record), for payGatePaid to make
-- again, and what the last one bound, if it was paid. A failed attempt counts: a
-- mana ability the payer kept stands (CR 734.1).
payGatePaidBy :: Game Result -> ObjectId -> ObjectId -> PlayerId -> PayOffer.PayOffer -> Seq.Seq (PlayerId, PayerAnswer.PayerAnswer) -> Map.Map SlotName (Set Recipient) -> PlayerId -> PayGate.PayGate -> Game (Seq.Seq (Map.Map SlotName (Set Recipient), Cost.Type.Cost Keyword.Type.Keyword, [Response.Response]), Maybe (Map.Map SlotName Binding.Type.Binding))
payGatePaidBy runSubgame resolving source controller offer earlier legal payer gate = attempt Seq.empty Set.empty
  where
    -- CR 734.1 / 118.12a: a payment that fails is reversed, which returns the
    -- payer to the choice, so they are asked again (payAgain).
    attempt tried failed = do
      (slots, options) <- State.gets (Common.gateCostOf resolving source controller legal gate)
      agreed <- Common.payGateAgreed resolving source offer earlier payer gate slots Nothing options
      case agreed of
        Nothing -> pure (tried, Nothing)
        Just cost -> do
          (paid, answered) <- payGateCost runSubgame (Common.takenBy earlier) slots payer source cost
          let attempts = tried Seq.|> (slots, cost, answered)
          Common.payAgain failed (cost, answered) paid ((,) attempts) (attempt attempts)

-- The payment of a cost the payer agreed to, against `source` (CR 113.7a): the
-- slots it bound when it was paid (foldPaid), Nothing when it was not, and the
-- answers the payment was given (payAgain). Its prompts tell the payer what the
-- earlier payers of the offer took (PaymentMoment.DuringResolution).
payGateCost :: Game Result -> Seq.Seq (PlayerId, Set ObjectId) -> Map.Map SlotName (Set Recipient) -> PlayerId -> ObjectId -> Cost.Type.Cost Keyword.Type.Keyword -> Game (Maybe (Map.Map SlotName Binding.Type.Binding), [Response.Response])
payGateCost runSubgame earlier slots payer source cost = Replay.recording (Monad.join (payGateCostThen runSubgame earlier slots payer source cost))

-- `payGateCost` unrecorded and split at its mana window (Cost.payReadingThen),
-- payGatePaid's two halves.
payGateCostThen :: Game Result -> Seq.Seq (PlayerId, Set ObjectId) -> Map.Map SlotName (Set Recipient) -> PlayerId -> ObjectId -> Cost.Type.Cost Keyword.Type.Keyword -> Game (Game (Maybe (Map.Map SlotName Binding.Type.Binding)))
payGateCostThen runSubgame earlier slots payer source cost = do
  gs <- State.get
  -- CR 118.13b: a symbol payable in multiple ways is announced by the
  -- PAYER "immediately before they pay that cost" -- after CR 118.12's
  -- "may" above, since what is announced is how to pay a cost already
  -- chosen, and before the mana window Cost.pay opens.
  --
  -- CR 601.2f's totalling is `pure`, which is the identity in the list
  -- applicative `Cost.announce` measures through: parun gathers cost
  -- adjustments for a SPELL (Cast.castSpell) and for an ACTIVATION
  -- (Activate.activateAbility) and nowhere else, and `Cost.pay` below
  -- applies none of its own, so the announced cost IS the cost that will
  -- be paid. A card that reduced a CR 118.12 cost would be the one to
  -- refute that, and `data/cards/` prints none. That also keeps the offer
  -- exactly as permissive as `Cost.canPay` above, which enumerates the
  -- same CR 601.2b nonhybrid equivalents through Mana.resolutions --
  -- so no route Mana.announce offers is one the gate refused, and its
  -- no-payable-route fallback stays unreachable from here. CR 701.67a's
  -- taps are folded into the totalling for the same reason, Activate's
  -- `substitutedManas` posture: the gate weighs them too.
  -- Discarded, Activate's reason: rule 702.150a asks about a spell's own
  -- cost, not about a cost paid during a resolution (CR 118.13b).
  (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced payer source (Cost.substitutedManas (Cost.statedSubstitutions (Cost.Type.components cost) slots payer source gs) pure) cost
  -- DuringResolution: rule 118.12's cost is paid as the spell or ability
  -- resolves, so its events keep the resolution's cause (Cost.payComponent).
  -- CR 734.1's reversal base: rule 118.12's payment IS the whole of
  -- the action that can fail, so the state it begins in is where a
  -- failure returns to and the CR 605.3a window is the payer's to keep
  -- (Cost.pay). Taken after the announcement above, which writes no
  -- state of its own.
  began <- State.get
  settle <- Cost.payReadingThen slots (performManaAbility runSubgame) began (PaymentMoment.DuringResolution earlier) PaymentSubject.ForNeither ManaSpending.AsProduced payer source announced
  pure (fmap (\outcome -> case outcome of Payment.Paid bound -> Just bound; Payment.Unpaid -> Nothing) settle)
