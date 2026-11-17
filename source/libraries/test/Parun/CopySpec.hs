{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Parun.Engine.Replacement's EntryR AsCopy arm (the CR 614.12a copy choice, run
-- from inside Parun.Engine.Event's changeZone) and its CR 707.9 exceptions
-- (Replacement.applyCopyExceptions -- CR 707.9b's Quicksilver Gargantuan and
-- Phyrexian Metamorph, the second of which is where CR 707.9d's carve-out keeps
-- the copied characteristic-defining ability, and CR
-- 707.9a's Dack's Duplicate and Omni-Changeling, the second of which is where CR
-- 604.3a makes the gained ability characteristic-defining, and CR 707.9b's
-- Sakashima the Impostor, whose name and additive supertype clauses are read
-- together by CR 704.5j, and CR 707.9a's quoted ability, Mercurial Pretender,
-- which a Clone of the copy keeps), its CR 707.5 eligible set
-- (Replacement.legalCopyTargets, Copy Enchantment's "any enchantment" against Clone's
-- "any creature", and Clever Impersonator's negated "any nonland permanent"), the
-- P2 copy gate (Clone), and
-- Parun.Engine.Resolve's CreateCopy arm (CR 707.2's token copy, Cackling
-- Counterpart and Watchful Radstag; its count, and the simultaneous entry that
-- count buys, kicked Rite of Replication; CR 122.6's entry rider on it,
-- Littjara Mirrorlake; its tap and attack riders and its bound slot, Flamerush
-- Rider; and CR 707.9b's exception riding it, Multiversal
-- Recruitment's "except it isn't legendary" read by CR 704.5j; CR 702.128a's
-- embalm and CR 702.129a's eternalize, whose colour, mana-cost, subtype and P/T
-- exceptions a Clone of the token keeps -- data/scenarios/copy) and its BecomeCopy arm (CR 707.4's
-- change of a permanent already on the battlefield, and CR 707.9a's "except it
-- has this ability" riding it -- Unstable Shapeshifter, which copies twice
-- because of it, and whose token copy copies again because CR 707.9a put the
-- ability in the copiable values; and CR 707.9c's decline-to-copy exception
-- riding both roads at once -- Vesuvan Doppelganger, whose copy keeps its own
-- colour on entry and keeps it again when the ability it quoted copies a second
-- creature), and CR 611.2a's stated DURATION riding it -- Mirrorweave, whose
-- copy ends in the cleanup step while a token copy taken under it keeps the
-- values (CR 707.2b), and under which a Clone's own stamp is masked and then
-- revealed again.
-- Gameplay-level: Clone enters via the zone-change funnel, the Counterpart is
-- cast and resolved, the Radstag evolves and the Shapeshifter's trigger resolves,
-- and their projected characteristics are asserted.
--
-- Also CR 305.7's copiable-effects clause, where a copy meets the layer system:
-- Vesuva is played as a land, copies Mutavault, and a Blood Moon arriving after
-- takes the copied abilities with the printed ones
-- (Parun.Engine.Projection.setLandSubtypeTo) -- plus the other order, where CR
-- 614.12 leaves Vesuva no copy ability to apply at all.
--
-- And CR 707.2's copiable values at the two BATTLEFIELD-WIDE SHORT-CIRCUITS that
-- decide whether a walk is needed at all -- Parun.Engine.Projection's
-- copiableReplacementsOf, anyCopiableKeyword and copiableMintsType, in
-- replacementsAffecting's baseHas, and Parun.Engine.CombatRestriction's
-- baseCouldMint -- on a board that has been left holding the only copy of the
-- departed original's text, whether an Unstable Shapeshifter or a Copy
-- Enchantment / Clever Impersonator put it there (copiedAbilitySpec).
--
-- And Parun.Engine.Resolve's CopyStackObject arm (CR 707.10's copy of a spell on the
-- stack, Twincast) with the CR 707.10c re-target prompt it raises -- including
-- the announcement that prompt's offer is judged inside, CR 707.10's copied X
-- read by a copied Stir the Grave's own target slot (data/scenarios/copy) -- the CR
-- 704.5e state-based action in Parun.Engine.Sba that removes the resolved copy,
-- and Parun.Engine.Stack's OfSpellCopy resolution arm.
--
-- And that arm's two answers to the rest of CR 707.10's sentence: who puts the
-- copy onto the stack, where the effect names somebody other than its own
-- controller (Meletis Charlatan, whose copy is the copied spell's controller's
-- and who is therefore CR 707.10c's chooser), and CR 707.9's "except ..." clause
-- riding it (Double Major's "except it isn't legendary", read by CR 704.5j over
-- the token CR 707.10f mints).
--
-- And that same arm over CR 707.10's other two nouns -- an activated and a
-- triggered ability on the stack, copied by Lithoform Engine
-- (copyAbilityOnStackSpec), where CR 707.10b keeps the original's source.
--
-- And CR 613.2b's ordering of the two halves of layer 1, where a permanent is
-- both a copy and face down: CR 708.2's listed characteristics win over what the
-- copy effect stamped, read off an attack declaration a copied Silent Arbiter
-- would otherwise bound and off the projected 2/2 (data/scenarios/copy).
--
-- And CR 707.10d's and CR 707.10e's answers whole, end to end: Zada, Hedron
-- Grinder's one copy per candidate and Ivy, Gleeful Spellthief's one copy on a
-- stated new target, the second of which is where "the copy isn't created" is
-- read off an illegal one. Radiate's candidates include players, and Precursor
-- Golem's "other" is not its source (all data/scenarios/copy).
--
-- And CR 707.12's copy of a CARD, made in the zone that card is in and then cast
-- (Parun.Engine.Resolve.Common's castableCopy, under Parun.Types.OfferCast's
-- `copied`): Mizzix's Mastery, whose exiled instant stays in exile while the copy
-- goes on the stack, and whose declined copy is swept by CR 704.5e
-- (data/scenarios/copy).
--
-- And CR 702.99's cipher, whose granted trigger casts a CR 707.12 copy of the
-- encoded card: Last Thoughts (data/scenarios/copy).
--
-- And CR 707.13's copy of a card defined by NAME, created outside the game and
-- then cast: Garth One-Eye (data/scenarios/copy, and garthSpec's unknown name).
--
-- And CR 707.14's copy of a card noted as it went face down: Magar of the
-- Magic Strings (data/scenarios/copy).
module Parun.CopySpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Action as Action
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Ante as Ante
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Damage as Damage
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as A
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Designation as Designation
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.KickerDecision as KickerDecision
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.ProjectedCharacteristics as PC
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.Sickness as Sickness
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Source as Source
import qualified Parun.Types.StepBegan as StepBegan
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

-- The battlefield objects whose PRINTED card has this name (a printed card is
-- unchanged by copying -- only the object's projected characteristics change).
printedOnBattlefield :: String -> GameState.GameState -> [ObjectId]
printedOnBattlefield name gs =
  let isIt oid = maybe False (\f -> Face.name f == CardName.MkCardName (Text.pack name)) (Game.faceOf oid gs)
   in filter isIt (Set.toList (GameState.battlefield gs))

-- Whether an object is still on the battlefield -- what a destroy is read
-- through, where printedOnBattlefield above answers by PRINTED name and so
-- cannot tell two copies of one card apart.
onBattlefield :: ObjectId -> GameState.GameState -> Bool
onBattlefield oid gs = Set.member oid (GameState.battlefield gs)

clonesOnBattlefield :: GameState.GameState -> [ObjectId]
clonesOnBattlefield = printedOnBattlefield "Clone"

cloneOnBattlefield :: GameState.GameState -> Maybe ObjectId
cloneOnBattlefield = Maybe.listToMaybe . clonesOnBattlefield

-- The highest-id (most recently entered) object in a list. Total (no partial
-- `maximum`): sort descending by Down, take the head via listToMaybe.
newest :: [ObjectId] -> Maybe ObjectId
newest = Maybe.listToMaybe . List.sortOn Ord.Down

-- Answers the as-enters copy choice with ONE NAMED object, whatever else is
-- legal, and delegates every other prompt to S.identityAnswer. Pinned rather
-- than searched (copyNewest's posture below) so that a mutation cannot be
-- repaired by the answerer finding some other legal source.
--
-- The same function serves the #222 case with an id that is not legal at all --
-- the lying interpreter. legalCopyTargets is the ONLY thing enforcing CR
-- 614.12a's same-batch exclusion, so an unchecked answer would let a Clone copy
-- something it may not.
copyNamed :: ObjectId -> Prompt.Prompt r -> r
copyNamed wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

copyNewest :: Prompt.Prompt r -> r
copyNewest p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> newest legal
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- copyNewest's opposite: decline every as-enters copy choice. The token-copy
-- tests answer with this so that a token which wrongly kept its base card's own
-- `EntryR AsCopy` (Clone's) copies NOTHING and dies as a 0/0, rather than being
-- repaired into the right answer by the answerer.
declineCopy :: Prompt.Prompt r -> r
declineCopy p = case p of
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- Aims a spell's one target slot at ONE PINNED id, whatever else is legal, and
-- orders any trigger batch as it arrives. Pinned rather than searched, `rites`'
-- posture: an answerer that looked for a legal creature would find the other one
-- after a mutation and repair the assertion.
targeting :: ObjectId -> Prompt.Prompt r -> r
targeting victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- `targeting` answered by FILTERING the offered set down to the named permanent
-- instead of building a Recipient: the pool decides which constructor the offer
-- wears, and a hand-built one of another shape is a different recipient that CR
-- 608.2b's re-read at resolution drops silently.
aimByFiltering :: ObjectId -> Prompt.Prompt r -> r
aimByFiltering oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) sets
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- The tokens on the battlefield (CR 111.6), newest first.
tokensOnBattlefield :: GameState.GameState -> [ObjectId]
tokensOnBattlefield gs = List.sortOn Ord.Down (filter (`Game.isToken` gs) (Set.toList (GameState.battlefield gs)))

-- alice casts `spell` (paying from lands already in play) and the stack top
-- resolves, then the board settles. Cast and resolution run under the same
-- answerer, so a prompt either side of the boundary is answered alike.
castAndResolve :: (forall r. Prompt.Prompt r -> r) -> Printing.Printing -> GameState.GameState -> GameState.GameState
castAndResolve answer spell board =
  let (staged, oid) = S.handOne spell board
      afterCast = S.runPure answer staged (S.cast S.alice oid)
   in resolveAndSettle answer afterCast

-- Run the priority loop to exhaustion: every trigger the board has raised
-- resolves, in order, with the state-based actions between them.
resolveAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAll answer gs = snd (Engine.runGamePure answer gs Engine.priorityLoop)

settle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
settle answer gs = snd (Engine.runGamePure answer gs Engine.settleForPriority)

-- Resolve the stack top (a permanent enters -- the copy choice is now made INSIDE
-- that resolution, CR 614.12a) AND run the settle boundary (so a 0/0 Clone with
-- nothing to copy dies to the CR 704.5f state-based action), under the given
-- answerer.
resolveAndSettle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAndSettle answer gs =
  snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- alice, with priority in her own precombat main phase and the rest of the turn
-- ahead, so a priority loop run over the board can offer her an activation.
readiedForAlice :: GameState.GameState -> GameState.GameState
readiedForAlice gs =
  gs
    { GameState.turn =
        (GameState.turn gs)
          { TurnRegisters.priority = Just S.alice,
            TurnRegisters.phase = Phase.PrecombatMain,
            TurnRegisters.activePlayer = S.alice,
            TurnRegisters.remaining = S.phasesAfter Phase.PrecombatMain
          }
    }

-- `aimByFiltering`, plus CR 603.5's "you may" taken. Vesuvan Doppelganger's
-- quoted trigger is both optional and targeted, and declining it would leave the
-- copy it already is.
becomesCopyOf :: ObjectId -> Prompt.Prompt r -> r
becomesCopyOf victim p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> aimByFiltering victim p

-- One upkeep for alice: the step's beginning recorded as the event a trigger
-- condition reads, then the priority loop run to exhaustion so what CR 603.3
-- gathered resolves.
upkeepForAlice :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
upkeepForAlice answer gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          ( gs
              { GameState.turn =
                  (GameState.turn gs)
                    { TurnRegisters.phase = upkeep,
                      TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.remaining = S.phasesAfter upkeep
                    }
              }
          )
   in resolveAll answer (settle answer began)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Copy" $ do
  Spec.it s "Clone copies a creature and projects its P/T (CR 707.2)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addPermanent piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId -> do
        Spec.assertEqWith s "Clone's power is the Piker's" (Projection.powerOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "Clone's toughness is the Piker's" (Projection.toughnessOf cloneId resolved) $ Just 1
        Spec.assertBool s (Projection.isCreatureOf cloneId resolved) "Clone is a creature"
        Spec.assertBool s (Projection.powerOf pikerId resolved == Just 2) "the copied Piker is untouched"

  Spec.it s "Clone with no creature to copy enters as a 0/0 and dies (CR 704.5f)" $ do
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, staged) = S.spellOnStack clone S.alice gs0
        resolved = resolveAndSettle copyNewest staged
    Spec.assertEqWith s "the 0/0 Clone is gone (state-based action)" (cloneOnBattlefield resolved) Nothing

  -- #222: an interpreter naming an id that was never offered must be refused --
  -- the Clone enters as a 0/0 and dies exactly as it does when it declines.
  --
  -- The Piker is on the board so the prompt is REALLY RAISED and really answered
  -- with the phantom; on an empty board there would be nothing eligible, the
  -- prompt would be skipped (#1512's elision), and this test would pass without
  -- the refusal ever running. The board is the "Clone copies a creature" board
  -- above, so the only variable is the answer.
  Spec.it s "#222 a copy target that was never offered is refused" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addPermanent piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        phantom = ObjectId.MkObjectId 9999
        resolved = resolveAndSettle (copyNamed phantom) staged
    Spec.assertEqWith s "the Clone copied nothing and died as a 0/0" (cloneOnBattlefield resolved) Nothing

  -- THE PROVING TEST for #1512: the eligible set is the CARD's noun phrase, not
  -- "any creature". Copy Enchantment reads "you may have this enchantment enter
  -- as a copy of any enchantment on the battlefield", so on a board carrying
  -- BOTH a creature and an enchantment the two halves must come apart -- and
  -- under the hardcoded creature set they could not, since the enchantment was
  -- not offered at all and the creature was.
  --
  -- One board, two pinned answers. The answers are pinned rather than searched
  -- so that widening the filter back to creatures cannot be repaired by an
  -- answerer finding the enchantment anyway.
  Spec.it s "Copy Enchantment copies an ENCHANTMENT the creature filter would not offer (CR 707.5)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    scales <- S.printingOf s registry "Hardened Scales"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withPiker) = S.addPermanent piker S.alice gs0
        (scalesId, board) = S.addPermanent scales S.alice withPiker
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        resolved = resolveAndSettle (copyNamed scalesId) staged
    case newest (printedOnBattlefield "Copy Enchantment" resolved) of
      Nothing -> Spec.assertFailure s "Copy Enchantment left the battlefield unexpectedly"
      Just copyId -> do
        Spec.assertEqWith s "it is the Scales" (Projection.namesOf copyId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Hardened Scales"
        Spec.assertBool s (not (Projection.isCreatureOf copyId resolved)) "and did not become a creature"

  -- The negative half, on the SAME board with the SAME mana and the SAME stock:
  -- only the pinned answer differs. The Piker is a legal copy target for a Clone
  -- and is not one for a Copy Enchantment, so the filtered-not-trusted check
  -- refuses it and the enchantment enters as its printed self.
  Spec.it s "Copy Enchantment refuses the creature on that same board (CR 707.5)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    scales <- S.printingOf s registry "Hardened Scales"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, withPiker) = S.addPermanent piker S.alice gs0
        (_, board) = S.addPermanent scales S.alice withPiker
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        resolved = resolveAndSettle (copyNamed pikerId) staged
    case newest (printedOnBattlefield "Copy Enchantment" resolved) of
      Nothing -> Spec.assertFailure s "Copy Enchantment left the battlefield unexpectedly"
      Just copyId -> do
        Spec.assertEqWith s "it stayed itself" (Projection.namesOf copyId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Copy Enchantment"
        Spec.assertBool s (not (Projection.isCreatureOf copyId resolved)) "it is not the Piker"

  -- The elision side of the invariant, which narrowing the eligible set is what
  -- makes reachable: with nothing eligible, declining is the only legal answer,
  -- so the prompt is not raised. A pair of boards differing in exactly one thing
  -- -- whether a second enchantment is on the battlefield -- since a board with
  -- no enchantment at all would also have no creature to tell "not asked" from
  -- "asked about nothing".
  Spec.it s "CR 707.5: a copy choice with nothing eligible is not asked" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    scales <- S.printingOf s registry "Hardened Scales"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseCopyTarget {} -> do
            State.modify' (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (copyNewest p)
        asks board =
          let (_, staged) = S.spellOnStack copyEnchantment S.alice board
           in State.execState (Engine.runGame countingAnswer staged (Stack.resolveTop >> Engine.settleForPriority)) 0
        gs0 = Setup.emptyGame S.bothPlayers
        (_, withPiker) = S.addPermanent piker S.alice gs0
        (_, withScales) = S.addPermanent scales S.alice withPiker
    Spec.assertEqWith s "a creature but no enchantment: nothing to ask" (asks withPiker) 0
    Spec.assertEqWith s "an enchantment beside it: one real decision" (asks withScales) 1

  -- CR 303.4f / 614.12a: Copy Enchantment that copies an Aura has its host
  -- chosen as it enters, after the copy choice -- the 2023-09-01 ruling's
  -- "you choose what the Aura will enchant just before it enters". TWO legal
  -- hosts, and the answer pinned to the SECOND, so an engine that took the first
  -- candidate (or entered unattached for CR 704.5m to bury) fails the Mammoth's
  -- P/T.
  Spec.it s "CR 303.4f Copy Enchantment copying Unholy Strength enchants the creature its controller chooses" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mammoth <- S.printingOf s registry "War Mammoth"
    unholy <- S.printingOf s registry "Unholy Strength"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, withPiker) = S.addPermanent piker S.alice gs0
        (mammothId, withMammoth) = S.addPermanent mammoth S.alice withPiker
        (unholyId, withUnholy) = S.addPermanent unholy S.alice withMammoth
        board = S.attach unholyId pikerId withUnholy
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseAttachment {} -> mammothId
          _ -> copyNamed unholyId p
        resolved = resolveAndSettle answer staged
    Spec.assertEqWith s "the War Mammoth gets +2/+1" (S.powerToughnessOf mammothId resolved) (Just (5, 4))
    Spec.assertEqWith s "and the Piker keeps only the original's +2/+1" (S.powerToughnessOf pikerId resolved) (Just (4, 2))
    Spec.assertEqWith
      s
      "the copy is on the battlefield, attached to the Mammoth"
      (fmap (\oid -> Game.lookupObject oid resolved >>= Object.attachedTo >>= Recipient.objectOf) (printedOnBattlefield "Copy Enchantment" resolved))
      [Just mammothId]

  -- CR 303.4f's "or player" (CR 702.5d) at the same door: Copy Enchantment
  -- copying bob's Curse of Death's Hold, which enchants carol, and alice
  -- choosing bob -- the SECOND of three candidates, so neither the first seat
  -- nor the original's host can stand in for him.
  Spec.it s "CR 303.4f Copy Enchantment copying a Curse enchants the player its controller chooses" $ do
    mammoth <- S.printingOf s registry "War Mammoth"
    maiden <- S.printingOf s registry "Bird Maiden"
    curse <- S.printingOf s registry "Curse of Death's Hold"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let (mammothId, withMammoth) = S.addPermanent mammoth S.bob S.threePlayerGame
        (maidenId, withMaiden) = S.addPermanent maiden S.carol withMammoth
        (curseId, withCurse) = S.addPermanent curse S.bob withMaiden
        board = S.attachTo curseId (Recipient.ToPlayer S.carol) withCurse
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChoosePlayer _ _ _ offered -> case NonEmpty.toList offered of
            _ : second : _ -> second
            _ -> NonEmpty.head offered
          _ -> copyNamed curseId p
        resolved = resolveAndSettle answer staged
    Spec.assertEqWith s "bob's War Mammoth gets -1/-1 from the copy" (S.powerToughnessOf mammothId resolved) (Just (2, 2))
    Spec.assertEqWith s "and carol's Bird Maiden only the original's" (S.powerToughnessOf maidenId resolved) (Just (0, 1))

  -- CR 303.4g's stack branch, as a pair differing in ONE thing: whether bob
  -- controls a creature. Bob's Betrayal ("enchant creature an opponent
  -- controls") is on alice's Balemurk Leech, so a copy alice controls can
  -- enchant only a creature of bob's. With none, the copy "is put into its
  -- owner's graveyard instead of entering the battlefield": the Leech's
  -- enchantment-enters trigger never fires and bob keeps his life. With one, the
  -- copy enters attached to it and the Leech drains bob.
  let betrayalBoard withBobCreature = do
        leech <- S.printingOf s registry "Balemurk Leech"
        betrayal <- S.printingOf s registry "Betrayal"
        piker <- S.printingOf s registry "Goblin Piker"
        copyEnchantment <- S.printingOf s registry "Copy Enchantment"
        let gs0 = Setup.emptyGame S.bothPlayers
            (leechId, withLeech) = S.addPermanent leech S.alice gs0
            (betrayalId, withBetrayal) = S.addPermanent betrayal S.bob withLeech
            attached = S.attach betrayalId leechId withBetrayal
            (bobPikerId, board) =
              if withBobCreature
                then let (pikerId, withPiker) = S.addPermanent piker S.bob attached in (Just pikerId, withPiker)
                else (Nothing, attached)
            (spellId, staged) = S.spellOnStack copyEnchantment S.alice board
            once = resolveAndSettle (copyNamed betrayalId) staged
            drained = if null (TurnRegisters.stack (GameState.turn once)) then once else resolveAndSettle (copyNamed betrayalId) once
        pure (bobPikerId, spellId, staged, drained)
  Spec.it s "CR 303.4g Copy Enchantment copying Betrayal with nothing to enchant goes to the graveyard without entering" $ do
    (_, spellId, staged, after) <- betrayalBoard False
    Spec.assertEqWith s "the Leech never saw an enchantment enter: bob keeps his life" (S.lifeOf S.bob after) (S.lifeOf S.bob staged)
    Spec.assertEqWith s "Copy Enchantment is in alice's graveyard" (fmap (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.alice after)) [Just (CardName.MkCardName (Text.pack "Copy Enchantment"))]
    Spec.assertEqWith s "and the spell is gone from the stack" (List.elem spellId (TurnRegisters.stack (GameState.turn after))) False
  Spec.it s "CR 303.4f Copy Enchantment copying Betrayal over a creature of bob's enters on it and the Leech drains" $ do
    (bobPikerId, _, staged, after) <- betrayalBoard True
    Spec.assertEqWith s "the Leech saw an enchantment enter: bob loses 1" (S.lifeOf S.bob after) (fmap (subtract 1) (S.lifeOf S.bob staged))
    Spec.assertEqWith
      s
      "the copy enchants bob's Piker"
      (fmap (\oid -> Game.lookupObject oid after >>= Object.attachedTo >>= Recipient.objectOf) (printedOnBattlefield "Copy Enchantment" after))
      [bobPikerId]

  Spec.it s "Clone copies base P/T, not a counter-boosted P/T (CR 707.2 falsifier)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board0) = S.addPermanent piker S.alice gs0
        -- Put a +1/+1 counter on the Piker: projected 3/2, base 2/1.
        board = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId board0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId -> do
        Spec.assertEqWith s "source is boosted to 3/2" (Projection.powerOf pikerId resolved) $ Just 3
        Spec.assertEqWith s "Clone copies the base 2, not 3" (Projection.powerOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "Clone copies the base 1, not 2" (Projection.toughnessOf cloneId resolved) $ Just 1

  Spec.it s "Clone copies a creature's activated abilities (CR 707.2)" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addPermanent prodigalSorcerer S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId ->
        Spec.assertBool
          s
          (not (null (Projection.abilitiesOf cloneId resolved)))
          "Clone has the copied activated ability"

  Spec.it s "a copy of a copy resolves to the underlying creature (self-reference)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addPermanent piker S.alice gs0
        (_, stagedA) = S.spellOnStack clone S.alice board
        afterA = resolveAndSettle copyNewest stagedA
        (_, stagedB) = S.spellOnStack clone S.alice afterA
        afterB = resolveAndSettle copyNewest stagedB
        -- Both Clones now name "Clone"; the newest (highest id) is B.
        afterBId = newest (clonesOnBattlefield afterB)
    case afterBId of
      Nothing -> Spec.assertFailure s "no Clones on the battlefield"
      Just bId -> do
        Spec.assertEqWith s "the copy-of-a-copy is a 2/1" (Projection.powerOf bId afterB) $ Just 2
        Spec.assertBool s (Projection.isCreatureOf bId afterB) "the copy-of-a-copy is a creature"

  Spec.it s "a copy survives its source leaving the battlefield (CR 707.5 lock)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addPermanent piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
        afterKill = S.runPure S.identityAnswer resolved (Event.destroy (pure ()) Regenerability.Regenerable [pikerId])
    case cloneOnBattlefield afterKill of
      Nothing -> Spec.assertFailure s "Clone should survive the source's death"
      Just cloneId -> do
        Spec.assertEqWith s "the source is gone" (Set.member pikerId (GameState.battlefield afterKill)) False
        Spec.assertEqWith s "the Clone is still a 2/1" (Projection.powerOf cloneId afterKill) $ Just 2
        Spec.assertEqWith s "the Clone is still 1 toughness" (Projection.toughnessOf cloneId afterKill) $ Just 1

  Spec.it s "Clone of Tarmogoyf copies the ABILITY, so both recompute (CR 707.2a)" $ do
    -- THE FALSIFIER for snapshotting the NUMBER: CR 707.2a says a copy
    -- acquires the abilities of the object it copies, because those values are
    -- derived from its rules text. Seeding the CDA as an evaluated integer
    -- would freeze the Clone at the graveyards' contents at the moment it
    -- entered -- P2's deferred bill, paid here.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board) = S.addPermanent tarmogoyf S.alice withBolt
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
        -- A second card type reaches a graveyard AFTER the Clone entered.
        (_, later) = S.addGraveyardCard piker S.bob resolved
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone did not reach the battlefield"
      Just cloneId -> do
        Spec.assertEqWith s "at entry the Clone is the Goyf's 1/2" (Projection.powerOf cloneId resolved) $ Just 1
        Spec.assertEqWith s "at entry, toughness 1+1" (Projection.toughnessOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "the source moves to 2" (Projection.powerOf goyfId later) $ Just 2
        Spec.assertEqWith s "and so does the COPY" (Projection.powerOf cloneId later) $ Just 2
        Spec.assertEqWith s "the copy's toughness moves too" (Projection.toughnessOf cloneId later) $ Just 3

  -- THE PROVING TEST for CR 707.9's exceptions. Quicksilver Gargantuan is CR
  -- 707.9d's own worked example: "except it's 7/7".
  --
  -- Three readings of the same Tarmogoyf on one board, and all three differ. The
  -- ORIGINAL carries a +1/+1 counter, so it projects one above its CDA; a Clone
  -- is the copy WITHOUT the exception, so it recomputes the CDA (CR 707.2a) at
  -- the counter-free value; the Gargantuan is the copy WITH it. Both copies are
  -- pinned to the Goyf rather than to each other, so neither reading can borrow
  -- the other's.
  Spec.it s "Quicksilver Gargantuan copies a Tarmogoyf but is 7/7 (CR 707.9b, CR 707.9d)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    clone <- S.printingOf s registry "Clone"
    gargantuan <- S.printingOf s registry "Quicksilver Gargantuan"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        -- One card type in a graveyard: the Goyf's CDA is 1/2.
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board0) = S.addPermanent tarmogoyf S.alice withBolt
        board = S.addCounter CounterKind.PlusOnePlusOne 1 goyfId board0
        (_, stagedClone) = S.spellOnStack clone S.alice board
        withClone = resolveAndSettle (copyNamed goyfId) stagedClone
        (_, stagedGargantuan) = S.spellOnStack gargantuan S.alice withClone
        resolved = resolveAndSettle (copyNamed goyfId) stagedGargantuan
        -- A second card type reaches a graveyard AFTER both copies entered.
        (_, later) = S.addGraveyardCard piker S.bob resolved
    case (cloneOnBattlefield resolved, newest (printedOnBattlefield "Quicksilver Gargantuan" resolved)) of
      (Just cloneId, Just gargantuanId) -> do
        Spec.assertEqWith s "the original is its CDA plus the counter" (S.powerToughnessOf goyfId resolved) $ Just (2, 3)
        Spec.assertEqWith s "the copy without the exception is the bare CDA" (S.powerToughnessOf cloneId resolved) $ Just (1, 2)
        Spec.assertEqWith s "the copy with it is 7/7" (S.powerToughnessOf gargantuanId resolved) $ Just (7, 7)
        -- CR 707.2 still ran: only P/T is excepted.
        Spec.assertEqWith s "and is otherwise the Goyf" (Projection.namesOf gargantuanId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Tarmogoyf"
        Spec.assertBool s (Set.member Subtype.Lhurgoyf (PC.subtypes (Projection.project gargantuanId resolved))) "the Gargantuan copied the Goyf's subtype"
        -- CR 707.9d: the CDA defining the excepted characteristic was not copied,
        -- so the Gargantuan alone does not move when the graveyards do.
        Spec.assertEqWith s "the original moves with the graveyards" (S.powerToughnessOf goyfId later) $ Just (3, 4)
        Spec.assertEqWith s "so does the copy that took the CDA" (S.powerToughnessOf cloneId later) $ Just (2, 3)
        Spec.assertEqWith s "the excepted copy does not" (S.powerToughnessOf gargantuanId later) $ Just (7, 7)
      _ -> Spec.assertFailure s "the Clone and the Gargantuan should both be on the battlefield"

  -- THE PROVING TEST for CR 707.9b's OTHER arm, the exception that adds a CARD
  -- TYPE. Phyrexian Metamorph {3}{U/P} Artifact Creature -- Phyrexian
  -- Shapeshifter 0/0: "You may have this creature enter as a copy of any
  -- artifact or creature on the battlefield, except it's an artifact in addition
  -- to its other types."
  --
  -- Read at GAMEPLAY level by a sweeper that asks the type rather than by a
  -- projection field: Bane of Progress destroys every artifact and enchantment as
  -- it enters, so the excepted copy of a Goblin Piker is destroyed and a CLONE of
  -- the SAME Piker -- the copy without the exception, entering the same way -- is
  -- not. The Piker itself is the third reading and survives too.
  --
  -- The Metamorph's PRINTED type line is artifact creature, which is exactly what
  -- CR 707.2 replaced when it copied the Piker: without the exception the copy is
  -- a plain creature. So "it is still an artifact" is the exception's doing and
  -- not the printed card's, and the name assertion ahead of the sweep is what
  -- pins that the copy happened at all.
  --
  -- The Bane's own +1/+1 counter is a diagnostic after the behaviour: it takes
  -- one per permanent destroyed, so 3/3 says EXACTLY ONE artifact was there and
  -- the sweep did not also take the Clone.
  Spec.it s "Phyrexian Metamorph's copy is an artifact and a sweeper takes it (CR 707.9b)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    metamorph <- S.printingOf s registry "Phyrexian Metamorph"
    bane <- S.printingOf s registry "Bane of Progress"
    let (pikerId, board) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        (_, stagedClone) = S.spellOnStack clone S.alice board
        withClone = resolveAndSettle (copyNamed pikerId) stagedClone
        (_, stagedMetamorph) = S.spellOnStack metamorph S.alice withClone
        entered = resolveAndSettle (copyNamed pikerId) stagedMetamorph
    case (cloneOnBattlefield entered, newest (printedOnBattlefield "Phyrexian Metamorph" entered)) of
      (Just cloneId, Just metamorphId) -> do
        let (baneId, staged) = S.entersWithTrigger bane S.alice entered
            -- The settle puts the Bane's CR 603.6a trigger on the stack; the
            -- resolve runs it.
            onStack = settle (copyNamed pikerId) staged
            swept = resolveAndSettle (copyNamed pikerId) onStack
        Spec.assertBool s (not (null (TurnRegisters.stack (GameState.turn onStack)))) "the Bane's trigger really was on the stack"
        -- CR 707.2 ran: the exception modified the copy, it did not replace it.
        Spec.assertEqWith s "the Metamorph is the Piker by name (CR 707.2)" (Projection.namesOf metamorphId entered) . Set.singleton . CardName.MkCardName $ Text.pack "Goblin Piker"
        Spec.assertEqWith s "and has the Piker's 2/1" (S.powerToughnessOf metamorphId entered) $ Just (2, 1)
        -- THE GAMEPLAY ASSERTION: the sweeper's type question found an artifact.
        Spec.assertBool s (not (onBattlefield metamorphId swept)) "the excepted copy was destroyed as an artifact (CR 707.9b)"
        -- The two controls, on the same board and copied from the same Piker.
        Spec.assertBool s (onBattlefield cloneId swept) "the copy without the exception is no artifact and survives"
        Spec.assertBool s (onBattlefield pikerId swept) "and the Piker they both copied is no artifact either"
        -- Diagnostics, after the behaviour: the copied type line kept its creature
        -- half (CR 205.1b's "in addition"), and exactly one permanent was swept.
        Spec.assertEqWith s "the copy is an artifact creature, not an artifact instead" (PC.cardTypes (Projection.project metamorphId entered)) (Set.fromList [CardType.Artifact, CardType.Creature])
        Spec.assertEqWith s "the Bane counted exactly one destroyed permanent" (S.powerToughnessOf baneId swept) $ Just (3, 3)
      _ -> Spec.assertFailure s "the Clone and the Metamorph should both be on the battlefield"

  -- THE PROVING TEST for CR 707.9d's CARVE-OUT: its strip of the copied object's
  -- characteristic-defining ability "does not apply to copy effects with
  -- exceptions that state the object is a certain card type, supertype, and/or
  -- subtype 'in addition to its other types'".
  --
  -- Two copies of ONE Tarmogoyf on one board, differing only in which CR 707.9b
  -- arm their card writes. Quicksilver Gargantuan's "except it's 7/7" PROVIDES
  -- values, so CR 707.9d takes the Goyf's CDA and the Gargantuan is deaf to the
  -- graveyards; the Metamorph's "in addition to its other types" is the
  -- carve-out's own wording, so the CDA came across and its copy moves when a
  -- second card type reaches a graveyard.
  --
  -- The graveyards move AFTER both copies entered, which is what makes the CDA
  -- claim bite: at entry both readings of the Metamorph agree at 1/2, and only
  -- the later board tells "kept the ability" from "kept the number".
  Spec.it s "a type exception keeps the copied CDA where a value exception does not (CR 707.9d)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    gargantuan <- S.printingOf s registry "Quicksilver Gargantuan"
    metamorph <- S.printingOf s registry "Phyrexian Metamorph"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        -- One card type in a graveyard: the Goyf's CDA is 1/2.
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board) = S.addPermanent tarmogoyf S.alice withBolt
        (_, stagedGargantuan) = S.spellOnStack gargantuan S.alice board
        withGargantuan = resolveAndSettle (copyNamed goyfId) stagedGargantuan
        (_, stagedMetamorph) = S.spellOnStack metamorph S.alice withGargantuan
        entered = resolveAndSettle (copyNamed goyfId) stagedMetamorph
        -- A second card type reaches a graveyard AFTER both copies entered.
        (_, later) = S.addGraveyardCard piker S.bob entered
        gargantuanId = newest (printedOnBattlefield "Quicksilver Gargantuan" entered)
        metamorphId = newest (printedOnBattlefield "Phyrexian Metamorph" entered)
        -- Read through the Maybe rather than behind a `case` guard, and that is
        -- the mutation talking: the wrong reading of CR 707.9d strips the copied
        -- CDA, which leaves the copy with NO power or toughness at all and a
        -- state-based action takes it off the battlefield (CR 704.5f). A guard
        -- would have absorbed that
        -- into "should be on the battlefield" and the gameplay assertion below
        -- would never have run.
        ptOf oid gs = oid >>= \o -> S.powerToughnessOf o gs
        projected f oid gs = fmap (\o -> f (Projection.project o gs)) oid
    -- THE GAMEPLAY ASSERTION, ahead of every reading taken at entry: the
    -- type-excepted copy is still there and still recomputes the Goyf's CDA.
    Spec.assertEqWith s "the type-excepted copy moves with the graveyards (CR 707.9d)" (ptOf metamorphId later) $ Just (2, 3)
    Spec.assertEqWith s "the original moves with them too" (S.powerToughnessOf goyfId later) $ Just (2, 3)
    -- The control, on the same board and off the same Goyf: the value exception
    -- DID strip the ability, which is CR 707.9d's main sentence.
    Spec.assertEqWith s "the value-excepted copy does not" (ptOf gargantuanId later) $ Just (7, 7)
    -- Diagnostics, after the behaviour: both copies are the Goyf, and only the
    -- Metamorph's gained the card type its card names.
    Spec.assertEqWith s "at entry the type-excepted copy is the bare CDA" (ptOf metamorphId entered) $ Just (1, 2)
    Spec.assertEqWith s "the type-excepted copy is the Goyf by name (CR 707.2)" (fmap (\o -> Projection.namesOf o entered) metamorphId) . Just . Set.singleton . CardName.MkCardName $ Text.pack "Tarmogoyf"
    Spec.assertEqWith s "and an artifact creature" (projected PC.cardTypes metamorphId entered) . Just $ Set.fromList [CardType.Artifact, CardType.Creature]
    Spec.assertEqWith s "where the value-excepted copy is a creature alone" (projected PC.cardTypes gargantuanId entered) . Just $ Set.singleton CardType.Creature

  -- THE PROVING TEST for CR 707.9c, CopyException.DontCopyColors. Vesuvan
  -- Doppelganger {3}{U}{U} Creature -- Shapeshifter 0/0: "You may have this
  -- creature enter as a copy of any creature on the battlefield, except it
  -- doesn't copy that creature's color and it has \"At the beginning of your
  -- upkeep, you may have this creature become a copy of target creature, except
  -- it doesn't copy that creature's color and it has this ability.\"" (Oracle
  -- text checked against api.scryfall.com, 2026-09-12. Scryfall o:"doesn't copy",
  -- 2026-09-12, returns this card and nothing else.)
  --
  -- Doom Blade is the gameplay reader for colour, as in the eternalize scenario:
  -- "Destroy target nonblack creature" is castable only while some creature on
  -- the board is not black. The Doppelganger copies a black Cabal Evangel (2/2),
  -- so the Blade has a target exactly when the copy kept its printed blue.
  --
  -- The control differs in ONE thing: a Clone -- also a blue 0/0 Shapeshifter,
  -- also entering as a copy of that same Evangel, off the same board with the
  -- same two Swamps -- copies the colour, so nothing nonblack stands.
  --
  -- A Clone of the DOPPELGANGER'S COPY is the copiable-values tripwire (CR 707.2
  -- / 707.9c): the retained blue is part of the copy's own copiable values, so a
  -- Clone reads it, where a CR 613 layer-5 write would be left behind.
  Spec.it s "CR 707.9c Vesuvan Doppelganger's copy keeps its own colour, and a Clone of that copy keeps it too" $ do
    swamp <- S.printingOf s registry "Swamp"
    evangel <- S.printingOf s registry "Cabal Evangel"
    vesuvan <- S.printingOf s registry "Vesuvan Doppelganger"
    clone <- S.printingOf s registry "Clone"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (evangelId, board0) = S.addPermanent evangel S.alice (readiedForAlice (S.landsFor swamp S.alice 2 (Setup.emptyGame S.bothPlayers)))
        entering victim printing gs = resolveAndSettle (copyNamed victim) (snd (S.spellOnStack printing S.alice gs))
        entered = entering evangelId vesuvan board0
        control = entering evangelId clone board0
    case (printedOnBattlefield "Vesuvan Doppelganger" entered, clonesOnBattlefield control) of
      ([vesuvanId], [controlId]) -> do
        let cloned = entering vesuvanId clone entered
            (withBlade, bladeId) = S.handOne doomBlade entered
            (controlBlade, controlBladeId) = S.handOne doomBlade control
        -- THE GAMEPLAY ASSERTIONS, ahead of every characteristic read.
        Spec.assertBool s (doomBladeOffered bladeId withBlade) "CR 707.9c the copy did not copy the Evangel's black, so Doom Blade has a target"
        Spec.assertBool s (not (doomBladeOffered controlBladeId controlBlade)) "where a Clone of the same Evangel is black and the Blade has none"
        case clonesOnBattlefield cloned of
          [cloneId] -> Spec.assertEqWith s "CR 707.2 a Clone of the copy reads the retained blue out of its copiable values" (Projection.colorsOf cloneId cloned) (Set.singleton Color.Blue)
          _ -> Spec.assertFailure s "expected one Clone of the Doppelganger's copy"
        -- Diagnostics, after the behaviour: the copy really was the Evangel, and
        -- the control really copied its colour.
        Spec.assertEqWith s "the Doppelganger entered as the Evangel's 2/2" (S.powerToughnessOf vesuvanId entered) (Just (2, 2))
        Spec.assertEqWith s "and it is blue, not the Evangel's black" (Projection.colorsOf vesuvanId entered) (Set.singleton Color.Blue)
        Spec.assertEqWith s "where the Clone without the exception is black" (Projection.colorsOf controlId control) (Set.singleton Color.Black)
      _ -> Spec.assertFailure s "expected one Doppelganger and one Clone, one per board"

  -- CR 707.9c on the OTHER road: the same clause is written inside the ability
  -- the entry exception quotes, so the copy the CR 707.4 BecomeCopy opcode makes
  -- must retain the colour too (Parun.Engine.Resolve.Arms.Objects's BecomeCopy arm,
  -- which reads the subject's own copiable values per subject).
  --
  -- The Doppelganger enters as the black Cabal Evangel (2/2), then its upkeep
  -- trigger copies a black Bog Wraith (3/3): every creature on the board is
  -- black, so Doom Blade has a target only if the SECOND copy retained the blue
  -- as well. Three distinct printed pairs -- 0/0, 2/2, 3/3 -- so the P/T read
  -- below names which copy happened rather than a coincidence.
  Spec.it s "CR 707.9c the ability the copy kept copies again, and that copy keeps the colour too" $ do
    swamp <- S.printingOf s registry "Swamp"
    evangel <- S.printingOf s registry "Cabal Evangel"
    wraith <- S.printingOf s registry "Bog Wraith"
    vesuvan <- S.printingOf s registry "Vesuvan Doppelganger"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (evangelId, board0) = S.addPermanent evangel S.alice (readiedForAlice (S.landsFor swamp S.alice 2 (Setup.emptyGame S.bothPlayers)))
        (wraithId, board1) = S.addPermanent wraith S.alice board0
        entered = resolveAndSettle (copyNamed evangelId) (snd (S.spellOnStack vesuvan S.alice board1))
    case printedOnBattlefield "Vesuvan Doppelganger" entered of
      [vesuvanId] -> do
        let after = readiedForAlice (upkeepForAlice (becomesCopyOf wraithId) entered)
            (withBlade, bladeId) = S.handOne doomBlade after
        -- THE GAMEPLAY ASSERTION, ahead of every characteristic read.
        Spec.assertBool s (doomBladeOffered bladeId withBlade) "CR 707.9c the second copy did not copy the Wraith's black either, so Doom Blade has a target"
        -- Diagnostics, after the behaviour: the second copy really happened, and
        -- the two creatures it was made from really are black.
        Spec.assertEqWith s "CR 707.4 the Doppelganger is the Wraith's 3/3 now" (S.powerToughnessOf vesuvanId after) (Just (3, 3))
        Spec.assertEqWith s "and still blue" (Projection.colorsOf vesuvanId after) (Set.singleton Color.Blue)
        Spec.assertEqWith s "where the Wraith it copied is black" (Projection.colorsOf wraithId after) (Set.singleton Color.Black)
        Spec.assertEqWith s "and so is the Evangel it copied first" (Projection.colorsOf evangelId after) (Set.singleton Color.Black)
      others -> Spec.assertFailure s ("expected exactly one Doppelganger, got " <> show (length others))

  -- Watchful Radstag {2}{G} 2/2 Elk Mutant: evolve, plus "whenever this creature
  -- evolves, create a token that's a copy of it". The copied permanent is the
  -- reserved self slot rather than a target, which is the whole reason this card
  -- reaches CR 608.2h where Cackling Counterpart cannot -- a gone target fizzles
  -- the spell first (CR 608.2b).
  --
  -- Hill Giant 3/3 is the entrant, beating the 2/2 on both axes so the Radstag
  -- evolves. It then carries a +1/+1 counter for the rest of both tests, which is
  -- what makes a token minted off the PROJECTION a 3/3 and CR 707.2's exclusion
  -- of counters observable.
  Spec.it s "Watchful Radstag mints a token copy of itself when it evolves (CR 702.100b, CR 707.2)" $ do
    radstag <- S.printingOf s registry "Watchful Radstag"
    giant <- S.printingOf s registry "Hill Giant"
    let (radstagId, board) = S.addPermanent radstag S.alice (Setup.emptyGame S.bothPlayers)
        (_, entered) = S.entersWithTrigger giant S.alice board
        after = resolveAll declineCopy (settle declineCopy entered)
    Spec.assertEqWith s "the Radstag evolved, so it is a 3/3" (S.powerToughnessOf radstagId after) $ Just (3, 3)
    case tokensOnBattlefield after of
      [tokenId] -> do
        Spec.assertEqWith s "the token is a Radstag" (Projection.namesOf tokenId after) . Set.singleton . CardName.MkCardName $ Text.pack "Watchful Radstag"
        Spec.assertEqWith s "and a 2/2, not the counter-boosted 3/3" (S.powerToughnessOf tokenId after) $ Just (2, 2)
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- THE PROVING TEST for #1183, CR 608.2h. Same board, with the Radstag killed
  -- while its own trigger is on the stack: a -5/-5 takes the evolved 3/3 to
  -- -2/-2 and CR 704.5f buries it. The token is still created, and is the
  -- Radstag's COPIABLE values -- so a fallback onto the last known PROJECTION
  -- would mint a -2/-2 that dies at once and leave no token at all.
  Spec.it s "a Radstag killed in response still mints its token copy (CR 608.2h)" $ do
    radstag <- S.printingOf s registry "Watchful Radstag"
    giant <- S.printingOf s registry "Hill Giant"
    let (radstagId, board) = S.addPermanent radstag S.alice (Setup.emptyGame S.bothPlayers)
        (_, entered) = S.entersWithTrigger giant S.alice board
        -- The evolve ability resolves; the settle that follows puts the
        -- Radstag's own "whenever this creature evolves" on the stack.
        onStack = resolveAndSettle declineCopy (settle declineCopy entered)
        shrunk = S.withEffect radstagId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal (-5)) (Quantity.Type.Literal (-5)))) onStack
        dead = settle declineCopy shrunk
        after = resolveAll declineCopy dead
    Spec.assertBool s (not (null (TurnRegisters.stack (GameState.turn onStack)))) "the Radstag's trigger really was on the stack"
    Spec.assertEqWith s "and the Radstag is gone before it resolves" (Set.member radstagId (GameState.battlefield dead)) False
    case tokensOnBattlefield after of
      [tokenId] -> do
        Spec.assertEqWith s "the token is a Radstag all the same" (Projection.namesOf tokenId after) . Set.singleton . CardName.MkCardName $ Text.pack "Watchful Radstag"
        Spec.assertEqWith s "at its copiable 2/2, not the -2/-2 it died at" (S.powerToughnessOf tokenId after) $ Just (2, 2)
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- THE PROVING TEST for CR 707.9a's second shape, the exception that names an
  -- ability rather than quoting one: Unstable Shapeshifter {3}{U} Creature --
  -- Shapeshifter 0/1, "Whenever another creature enters, this creature becomes a
  -- copy of that creature, except it has this ability."
  --
  -- Read at GAMEPLAY level over TWO entries. The first copy is what every board
  -- above already shows; the SECOND is the one only the exception can produce,
  -- since a Shapeshifter that took the Hill Giant's abilities and only those has
  -- nothing left to trigger. So 4/3 at the end is "the kept ability fired on the
  -- copy" and 3/3 is "it did not".
  --
  -- A Clone copying the Piker is the control, and it is the copy made WITHOUT the
  -- exception: it entered before the Shapeshifter was placed, so nothing it did
  -- is what stopped it copying again -- it simply never had the ability. It stays
  -- a 2/1 through both entries.
  --
  -- Three distinct printed pairs, so no reading is reached by a coincidence: the
  -- Piker's 2/1, the Hill Giant's 3/3, the Blind-Spot Giant's 4/3.
  Spec.it s "Unstable Shapeshifter keeps the ability that copied and copies again (CR 707.9a)" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    piker <- S.printingOf s registry "Goblin Piker"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    clone <- S.printingOf s registry "Clone"
    let (pikerId, board0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (_, stagedClone) = S.spellOnStack clone S.alice board0
        withClone = resolveAndSettle (copyNamed pikerId) stagedClone
        -- The Shapeshifter arrives AFTER the Clone, so the Clone's own entry is
        -- not what it copies and the two copies below are made by one rule each.
        (shifterId, withShifter) = S.addPermanent shapeshifter S.alice withClone
        (_, firstEntry) = S.entersWithTrigger hillGiant S.alice withShifter
        afterFirst = resolveAndSettle (targeting shifterId) (settle (targeting shifterId) firstEntry)
        (_, secondEntry) = S.entersWithTrigger blindSpotGiant S.alice afterFirst
        afterSecond = resolveAndSettle (targeting shifterId) (settle (targeting shifterId) secondEntry)
    case cloneOnBattlefield withClone of
      Nothing -> Spec.assertFailure s "the Clone should be on the battlefield"
      Just cloneId -> do
        -- THE GAMEPLAY ASSERTION, ahead of every diagnostic: the second creature
        -- to enter is what the Shapeshifter is now a copy of, which only the
        -- ability the exception kept can have done.
        Spec.assertEqWith s "the Shapeshifter copied a second creature (CR 707.9a)" (S.powerToughnessOf shifterId afterSecond) $ Just (4, 3)
        Spec.assertEqWith s "and it is the second creature by name (CR 707.2)" (Projection.namesOf shifterId afterSecond) . Set.singleton . CardName.MkCardName $ Text.pack "Blind-Spot Giant"
        -- The control, on the same board: the copy made without the exception
        -- never gains the ability, so neither entry moves it.
        Spec.assertEqWith s "the copy without the exception is still the Piker's 2/1" (S.powerToughnessOf cloneId afterSecond) $ Just (2, 1)
        Spec.assertEqWith s "and it has no triggered ability to have copied with" (length (Projection.triggeredAbilitiesOf cloneId afterSecond)) 0
        Spec.assertEqWith s "and the Piker it copied is untouched" (S.powerToughnessOf pikerId afterSecond) $ Just (2, 1)
        -- Diagnostics, after the behaviour: the first copy really happened, and
        -- the kept ability is on the copy rather than on the printed card.
        Spec.assertEqWith s "the first copy was the Hill Giant's 3/3" (S.powerToughnessOf shifterId afterFirst) $ Just (3, 3)
        Spec.assertEqWith s "with the Hill Giant's abilities plus the kept one" (length (Projection.triggeredAbilitiesOf shifterId afterFirst)) 1

-- Append one card of `printing` to `pid`'s hand -- S.handOne overwrites alice's
-- hand, so a second card in it must be appended. Group-local rather than in
-- Parun.Support: Parun.CounterspellSpec keeps its own copy of the same shape, and
-- Parun.Support rebuilds every spec in the tree.
handAppend :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId, GameState.GameState)
handAppend printing pid gs =
  let (printingId, gsP) = Game.intern printing gs
      (oid, gs1) = Game.freshObjectId gsP
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        (Game.cardObject oid pid printingId Zone.Hand ts)
          { Object.sickness = Sickness.Settled pid
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs2)
          }
      )

-- Answer a ChooseTargets by FILTERING the offered set down to one recipient,
-- never by building one: CR 608.2b re-reads what was chosen, and a hand-built
-- Recipient.ToObject of the same permanent is a different recipient that the
-- re-read drops with no error.
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (== recipient) offered) asked
  _ -> S.identityAnswer p

-- Promises a gift to carol, FILTERED out of the offer: bob heads it, so carol
-- is the answer the fallback would not give.
promisesCarol :: Prompt.Prompt r -> r
promisesCarol p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision 1
  Prompt.ChooseOpponent _ _ _ (Just PlayerChoice.Promised {}) offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== S.carol) (NonEmpty.toList offered))
  _ -> S.identityAnswer p

-- The stack's top object, which after a cast is the spell just cast.
topOfStack :: GameState.GameState -> Maybe ObjectId
topOfStack = Maybe.listToMaybe . (TurnRegisters.stack . GameState.turn)

-- Resolve one object and settle: CR 704 runs between resolutions, which is
-- where CR 704.5e removes a resolved copy.
resolveOne :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveOne answer gs = snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- Synthetic Mimicry's announcement, pinned per slot name and FILTERED out of the
-- offered set for pinTarget's reason: `subject` becomes a copy of `original`.
aimMimicry :: ObjectId -> ObjectId -> Prompt.Prompt r -> r
aimMimicry subjectId originalId p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == SlotName.MkSlotName (Text.pack "subject") then subjectId else originalId
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

copySpellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copySpellSpec s registry = Spec.describe s "Parun.Engine.Copy" $ do
  -- CR 707.2 with CR 702.174a: the opponent a gift was promised to is a choice
  -- made when casting, so a Divination that becomes a copy of Crumb and Get It
  -- promised to carol gives carol the copy's Food too.
  Spec.it s "CR 707.2 a spell that becomes a copy of a promised Crumb and Get It gives the same opponent a Food" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    divination <- S.printingOf s registry "Divination"
    crumb <- S.printingOf s registry "Crumb and Get It"
    bears <- S.printingOf s registry "Grizzly Bears"
    mimicry <- S.printingOf s registry "Synthetic Mimicry"
    let lands = S.landsFor plains S.alice 1 (S.landsFor island S.alice 5 S.threePlayerGame)
        withBears = snd (S.addPermanent bears S.alice lands)
        (withDivination, divinationId) = S.handOne divination withBears
        (crumbId, withCrumb) = S.addHandCard crumb S.alice withDivination
        (mimicryId, board) = S.addHandCard mimicry S.alice withCrumb
        cast1 = snd (Engine.runGamePure S.identityAnswer board (S.cast S.alice divinationId))
        cast2 = snd (Engine.runGamePure promisesCarol cast1 (S.cast S.alice crumbId))
        foods gs = length (filter (\o -> Set.member Subtype.Food (Projection.subtypesOf o gs)) (Game.zoneMembers Zone.Battlefield S.carol gs))
    case (topOfStack cast1, topOfStack cast2) of
      (Just divinationSpell, Just crumbSpell) | divinationSpell /= crumbSpell -> do
        let cast3 = snd (Engine.runGamePure (aimMimicry divinationSpell crumbSpell) cast2 (S.cast S.alice mimicryId))
            -- Mimicry, then Crumb and Get It, then the copy.
            afterCrumb = resolveOne S.identityAnswer (resolveOne S.identityAnswer cast3)
            after = resolveOne S.identityAnswer afterCrumb
        Spec.assertEqWith s "CR 702.174d carol created the copy's Food as well as the original's" (foods after) 2
        Spec.assertEqWith s "the original alone gave her one" (foods afterCrumb) 1
      _ -> Spec.assertFailure s "the spells never reached the stack"

  -- CR 707.10: "a copy of a spell is owned by the player under whose control it
  -- was put on the stack ... a copy of a spell or ability is controlled by the
  -- player under whose control it was put on the stack". Twincast states nobody
  -- else, so CopyStackObject.copier is its elided default and the seat is the
  -- copying effect's controller rather than the copied spell's; Meletis
  -- Charlatan is the case that names somebody else.
  --
  -- Renewed Faith ("You gain 6 life") rather than the Bolt above, because the
  -- Bolt cannot show this: its damage lands on a target either way, so a copy
  -- controlled by the wrong player deals the same 3 to the same player. Here the
  -- effect reads "you", so the two readings give alice 26 / bob 26 against alice
  -- 20 / bob 32, and no number is shared.
  Spec.it s "CR 707.10 the copy is controlled by the copying effect's controller" $ do
    island <- S.printingOf s registry "Island"
    twincast <- S.printingOf s registry "Twincast"
    renewedFaith <- S.printingOf s registry "Renewed Faith"
    plains <- S.printingOf s registry "Plains"
    let lands = S.landsFor plains S.bob 3 (S.landsFor island S.alice 2 S.threePlayerGame)
        (withTwincast, twincastId) = S.handOne twincast lands
        (faithId, board) = handAppend renewedFaith S.bob withTwincast
        -- bob CASTS it rather than being handed a stack object: a spell placed
        -- on the stack by hand carries no chosen modes, so nothing about it
        -- resolves and the copy would inherit that emptiness (CR 707.10 copies
        -- the decisions, and there would be none to copy).
        castFaith = snd (Engine.runGamePure S.identityAnswer board (S.cast S.bob faithId))
    case topOfStack castFaith of
      Nothing -> Spec.assertFailure s "Renewed Faith never reached the stack"
      Just faithSpell -> do
        let cast = snd (Engine.runGamePure (pinTarget (Recipient.ToObject faithSpell)) castFaith (S.cast S.alice twincastId))
            -- Twincast, then the copy, then bob's own Renewed Faith.
            after = resolveOne S.identityAnswer (resolveOne S.identityAnswer (resolveOne S.identityAnswer cast))
        Spec.assertEqWith s "alice controls the copy, so alice gains the 6" (S.lifeOf S.alice after) (Just 26)
        Spec.assertEqWith s "bob gains only his own 6" (S.lifeOf S.bob after) (Just 26)
        Spec.assertEqWith s "carol gains nothing" (S.lifeOf S.carol after) (Just 20)
        Spec.assertEqWith s "and the stack is empty" (length (TurnRegisters.stack (GameState.turn after))) 0
  -- Twincast's copy is built from bob's spell's object and is owned by alice,
  -- its copier (CR 707.10). A copy is no card, so it carries no identity, and
  -- the ownership report cannot mistake it for a card bob began the game with
  -- and alice now owns.
  Spec.it s "CR 707.10 a copy of a spell is no card, so the ownership report never lists it" $ do
    island <- S.printingOf s registry "Island"
    twincast <- S.printingOf s registry "Twincast"
    renewedFaith <- S.printingOf s registry "Renewed Faith"
    plains <- S.printingOf s registry "Plains"
    let lands = S.landsFor plains S.bob 3 (S.landsFor island S.alice 2 S.threePlayerGame)
        (withTwincast, twincastId) = S.handOne twincast lands
        (faithId, board) = handAppend renewedFaith S.bob withTwincast
        castFaith = snd (Engine.runGamePure S.identityAnswer board (S.cast S.bob faithId))
    case topOfStack castFaith of
      Nothing -> Spec.assertFailure s "Renewed Faith never reached the stack"
      Just faithSpell -> do
        let cast = snd (Engine.runGamePure (pinTarget (Recipient.ToObject faithSpell)) castFaith (S.cast S.alice twincastId))
            copied = resolveOne S.identityAnswer cast
        Spec.assertEqWith s "CR 707.10 the copy and bob's spell are on the stack" (length (TurnRegisters.stack (GameState.turn copied))) 2
        Spec.assertEqWith s "CR 108.3 and no card's owner has changed" (Ante.ownershipChanges copied) Seq.empty
  -- CR 707.9's exception riding CR 707.10's opcode, on Double Major {G}{U}
  -- Instant, "Copy target creature spell you control, except it isn't legendary
  -- if the spell is legendary" (Oracle text verified 2026-09-14) --
  -- CopyException.RemoveSupertypes, the same list BecomeCopy and CreateCopy
  -- carry.
  --
  -- Rograkh, Son of Rohgahh is the copied spell: a {0} Legendary Creature whose
  -- whole text box is keywords, so nothing but CR 704.5j reads the board. The
  -- copy resolves into a token (CR 707.10f) with the same NAME, so the legend
  -- rule is what the exception is visible through: without it alice controls two
  -- legendary permanents named Rograkh and puts one into a graveyard, and the
  -- token ceases to exist (CR 111.7).
  Spec.it s "CR 707.9b Double Major's copy isn't legendary, so CR 704.5j leaves both" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    rograkh <- S.printingOf s registry "Rograkh, Son of Rohgahh"
    doubleMajor <- S.printingOf s registry "Double Major"
    let lands = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 S.threePlayerGame)
        (rograkhId, withRograkh) = handAppend rograkh S.alice lands
        (majorId, board) = handAppend doubleMajor S.alice withRograkh
        cast1 = S.runPure S.identityAnswer board (S.cast S.alice rograkhId)
    case topOfStack cast1 of
      Nothing -> Spec.assertFailure s "Rograkh never reached the stack"
      Just rograkhSpell -> do
        let castMajor = S.runPure (pinTarget (Recipient.ToObject rograkhSpell)) cast1 (S.cast S.alice majorId)
            -- Double Major, then the copy it put on the stack, then Rograkh.
            after = resolveOne S.identityAnswer (resolveOne S.identityAnswer (resolveOne S.identityAnswer castMajor))
            -- By PROJECTED name (CR 707.2), which is the only read that sees both
            -- the printed card and the token the copy became.
            rograkhs gs = filter (\oid -> Set.member (cardNamed "Rograkh, Son of Rohgahh") (Projection.namesOf oid gs)) (Set.toList (GameState.battlefield gs))
        Spec.assertEqWith s "CR 707.9b / 704.5j both Rograkhs are on the battlefield, the copy not being legendary" (length (rograkhs after)) 2
        case S.tokensOf after of
          [tokenId] -> do
            Spec.assertBool s (not (Set.member Supertype.Legendary (Projection.supertypesOf tokenId after))) "CR 707.9b the token copy lost the supertype the exception named"
            Spec.assertEqWith s "CR 707.2 and kept the copied name, which is what CR 704.5j reads" (Projection.namesOf tokenId after) (Set.singleton (cardNamed "Rograkh, Son of Rohgahh"))
          other -> Spec.assertFailure s ("expected exactly one token copy, got " <> show (length other))
        Spec.assertEqWith s "and the stack is empty" (TurnRegisters.stack (GameState.turn after)) []

-- Resolve until the stack is empty. The two readings of a copy's targets put
-- DIFFERENT numbers of objects on the stack -- one ward trigger more under the
-- rule -- so a fixed count of resolutions would leave the two boards at
-- different depths and the assertion would be reading two different moments.
drainStack :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
drainStack answer =
  let go fuel gs
        | fuel <= 0 || null (TurnRegisters.stack (GameState.turn gs)) = gs
        | otherwise = go (fuel - 1) (resolveOne answer gs)
   in go (10 :: Int)

-- alice's Unstable Shapeshifter becomes a copy of `original`, and the original
-- then LEAVES the battlefield.
--
-- The departure is the point. Parun.Engine.Projection.replacementsAffecting and
-- Parun.Engine.CombatRestriction.cantBlock are whole-board short-circuits, so
-- while the original is still there its own printed face answers for every
-- permanent and the printed read and the copiable read cannot be told apart. CR
-- 707.2b is what makes the board after its departure legal: "once an object has
-- been copied, changing the copiable values of the original object won't cause
-- the copy to change."
--
-- Every other permanent is chosen to trip neither short-circuit: bob's Cabal
-- Evangel is a black 2/2 with no abilities at all, alice's Giant Spider a green
-- 2/4 whose one keyword (reach) mints nothing, and Setup.emptyGame puts no land
-- down. Returns the Shapeshifter, the Evangel, the Spider and the board.
becameCopyBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, ObjectId, GameState.GameState)
becameCopyBoard shapeshifter evangel spider original =
  let (shifterId, board0) = S.addPermanent shapeshifter S.alice (Setup.emptyGame S.bothPlayers)
      (evangelId, board1) = S.addPermanent evangel S.bob board0
      (spiderId, board2) = S.addPermanent spider S.alice board1
      -- addPermanent alone arranges a board and fires nothing; the original is the
      -- one permanent that ENTERS, which is what raises CR 707.4's trigger.
      (originalId, entered) = S.entersWithTrigger original S.alice board2
      copied = resolveAndSettle S.identityAnswer (settle S.identityAnswer entered)
      gone = S.runPure S.identityAnswer copied (Event.changeZone (pure ()) originalId Zone.Graveyard)
   in (shifterId, evangelId, spiderId, gone)

-- Mark `amount` damage on one permanent through the funnel that consults the
-- replacement effects, then run CR 704's state-based actions.
dealTo :: ObjectId -> ObjectId -> Natural.Natural -> GameState.GameState -> GameState.GameState
dealTo src victim amount gs =
  S.settleSba
    ( S.runPure
        S.identityAnswer
        gs
        (Damage.applyDamage (pure ()) [DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) amount 0 False False False 0 Nothing Nothing mempty False mempty mempty mempty mempty Nothing mempty mempty Nothing DamageKind.Noncombat])
    )

cardNamed :: String -> CardName.CardName
cardNamed = CardName.MkCardName . Text.pack

-- CR 707.2 / 707.2a: "the copiable values are the values derived from the text
-- printed on the object (that text being name, mana cost, color indicator, card
-- type, subtype, supertype, rules text, power, toughness, and/or loyalty)."
-- Readers that asked that question off the COPIER's printed face rather than the
-- copied one, each behind a whole-board short-circuit that a copy could take out
-- entirely.
copiedAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copiedAbilitySpec s registry = Spec.describe s "Parun.Engine.Copy" $ do
  -- Site one: the KEYWORD disjunct of Projection.replacementsAffecting's
  -- baseHas. Protection is the only keyword in Keyword.mintsReplacement's set
  -- that mints something a permanent which became a copy AFTER entering can
  -- still use -- every other one mints an entry or turn-up rewrite, which that
  -- permanent's entry is long past.
  --
  -- A PAIR OF SOURCES on ONE board, differing only in colour, so the survival
  -- below is rule 702.16e and not the Apostle's toughness: the black Cabal
  -- Evangel and the red Goblin Piker each deal the same 2 to the same 2/1 copy.
  Spec.it s "CR 707.2a a copy's protection is minted off the COPIED face" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    apostle <- S.printingOf s registry "Apostle of Purifying Light"
    evangel <- S.printingOf s registry "Cabal Evangel"
    piker <- S.printingOf s registry "Goblin Piker"
    let (shifterId, evangelId, pikerId, board) = becameCopyBoard shapeshifter evangel piker apostle
        black = dealTo evangelId shifterId 2 board
        red = dealTo pikerId shifterId 2 board
    Spec.assertBool s (S.onBattlefield shifterId black) "CR 702.16e the copy survives the black source's lethal 2"
    Spec.assertEqWith s "CR 615.6 with nothing marked on it" (S.damageOf shifterId black) (Just 0)
    Spec.assertBool s (not (S.onBattlefield shifterId red)) "and the same 2 from the red source kills it, so 2 really is lethal here"
    -- The fixture's own preconditions, after the behaviour so neither can absorb
    -- a mutation aimed at it.
    Spec.assertEqWith s "the Shapeshifter is the Apostle by name (CR 707.2)" (Projection.namesOf shifterId board) (Set.singleton (cardNamed "Apostle of Purifying Light"))
    Spec.assertEqWith s "and the printed Apostle has left the battlefield, so nothing else answers for the board" (length (printedOnBattlefield "Apostle of Purifying Light" board)) 0

  -- Site two: CombatRestriction.baseCouldMint. Unleash is the ONLY keyword
  -- Keyword.mintsCombatRestriction answers True for.
  --
  -- CR 707.2's last sentence -- "counters ... are not copied" -- is why the
  -- counter is placed by hand: rule 702.98a restricts the permanent "as long as
  -- it has a +1/+1 counter on it", and unleash's own entry replacement fired as
  -- the CHAINWALKER entered, long after the Shapeshifter did. Without the
  -- counter both readings say "can block" and the case is vacuous, which is what
  -- the untouched leg below asserts.
  --
  -- The Spider carries the SAME counter and no unleash, so a reading that
  -- restricted every counter-bearing creature is distinguished too.
  Spec.it s "CR 707.2a a copy's unleash restricts blocking off the COPIED face" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    evangel <- S.printingOf s registry "Cabal Evangel"
    spider <- S.printingOf s registry "Giant Spider"
    let (shifterId, _, spiderId, board) = becameCopyBoard shapeshifter evangel spider chainwalker
        counted = S.addCounter CounterKind.PlusOnePlusOne 1 spiderId (S.addCounter CounterKind.PlusOnePlusOne 1 shifterId board)
    Spec.assertBool s (not (Combat.canBlock S.alice shifterId counted)) "CR 509.1b / 702.98a the copy with a +1/+1 counter cannot block"
    Spec.assertBool s (Combat.canBlock S.alice spiderId counted) "while the Spider with the same counter can"
    Spec.assertEqWith s "so only the Spider is offered as a blocker" (Combat.legalBlockers S.alice counted) [spiderId]
    -- Rule 702.98a's own condition, which is what keeps the leg above from
    -- passing for a copy that lost blocking outright.
    Spec.assertBool s (Combat.canBlock S.alice shifterId board) "and without the counter the same copy blocks"
    Spec.assertEqWith s "the Shapeshifter is the Chainwalker by name (CR 707.2)" (Projection.namesOf shifterId board) (Set.singleton (cardNamed "Gore-House Chainwalker"))
    Spec.assertEqWith s "and the printed Chainwalker has left the battlefield" (length (printedOnBattlefield "Gore-House Chainwalker" board)) 0

  -- Site three: the PRINTED-replacement disjunct of the same baseHas. Glittering
  -- Lion prints CR 615.1's shield rather than minting it from a keyword, so it
  -- separates this disjunct from the one the first case proves.
  --
  -- The Evangel takes the same kind of damage on the same board and marks it,
  -- which is what says the shield is the Shapeshifter's own rather than a board
  -- on which no damage lands at all. Three against one, so no arithmetic
  -- coincidence pairs the two readings.
  Spec.it s "CR 707.2a a copy's PRINTED replacement effect is gathered too" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    lion <- S.printingOf s registry "Glittering Lion"
    evangel <- S.printingOf s registry "Cabal Evangel"
    spider <- S.printingOf s registry "Giant Spider"
    let (shifterId, evangelId, _, board) = becameCopyBoard shapeshifter evangel spider lion
        shielded = dealTo evangelId shifterId 3 board
        bystander = dealTo shifterId evangelId 1 board
    Spec.assertEqWith s "CR 615.1 the Lion's printed shield prevents all 3" (S.damageOf shifterId shielded) (Just 0)
    Spec.assertBool s (S.onBattlefield shifterId shielded) "so the 2/2 copy survives what would otherwise be lethal"
    Spec.assertEqWith s "while the Evangel beside it marks its 1" (S.damageOf evangelId bystander) (Just 1)
    Spec.assertEqWith s "the Shapeshifter is the Lion by name (CR 707.2)" (Projection.namesOf shifterId board) (Set.singleton (cardNamed "Glittering Lion"))
    Spec.assertEqWith s "and the printed Lion has left the battlefield" (length (printedOnBattlefield "Glittering Lion" board)) 0

  -- Site four: the TYPE disjunct of that same baseHas (Projection.copiableMintsType),
  -- which reads a card type and a subtype -- both copiable values, CR 707.2 says
  -- so outright -- for CR 306.5b's planeswalker, CR 310.4b's battle and CR
  -- 715.3a's Saga -- one case each. Copy Enchantment reaches the SUBTYPE half,
  -- since a Saga is an enchantment (CR 205.3h); Clever Impersonator's "any
  -- nonland permanent" reaches both equalities of the CARD TYPE half.
  --
  -- TWO ENTRIES, so the board that observes the disjunct differs from the one
  -- that does not in exactly the original Saga's presence. The first Copy
  -- Enchantment enters while History of Benalia is still out, so the PRINTED
  -- Saga answers the whole-board short-circuit and the lore counter goes on
  -- either way. Then the original leaves, and the second copies the first: now
  -- no printed face on the battlefield carries a Saga subtype, and reading the
  -- disjunct off the copier's face gathers nothing at all.
  --
  -- CR 707.2's last sentence keeps the second entry honest -- counters are not
  -- copied -- so the counter it ends with is CR 715.3a's own, minted for it.
  --
  -- `newest` names the SECOND copy on the settled board rather than a pre-settle
  -- one (the planeswalker case below needs that): a Saga with no lore counters is
  -- below its final chapter number, so CR 704.5s does not sacrifice it and the
  -- wrong reading leaves it on the battlefield reporting zero.
  Spec.it s "CR 707.2 a copy of a Saga enters with CR 715.3a's lore counter" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let (benaliaId, board0) = S.addPermanent benalia S.alice (Setup.emptyGame S.bothPlayers)
        (_, staged1) = S.spellOnStack copyEnchantment S.alice board0
        withOriginal = resolveAndSettle (copyNamed benaliaId) staged1
        firstId = newest (printedOnBattlefield "Copy Enchantment" withOriginal)
        gone oid = S.runPure S.identityAnswer withOriginal (Event.changeZone (pure ()) oid Zone.Graveyard)
        second oid = resolveAndSettle (copyNamed oid) (snd (S.spellOnStack copyEnchantment S.alice (gone benaliaId)))
    case firstId of
      Nothing -> Spec.assertFailure s "the first Copy Enchantment left the battlefield unexpectedly"
      Just copy1 -> do
        let final = second copy1
        case newest (printedOnBattlefield "Copy Enchantment" final) of
          Nothing -> Spec.assertFailure s "the second Copy Enchantment left the battlefield unexpectedly"
          Just copy2 -> do
            -- The gameplay-level assertion this case exists for, ahead of every
            -- proxy: the copy of a copy of a Saga is a Saga, so CR 715.3a mints
            -- its entry row even with no printed Saga left on the battlefield.
            Spec.assertEqWith s "CR 715.3a one lore counter on the copy of a copy" (S.counterOf CounterKind.Lore copy2 final) 1
            Spec.assertBool s (elem Subtype.Saga (Set.toList (Projection.subtypesOf copy2 final))) "and it really is a Saga (CR 707.2)"
            Spec.assertEqWith s "CR 707.3 by the copied card's name, not the copier's" (Projection.namesOf copy2 final) (Set.singleton (cardNamed "History of Benalia"))
        -- The preconditions, after the behaviour so neither can absorb a mutation
        -- aimed at it: the first copy got its counter with the printed Saga still
        -- out, and that printed Saga really has left the battlefield.
        Spec.assertEqWith s "the first copy entered with one too, while the original was out" (S.counterOf CounterKind.Lore copy1 withOriginal) 1
        Spec.assertEqWith s "and no printed Saga is left on the battlefield for the second entry" (length (printedOnBattlefield "History of Benalia" (gone benaliaId))) 0

  -- The CARD TYPE half of that same read, on Clever Impersonator ({2}{U}{U}
  -- Creature -- Shapeshifter 0/0, "You may have this creature enter as a copy of
  -- any nonland permanent on the battlefield") and CR 306.5b's loyalty.
  --
  -- The two entries the case above needs, plus a change of controller the legend
  -- rule forces: every printed planeswalker is legendary, so bob copies alice's
  -- Jace and alice copies bob's copy, leaving CR 704.5j one of each per player.
  --
  -- CR 704.5i is what makes the wrong reading loud rather than quiet: a
  -- planeswalker that entered with no loyalty counters is put into its owner's
  -- graveyard the moment state-based actions run. So the id is taken from the
  -- board BEFORE the settle -- otherwise `newest` would answer with the surviving
  -- first copy and read ITS three counters.
  Spec.it s "CR 707.2 a copy of a planeswalker enters with CR 306.5b's loyalty counters" $ do
    jace <- S.printingOf s registry "Jace Beleren"
    impersonator <- S.printingOf s registry "Clever Impersonator"
    let (jaceId, bare) = S.addPermanent jace S.alice (Setup.emptyGame S.bothPlayers)
        board0 = S.addCounter CounterKind.Loyalty 3 jaceId bare
        (_, staged1) = S.spellOnStack impersonator S.bob board0
        withOriginal = resolveAndSettle (copyNamed jaceId) staged1
        gone = S.runPure S.identityAnswer withOriginal (Event.changeZone (pure ()) jaceId Zone.Graveyard)
    case newest (printedOnBattlefield "Clever Impersonator" withOriginal) of
      Nothing -> Spec.assertFailure s "the first Clever Impersonator left the battlefield unexpectedly"
      Just copy1 -> do
        let entered = S.runPure (copyNamed copy1) (snd (S.spellOnStack impersonator S.alice gone)) Stack.resolveTop
            final = settle S.identityAnswer entered
        case newest (printedOnBattlefield "Clever Impersonator" entered) of
          Nothing -> Spec.assertFailure s "the second Clever Impersonator never reached the battlefield"
          Just copy2 -> do
            Spec.assertEqWith s "CR 306.5b three loyalty counters on the copy of a copy" (S.counterOf CounterKind.Loyalty copy2 final) 3
            Spec.assertBool s (S.onBattlefield copy2 final) "so CR 704.5i does not put it into the graveyard"
            Spec.assertBool s (Projection.isPlaneswalkerOf copy2 final) "and it really is a planeswalker (CR 707.2)"
        Spec.assertEqWith s "the first copy entered with three too, while the original was out" (S.counterOf CounterKind.Loyalty copy1 withOriginal) 3
        Spec.assertEqWith s "and no printed planeswalker is left on the battlefield for the second entry" (length (printedOnBattlefield "Jace Beleren" gone)) 0

  -- CR 310.4b's arm of the same read, and the third of Clever Impersonator's
  -- three eligible card types. Invasion of Dominaria is not legendary, so the
  -- controller swap the planeswalker case needs is not forced here -- it is kept
  -- anyway, because a Siege's protector must be an opponent of its controller
  -- (CR 310.12a) and two seats then leave the two copies distinguishable.
  --
  -- CR 704.5v is this case's CR 704.5i: a Siege with defense 0 is put into its
  -- owner's graveyard, so the id is again taken before the settle.
  Spec.it s "CR 707.2 a copy of a battle enters with CR 310.4b's defense counters" $ do
    invasion <- S.printingOf s registry "Invasion of Dominaria"
    impersonator <- S.printingOf s registry "Clever Impersonator"
    let (invasionId, bare) = S.addPermanent invasion S.alice (Setup.emptyGame S.bothPlayers)
        board0 = S.addCounter CounterKind.Defense 5 invasionId bare
        (_, staged1) = S.spellOnStack impersonator S.bob board0
        withOriginal = resolveAndSettle (copyNamed invasionId) staged1
        gone = S.runPure S.identityAnswer withOriginal (Event.changeZone (pure ()) invasionId Zone.Graveyard)
    case newest (printedOnBattlefield "Clever Impersonator" withOriginal) of
      Nothing -> Spec.assertFailure s "the first Clever Impersonator left the battlefield unexpectedly"
      Just copy1 -> do
        let entered = S.runPure (copyNamed copy1) (snd (S.spellOnStack impersonator S.alice gone)) Stack.resolveTop
            final = settle S.identityAnswer entered
        case newest (printedOnBattlefield "Clever Impersonator" entered) of
          Nothing -> Spec.assertFailure s "the second Clever Impersonator never reached the battlefield"
          Just copy2 -> do
            Spec.assertEqWith s "CR 310.4b five defense counters on the copy of a copy" (S.counterOf CounterKind.Defense copy2 final) 5
            Spec.assertBool s (S.onBattlefield copy2 final) "so CR 704.5v does not put it into the graveyard"
        Spec.assertEqWith s "the first copy entered with five too, while the original was out" (S.counterOf CounterKind.Defense copy1 withOriginal) 5
        Spec.assertEqWith s "and no printed battle is left on the battlefield for the second entry" (length (printedOnBattlefield "Invasion of Dominaria" gone)) 0

-- Lithoform Engine's {2} ability, picked by its COST rather than by index, so a
-- reordering of the card file cannot silently aim these cases at the
-- spell-copying legs and have the activation refuse for want of a target --
-- copyPermanentSpell's reason, one ability along.
engineAbilityCopyingAbilities :: ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
engineAbilityCopyingAbilities engineId gs =
  List.find
    ((== Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) . Cost.Type.mana . ActivatedAbility.cost)
    (Projection.abilitiesOf engineId gs)

-- CR 707.10's OTHER two nouns, which Twincast above does not reach: an activated
-- and a triggered ability on the stack, copied by Lithoform Engine's first
-- ability -- "{2}, {T}: Copy target activated or triggered ability you control.
-- You may choose new targets for the copy" (data/cards/lithoform-engine.json,
-- Oracle text verified 2026-09-03).
--
-- CR 707.10b is what makes an ability copy different from a spell copy, and its
-- three sentences split as follows. The FIRST -- "a copy of an ability has the
-- same source as the original ability" -- and the SECOND -- "if the ability
-- refers to its source by name, the copy refers to that same object and not to
-- any other object with the same name" -- share one board: two Longtusk Cubs,
-- whose "Pay {E}{E}: Put a +1/+1 counter on Longtusk Cub" is the pool's activated
-- ability that names its own source, so the copy landing on the OTHER Cub is a
-- readable wrong answer rather than an unobservable one.
--
-- The THIRD -- "the copy is considered to be the same ability by effects that
-- count how many times that ability has resolved during the turn" -- has a board
-- of its own below, Ashling the Pilgrim being the pool's counter of resolutions.
--
-- CR 707.10's "a copy of an activated ability isn't activated" rides on the first
-- case as alice's energy: the cost was paid once, by the activation, and the copy
-- pays nothing. Its other half -- the copy carries no record of what the
-- activation SPENT, the rule's Dawnglow Infusion example putting mana outside the
-- objects-used-to-pay sentence -- is Forsworn Paladin's case in
-- data/scenarios/copy, which is also where CR 602.2a's thisAbility slot is shown
-- naming the copy, and where its two CR 707.10b Stifle cases show the count
-- still answering once the original has left the stack.
copyAbilityOnStackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copyAbilityOnStackSpec s registry = Spec.describe s "Parun.Engine.Copy" $ do
  -- CR 707.10b's THIRD sentence, which needs a counter of resolutions to be
  -- observable at all: Ashling the Pilgrim's "if this is the third time this
  -- ability has resolved this turn, remove all +1/+1 counters from Ashling, and
  -- it deals that much damage to each creature and each player".
  --
  -- ONE activation resolves first, so the copy is the SECOND resolution and the
  -- original the third. An engine filing the copy under a key of its own leaves
  -- both of them short of three, and the board says so in two places: Ashling
  -- alive with three counters, and bob at 20.
  --
  -- Four Mountains: two for the first activation, two for the second, and the
  -- Engine's {2} off the two its own board leaves -- six in all.
  Spec.it s "CR 707.10b a copy of an activated ability counts toward the same turn's total" $ do
    mountain <- S.printingOf s registry "Mountain"
    engine <- S.printingOf s registry "Lithoform Engine"
    ashling <- S.printingOf s registry "Ashling the Pilgrim"
    let (engineId, withEngine) = S.addPermanent engine S.alice (S.landsInPlay mountain 6)
        (ashlingId, board) = S.addPermanent ashling S.alice withEngine
    case (Maybe.listToMaybe (Projection.abilitiesOf ashlingId board), engineAbilityCopyingAbilities engineId board) of
      (Just pump, Just copier) -> do
        let once =
              resolveOne
                S.identityAnswer
                ( S.runPure
                    S.identityAnswer
                    board
                      { GameState.turn =
                          (GameState.turn board)
                            { TurnRegisters.priority = Just S.alice
                            }
                      }
                    (Activate.activateAbility S.alice ashlingId pump)
                )
            twice =
              S.runPure
                S.identityAnswer
                once
                  { GameState.turn =
                      (GameState.turn once)
                        { TurnRegisters.priority = Just S.alice
                        }
                  }
                (Activate.activateAbility S.alice ashlingId pump)
        case topOfStack twice of
          Nothing -> Spec.assertFailure s "Ashling's second activation should be on the stack"
          Just abilId -> do
            let staged =
                  S.runPure
                    (pinTarget (Recipient.ToObject abilId))
                    twice
                      { GameState.turn =
                          (GameState.turn twice)
                            { TurnRegisters.priority = Just S.alice
                            }
                      }
                    (Activate.activateAbility S.alice engineId copier)
                -- The Engine's ability, then the copy it minted, then Ashling's
                -- own second activation.
                afterEngine = resolveOne S.identityAnswer staged
                afterCopy = resolveOne S.identityAnswer afterEngine
                afterBoth = resolveOne S.identityAnswer afterCopy
            Spec.assertEqWith s "CR 707.10b the original was the third resolution, counting the copy, so each player was dealt the 3 counters removed" (S.lifeOf S.bob afterBoth) (Just 17)
            Spec.assertEqWith s "and Ashling met its own 3 damage as a 1/1 once they came off" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Ashling the Pilgrim")) S.alice afterBoth) 0
            -- Supporting, and after the reads above so it can absorb no mutation
            -- they should catch: the copy was the second resolution and dealt
            -- nothing, leaving two counters standing.
            Spec.assertEqWith s "the copy resolved before the original and was only the second time" (S.counterOf CounterKind.PlusOnePlusOne ashlingId afterCopy) 2
            Spec.assertEqWith s "and bob was untouched at that point" (S.lifeOf S.bob afterCopy) (Just 20)
      _ -> Spec.assertFailure s "Ashling the Pilgrim should declare one activated ability, and Lithoform Engine a {2} one"
  -- CR 602.2b's trigger rather than Lithoform Engine's targeted copy: Kurkesh,
  -- Onakke Ancient's "copy that ability" names the activated ability through
  -- Binding.thatAbility, and the copy goes above it, so bob's Stifle
  -- counters the original out from under the copy (Rings of Brighthearth's
  -- ruling: they resolve even if that ability is countered). Bob at 19 is the
  -- copy resolving; 20 is no copy, 18 an uncountered original.
  Spec.it s "CR 707.10 Kurkesh, Onakke Ancient's copy resolves though the original is countered" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    kurkesh <- S.printingOf s registry "Kurkesh, Onakke Ancient"
    rod <- S.printingOf s registry "Rod of Ruin"
    stifle <- S.printingOf s registry "Stifle"
    let (_, withKurkesh) = S.addPermanent kurkesh S.alice (S.landsFor island S.bob 1 (S.landsInPlay mountain 4))
        (rodId, withRod) = S.addPermanent rod S.alice withKurkesh
        (stifleId, board) = S.addHandCard stifle S.bob withRod
        pays :: Prompt.Prompt r -> r
        pays p = case p of
          Prompt.ChooseToPay {} -> PaymentDecision.Pays
          _ -> pinTarget (Recipient.ToPlayer S.bob) p
    case Projection.abilitiesOf rodId board of
      [ping] -> do
        let activated =
              S.runPure
                (pinTarget (Recipient.ToPlayer S.bob))
                board
                  { GameState.turn =
                      (GameState.turn board)
                        { TurnRegisters.priority = Just S.alice
                        }
                  }
                (Activate.activateAbility S.alice rodId ping >> Engine.settleForPriority)
        case TurnRegisters.stack (GameState.turn activated) of
          [_, abilId] -> do
            let afterKurkesh = resolveOne pays activated
                cast =
                  S.runPure
                    (pinTarget (Recipient.ToObject abilId))
                    afterKurkesh
                      { GameState.turn =
                          (GameState.turn afterKurkesh)
                            { TurnRegisters.priority = Just S.bob
                            }
                      }
                    (S.cast S.bob stifleId)
                stifled = resolveOne S.identityAnswer cast
                afterCopy = resolveOne S.identityAnswer stifled
            Spec.assertEqWith s "CR 707.10 the copy dealt bob 1 and the countered original nothing" (S.lifeOf S.bob afterCopy) (Just 19)
            Spec.assertEqWith s "and the stack is empty" (TurnRegisters.stack (GameState.turn afterCopy)) []
            -- Supporting, and after the reads above.
            Spec.assertBool s (notElem abilId (TurnRegisters.stack (GameState.turn stifled))) "setup: the Stifle countered the original while the copy was still on the stack"
            Spec.assertEqWith s "setup: the copy sat above the original before the Stifle" (length (TurnRegisters.stack (GameState.turn afterKurkesh))) 2
          stack -> Spec.assertFailure s ("Kurkesh's trigger should sit above the Rod's ability, not " <> show stack)
      _ -> Spec.assertFailure s "Rod of Ruin should declare one activated ability"

-- CR 707.10d, end to end: Zada, Hedron Grinder {3}{R} Legendary Creature --
-- Goblin Ally 3/3, "Whenever you cast an instant or sorcery spell that targets
-- only Zada, copy that spell for each other creature you control that the spell
-- could target. Each copy targets a different one of those creatures."
-- (data/cards/zada-hedron-grinder.json, Oracle text verified 2026-09-03.)
zadaSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
zadaSpec s registry =
  let boardOf = do
        forest <- S.printingOf s registry "Forest"
        zada <- S.printingOf s registry "Zada, Hedron Grinder"
        piker <- S.printingOf s registry "Goblin Piker"
        spider <- S.printingOf s registry "Giant Spider"
        wall <- S.printingOf s registry "Wall of Stone"
        mongoose <- S.printingOf s registry "Blurred Mongoose"
        growth <- S.printingOf s registry "Giant Growth"
        let lands = S.landsFor forest S.alice 1 S.threePlayerGame
            (zadaId, g1) = S.addPermanent zada S.alice lands
            (pikerId, g2) = S.addPermanent piker S.alice g1
            (spiderId, g3) = S.addPermanent spider S.alice g2
            (wallId, g4) = S.addPermanent wall S.alice g3
            (mongooseId, g5) = S.addPermanent mongoose S.alice g4
            (withGrowth, growthId) = S.handOne growth g5
        pure (zadaId, pikerId, spiderId, wallId, mongooseId, growthId, withGrowth)
      -- The Growth is aimed at Zada and at nothing else, which is the trigger's
      -- whole condition; the copies' targets are the effect's and reach no
      -- prompt at all.
      atZada :: ObjectId -> Prompt.Prompt r -> r
      atZada zadaId p = case p of
        Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter ((== Just zadaId) . Recipient.objectOf) offered) asked
        _ -> S.identityAnswer p
   in Spec.describe s "Parun.Engine.Copy" $ do
        -- CR 608.2h through rule 707.10d's "could target": carol Cancels the
        -- Growth while Zada's trigger waits, and the trigger still copies it for
        -- each other creature the Growth, as it last existed, could target.
        Spec.it s "CR 608.2h Zada still copies a Growth countered while the trigger waited" $ do
          (zadaId, pikerId, _, _, _, growthId, base) <- boardOf
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          let (cancelId, board) = S.addHandCard cancel S.carol (S.landsFor island S.carol 3 base)
              placed =
                snd
                  ( Engine.runGamePure
                      (atZada zadaId)
                      ( snd
                          ( Engine.runGamePure
                              (atZada zadaId)
                              board
                                { GameState.turn =
                                    (GameState.turn board)
                                      { TurnRegisters.priority = Just S.alice
                                      }
                                }
                              (S.cast S.alice growthId)
                          )
                      )
                      Engine.settleForPriority
                  )
              growthSpell = last (TurnRegisters.stack (GameState.turn placed))
              cancelled =
                snd
                  ( Engine.runGamePure
                      (atZada growthSpell)
                      placed
                        { GameState.turn =
                            (GameState.turn placed)
                              { TurnRegisters.priority = Just S.carol
                              }
                        }
                      (S.cast S.carol cancelId)
                  )
              after = drainStack (atZada zadaId) cancelled
          Spec.assertEqWith s "CR 608.2h a copy of the countered Growth pumped the Piker" (S.powerToughnessOf pikerId after) (Just (5, 4))
          Spec.assertEqWith s "CR 701.6a the countered Growth itself pumped nothing, and Zada stays 3/3" (S.powerToughnessOf zadaId after) (Just (3, 3))

-- CR 707.10e, end to end: Ivy, Gleeful Spellthief {G}{U} Legendary Creature --
-- Faerie Rogue 2/1, "Flying. Whenever a player casts a spell that targets only a
-- single creature other than Ivy, you may copy that spell. The copy targets
-- Ivy." (data/cards/ivy-gleeful-spellthief.json, Oracle text verified
-- 2026-09-05.)
ivySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ivySpec s registry =
  let growthBoard = do
        forest <- S.printingOf s registry "Forest"
        ivy <- S.printingOf s registry "Ivy, Gleeful Spellthief"
        spider <- S.printingOf s registry "Giant Spider"
        growth <- S.printingOf s registry "Giant Growth"
        let lands = S.landsFor forest S.bob 1 S.threePlayerGame
            (ivyId, g1) = S.addPermanent ivy S.alice lands
            (spiderId, g2) = S.addPermanent spider S.bob g1
            (growthId, g3) = S.addHandCard growth S.bob g2
        pure (ivyId, spiderId, growthId, g3)
      -- What each object on the stack targets, top first, read live off its
      -- bindings the way Parun.Engine.Resolve.Common.targetsOnStack does, less CR
      -- 201.5's reserved self slot, the sibling helper's reason.
      stackTargets gs = fmap (\oid -> Set.toList (Foldable.fold (Map.elems (Map.delete Binding.triggerSource (Binding.targetsOf (maybe Map.empty Object.bindings (Game.lookupObject oid gs))))))) (TurnRegisters.stack (GameState.turn gs))
   in Spec.describe s "Parun.Engine.Copy" $ do
        -- CR 608.2h over a spell that has left the stack: carol Cancels the
        -- Growth while Ivy's trigger waits, and the trigger still copies it, as
        -- the Growth last existed -- Ivy's ruling (2022-09-09). CR 707.10e's
        -- "could target" is that last-known spell's, so the copy is aimed at Ivy.
        Spec.it s "CR 608.2h the copy is made even when the Growth was countered first" $ do
          (ivyId, spiderId, growthId, base) <- growthBoard
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          let (cancelId, board) = S.addHandCard cancel S.carol (S.landsFor island S.carol 3 base)
              -- bob aims the Growth at his Spider and carol the Cancel at the
              -- Growth's stack object (CR 400.7 gave it a new id), each FILTERED
              -- out of the offered set.
              aimed :: [ObjectId] -> Prompt.Prompt r -> r
              aimed victims p = case p of
                Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (maybe False (`elem` victims) . Recipient.objectOf) offered) asked
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                _ -> S.identityAnswer p
              cast =
                snd
                  ( Engine.runGamePure
                      (aimed [spiderId])
                      board
                        { GameState.turn =
                            (GameState.turn board)
                              { TurnRegisters.priority = Just S.bob
                              }
                        }
                      (S.cast S.bob growthId)
                  )
              triggered = snd (Engine.runGamePure (aimed [spiderId]) cast Engine.settleForPriority)
              growthSpell = last (TurnRegisters.stack (GameState.turn triggered))
              answer :: Prompt.Prompt r -> r
              answer = aimed [spiderId, growthSpell]
              cancelled =
                snd
                  ( Engine.runGamePure
                      answer
                      triggered
                        { GameState.turn =
                            (GameState.turn triggered)
                              { TurnRegisters.priority = Just S.carol
                              }
                        }
                      (S.cast S.carol cancelId)
                  )
              -- The Cancel, then Ivy's trigger: the moment the copy either
              -- exists or does not, with the Growth already in bob's graveyard.
              afterTrigger = resolveOne answer (resolveOne answer cancelled)
              after = drainStack answer afterTrigger
          Spec.assertEqWith s "CR 608.2h the copy of the countered Growth pumped Ivy" (S.powerToughnessOf ivyId after) (Just (5, 4))
          Spec.assertEqWith s "CR 707.10e the copy alone was on the stack, naming Ivy" (fmap (Maybe.mapMaybe Recipient.objectOf) (stackTargets afterTrigger)) [[ivyId]]
          Spec.assertEqWith s "CR 701.6a the countered Growth pumped nothing, and the Spider stays 2/4" (S.powerToughnessOf spiderId after) (Just (2, 4))

-- CR 115.1's "targets only a single ..." NARROWED by a description of the one
-- target, end to end: Leyline of Resonance {2}{R}{R} Enchantment, "If this card
-- is in your opening hand, you may begin the game with it on the battlefield.
-- Whenever you cast an instant or sorcery spell that targets only a single
-- creature you control, copy that spell. You may choose new targets for the
-- copy." (data/cards/leyline-of-resonance.json, Oracle text verified
-- 2026-09-05.)
--
-- The spell alice casts is Angelic Edict, whose slot names CR 110.1's PERMANENT
-- pool, so its one target is a Recipient.ToObject. That is deliberate: the
-- condition is answered off the TARGET's own view, so a "target permanent" spell
-- aimed at a creature satisfies "a single creature" exactly as a "target
-- creature" spell does, which is what the printed template asks and what the
-- recipient tag alone could not say.
--
-- Two boards differing in ONE thing -- which creature the Edict names -- carry
-- the "you control" half: alice's own Spider on one, bob's Piker on the other,
-- with carol sitting out so the three roles stay apart.
leylineOfResonanceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
leylineOfResonanceSpec s registry =
  let boardOf = do
        plains <- S.printingOf s registry "Plains"
        leyline <- S.printingOf s registry "Leyline of Resonance"
        spider <- S.printingOf s registry "Giant Spider"
        berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
        piker <- S.printingOf s registry "Goblin Piker"
        edict <- S.printingOf s registry "Angelic Edict"
        let lands = S.landsFor plains S.alice 5 S.threePlayerGame
            (_, g1) = S.addPermanent leyline S.alice lands
            (spiderId, g2) = S.addPermanent spider S.alice g1
            (berserkersId, g3) = S.addPermanent berserkers S.alice g2
            (pikerId, g4) = S.addPermanent piker S.bob g3
            (edictId, g5) = S.addHandCard edict S.alice g4
        pure (spiderId, berserkersId, pikerId, edictId, g5)
      -- Pin one announcement by FILTERING the offered set, never by building a
      -- recipient: CR 608.2b re-reads what was chosen. Reaches the cast and CR
      -- 707.10c's re-target prompt alike, so each phase below is run with its
      -- own.
      aimAt :: ObjectId -> Prompt.Prompt r -> r
      aimAt oid p = case p of
        Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) asked
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- alice casts the Edict at `victim`, CR 603.3b puts any trigger above it,
      -- and that trigger alone resolves -- the moment the copy either exists or
      -- does not. CR 707.10c's prompt then aims the copy at `newTarget`.
      afterTrigger victim newTarget edictId board =
        let cast =
              snd
                ( Engine.runGamePure
                    (aimAt victim)
                    board
                      { GameState.turn =
                          (GameState.turn board)
                            { TurnRegisters.priority = Just S.alice
                            }
                      }
                    (S.cast S.alice edictId)
                )
         in resolveOne (aimAt newTarget) (snd (Engine.runGamePure S.identityAnswer cast Engine.settleForPriority))
   in Spec.describe s "Parun.Engine.Copy" . Spec.it s "CR 115.1 the trigger reads the one target's own description, not the pool its slot named" $ do
        (spiderId, berserkersId, pikerId, edictId, board) <- boardOf
        let ownRun = drainStack S.identityAnswer (afterTrigger spiderId berserkersId edictId board)
            othersRun = drainStack S.identityAnswer (afterTrigger pikerId berserkersId edictId board)
            standing gs = (onBattlefield spiderId gs, onBattlefield berserkersId gs, onBattlefield pikerId gs)
        -- The fixture's own precondition: all three creatures are on the
        -- battlefield to start with, so every read below is of a change.
        Spec.assertEqWith s "all three creatures start on the battlefield" (standing board) (True, True, True)
        Spec.assertEqWith s "CR 115.1 alice's Spider is one creature she controls, so the copy exiled the Berserkers too" (standing ownRun) (False, False, True)
        Spec.assertEqWith s "CR 115.1 bob's Piker is not, so no copy was made and both of alice's creatures stand" (standing othersRun) (True, True, False)
        Spec.assertBool s (spiderId /= berserkersId && berserkersId /= pikerId) "the three creatures are distinct objects"

-- alice with one copy of `card` in her graveyard and `lands` untapped, holding
-- priority in her main phase with an empty stack, so CR 602.5d's sorcery timing
-- is met. `twins` more copies of `card` sit on her battlefield.
graveyardCopyBoard :: Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId, [ObjectId], GameState.GameState)
graveyardCopyBoard card land lands twins =
  let add (ids, gs) _ = let (oid, gs') = S.addPermanent card S.alice gs in (ids <> [oid], gs')
      (twinIds, withTwins) = List.foldl' add ([], S.landsInPlay land lands) [1 .. twins]
      (gyId, withCard) = S.addGraveyardCard card S.alice withTwins
   in ( gyId,
        twinIds,
        withCard
          { GameState.turn =
              (GameState.turn withCard)
                { TurnRegisters.priority = Just S.alice,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PostcombatMain
                }
          }
      )

-- Activate the one ability the graveyard card offers and resolve it, under a
-- given answerer, for a token whose own entry asks something (Vizier of Many
-- Faces' copy choice).
activateFromGraveyardWith :: (forall r. Prompt.Prompt r -> r) -> ObjectId -> GameState.GameState -> GameState.GameState
activateFromGraveyardWith answer gyId gs = case Activatable.abilitiesFor gyId gs of
  [ability] -> S.runPure answer gs (Activate.activateAbility S.alice gyId ability >> Stack.resolveTop)
  _ -> gs

-- Is Doom Blade, sitting in alice's hand, castable? CR 601.2c: only with a legal
-- target, and its one slot is "target nonblack creature".
doomBladeOffered :: ObjectId -> GameState.GameState -> Bool
doomBladeOffered bladeId gs =
  any
    ( \a -> case a of
        A.Cast o _ _ -> o == bladeId
        _ -> False
    )
    (Action.legalActions S.alice gs)

-- CR 702.128a and CR 702.129a, Parun.Engine.Keyword.graveyardTokenCopy: the card
-- is exiled as a cost and a token copy of it is created with CR 707.9b's
-- exceptions. Embalm's and eternalize's own cases are
-- data/scenarios/copy/cr-702-128a-* and cr-702-129a-*.
graveyardTokenCopySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graveyardTokenCopySpec s registry = Spec.describe s "Parun.Engine.Copy" $ do
  -- CR 202.3b asks whether a copy copies a BACK FACE, which is the copiable
  -- values' question and not the printed card's: a transformed Blightreaper
  -- Thallid that Mirrorweave then makes a Hill Giant has the Hill Giant's
  -- copiable values (CR 707.2), so a Clone of it is a Hill Giant with mana value
  -- 4. The same board without Mirrorweave is the pair: a Clone of the
  -- transformed Thallid copies its back face and has mana value 0.
  Spec.it s "CR 202.3b a Clone of a transformed Thallid that Mirrorweave made a Hill Giant has mana value 4" $ do
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    hillGiant <- S.printingOf s registry "Hill Giant"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    clone <- S.printingOf s registry "Clone"
    plains <- S.printingOf s registry "Plains"
    let (thallidId, withThallid) = S.addPermanent thallid S.alice (S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers))
        (giant, withGiant) = S.addPermanent hillGiant S.alice withThallid
        -- PLACED back face up, which no move reaches more directly.
        transformed = withGiant {GameState.objects = Map.adjust (\o -> o {Object.face = Just (CardName.MkCardName (Text.pack "Blightsower Thallid"))}) thallidId (GameState.objects withGiant)}
        (staged, weaveId) = S.handOne mirrorweave transformed
        woven = snd (Engine.runGamePure (targetingCard giant) staged (S.cast S.alice weaveId >> Stack.resolveTop >> Engine.settleForPriority))
        cloneOf board =
          let cloned = cloneOnto clone thallidId board
           in fmap (\copy -> (Projection.namesOf copy cloned, PC.manaValue (Projection.project copy cloned))) (cloneOnBattlefield cloned)
    Spec.assertEqWith s "CR 707.2 the Clone is a Hill Giant, mana value 4" (cloneOf woven) (Just (Set.singleton (CardName.MkCardName (Text.pack "Hill Giant")), Just 4))
    Spec.assertEqWith s "CR 202.3b without Mirrorweave it copies the back face, mana value 0" (cloneOf transformed) (Just (Set.singleton (CardName.MkCardName (Text.pack "Blightsower Thallid")), Just 0))
    Spec.assertEqWith s "setup: Mirrorweave made the transformed Thallid a Hill Giant" (Projection.namesOf thallidId woven) (Set.singleton (CardName.MkCardName (Text.pack "Hill Giant")))
  -- CR 702.128b / 707.9f on Vizier of Many Faces {2}{U}{U}, "You may have this
  -- creature enter as a copy of any creature on the battlefield, except if this
  -- creature was embalmed, the token has no mana cost, it's white, and it's a
  -- Zombie in addition to its other types. / Embalm {3}{U}{U}" (Oracle text and
  -- rulings checked on Scryfall, 2026-10-12). The red {3}{R} Hill Giant is what
  -- every Vizier copies, so each excepted characteristic differs from the
  -- copied one.
  --
  -- The cast Vizier is the control: the same copy of the same Giant, its
  -- condition failing. A Clone of the embalmed token is the copiable-values
  -- tripwire (CR 707.9b): the excepted values ride along, the embalmed mark does
  -- not (CR 109.3 makes it no characteristic).
  Spec.it s "CR 707.9f Vizier of Many Faces' embalmed token is a white Zombie copy with no mana cost, a cast Vizier's copy is not, and a Clone of the token keeps the exceptions" $ do
    island <- S.printingOf s registry "Island"
    vizier <- S.printingOf s registry "Vizier of Many Faces"
    giant <- S.printingOf s registry "Hill Giant"
    clone <- S.printingOf s registry "Clone"
    let (gyId, _, bare) = graveyardCopyBoard vizier island 13 0
        (giantId, board) = S.addPermanent giant S.alice bare
        embalmed = activateFromGraveyardWith (copyNamed giantId) gyId board
        cast = castAndResolve (copyNamed giantId) vizier board
        giantName = Set.singleton (CardName.MkCardName (Text.pack "Hill Giant"))
        designationsOf oid gs = fmap Object.designations (Game.lookupObject oid gs)
    case (tokensOnBattlefield embalmed, printedOnBattlefield "Vizier of Many Faces" cast) of
      ([tokenId], [castId]) -> do
        Spec.assertEqWith s "CR 707.9f the embalmed token's copy is white, not the Giant's red" (Projection.colorsOf tokenId embalmed) (Set.singleton Color.White)
        Spec.assertEqWith s "CR 707.9f and has no mana cost" (PC.manaCost (Projection.project tokenId embalmed)) Nothing
        Spec.assertEqWith s "CR 707.9f and is a Zombie in addition" (Projection.subtypesOf tokenId embalmed) (Set.fromList [Subtype.Giant, Subtype.Zombie])
        Spec.assertEqWith s "CR 707.2 otherwise the Giant: its name" (PC.names (Projection.project tokenId embalmed)) giantName
        Spec.assertEqWith s "CR 707.2 and its 3/3" (S.powerToughnessOf tokenId embalmed) (Just (3, 3))
        Spec.assertEqWith s "CR 702.128b the token is embalmed" (designationsOf tokenId embalmed) (Just (Set.singleton Designation.Embalmed))
        Spec.assertEqWith s "CR 707.9f a cast Vizier's copy keeps the Giant's red" (Projection.colorsOf castId cast) (Set.singleton Color.Red)
        Spec.assertEqWith s "and its mana value 4" (PC.manaValue (Projection.project castId cast)) (Just 4)
        Spec.assertEqWith s "and is no Zombie" (Projection.subtypesOf castId cast) (Set.singleton Subtype.Giant)
        Spec.assertEqWith s "but it is the Giant" (PC.names (Projection.project castId cast)) giantName
        Spec.assertEqWith s "and is not embalmed" (designationsOf castId cast) (Just Set.empty)
        let cloned = castAndResolve (copyNamed tokenId) clone embalmed
        case cloneOnBattlefield cloned of
          Just cloneId -> do
            Spec.assertEqWith s "CR 707.9b a Clone of the token is white" (Projection.colorsOf cloneId cloned) (Set.singleton Color.White)
            Spec.assertEqWith s "CR 707.9b with no mana cost" (PC.manaCost (Projection.project cloneId cloned)) Nothing
            Spec.assertEqWith s "CR 707.9b and a Zombie" (Projection.subtypesOf cloneId cloned) (Set.fromList [Subtype.Giant, Subtype.Zombie])
            Spec.assertEqWith s "CR 109.3 but not embalmed" (designationsOf cloneId cloned) (Just Set.empty)
          Nothing -> Spec.assertFailure s "the Clone should be on the battlefield"
      (tokens, casts) -> Spec.assertFailure s ("expected one token and one cast Vizier, got " <> show (length tokens, length casts))

-- CR 707.2's token copy entering tapped and attacking (CR 110.5b, CR 508.4), and
-- named later in the same resolution (CR 603.7c): Flamerush Rider, "Whenever this
-- creature attacks, create a token that's a copy of another target attacking
-- creature and that's tapped and attacking. Exile the token at end of combat."
--
-- alice attacks bob with the 3/3 Rider and a 2/1 Goblin Piker; the trigger's one
-- legal target is the Piker, so the token is a second 2/1 Piker.
flamerushRiderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
flamerushRiderSpec s registry = Spec.describe s "Parun.Engine.Copy" $ do
  Spec.it s "CR 508.4 Flamerush Rider's copy of the Piker enters tapped and attacking, deals damage, and is exiled at end of combat" $ do
    rider <- S.printingOf s registry "Flamerush Rider"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [rider, piker] []
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        tokens = S.tokensOf atBlockers
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith
      s
      "CR 110.5b / CR 508.4 one Piker token, tapped and attacking bob"
      (fmap (\oid -> (PC.names (Projection.project oid atBlockers), fmap Object.tapped (Game.lookupObject oid atBlockers), Map.lookup oid (Combat.Type.attackers (TurnRegisters.combat (GameState.turn atBlockers))))) tokens)
      [(Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")), Just TapState.Tapped, Just (AttackTarget.OfPlayer S.bob))]
    Spec.assertEqWith s "CR 510.1b bob takes 3 from the Rider, 2 from the Piker and 2 from its copy" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith s "CR 603.7c the token named by the trigger is exiled at end of combat" (S.tokensOf after) []

-- CR 707.13 on Garth One-Eye {W}{U}{B}{R}{G}, "{T}: Choose a card name that
-- hasn't been chosen from among Disenchant, Braingeyser, Terror, Shivan Dragon,
-- Regrowth, and Black Lotus. Create a copy of the card with the chosen name. You
-- may cast the copy. (You still pay its costs.)" (Oracle text and rulings checked
-- on Scryfall, 2026-09-23). The cast, declined and spent-name cases are
-- data/scenarios/copy/cr-707-13-*; this one answers CR 108.1's lookup from a
-- reference lacking the name, which no scenario's card data does.
garthSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
garthSpec s registry = Spec.describe s "GarthOneEye" $ do
  Spec.it s "CR 707.13 a name the reference does not know makes no copy" $ do
    garth <- S.printingOf s registry "Garth One-Eye"
    let (garthId, board) = garthBoard garth (Setup.emptyGame S.bothPlayers)
        unknown = activateGarth (naming [] lotusName OptionalDecision.Exercises) garthId board
        copies gs = filter (\o -> case Object.source o of Source.OfCardCopy _ -> True; _ -> False) (Map.elems (GameState.objects gs))
    Spec.assertEqWith s "unknown: nothing was cast" (length (TurnRegisters.stack (GameState.turn unknown)), length (copies unknown)) (0, 0)

lotusName :: CardName.CardName
lotusName = CardName.MkCardName (Text.pack "Black Lotus")

-- alice's settled Garth in her main phase, with priority.
garthBoard :: Printing.Printing -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
garthBoard garth gs =
  let (garthId, withGarth) = S.addPermanent garth S.alice gs
   in ( garthId,
        withGarth
          { GameState.turn =
              (GameState.turn withGarth)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

-- Activates Garth's one ability and resolves it.
activateGarth :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateGarth answer garthId board = case Activatable.abilitiesFor garthId board of
  [ability] -> S.runPure answer board (Activate.activateAbility S.alice garthId ability >> Stack.resolveTop)
  _ -> board

-- Names `name` when a card name is asked for, answers CR 108.1's lookup from
-- `reference` alone, and takes or declines the offered cast.
naming :: [Printing.Printing] -> CardName.CardName -> OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
naming reference name decision p = case p of
  Prompt.ChooseCardName {} -> name
  Prompt.LookUpCard wanted -> fmap Printing.card (List.find ((== wanted) . S.printingName) reference)
  Prompt.OfferedCast {} -> decision
  _ -> S.identityAnswer p

-- alice casts Clone and copies `original` as it enters (CR 707.5).
cloneOnto :: Printing.Printing -> ObjectId -> GameState.GameState -> GameState.GameState
cloneOnto clone original board =
  let (_, staged) = S.spellOnStack clone S.alice board
   in snd (Engine.runGamePure (copyingOnto original) staged (Stack.resolveTop >> Engine.settleForPriority))

copyingOnto :: ObjectId -> Prompt.Prompt r -> r
copyingOnto original p = case p of
  Prompt.ChooseCopyTarget {} -> Just original
  _ -> S.identityAnswer p

-- Aims every target slot at this card when it is offered.
targetingCard :: ObjectId -> Prompt.Prompt r -> r
targetingCard card p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just card) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- CR 707.2c / 611.3a: Metamorphic Alteration's "Enchanted creature is a copy
-- of the chosen creature", a static ability's copy effect. Its copiable values
-- are fixed as it first applies, and it applies to whatever the Aura enchants.
-- The board: alice's tapped Goblin Piker (2/1) with a +1/+1 counter and Unholy
-- Strength (+2/+1), her War Mammoth (3/3) and Grizzly Bears (2/2), and two
-- Islands, four of them left for a Mirrorweave. The Aura is cast on the Piker
-- choosing the Mammoth, the second of three candidates, so the fallback first
-- candidate cannot stand in for it.
metamorphicAlterationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
metamorphicAlterationSpec s registry = Spec.describe s "Parun.Engine.Copy" $ do
  let enchantedBoard = do
        island <- S.printingOf s registry "Island"
        piker <- S.printingOf s registry "Goblin Piker"
        mammoth <- S.printingOf s registry "War Mammoth"
        bears <- S.printingOf s registry "Grizzly Bears"
        unholy <- S.printingOf s registry "Unholy Strength"
        alteration <- S.printingOf s registry "Metamorphic Alteration"
        let (pikerId, g1) = S.addPermanent piker S.alice (S.landsInPlay island 6)
            (mammothId, g2) = S.addPermanent mammoth S.alice g1
            (bearsId, g3) = S.addPermanent bears S.alice g2
            (unholyId, g4) = S.addPermanent unholy S.alice g3
            g5 = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId (S.attach unholyId pikerId g4)
            board = g5 {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) pikerId (GameState.objects g5)}
            (withSpell, spellId) = S.handOne alteration board
            answer :: Prompt.Prompt r -> r
            answer p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just pikerId) . Recipient.objectOf) . snd) sets
              Prompt.ChoosePermanent _ _ _ candidates _ -> if elem mammothId candidates then mammothId else NonEmpty.head candidates
              _ -> S.identityAnswer p
            cast = snd (Engine.runGamePure answer withSpell (S.cast S.alice spellId))
            resolved = resolveAndSettle answer cast
            auraId = List.find (\oid -> fmap Object.attachedTo (Game.lookupObject oid resolved) == Just (Just (Recipient.ToCreature pikerId)) && oid /= unholyId) (Set.toList (GameState.battlefield resolved))
        pure (pikerId, mammothId, bearsId, auraId, resolved)
      named name = Set.singleton (CardName.MkCardName (Text.pack name))

  Spec.it s "CR 707.2 Metamorphic Alteration makes the enchanted creature a copy, keeping its counters, Auras and status" $ do
    (pikerId, mammothId, _, auraId, resolved) <- enchantedBoard
    Spec.assertEqWith s "the Piker is named War Mammoth" (Projection.namesOf pikerId resolved) (named "War Mammoth")
    Spec.assertEqWith s "with the Mammoth's 3/3 under its counter and Unholy Strength" (S.powerToughnessOf pikerId resolved) (Just (6, 5))
    Spec.assertEqWith s "still tapped" (fmap Object.tapped (Game.lookupObject pikerId resolved)) (Just TapState.Tapped)
    Spec.assertEqWith s "the chosen Mammoth is itself" (S.powerToughnessOf mammothId resolved) (Just (3, 3))
    Spec.assertBool s (Maybe.isJust auraId) "the Aura enchants the Piker"
    -- CR 707.2c: fixed as the effect first applied, so the Mammoth leaving
    -- changes nothing.
    let mammothGone = S.runPure S.identityAnswer resolved (Event.destroy (pure ()) Regenerability.Regenerable [mammothId])
    Spec.assertEqWith s "the Mammoth destroyed, the Piker is still a War Mammoth" (Projection.namesOf pikerId mammothGone) (named "War Mammoth")
    Spec.assertEqWith s "and still 6/5" (S.powerToughnessOf pikerId mammothGone) (Just (6, 5))
    -- CR 611.3b: the Aura gone, the Piker is itself again.
    let auraGone = S.runPure S.identityAnswer mammothGone (Event.destroy (pure ()) Regenerability.Regenerable (Maybe.maybeToList auraId))
    Spec.assertEqWith s "the Aura destroyed, the Piker is a Goblin Piker again" (Projection.namesOf pikerId auraGone) (named "Goblin Piker")
    Spec.assertEqWith s "2/1 under its counter and Unholy Strength" (S.powerToughnessOf pikerId auraGone) (Just (5, 3))

  -- CR 611.3a: the effect applies to whatever the Aura enchants, so moving it
  -- moves the copy.
  Spec.it s "CR 611.3a Metamorphic Alteration moved to another creature takes the copy with it" $ do
    (pikerId, _, bearsId, auraId, resolved) <- enchantedBoard
    let moved = S.runPure S.identityAnswer resolved (Foldable.for_ auraId (\aura -> Event.attach aura (Recipient.ToCreature bearsId)))
    Spec.assertEqWith s "the Bears are a War Mammoth" (Projection.namesOf bearsId moved) (named "War Mammoth")
    Spec.assertEqWith s "and 3/3" (S.powerToughnessOf bearsId moved) (Just (3, 3))
    Spec.assertEqWith s "the Piker is a Goblin Piker again" (Projection.namesOf pikerId moved) (named "Goblin Piker")
    Spec.assertEqWith s "2/1 under its counter and Unholy Strength" (S.powerToughnessOf pikerId moved) (Just (5, 3))

  -- CR 707.3: a Clone of the enchanted creature copies the copiable values the
  -- Aura gave it, and keeps them once the Aura is gone (CR 707.2b).
  Spec.it s "CR 707.3 a Clone of the creature Metamorphic Alteration enchants is a War Mammoth" $ do
    (pikerId, _, _, auraId, resolved) <- enchantedBoard
    clone <- S.printingOf s registry "Clone"
    let (_, staged) = S.spellOnStack clone S.alice resolved
        cloned = resolveAndSettle (copyNamed pikerId) staged
        cloneId = newest (printedOnBattlefield "Clone" cloned)
        auraGone = S.runPure S.identityAnswer cloned (Event.destroy (pure ()) Regenerability.Regenerable (Maybe.maybeToList auraId))
    Spec.assertEqWith s "the Clone is named War Mammoth" (fmap (`Projection.namesOf` cloned) cloneId) (Just (named "War Mammoth"))
    Spec.assertEqWith s "and is 3/3, with no counter of the Piker's" (cloneId >>= (`S.powerToughnessOf` cloned)) (Just (3, 3))
    Spec.assertEqWith s "the Aura destroyed, the Clone is still a War Mammoth" (fmap (`Projection.namesOf` auraGone) cloneId) (Just (named "War Mammoth"))
    Spec.assertEqWith s "while the Piker is itself" (Projection.namesOf pikerId auraGone) (named "Goblin Piker")

  -- CR 613.7e: a copy effect stored after the Aura's outranks it, until the
  -- Aura becomes attached again and takes a new timestamp. Mirrorweave makes
  -- every creature but the Bears a Grizzly Bears; the Aura then moves to the
  -- Mammoth and back.
  Spec.it s "CR 613.7e Metamorphic Alteration attached again outranks a Mirrorweave cast after it" $ do
    (pikerId, mammothId, bearsId, auraId, resolved) <- enchantedBoard
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    let (withSpell, spellId) = S.handOne mirrorweave resolved
        atBears :: Prompt.Prompt r -> r
        atBears p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just bearsId) . Recipient.objectOf) . snd) sets
          _ -> S.identityAnswer p
        woven = resolveAndSettle atBears (snd (Engine.runGamePure atBears withSpell (S.cast S.alice spellId)))
        moveTo host = Foldable.for_ auraId (\aura -> Event.attach aura (Recipient.ToCreature host))
        away = S.runPure S.identityAnswer woven (moveTo mammothId)
        back = S.runPure S.identityAnswer away (moveTo pikerId)
    Spec.assertEqWith s "under Mirrorweave the Piker is a Grizzly Bears" (Projection.namesOf pikerId woven) (named "Grizzly Bears")
    Spec.assertEqWith s "the Aura moved back, the Piker is a War Mammoth again" (Projection.namesOf pikerId back) (named "War Mammoth")
    Spec.assertEqWith s "and the Mammoth a Grizzly Bears" (Projection.namesOf mammothId back) (named "Grizzly Bears")
