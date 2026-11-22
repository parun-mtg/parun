{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Parun.Engine.Replacement over damage prevention (CR 615) and the prevention
-- shields' referents (CR 609.7): step skipping, the shield cards from Mending
-- Hands to Test of Faith, and their text-changed forms. Split out of
-- Parun.ReplacementSpec, which keeps the machinery.
module Parun.PreventionSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Damage as Damage
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Mint as Mint
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Extra.Int as Int
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Activator as Activator
import qualified Parun.Types.ActiveReplacement as ActiveReplacement
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.Copyability as Copyability
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamagePrevented as DamagePrevented
import qualified Parun.Types.DamageR as DamageR
import qualified Parun.Types.DamageRewrite as DamageRewrite
import qualified Parun.Types.Expiry as Expiry
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.ReplacementEntry as ReplacementEntry
import qualified Parun.Types.ReplacementOrigin as ReplacementOrigin
import qualified Parun.Types.Response as Response
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.SourceChoices as SourceChoices
import qualified Parun.Types.StepBegan as StepBegan
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.Timestamp as Timestamp
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Uses as Uses
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChangePattern as ZoneChangePattern
import qualified Parun.Types.ZoneChangeR as ZoneChangeR

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- indistinguishable and correctly elided).
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

-- The single activated ability of a printing (Drudge Skeletons and Liquimetal
-- Coating each have exactly one). Total: the empty-ability fallback is
-- unreachable in this fixture.
-- Same shape as ActivateSpec.theAbility -- duplicated per this test suite's
-- existing convention of group-local helpers rather than centralizing a helper
-- this small in Support.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] [] Nothing (Mint.oneModeOf Map.empty Seq.empty) [] (Activator.Related PlayerRelation.You) Nothing Nothing Nothing Copyability.Copyable Nothing

wasAskedToReplace :: [Response.Response] -> Bool
wasAskedToReplace responses =
  let isReplacement r = case r of
        Response.ChoseReplacement _ -> True
        _ -> False
   in any isReplacement responses

wasAskedForEntryOption :: [Response.Response] -> Bool
wasAskedForEntryOption responses =
  let isEntryOption r = case r of
        Response.ChoseEntryOption _ -> True
        _ -> False
   in any isEntryOption responses

-- alice controls one Forest plus `mine`; bob controls `theirs`; alice holds one
-- Battlegrowth ({G} instant: put a +1/+1 counter on target creature). Returns the
-- state, Battlegrowth's hand id, and the two id lists in the order given.
counterBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
counterBoard forest battlegrowth mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = S.addPermanent p pid g in (ids <> [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll S.alice mine (S.landsInPlay forest 1)
      (yours, gs2) = addAll S.bob theirs gs1
      (gs3, spellId) = S.handOne battlegrowth gs2
   in (gs3, spellId, ours, yours)

-- Aim every target slot at `victim`, and answer a CR 616.1 race by picking the
-- candidate whose SOURCE is `preferred` -- by id, so the assertion does not
-- depend on the engine's canonical candidate order.
raceAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
raceAnswer preferred victim p = case p of
  Prompt.ChooseReplacement _ _ entries -> maybe 0 Int.toNaturalSaturating (List.findIndex ((== preferred) . ReplacementEntry.source) entries)
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> S.identityAnswer p

-- Aim every target slot at one object. Recipient.ToObject, not ToCreature as
-- raceAnswer above uses: both slots this answers -- Liquimetal Coating's and
-- Skilled Animator's -- are Pool.Permanents, and a recipient tagged for the wrong
-- pool is not in the legal set at all.
aimObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- What each COUNTDOWN shield on the board has left (CR 615.7), read off the
-- floating rows themselves. An empty list is a shield spent to 0 and dropped,
-- which is why the count of rows would not say the same thing; a Fog-shaped or
-- counter-backed prevention does not appear here at all, having no amount on its
-- row to report.
shieldsLeft :: GameState.GameState -> [Natural.Natural]
shieldsLeft gs =
  let remaining re = case re of
        ReplacementEffect.DamageR (DamageR.MkDamageR _ (DamageRewrite.PreventNext n) _) -> Just n
        _ -> Nothing
   in Maybe.mapMaybe (remaining . ActiveReplacement.effect) (StoredEffects.replacements (GameState.stored gs))

-- The damage CR 615.5's rider dealt to `pid`, ONE ENTRY PER EVENT rather than
-- summed. What tells CR 615.13's one application apart from one application per
-- recipient: both throw the same total back, and only the number of events
-- differs.
riderHits :: PlayerId.PlayerId -> GameState.GameState -> [Natural.Natural]
riderHits pid gs =
  fmap DamageEvent.amount (filter (\de -> DamageEvent.target de == Recipient.ToPlayer pid) (S.damageEventsOf gs))

-- The CR 615.13 records a board holds, in the order Parun.Engine.Damage wrote
-- them. One per applying instance, whatever the batch was addressed to.
preventionsRecorded :: GameState.GameState -> [DamagePrevented.DamagePrevented]
preventionsRecorded gs =
  let pick event = case event of
        GameEvent.DamagePrevented prevented -> Just prevented
        _ -> Nothing
   in Maybe.mapMaybe pick (S.eventsOf gs)

-- Was CR 615.7's batch-order question raised at all? The elision half of every
-- group below asserts the negative of this, so the boards that ask nothing are
-- told apart from the boards that ask.
wasAskedToOrderDamage :: [Response.Response] -> Bool
wasAskedToOrderDamage =
  let isOrder r = case r of
        Response.OrderedDamage _ -> True
        _ -> False
   in any isOrder

-- Was CR 615.7's batch-division question raised at all? The order question's
-- twin, for the countdowns counted in points rather than in whole events.
wasAskedToAllocateDamage :: [Response.Response] -> Bool
wasAskedToAllocateDamage =
  let isAllocation r = case r of
        Response.AllocatedDamage _ -> True
        _ -> False
   in any isAllocation

countersOn :: CounterKind.CounterKind Keyword.Keyword -> ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
countersOn kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

castAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castAndResolve answer gs spellId =
  S.runPure answer gs (S.cast S.alice spellId >> Stack.resolveTop)

-- alice controls `n` untapped Islands in a main phase with priority, holding one
-- card of each printing in `hand`. Returns the state and the hand ids in order --
-- unlike S.handOne, which replaces the whole hand.
blueBoard :: Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
blueBoard island n hand =
  let base = S.landsInPlay island n
      addOne (ids, g) p = let (oid, g1) = S.addHandCard p S.alice g in (ids <> [oid], g1)
      (held, gs) = List.foldl' addOne ([], base) hand
   in ( gs
          { GameState.turn =
              (GameState.turn gs)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          },
        held
      )

-- The newest battlefield object whose printed card has this name.
newestNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

-- Leyline of the Void's redirect, as a floating replacement: any card headed for
-- an OPPONENT's graveyard is exiled instead. CR 400.3 makes that graveyard the
-- card's OWNER's, which is what Replacement.matchesZoneOwner tests.
leylineShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
leylineShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR (ZoneChangePattern.MkZoneChangePattern (Just Zone.Graveyard) (ControllerRelation.Related PlayerRelation.Opponent) (Filter.Type.And []) Nothing Nothing False) Zone.Exile False False LibraryPosition.defaultValue False Nothing Seq.empty),
      ActiveReplacement.source = src,
      ActiveReplacement.controller = S.alice,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.Other,
      ActiveReplacement.condition = Nothing,
      ActiveReplacement.rider = Nothing,
      ActiveReplacement.slots = Map.empty,
      ActiveReplacement.choices = SourceChoices.MkSourceChoices Map.empty Set.empty Nothing Nothing Nothing Acquisition.Own Nothing
    }

-- Aim every target slot at one player. The player-side twin of `aimObject`
-- above; Fatigue's slot is Pool.Players, so a recipient tagged for any other
-- pool is not in its legal set at all.
aimPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer pid))) sets
  _ -> S.identityAnswer p

-- Fatigue {1}{U} Sorcery: "Target player skips their next draw step."
--
-- The three things Eon Hub above does not do, and this card does:
--
--   1. The skip is created by an EFFECT. Nothing on the battlefield carries it --
--      Fatigue is in a graveyard by the time the skip matters -- so it lives in
--      StoredEffects.replacements, the floating store CR 614.3 describes as lasting
--      "until they're used up".
--   2. It is scoped to ONE player (PhasePattern.whosePhase), where Eon Hub's
--      "players skip their upkeep steps" is symmetric.
--   3. CR 614.10a: it is CONSUMED after one occurrence, and two of them
--      ACCUMULATE rather than coalescing -- "if two effects each cause a player
--      to skip their next occurrence, that player must skip the next two".
--
-- The observables are the same pair the Eon Hub cases use, read together so a
-- skipped step cannot be confused with a step that happened and drew nothing:
-- the CR 603.2b StepBegan record (CR 614.6, "if an event is replaced, it never
-- happens"), and alice's library, which a real draw step empties by one.
fatigueSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
fatigueSpec s registry = Spec.describe s "Fatigue" $ do
  let drawStep = Phase.Beginning BeginningStep.DrawStep
      -- alice's turn, positioned at her draw step with the precombat main phase
      -- scheduled after it. Not an empty schedule: that hands the turn off, which
      -- would clear the event log these cases count and move the active player.
      --
      -- CR 103.8a's first-turn skip is the rules' (Engine.skipsDraw), not a
      -- replacement effect, and it would skip the control case's draw step;
      -- turn 2 puts it out of the way, which is also what makes this the SECOND
      -- turn's draw step -- exactly the "next" one Fatigue named.
      atDraw gs =
        gs
          { GameState.turn =
              (GameState.turn gs)
                { TurnRegisters.phase = drawStep,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.remaining = Seq.singleton Phase.PrecombatMain,
                  TurnRegisters.turnNumber = 2
                }
          }
      -- One draw step. Applied repeatedly, each call is alice's NEXT draw step:
      -- the store under test is not turn-scoped (Expiry.Never, and no sweep ends
      -- it -- every Parun.Engine.Expiry sweep keeps a Never), so what a real
      -- intervening turn would contribute is a longer log, not a different
      -- answer.
      runDraw gs = snd (Engine.runGamePure S.identityAnswer (atDraw gs) Engine.runStep)
      begun gs = length (filter (== GameEvent.StepBegan (StepBegan.MkStepBegan drawStep S.alice)) (S.eventsOf gs))
      libraryOf pid gs = length (Game.zoneMembers Zone.Library pid gs)
      -- alice: two Islands per Fatigue to cast, a stocked library to draw from,
      -- and `n` Fatigues in hand. Only alice's draw step is ever run below, so
      -- only her library needs stocking.
      board island piker fatigue n =
        let (base, held) = blueBoard island (2 * n) (replicate n fatigue)
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) base [1 .. (5 :: Int)]
         in (stocked, held)
  -- The control: the same board with the spell never cast. Without it
  -- alice's draw step begins and draws, which is what every case below is
  -- measured against.
  Spec.it s "CR 500.6 without a skip alice's draw step begins and draws" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    fatigue <- S.printingOf s registry "Fatigue"
    let (gs, _) = board island piker fatigue 1
        after = runDraw gs
    Spec.assertEqWith s "the draw step began" (begun after) 1
    Spec.assertEqWith s "and took a card off the library" (libraryOf S.alice after) 4

-- Run whole steps until `done` holds of the board, the game ends, or the bound
-- runs out. The bound is three turns' worth of steps, so a skip that dropped
-- more of the schedule than it should still terminates and fails an assertion
-- rather than hanging.
runUntil :: (GameState.GameState -> Bool) -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runUntil done answer gs0 =
  let go n g =
        if n <= (0 :: Int) || done g || Maybe.isJust (GameState.result g)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 40 gs0

-- Run whole steps until the turn hands off, leaving the board at the first step
-- of the next turn.
nextTurn :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
nextTurn answer gs = runUntil ((/= TurnRegisters.turnNumber (GameState.turn gs)) . (TurnRegisters.turnNumber . GameState.turn)) answer gs

-- CR 615.7's prevention shield, whose plainest producer in data/cards/ is Mending
-- Hands ({W} Instant: "Prevent the next 4 damage that would be dealt to any
-- target this turn") -- the same countdown shield as Healing Grace below, minus
-- CR 609.7a's chosen source.
--
-- Three properties, and they are the three halves of the rule: the shield is
-- spent in DAMAGE rather than in events ("such effects count only the amount of
-- damage; the number of events or sources dealing it doesn't matter"), it is
-- scoped to the recipient it shields, and where two simultaneous sources contend
-- for it the shielded side chooses which damage it prevents rather than the
-- engine.
--
-- The group's last case is not about CR 615.7 at all: this card is the pool's
-- floating replacement that grants nobody CONTROL, which is what makes it the
-- discriminating twin for CR 800.4a's second clause
-- (Departure.givesControlOnEntryTo). It says why it is here.
--
-- The DAMAGE BATCHES below are hand-built and the SPELL is not: casting Mending
-- Hands for real is what proves the card, and reaching a real combat-damage batch
-- of two attackers with different powers would mean driving a whole combat phase
-- to produce a fixture these assertions read straight off.
mendingHandsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mendingHandsSpec s registry = Spec.describe s "Mending Hands (CR 615.7)" $ do
  let -- One noncombat damage event, from `src`, at `n`.
      hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n 0 False False False 0 Nothing Nothing mempty False mempty mempty mempty mempty Nothing mempty mempty Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- Order a contested batch by preferring the event from `src`, by SOURCE id
      -- rather than by position, so the assertion does not depend on the order
      -- the batch was gathered in.
      -- Take 2 off every event the shield is offered. Pinned by shape rather
      -- than searched for, so a mutation cannot be repaired into an answer that
      -- happens to be legal.
      twoEach :: Prompt.Prompt r -> r
      twoEach p = case p of
        Prompt.AllocateDamage _ _ events _ -> fmap (const 2) events
        _ -> S.identityAnswer p
  -- CR 615.7's arithmetic, one event at a time: the shield takes what it can of
  -- each event and reduces by exactly that much. Three 3-damage events against a
  -- shield of 4 -- so the first is prevented whole, the second only partly, and
  -- the third not at all.
  Spec.it s "CR 615.7 the shield counts DAMAGE, not events: 4 covers one 3 and part of the next" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addPermanent pikerPrinting S.alice base
        (attacker, g2) = S.addPermanent pikerPrinting S.bob g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        strike g = S.runPure S.identityAnswer g (Damage.applyDamage Settle.none [hit attacker (Recipient.ToCreature victim) 3])
        once = strike shielded
        twice = strike once
        thrice = strike twice
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (StoredEffects.replacements (GameState.stored shielded))) 1
    -- CR 615.6: a fully prevented event never happens, so nothing is marked and
    -- nothing is recorded.
    Spec.assertEqWith s "the first 3 is prevented whole" (S.damageOf victim once) (Just 0)
    Spec.assertEqWith s "and no damage event happened at all" (amounts once) []
    Spec.assertEqWith s "the shield is still there, holding 1" (length (StoredEffects.replacements (GameState.stored once))) 1
    -- The partial case, which is what makes this a shield rather than a Fog: 1
    -- of the second 3 is prevented and the other 2 are dealt.
    Spec.assertEqWith s "1 of the second 3 is prevented, 2 are dealt" (S.damageOf victim twice) (Just 2)
    Spec.assertEqWith s "and the surviving event carries the reduced amount" (amounts twice) [2]
    -- CR 615.7: "once the shield has been reduced to 0, any remaining damage is
    -- dealt normally."
    Spec.assertEqWith s "the spent shield is gone" (StoredEffects.replacements (GameState.stored twice)) []
    Spec.assertEqWith s "so the third 3 lands in full" (S.damageOf victim thrice) (Just 5)
  -- CR 615.7's "shielded permanent": a shield names ONE recipient, so a second
  -- creature is not covered by it and does not spend it. The discriminating twin
  -- of the case above -- a shield that ignored DamagePattern.whichRecipient,
  -- which is what every damage pattern in the pool did before this card, would
  -- prevent this damage and be spent doing it.
  Spec.it s "CR 615.7 a shield covers the recipient it names and no other" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (shieldedOne, g1) = S.addPermanent pikerPrinting S.alice base
        (bystander, g2) = S.addPermanent pikerPrinting S.alice g1
        (attacker, g3) = S.addPermanent pikerPrinting S.bob g2
        (g4, spellId) = S.handOne mendingHands g3
        shielded = castAndResolve (aimCreature shieldedOne) g4 spellId
        after = S.runPure S.identityAnswer shielded (Damage.applyDamage Settle.none [hit attacker (Recipient.ToCreature bystander) 3])
    Spec.assertEqWith s "the unshielded creature takes all of it" (S.damageOf bystander after) (Just 3)
    Spec.assertEqWith s "and the shield was not spent on damage it does not cover" (length (StoredEffects.replacements (GameState.stored after))) 1
  -- CR 615.7 by the POINT rather than by the event: "each 1 damage that would be
  -- dealt to the shielded permanent or player is prevented", so the shielded
  -- side may split the shield across the batch and not merely rank it. The same
  -- board as the case above, divided 2 and 2: both events survive, at 3 and at
  -- 1, which no ranking of a 5 and a 3 against a shield of 4 can produce -- the
  -- orders reach [1,3] and [4] and nothing else.
  Spec.it s "CR 615.7 bob splits the shield between the two events, which no order can do" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (big, g1) = S.addPermanent pikerPrinting S.alice base
        (small, g2) = S.addPermanent pikerPrinting S.alice g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimPlayer S.bob) g3 spellId
        batch = [hit big (Recipient.ToPlayer S.bob) 5, hit small (Recipient.ToPlayer S.bob) 3]
        halved = settleDamage twoEach shielded batch
    -- THE gameplay assertion: both events happened, each cut by 2.
    Spec.assertEqWith s "2 off each: a 3 and a 1 are dealt" (amounts halved) [3, 1]
    Spec.assertEqWith s "and the shield still prevented exactly 4" (S.lifeOf S.bob halved) (Just 16)
    Spec.assertEqWith s "and it is spent" (StoredEffects.replacements (GameState.stored halved)) []

-- Aim every target slot at `victim` and answer CR 609.7a's source choice with
-- `src`. FILTERED, not built: the id is taken from the offered set, so an answer
-- the prompt never offered cannot reach the engine.
aimAndChoose :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAndChoose victim src p = case p of
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> aimCreature victim p

-- Which sources CR 609.7a's prompt was answered with, in order. The proxy half
-- of the group below: it says a choice was MADE, where the damage assertions say
-- the shield watches what was chosen.
chosenSourcesIn :: [Response.Response] -> [ObjectId.ObjectId]
chosenSourcesIn =
  let chosen r = case r of
        Response.ChoseDamageSource oid -> Just oid
        _ -> Nothing
   in Maybe.mapMaybe chosen

-- CR 118.12's gate PAID, with its sacrifice pinned to one permanent: the delayed
-- trigger case below runs on a board where alice controls four creatures, so the
-- offered set is a real choice and a default answer would eat the Fire-Eater the
-- case is about. FILTERED rather than hand-built, the posture every
-- choose-don't-target answerer in this file takes.
payingGate :: ObjectId.ObjectId -> Prompt.Prompt r -> r
payingGate fodder p = case p of
  Prompt.ChooseToPay {} -> PaymentDecision.Pays
  Prompt.ChooseSacrifices _ _ _ offered _ _ -> Set.fromList (filter (== fodder) offered)
  _ -> S.identityAnswer p

-- Answer CR 609.7a's source choice with `src` and aim every target slot at one
-- PLAYER. FILTERED, not built: the id is taken from the offered set, so a `src`
-- the prompt never offered falls back to the head of the pool, which is the
-- observable the group below reads.
choosePlayerAndSource :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
choosePlayerAndSource pid src p = case p of
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> aimPlayer pid p

-- CR 609.7a's chosen source on the UNBOUNDED shield, whose producer is Auriok
-- Replica ({3} Artifact Creature -- Cleric, 2/2: "{W}, Sacrifice this creature:
-- Prevent all damage a source of your choice would deal to you this turn"), with
-- Ghitu Fire-Eater ({T}, Sacrifice this creature: "It deals damage equal to its
-- power to any target") supplying the damage. The rest of the rule's classes are
-- scenarios under data/scenarios/prevention/; this case's waiting entry is a
-- state no sequence of moves holds.
auriokReplicaSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
auriokReplicaSpec s registry = Spec.describe s "Auriok Replica (CR 609.7a)" $ do
  -- CR 609.7a's binding-reading carrier: "any object referred to by ... a
  -- delayed triggered ability that's waiting to trigger". Effect.ArmDelayedTrigger
  -- captures the resolving object's whole binding environment (CR 603.7c), so an
  -- activated ability that arms one leaves its OWN id (Binding.thisAbility) in the
  -- entry -- and the ability has ceased by then, which is exactly the
  -- parenthetical case "even if that object is no longer in the zone it used to
  -- be in" and so is not filtered out anywhere downstream.
  --
  -- Grist, the Hunger Tide's -2 is an activated ability that arms a
  -- delayed trigger ("You may sacrifice a creature. When you do, destroy target
  -- creature or planeswalker"), and the entry is still armed once the -2 has
  -- resolved -- the reflexive ability is placed at the next gather, which this
  -- case deliberately does not run.
  --
  -- alice names the ceased ability, the answerer falls back to the head of the
  -- ascending pool when that id is not offered, and the Fire-Eater is placed
  -- FIRST so the head is the one source whose damage this shield can prevent.
  -- Offering the ability installs a shield over an object no damage event
  -- carries and alice takes the 2.
  Spec.it s "CR 609.7a an activated ability a waiting delayed trigger still names is not offered as a source" $ do
    plains <- S.printingOf s registry "Plains"
    replica <- S.printingOf s registry "Auriok Replica"
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    grist <- S.printingOf s registry "Grist, the Hunger Tide"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    -- The Fire-Eater FIRST, so its id is the ascending pool's head and the
    -- fallback lands on it; the Piker LAST among the creatures, so Grist's CR
    -- 118.12 gate has exactly one candidate to eat.
    let (fireEater, g1) = S.addPermanent ghitu S.alice (Setup.emptyGame S.bothPlayers)
        (replicaId, g2) = S.addPermanent replica S.alice g1
        (gristId, g3) = S.addPermanent grist S.alice g2
        g4 = S.addCounter CounterKind.Loyalty 3 gristId g3
        (fodder, g5) = S.addPermanent pikerPrinting S.alice g4
        (decoy, g6) = S.addPermanent pikerPrinting S.bob g5
        board = S.landsFor plains S.alice 1 g6
        -- The card file's second ability, after the +1.
        minusTwo = case Face.activatedAbilities (S.combinedFace grist) of
          _ : ab : _ -> ab
          _ -> theAbility grist
        -- Grist's -2 alone, paid, so the delayed trigger is armed and its
        -- captured environment holds the ability's own id. Its reflexive trigger
        -- is left unplaced, which is what keeps the entry WAITING.
        armed = S.runPure (payingGate fodder) board (Activate.activateAbility S.alice gristId minusTwo Monad.>> Stack.resolveTop)
        -- The id the entry still names, read off the stack while the -2 is on it
        -- rather than guessed.
        onStack = S.runPure (payingGate fodder) board (Activate.activateAbility S.alice gristId minusTwo)
        abilityId = case TurnRegisters.stack (GameState.turn onStack) of
          oid : _ -> oid
          [] -> gristId
        rest =
          Activate.activateAbility S.alice fireEater (theAbility ghitu)
            Monad.>> Activate.activateAbility S.alice replicaId (theAbility replica)
            Monad.>> Stack.resolveTop
            Monad.>> Stack.resolveTop
        after src = S.runPure (choosePlayerAndSource S.alice src) armed rest
    -- THE gameplay assertion: naming the ceased ability gets alice the FALLBACK,
    -- and the fallback prevents the Fire-Eater's 2. An engine that let the
    -- delayed trigger's captured environment offer the ability would shield an
    -- object that deals no damage and leave alice on 18.
    Spec.assertEqWith s "the ability the delayed trigger names was not offered, so the fallback shielded the Fire-Eater" (S.lifeOf S.alice (after abilityId)) (Just 20)
    -- Its twin, differing in the ANSWER alone and naming something the rule DOES
    -- admit through this very carrier: the departed Fire-Eater, which the waiting
    -- ability on the stack refers to. So the 20 above is not "nothing was
    -- offered at all".
    Spec.assertEqWith s "naming the departed Fire-Eater, which IS offered, prevents the same 2" (S.lifeOf S.alice (after fireEater)) (Just 20)
    -- And the reading that separates them: aim the shield at bob's Piker -- an
    -- ordinary permanent, offered by CR 609.7a's first class, that deals no damage
    -- here -- and the 2 lands, so neither 20 above is "everything was prevented".
    Spec.assertEqWith s "aiming it at bob's Piker, which shields nothing that happens here, leaves the 2 landing" (S.lifeOf S.alice (after decoy)) (Just 18)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "setup: the -2 armed a delayed trigger and it is still waiting" (length (StoredEffects.delayedTriggers (GameState.stored armed))) 1
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject abilityId armed)) "setup: CR 608.2m the -2's ability object has ceased, so only the entry still names it"
    Spec.assertBool s (abilityId /= gristId && abilityId /= fireEater) "setup: the id named is the ability object, not Grist and not the Fire-Eater"
    Spec.assertBool s (decoy /= replicaId && decoy /= fireEater) "setup: the id the twin names is bob's Piker"
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject fodder armed) && Set.member fireEater (GameState.battlefield armed)) "setup: the gate ate the spare Piker and left the Fire-Eater standing"
    Spec.assertEqWith s "the answer recorded is the fallback, not the ability alice named" (chosenSourcesIn (answersFor (choosePlayerAndSource S.alice abilityId) armed rest)) [fireEater]

-- CR 611.2c's LIVE recipient set on the UNBOUNDED shield, whose producer is Pack
-- Leader ({1}{W} Creature -- Dog, 2/2: "Other Dogs you control get +1/+1.
-- Whenever this creature attacks, prevent all combat damage that would be dealt
-- this turn to Dogs you control.").
--
-- The shield DESCRIBES its recipients rather than naming them, and that is the
-- whole point of the card here: a prevention effect modifies no characteristic
-- and no controller, so CR 611.2c leaves its set live and the description is
-- re-asked at each damage event. Spelling the same clause as a ref over "each Dog
-- you control" would sweep the set as the trigger resolved -- weaker than printed
-- in one direction and stricter in the other -- which is what the third case
-- below tells apart.
--
-- Two seats is enough: the card says "you control" and names no opponent, so the
-- three-seat trap does not apply. bob's own Dog is what makes "you control" do
-- work, and it is a recipient rather than a role.
packLeaderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
packLeaderSpec s registry = Spec.describe s "Pack Leader (CR 611.2c)" $ do
  Spec.it s "CR 611.2c the shield covers whatever matches its description when the damage would happen" $ do
    leader <- S.printingOf s registry "Pack Leader"
    tracker <- S.printingOf s registry "Ainok Tracker"
    piker <- S.printingOf s registry "Goblin Piker"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    let (gs, mine, theirs) = S.combatBoardOf [leader, tracker, piker] [tracker, piker]
        -- The declare blockers step, Hanweir Garrison's vantage point: the
        -- trigger fired at the declaration (CR 508.2b) and resolved in the
        -- declare attackers step's priority round, so the shield is up and no
        -- combat damage has been dealt yet.
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        -- A Dog that comes under alice's control AFTER the shield resolved, which
        -- is exactly the object CR 611.2c's second sentence admits and a swept
        -- set could not.
        (latecomer, board) = S.addPermanent kinGuard S.alice atBlockers
        hit src recipient n =
          DamageEvent.MkDamageEvent src recipient n 0 False False False 0 Nothing Nothing mempty False mempty mempty mempty mempty Nothing mempty mempty Nothing DamageKind.Combat
        -- ONE source for every case, so each pair below differs in the recipient
        -- alone: the shield says nothing about its source, and a case that
        -- swapped sources would not be a twin of the one beside it.
        strike src oid n = S.damageOf oid (S.runPure S.identityAnswer board (Damage.applyDamage Settle.none [hit src (Recipient.ToCreature oid) n]))
    case (mine, theirs) of
      ([leaderId, ourDog, ourPiker], [theirDog, theirPiker]) -> do
        let strikeFrom = strike theirPiker
        -- THE gameplay assertion: a recipient the shield only DESCRIBES is
        -- covered, which is the field this card exists to exercise.
        Spec.assertEqWith s "the 3 combat damage to a Dog alice controls is prevented whole" (strikeFrom ourDog 3) (Just 0)
        -- Its twin, differing in the RECIPIENT alone and on the same board: a
        -- creature of alice's that is not a Dog takes its damage, so the case
        -- above cannot pass on a shield that covers everything.
        Spec.assertEqWith s "the 4 to alice's non-Dog Piker is marked in full" (strikeFrom ourPiker 4) (Just 4)
        -- CR 611.2c's own case: this Dog was not on the battlefield when the
        -- shield was created, and the shield covers it anyway.
        Spec.assertEqWith s "CR 611.2c the Dog that entered after the shield resolved is covered too" (strikeFrom latecomer 5) (Just 0)
        -- The other half of the description -- "you control" -- with the same
        -- creature type on the other side of the table.
        Spec.assertEqWith s "bob's own Dog is not covered, the clause saying you control" (strikeFrom theirDog 6) (Just 6)
        -- The proxies, after the behaviour.
        Spec.assertEqWith s "setup: the shield is one floating replacement" (length (StoredEffects.replacements (GameState.stored atBlockers))) 1
        Spec.assertEqWith s "setup: Pack Leader was declared as an attacker" (elem leaderId (S.attackerDeclarationsOf atBlockers)) True
        Spec.assertEqWith s "setup: the latecomer is on the battlefield and undamaged" (S.damageOf latecomer board) (Just 0)
      _ -> Spec.assertFailure s "fixture should have three creatures for alice and two for bob"

-- CR 615.5's ADDITIONAL EFFECT, whose producer is Test of Faith ({1}{W} Instant:
-- "Prevent the next 3 damage that would be dealt to target creature this turn.
-- For each 1 damage prevented this way, put a +1/+1 counter on that creature").
--
-- Not a triggered ability, and that is the whole of what these cases
-- discriminate. Test of Faith's 2004-12-01 ruling: "The +1/+1 counters are put
-- onto the creature at the same time the damage is prevented. If a 1/1 creature
-- would be dealt 6 damage, 3 damage is prevented and three +1/+1 counters are
-- put on the creature." Under a triggered reading the counters would wait for
-- the stack and CR 704.5g would have destroyed the creature first.
--
-- Two seats is enough: no relational text is under test -- the rider names the
-- shielded creature, never "an opponent" or "the defending player" -- so the
-- three-seat trap does not apply.
--
-- The COMBAT case really runs the combat steps through Parun.Engine.Engine, which
-- the other groups in this file avoid: the ordering under test is the one
-- between the rider and the step's state-based action check, and only the engine
-- has both.
testOfFaithSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
testOfFaithSpec s registry = Spec.describe s "Test of Faith (CR 615.5)" $ do
  let -- Put a board at declare attackers with alice active and bob defending,
  -- The other funnel: damage dealt by a RESOLVING ability rather than by combat,
  -- which drains the rider inside Parun.Engine.Resolve instead of inside the
  -- combat damage step. One ping against a shield of 3 -- so the counter count
  -- is 1, the amount THIS application prevented, and not the shield's printed 3.
  Spec.it s "CR 615.5 a shield spent by a resolving ability runs its rider too" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    testOfFaith <- S.printingOf s registry "Test of Faith"
    let base = S.landsInPlay plains 2
        (victim, g1) = S.addPermanent pikerPrinting S.alice base
        (pinger, g2) = S.addPermanent sorcerer S.alice g1
        (g3, spellId) = S.handOne testOfFaith g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        ping g = S.runPure (aimCreature victim) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        control = ping g3
    Spec.assertEqWith s "setup: the shield holds 3" (shieldsLeft shielded) [3]
    Spec.assertEqWith s "the ping's 1 damage is prevented, so nothing is marked" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and exactly one counter goes on -- the amount prevented, not the shield" (countersOn CounterKind.PlusOnePlusOne victim after) 1
    Spec.assertEqWith s "so the 2/1 is a 3/2" (S.powerToughnessOf victim after) (Just (3, 2))
    Spec.assertEqWith s "with 2 of the shield left (CR 615.7)" (shieldsLeft after) [2]
    Spec.assertEqWith s "and unshielded the same ping marks 1 and puts no counter on" (S.damageOf victim control, countersOn CounterKind.PlusOnePlusOne victim control) (Just 1, 0)

-- How many UNBOUNDED shields (CR 615.1 / 615.3) are on the board. `shieldsLeft`
-- above is the wrong reader for these: it reports a countdown amount, and this
-- row has none, so it answers the empty list whether the shield is installed or
-- not.
preventAllRows :: GameState.GameState -> Int
preventAllRows gs =
  let isPreventAll re = case re of
        ReplacementEffect.DamageR (DamageR.MkDamageR _ DamageRewrite.PreventAll _) -> True
        _ -> False
   in length (filter (isPreventAll . ActiveReplacement.effect) (StoredEffects.replacements (GameState.stored gs)))

-- Attacks with everything and blocks with NOTHING, so an attack's damage reaches
-- the defending player.
attackNoBlock :: Prompt.Prompt r -> r
attackNoBlock p = case p of
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.identityAnswer p

-- Put a board at declare attackers with BOB active and alice defending -- the
-- mirror of testOfFaithSpec's `atCombat`, since a shield over "you" is over the
-- player who activated it and it takes combat damage only when that player is
-- the one being attacked.
bobAttacks :: GameState.GameState -> GameState.GameState
bobAttacks gs =
  gs
    { GameState.turn =
        (GameState.turn gs)
          { TurnRegisters.activePlayer = S.bob,
            TurnRegisters.phase = Phase.Combat CombatStep.DeclareAttackers,
            -- CR 703.4h has already happened on this board, so the defending player is
            -- stated rather than derived (S.combatBoardOf's posture).
            TurnRegisters.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice]},
            TurnRegisters.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
          }
    }

-- Aim every target slot at one creature by FILTERING the offered set rather than
-- building a recipient, so a candidate the card's filter excludes cannot be
-- smuggled back in: an illegal aim leaves the slot empty instead of silently
-- becoming a legal one.
onlyCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
onlyCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature oid) . snd) sets
  _ -> S.identityAnswer p

-- CR 510.2 vs CR 608: a shield that names a KIND. Decorated Griffin ({4}{W}
-- Creature -- Griffin 2/3, flying) prints "{1}{W}: Prevent the next 1 combat
-- damage that would be dealt to you this turn" -- a counted shield (CR 615.7)
-- over a PLAYER, with no CR 615.5 clause, so the kind is the only thing under
-- test. Inkshield below is the same recipient WITH the CR 615.5 clause.
--
-- The discrimination needs both halves and a control each. A group using only
-- combat damage would pass identically on a shield that named no kind at all.
--
-- Numbers all distinct: the ping is 1 and the attack is 5, so alice ends on 19
-- where the kind is respected and 16 where the shield bites, against 20 and 15
-- for the two readings that are wrong.
decoratedGriffinSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
decoratedGriffinSpec s registry = Spec.describe s "Decorated Griffin (CR 510.2)" $ do
  Spec.it s "a combat-only shield leaves noncombat damage alone (CR 608)" $ do
    plains <- S.printingOf s registry "Plains"
    griffin <- S.printingOf s registry "Decorated Griffin"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let base = S.landsInPlay plains 2
        (bird, g1) = S.addPermanent griffin S.alice base
        (pinger, g2) = S.addPermanent sorcerer S.alice g1
        shielded = S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice bird (theAbility griffin) Monad.>> Stack.resolveTop)
        ping g = S.runPure (aimPlayer S.alice) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        -- The CONTROL is the same board with the ability never activated, so
        -- the only difference is the shield.
        control = ping g2
    Spec.assertEqWith s "setup: the activation installed a shield of 1" (shieldsLeft shielded) [1]
    Spec.assertEqWith s "the Sorcerer's noncombat 1 is dealt anyway (CR 608)" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "and the combat-only shield is untouched" (shieldsLeft after) [1]
    Spec.assertEqWith s "which is what the unshielded board does too" (S.lifeOf S.alice control) (Just 19)

-- CR 307.1: hand a player their own precombat main phase, so a sorcery they hold
-- is castable. `bobAttacks` above for the combat half.
inMainPhase :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
inMainPhase pid gs =
  gs
    { GameState.turn =
        (GameState.turn gs)
          { TurnRegisters.activePlayer = pid,
            TurnRegisters.phase = Phase.PrecombatMain
          }
    }

-- Put one player at an exact life total, so a board can be built where the very
-- next event would carry them past Worship's floor.
atLife :: PlayerId.PlayerId -> Integer -> GameState.GameState -> GameState.GameState
atLife pid n gs = gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}

-- Apply one damage batch under a given interpreter. Top-level rather than a
-- `where` binding because the answer is rank-2 and GHC will not infer it.
settleDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> [DamageEvent.DamageEvent] -> GameState.GameState
settleDamage answer gs batch = S.runPure answer gs (Damage.applyDamage Settle.none batch)

-- Aim every target slot at one creature. Mending Hands' slot is Pool.AnyTarget
-- (CR 115.4), whose creature members are tagged ToCreature.
aimCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Replacement" $ do
  fatigueSpec s registry
  mendingHandsSpec s registry
  auriokReplicaSpec s registry
  packLeaderSpec s registry
  testOfFaithSpec s registry
  decoratedGriffinSpec s registry
