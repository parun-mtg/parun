{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Parun.Engine.Turn: turn structure, the phase schedule, the CR 508.8 skips, and
-- CR 500.8's added phases -- Aggravated Assault and Relentless Assault for the
-- combat-and-main pair, Aurelia, the Warleader for one added from INSIDE a
-- combat phase, and Full Throttle for two combat phases and none of main.
--
-- Also CR 500.7's extra TURNS, which Engine.handoffTurn deals out, through Time
-- Warp and Final Fortune; Savor the Moment's CR 500.11 skip scoped to ONE of them
-- is data/scenarios/turn.
--
-- And CR 725's two procedures, which rewrite the schedule from inside a
-- resolution -- Time Stop for the turn and Mandate of Peace for the combat
-- phase.
module Parun.TurnSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Resolve.Effect as Resolve
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Engine.Turn as Turn
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.AddPhases as AddPhases
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.ExtraPhase as ExtraPhase
import qualified Parun.Types.ExtraTurn as ExtraTurn
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.ObjectId as ObjectId
import Parun.Types.Phase (Phase)
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PhaseAnchor as PhaseAnchor
import qualified Parun.Types.PhaseSelector as PhaseSelector
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.StepBegan as StepBegan
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

turnSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
turnSpec s = Spec.describe s "Turn" $ do
  Spec.it s "firstPhase is the untap step" $
    Spec.assertEqWith s "firstPhase" (Turn.firstPhase False) (Phase.Beginning BeginningStep.Untap)
  Spec.it s "a turn has twelve steps" $
    Spec.assertEqWith s "twelve" (length Turn.allPhases) 12
  Spec.it s "firstPhase and laterPhases reconstruct the turn template" $
    Spec.assertEqWith s "reconstruct" (Turn.laterPhases False) (Seq.fromList (drop 1 Turn.allPhases))
  -- CR 500.1 / 505.1a / Topsy Turvy's ruling: the phases run the other way
  -- round, each keeping its steps in order, and the main phase before combat
  -- is the turn's first, so the precombat one.
  Spec.it s "a turn whose phases are reversed opens at its end step and closes with its draw step" $
    Spec.assertEqWith
      s
      "reversed"
      (Turn.firstPhase True Seq.<| Turn.laterPhases True)
      ( Seq.fromList
          [ Phase.Ending EndingStep.EndStep,
            Phase.Ending EndingStep.Cleanup,
            Phase.PrecombatMain,
            Phase.Combat CombatStep.BeginningOfCombat,
            Phase.Combat CombatStep.DeclareAttackers,
            Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain,
            Phase.Beginning BeginningStep.Untap,
            Phase.Beginning BeginningStep.Upkeep,
            Phase.Beginning BeginningStep.DrawStep
          ]
      )
  -- CR 502.4 outright, CR 514.3's "normally" for the cleanup step -- whose
  -- CR 514.3a exception is Engine.grantsPriorityNow's question, not this
  -- one's (Parun.GameSpec's "extra cleanup step").
  Spec.it s "untap grants no priority, and cleanup none by the phase alone" $
    Spec.assertBool
      s
      ( not (Turn.grantsPriority (Phase.Beginning BeginningStep.Untap))
          && not (Turn.grantsPriority (Phase.Ending EndingStep.Cleanup))
      )
      "no priority"
  -- CR 514.3a: "another cleanup step begins" -- at the head, so it runs next,
  -- exactly as CR 510.4's second combat damage step does.
  Spec.it s "CR 514.3a spliceStep puts another cleanup step next" $ do
    Spec.assertEqWith
      s
      "the ordinary case: the cleanup step is the last of the turn"
      (Turn.spliceStep (Phase.Ending EndingStep.Cleanup) Seq.empty)
      (Seq.singleton (Phase.Ending EndingStep.Cleanup))
    Spec.assertEqWith
      s
      "and it goes IN FRONT of a CR 500.8 phase added after the ending phase"
      (Turn.spliceStep (Phase.Ending EndingStep.Cleanup) Turn.combatAndMainPhase)
      (Phase.Ending EndingStep.Cleanup Seq.<| Turn.combatAndMainPhase)
  Spec.it s "a turn never revisits a phase" $
    Spec.assertEqWith s "no repeats" (ListUtils.nubOrd Turn.allPhases) Turn.allPhases

turnDataSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
turnDataSpec s = Spec.describe s "TurnData" $ do
  Spec.it s "advance pops the schedule head into the current phase" $
    let gs0 = Setup.emptyGame S.bothPlayers
        gs =
          gs0
            { GameState.turn =
                (GameState.turn gs0)
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
                  }
            }
        after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.advance)
     in do
          Spec.assertEqWith s "phase" (TurnRegisters.phase (GameState.turn after)) (Phase.Combat CombatStep.BeginningOfCombat)
          Spec.assertEqWith s "remaining" (TurnRegisters.remaining (GameState.turn after)) (Seq.fromList [Phase.PostcombatMain])
  Spec.it s "advance on an empty schedule hands off the turn" $
    let gs0 = Setup.emptyGame S.bothPlayers
        gs =
          gs0
            { GameState.turn =
                (GameState.turn gs0)
                  { TurnRegisters.phase = Phase.Ending EndingStep.Cleanup,
                    TurnRegisters.remaining = Seq.empty,
                    TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.turnNumber = 1
                  }
            }
        after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.advance)
     in do
          Spec.assertEqWith s "new active player" (TurnRegisters.activePlayer (GameState.turn after)) S.bob
          Spec.assertEqWith s "phase reset" (TurnRegisters.phase (GameState.turn after)) (Turn.firstPhase False)
          Spec.assertEqWith s "schedule refilled" (TurnRegisters.remaining (GameState.turn after)) (Turn.laterPhases False)
          Spec.assertEqWith s "turn incremented" (TurnRegisters.turnNumber (GameState.turn after)) 2
  Spec.it s "a fresh game starts at untap with the rest of the turn scheduled" $
    let gs = Setup.emptyGame S.bothPlayers
     in do
          Spec.assertEqWith s "phase" (TurnRegisters.phase (GameState.turn gs)) (Turn.firstPhase False)
          Spec.assertEqWith s "remaining" (TurnRegisters.remaining (GameState.turn gs)) (Turn.laterPhases False)
  -- CR 500.1: only the FIRST step of a phase that has steps opens that
  -- phase, and both main phases open none -- CR 505.2 makes each of them a
  -- single schedule entry, which PhaseSelector.Step already names.
  --
  -- This is what keeps CR 614.10's "once a step, phase, or turn has started,
  -- it can no longer be skipped" true at phase grain: Engine.runStep asks the
  -- whole-phase question only where this answers Just.
  Spec.it s "CR 500.1 phaseBeginningAt opens a phase only at its first step" $
    Spec.assertEqWith
      s
      "one Just per stepped phase"
      (fmap (Turn.phaseBeginningAt Nothing) Turn.allPhases)
      [ Just PhaseSelector.BeginningPhase,
        Nothing,
        Nothing,
        Nothing,
        Just PhaseSelector.CombatPhase,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Just PhaseSelector.EndingPhase,
        Nothing
      ]
  -- CR 500.5's other grain, and phaseBeginningAt's exact mirror: a phase ENDS
  -- as its LAST step ends. CR 511.3 names combat's ("After the end of combat
  -- step ends, the combat phase is over"), CR 501.1 the beginning phase's three
  -- steps and CR 512.1 the ending phase's two.
  --
  -- The two lists must not agree anywhere except on the Nothings: a phase begins
  -- at one step and ends at another, and an implementation that confused them
  -- would expire an "until end of combat" effect at the START of combat.
  Spec.it s "CR 500.5 phaseEndingAt closes a phase only at its last step" $
    Spec.assertEqWith
      s
      "one Just per stepped phase"
      (fmap Turn.phaseEndingAt Turn.allPhases)
      [ Nothing,
        Nothing,
        Just PhaseSelector.BeginningPhase,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Nothing,
        Just PhaseSelector.CombatPhase,
        Nothing,
        Nothing,
        Just PhaseSelector.EndingPhase
      ]
  -- CR 500.1: "during" is CONTAINMENT, and it is what tells a rider naming a
  -- whole phase (Jade Statue's "only during combat") from one naming a step of
  -- it (Desert's "only during the end of combat step"). Both are PhaseSelectors,
  -- so nothing but this function distinguishes them.
  Spec.it s "CR 500.1/506.1 inWindow: a whole-phase window covers every step of it" $ do
    Spec.assertEqWith
      s
      "CombatPhase covers all five of CR 506.1's steps and nothing else"
      (filter (Turn.inWindow PhaseSelector.CombatPhase) Turn.allPhases)
      [ Phase.Combat CombatStep.BeginningOfCombat,
        Phase.Combat CombatStep.DeclareAttackers,
        Phase.Combat CombatStep.DeclareBlockers,
        Phase.Combat CombatStep.CombatDamage,
        Phase.Combat CombatStep.EndOfCombat
      ]
    Spec.assertEqWith
      s
      "CR 501.1 BeginningPhase covers untap, upkeep and draw"
      (filter (Turn.inWindow PhaseSelector.BeginningPhase) Turn.allPhases)
      [ Phase.Beginning BeginningStep.Untap,
        Phase.Beginning BeginningStep.Upkeep,
        Phase.Beginning BeginningStep.DrawStep
      ]
    Spec.assertEqWith
      s
      "CR 512.1 EndingPhase covers the end step and the cleanup step"
      (filter (Turn.inWindow PhaseSelector.EndingPhase) Turn.allPhases)
      [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
  -- The other arm, and the reason Desert goes on working unchanged: a Step
  -- window matches exactly its own schedule entry. CR 505.2 makes a main phase
  -- one such entry, so naming it and naming "the phase" are the same act -- and
  -- there is deliberately no whole-phase spelling of either main phase to
  -- disagree with it.
  Spec.it s "CR 500.1 inWindow: a Step window matches exactly one schedule entry" $ do
    Spec.assertEqWith
      s
      "Desert's end of combat step, and only it"
      (filter (Turn.inWindow (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat))) Turn.allPhases)
      [Phase.Combat CombatStep.EndOfCombat]
    Spec.assertEqWith
      s
      "CR 505.2 a stepless main phase is its own window"
      (filter (Turn.inWindow (PhaseSelector.Step Phase.PostcombatMain)) Turn.allPhases)
      [Phase.PostcombatMain]
  -- CR 500.11: "to skip a step, phase, or turn is to proceed past it as
  -- though it didn't exist" -- past the PHASE, so its four remaining steps go
  -- and the postcombat main phase (CR 511.3) is what is left.
  --
  -- Positional, like dropSkippedCombatSteps: a SECOND combat phase later in
  -- the turn (CR 500.8) survives untouched, which is what a filter over the
  -- whole schedule would get wrong.
  Spec.it s "CR 500.11 dropRestOfPhase drops this combat phase and leaves a later one" $
    let remaining =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareAttackers,
              Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain,
              Phase.Combat CombatStep.BeginningOfCombat,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.Ending EndingStep.EndStep
            ]
     in Spec.assertEqWith
          s
          "only the current phase's steps go"
          (Turn.dropRestOfPhase (Phase.Combat CombatStep.BeginningOfCombat) remaining)
          ( Seq.fromList
              [ Phase.PostcombatMain,
                Phase.Combat CombatStep.BeginningOfCombat,
                Phase.Combat CombatStep.EndOfCombat,
                Phase.Ending EndingStep.EndStep
              ]
          )

skipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
skipSpec s registry = Spec.describe s "Skip" $ do
  Spec.it s "CR 511.3 thisPhase inside a combat phase ends at ITS end of combat" $
    -- Two whole combat phases back to back -- the arrangement CR 500.8
    -- permits and Aurelia, the Warleader builds. Splitting at the FIRST end
    -- of combat step is what leaves the second one whole.
    let remaining =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.Combat CombatStep.BeginningOfCombat,
              Phase.Combat CombatStep.DeclareAttackers,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain
            ]
        expected =
          ( Seq.fromList
              [ Phase.Combat CombatStep.DeclareBlockers,
                Phase.Combat CombatStep.CombatDamage,
                Phase.Combat CombatStep.EndOfCombat
              ],
            Seq.fromList
              [ Phase.Combat CombatStep.BeginningOfCombat,
                Phase.Combat CombatStep.DeclareAttackers,
                Phase.Combat CombatStep.EndOfCombat,
                Phase.PostcombatMain
              ]
          )
     in Spec.assertEqWith
          s
          "split at the first end of combat, inclusive"
          (Turn.thisPhase (Phase.Combat CombatStep.DeclareAttackers) remaining)
          expected
  Spec.it s "CR 505.2 thisPhase in a main phase has no steps of its own" $
    -- "The main phase has no steps", so there is nothing of it left in the
    -- schedule and everything remaining is already after it.
    let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
     in Spec.assertEqWith s "empty prefix" (Turn.thisPhase Phase.PrecombatMain remaining) (Seq.empty, remaining)
  Spec.it s "CR 500.9 an end step added after the end step stays in the ending phase" $
    let added = Phase.Ending EndingStep.EndStep
        remaining = Seq.fromList [added, Phase.Ending EndingStep.Cleanup]
     in do
          Spec.assertEqWith s "no phase begins" (Turn.phaseBeginningAt (Just added) added) Nothing
          Spec.assertEqWith s "whole prefix" (Turn.thisPhase added remaining) (remaining, Seq.empty)
  Spec.it s "CR 500.8 a combat phase after a combat phase begins anew" $
    Spec.assertEqWith
      s
      "combat begins"
      (Turn.phaseBeginningAt (Just (Phase.Combat CombatStep.EndOfCombat)) (Phase.Combat CombatStep.BeginningOfCombat))
      (Just PhaseSelector.CombatPhase)
  Spec.it s "thisPhase yields an empty prefix when this phase's final step is gone" $
    -- Unreachable from either caller, and asserted anyway: dropping nothing
    -- is the safer failure than treating the whole rest of the turn as this
    -- phase.
    let remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep]
     in Spec.assertEqWith
          s
          "empty prefix"
          (Turn.thisPhase (Phase.Combat CombatStep.EndOfCombat) remaining)
          (Seq.empty, remaining)
  Spec.it s "CR 508.8 dropSkippedCombatSteps removes declare blockers and combat damage" $
    let full =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain
            ]
        expected = Seq.fromList [Phase.Combat CombatStep.EndOfCombat, Phase.PostcombatMain]
     in Spec.assertEqWith s "dropped" (Turn.dropSkippedCombatSteps (Phase.Combat CombatStep.DeclareAttackers) full) expected
  Spec.it s "CR 500.8 dropSkippedCombatSteps spares a LATER combat phase's steps" $
    -- The schedule a CR 500.8 additional combat phase leaves behind: this
    -- combat's tail, then an additional main phase, then the turn's normal
    -- combat phase in full. CR 508.8 skipped THIS combat, so only the two
    -- steps before this phase's end of combat step (CR 511.3: "after the end
    -- of combat step ends, the combat phase is over") may go.
    let full =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain,
              Phase.Combat CombatStep.BeginningOfCombat,
              Phase.Combat CombatStep.DeclareAttackers,
              Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain
            ]
        expected =
          Seq.fromList
            [ Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain,
              Phase.Combat CombatStep.BeginningOfCombat,
              Phase.Combat CombatStep.DeclareAttackers,
              Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain
            ]
     in Spec.assertEqWith
          s
          "only this phase's steps dropped"
          (Turn.dropSkippedCombatSteps (Phase.Combat CombatStep.DeclareAttackers) full)
          expected
  Spec.it s "CR 508.8 a creature put onto the battlefield attacking keeps the two steps" $ do
    -- The rule's SECOND clause on its own, with nothing declared. A DIRECT
    -- call, so the clause is stated with no card in the way: it is what
    -- stops the fix above from being narrowed to the declaration.
    -- Parun.CombatSpec's MeanderingTowershell group reaches the same clause
    -- through a card, and the two are kept because they fail for different
    -- reasons -- this one for a flag dropped from putOntoBattlefieldAttacking,
    -- that one for anything between the attack and the return going wrong.
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, ours, _) = S.combatBoardOf [piker] []
        joined = S.runPure S.identityAnswer base (Foldable.traverse_ (Combat.putOntoBattlefieldAttacking Combat.Any) ours)
        after = Combat.skipEmptyCombat joined
        remaining = foldr (:) [] (TurnRegisters.remaining (GameState.turn after))
    Spec.assertEqWith s "no declaration was made" (S.attackerDeclarationsOf after) []
    Spec.assertBool s (elem (Phase.Combat CombatStep.DeclareBlockers) remaining) "blockers step kept"
    Spec.assertBool s (elem (Phase.Combat CombatStep.CombatDamage) remaining) "damage step kept"

-- The schedule Engine.advance leaves once the precombat main phase is current:
-- everything after it in an ordinary turn (Turn.allPhases).
afterPrecombatMain :: Seq Phase
afterPrecombatMain = S.phasesAfter Phase.PrecombatMain

-- alice, in her precombat main phase with priority, controlling five untapped
-- Mountains (exactly the {3}{R}{R} activation), an Aggravated Assault, and one
-- TAPPED creature of the given printing. bob has a tapped Goblin Piker, which
-- the ability's "you control" must leave alone.
assaultBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId, ObjectId)
assaultBoard mountain assault mine piker =
  let (enchantment, gs1) = S.addPermanent assault S.alice (S.landsInPlay mountain 5)
      (ours, gs2) = S.addPermanent mine S.alice gs1
      (theirs, gs3) = S.addPermanent piker S.bob gs2
      gs =
        Game.overTurn
          ( \registers ->
              registers
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.remaining = afterPrecombatMain
                }
          )
          (S.tapObject theirs (S.tapObject ours gs3))
   in (gs, enchantment, ours, theirs)

-- The card's one printed activated ability.
assaultAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
assaultAbility assault = case Face.activatedAbilities (S.combinedFace assault) of
  [] -> Nothing
  ability : _ -> Just ability

-- Activate it and let it resolve, through the real activation path (the
-- Bonesplitter shape in Parun.AuraSpec) -- not Resolve.applyEffect, so the
-- {3}{R}{R} is genuinely paid off the board. The CR 307.5 rider is NOT checked
-- here: ActivationRestriction.restrictionsOk gates Action.legalActions, and a direct
-- activateAbility call goes around it, so the test that cares asks
-- Activatable.activatable itself.
activateAssault :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> ObjectId -> GameState.GameState -> GameState.GameState
activateAssault ability enchantment gs =
  let activated = snd (Engine.runGamePure S.identityAnswer gs (Activate.activateAbility S.alice enchantment ability))
   in snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)

-- Run whole steps until the turn hands off (Engine.advance on an empty
-- schedule) or the game ends, keeping the phase each step ran in alongside the
-- board the turn left. Bounded so a bug cannot loop forever.
runTurn :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> (GameState.GameState, [Phase])
runTurn answer gs0 =
  let turn = TurnRegisters.turnNumber (GameState.turn gs0)
      go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || TurnRegisters.turnNumber (GameState.turn g) /= turn
          then (g, [])
          else
            let (final, later) = go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
             in (final, TurnRegisters.phase (GameState.turn g) : later)
   in go 32 gs0

-- alice in her precombat main phase with priority, six untapped Mountains
-- (exactly Full Throttle's {4}{R}{R}), one Settled creature of the given
-- printing, and the spell in hand.
throttleBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId)
throttleBoard mountain throttle piker =
  let (attacker, gs1) = S.addPermanent piker S.alice (S.landsInPlay mountain 6)
      (gs2, spell) = S.handOne throttle gs1
   in ( gs2
          { GameState.turn =
              (GameState.turn gs2)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.remaining = afterPrecombatMain
                }
          },
        spell,
        attacker
      )

-- Cast a spell from alice's hand through the real path -- Cast.castSpell pays
-- its cost off the board -- and let it resolve.
castAndResolve :: ObjectId -> GameState.GameState -> GameState.GameState
castAndResolve = castAndResolveWith S.identityAnswer

castAndResolveWith :: (forall r. Prompt.Prompt r -> r) -> ObjectId -> GameState.GameState -> GameState.GameState
castAndResolveWith answer spell gs =
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice spell))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- CR 500.8's added phases, end to end, through the cards in the pool that add
-- any: Aggravated Assault ({3}{R}{R}: Untap all creatures you control. After
-- this main phase, there is an additional combat phase followed by an additional
-- main phase. Activate only as a sorcery.) and Relentless Assault ({2}{R}{R}:
-- Untap all creatures that attacked this turn. After this main phase, there is
-- an additional combat phase followed by an additional main phase.)
-- Turn.splicePhases after the phase under way, which always names one.
spliceHere :: Phase.Phase -> [ExtraPhase.ExtraPhase] -> Seq.Seq Phase.Phase -> Seq.Seq Phase.Phase
spliceHere phase extras = Foldable.fold . Turn.splicePhases (AddPhases.MkAddPhases PhaseAnchor.ThisPhase extras) 0 phase

extraPhaseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extraPhaseSpec s registry = Spec.describe s "ExtraPhase" $ do
  Spec.it s "CR 500.8 splicePhases from a MAIN phase goes at the head" $
    -- CR 505.2: a main phase has no steps, so nothing of it is left in the
    -- schedule and "directly after the specified phase" is the head. This is
    -- Aggravated Assault's case, and the reason its behaviour is unchanged.
    let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
     in Spec.assertEqWith
          s
          "directly after this phase"
          (spliceHere Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain] remaining)
          (Turn.combatAndMainPhase <> remaining)
  Spec.it s "CR 500.8 splicePhases from INSIDE a combat phase goes after its end of combat" $
    -- Aurelia, the Warleader's case. Her trigger resolves in the declare
    -- attackers step, where this phase's own later steps are still in
    -- `remaining` -- so the head is INSIDE the phase the added one must
    -- follow, and CR 511.3's boundary is what puts it in the right place.
    let remaining =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain
            ]
        expected =
          Seq.fromList
            [ Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat
            ]
            <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
            <> Seq.fromList [Phase.PostcombatMain]
     in Spec.assertEqWith
          s
          "not inside the current phase"
          (spliceHere (Phase.Combat CombatStep.DeclareAttackers) [ExtraPhase.ExtraCombat] remaining)
          expected
  Spec.it s "CR 500.8 splicePhases adds directly after a combat phase in its last step, ahead of one already added" $
    -- Resolving in the end of combat step, with an added combat phase next:
    -- that phase's own end of combat step is not this phase's.
    let added = Turn.expandExtraPhase ExtraPhase.ExtraCombat
        remaining = added <> Seq.fromList [Phase.PostcombatMain]
     in Spec.assertEqWith
          s
          "ahead of the phase already scheduled"
          (spliceHere (Phase.Combat CombatStep.EndOfCombat) [ExtraPhase.ExtraMain] remaining)
          (Seq.fromList [Phase.PostcombatMain] <> remaining)
  Spec.it s "CR 500.8 / 500.10 splicePhases from a lone upkeep goes at the head, ahead of a later beginning phase" $
    -- Obeka, Splitter of Seconds' added upkeep is a phase of its own with no
    -- draw step, so the draw step still scheduled is a later phase's.
    let remaining =
          Turn.expandExtraPhase ExtraPhase.ExtraUpkeep
            <> Turn.expandExtraPhase ExtraPhase.ExtraBeginning
            <> Seq.fromList [Phase.PostcombatMain]
     in Spec.assertEqWith
          s
          "directly after the lone upkeep"
          (spliceHere (Phase.Beginning BeginningStep.Upkeep) [ExtraPhase.ExtraMain] remaining)
          (Seq.fromList [Phase.PostcombatMain] <> remaining)
  Spec.it s "CR 500.8 splicePhases inserts a multi-phase list in written order" $
    -- Full Throttle's "there are two additional combat phases": two whole
    -- combat phases, back to back, and no main phase between them.
    Spec.assertEqWith
      s
      "two combat phases, back to back"
      (spliceHere Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat] Seq.empty)
      (Turn.expandExtraPhase ExtraPhase.ExtraCombat <> Turn.expandExtraPhase ExtraPhase.ExtraCombat)
  Spec.it s "CR 500.8 / 505.1b splicePhases after the second main phase counts the main phases begun" $ do
    -- World at War: from the first main phase, after the next main phase;
    -- from the upkeep, after the one after it; from the second, after it.
    let schedule = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain, Phase.Ending EndingStep.EndStep]
        second = Turn.splicePhases (AddPhases.MkAddPhases PhaseAnchor.SecondMainPhase [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain])
        afterSecond = Just (Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain] <> Turn.combatAndMainPhase <> Seq.fromList [Phase.Ending EndingStep.EndStep])
    Spec.assertEqWith s "from the first main phase" (second 1 Phase.PrecombatMain schedule) afterSecond
    Spec.assertEqWith s "from the upkeep" (second 0 (Phase.Beginning BeginningStep.Upkeep) (Seq.fromList [Phase.Beginning BeginningStep.DrawStep, Phase.PrecombatMain] <> schedule)) (fmap (Seq.fromList [Phase.Beginning BeginningStep.DrawStep, Phase.PrecombatMain] <>) afterSecond)
    Spec.assertEqWith s "from the second main phase" (second 2 Phase.PostcombatMain (Seq.fromList [Phase.Ending EndingStep.EndStep])) (Just (Turn.combatAndMainPhase <> Seq.fromList [Phase.Ending EndingStep.EndStep]))
  Spec.it s "CR 500.8 / 505.1b splicePhases after the second main phase adds nothing once it is over" $
    -- World at War's 2010-06-15 ruling: cast in a third main phase, no
    -- phases are created.
    Spec.assertEqWith
      s
      "no second main phase left"
      (Turn.splicePhases (AddPhases.MkAddPhases PhaseAnchor.SecondMainPhase [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain]) 3 Phase.PostcombatMain (Seq.fromList [Phase.Ending EndingStep.EndStep]))
      Nothing
  Spec.it s "CR 500.8 whole card: a vigilant creature attacks in the added combat phase AND the turn's own" $ do
    -- CR 506.1's five steps really run twice: Windseeker Centaur has
    -- vigilance (CR 702.20b: attacking does not tap it), so it is a legal
    -- attacker again in the second declare attackers step, and bob takes 2
    -- in each combat damage step.
    mountain <- S.printingOf s registry "Mountain"
    assault <- S.printingOf s registry "Aggravated Assault"
    centaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, enchantment, _, _) = assaultBoard mountain assault centaur piker
    case assaultAbility assault of
      Nothing -> Spec.assertFailure s "Aggravated Assault should print one activated ability"
      Just ability -> do
        let (played, ran) = runTurn (S.attackTo S.bob) (activateAssault ability enchantment gs)
        Spec.assertEqWith
          s
          "the turn ran a whole extra combat phase and main phase"
          ran
          [ Phase.PrecombatMain,
            Phase.Combat CombatStep.BeginningOfCombat,
            Phase.Combat CombatStep.DeclareAttackers,
            Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain,
            Phase.Combat CombatStep.BeginningOfCombat,
            Phase.Combat CombatStep.DeclareAttackers,
            Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain,
            Phase.Ending EndingStep.EndStep,
            Phase.Ending EndingStep.Cleanup
          ]
        Spec.assertEqWith s "bob took 2 in each of the two combats" (S.lifeOf S.bob played) (Just 16)
  Spec.it s "CR 500.8 Aurelia's second attack adds no third combat phase" $ do
    -- "For the first time each turn" is load-bearing: without it Aurelia
    -- attacks in the phase she added, adds another, and the turn never ends.
    aurelia <- S.printingOf s registry "Aurelia, the Warleader"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [aurelia, piker] []
        (played, ran) = runTurn (S.attackTo S.bob) gs
    Spec.assertEqWith
      s
      "exactly two combat phases, and the second adds nothing"
      ran
      [ Phase.Combat CombatStep.DeclareAttackers,
        Phase.Combat CombatStep.DeclareBlockers,
        Phase.Combat CombatStep.CombatDamage,
        Phase.Combat CombatStep.EndOfCombat,
        Phase.Combat CombatStep.BeginningOfCombat,
        Phase.Combat CombatStep.DeclareAttackers,
        Phase.Combat CombatStep.DeclareBlockers,
        Phase.Combat CombatStep.CombatDamage,
        Phase.Combat CombatStep.EndOfCombat,
        Phase.PostcombatMain,
        Phase.Ending EndingStep.EndStep,
        Phase.Ending EndingStep.Cleanup
      ]
    -- 3 + 2 in each of the two combats.
    Spec.assertEqWith s "bob took both combats" (S.lifeOf S.bob played) (Just 10)
  Spec.it s "CR 603.7b whole card: Full Throttle's delayed trigger fires at EVERY combat this turn" $ do
    -- The falsifier for CR 603.7b's one shot. "At the beginning of each
    -- combat this turn" is a STATED duration, so the ability stays armed and
    -- fires at all three of the turn's beginning of combat steps -- the two
    -- it added and the turn's own. Under the old store it would fire once,
    -- the Piker would stay tapped from its first attack (CR 508.1f), and bob
    -- would take 2 instead of 6.
    mountain <- S.printingOf s registry "Mountain"
    throttle <- S.printingOf s registry "Full Throttle"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spell, _) = throttleBoard mountain throttle piker
        (played, ran) = runTurn (S.attackTo S.bob) (castAndResolve spell gs)
        combatPhase = Turn.expandExtraPhase ExtraPhase.ExtraCombat
    Spec.assertEqWith
      s
      "three whole combat phases ran"
      ran
      ( [Phase.PrecombatMain]
          <> concat (replicate 3 (Foldable.toList combatPhase))
          <> [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
      )
    -- The Piker attacked in every one of them, which it could only do if the
    -- delayed ability untapped it before each -- and it found it by
    -- Filter.DeclaredThisTurn Attacker, since CR 511.3 had cleared the combat record.
    Spec.assertEqWith s "bob took 2 in each of the three combats" (S.lifeOf S.bob played) (Just 14)
    -- CR 514.2 ends the stated duration, so nothing is left armed.
    Spec.assertEqWith s "and the store is empty by the end of the turn" (Seq.length (StoredEffects.delayedTriggers (GameState.stored played))) 0

-- Aim every target slot at one player. Time Warp's slot is Pool.Players, so a
-- recipient tagged for any other pool is not in its legal set at all.
aimPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer pid))) sets
  _ -> S.identityAnswer p

-- alice in her precombat main phase with priority, five untapped Islands per
-- Time Warp (exactly {3}{U}{U} each) and that many Time Warps in hand. Both
-- libraries are stocked, because these run SEVERAL whole turns and each one's
-- draw step takes a card -- an empty library would end the game by CR 704.5b
-- before the turn order could be read off. `fortuneBoard` and the
-- Pact board below stock them for the same reason.
warpBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (GameState.GameState, [ObjectId])
warpBoard island warp piker n =
  let addOne (ids, g) _ = let (oid, g1) = S.addHandCard warp S.alice g in (ids <> [oid], g1)
      (held, withHand) = List.foldl' addOne ([], S.landsInPlay island (5 * n)) [1 .. n]
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard piker pid g1)) g [1 .. (10 :: Int)]
   in ( (stock (stock withHand S.alice) S.bob)
          { GameState.turn =
              (GameState.turn (stock (stock withHand S.alice) S.bob))
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          },
        held
      )

-- The seats the pending extra turns will be dealt out to, most recently created
-- first -- GameState.extraTurns with each entry's own skips (which only Savor the
-- Moment writes) set aside.
takersOf :: GameState.GameState -> [PlayerId.PlayerId]
takersOf = fmap ExtraTurn.taker . GameState.extraTurns

-- The seats the next `n` turns are dealt out to, in order, by running each turn
-- to its handoff. The one observable CR 500.7 is about.
turnTakers :: Int -> GameState.GameState -> [PlayerId.PlayerId]
turnTakers n gs =
  if n <= 0
    then []
    else
      let next = fst (runTurn S.identityAnswer gs)
       in TurnRegisters.activePlayer (GameState.turn next) : turnTakers (n - 1) next

-- CR 500.7's extra turns, end to end, through the card that creates one for
-- ANOTHER player: Time Warp ({3}{U}{U} Sorcery, "Target player takes an extra
-- turn after this one."). Savor the Moment creates one too, and what it adds --
-- a skip scoped to that turn -- is data/scenarios/turn's.
extraTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extraTurnSpec s registry = Spec.describe s "ExtraTurn" $ do
  let boardOf n = do
        island <- S.printingOf s registry "Island"
        warp <- S.printingOf s registry "Time Warp"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (warpBoard island warp piker n)
  -- The control: without the spell the turn hands off down the seating
  -- order, which is what every case below is measured against.
  Spec.it s "CR 103.1 without an extra turn the seating order alternates" $ do
    (gs, _) <- boardOf 1
    Spec.assertEqWith s "bob, alice, bob follow alice's turn" (turnTakers 3 gs) [S.bob, S.alice, S.bob]
  Spec.it s "CR 500.7 whole card: Time Warp aimed at yourself takes the next turn" $ do
    (gs, held) <- boardOf 1
    case held of
      [spell] -> do
        let resolved = castAndResolveWith (aimPlayer S.alice) spell gs
        Spec.assertEqWith s "one turn was created" (takersOf resolved) [S.alice]
        -- CR 500.7: "directly after the specified turn" -- alice takes
        -- it immediately, and it is a TURN, not a continuation of this
        -- one, so the turn number moves.
        let (next, _) = runTurn S.identityAnswer resolved
        Spec.assertEqWith s "alice takes it rather than bob" (TurnRegisters.activePlayer (GameState.turn next)) S.alice
        Spec.assertEqWith s "and it is a turn of its own" (TurnRegisters.turnNumber (GameState.turn next)) 2
        Spec.assertEqWith s "the pending stack is spent" (takersOf next) []
        -- CR 500.7 ADDS a turn; it does not reorder the rest. Once the
        -- extra turn is taken the seating order picks up where it was.
        Spec.assertEqWith s "then the ordinary order resumes" (turnTakers 2 next) [S.bob, S.alice]
      _ -> Spec.assertFailure s "warpBoard should deal exactly one Time Warp"
  -- THE ANCHOR CASE. CR 500.7 adds a turn "directly after the specified
  -- turn" and takes none away, so bob's OWN turn still follows alice's.
  -- Fails against a handoff anchored on TurnRegisters.activePlayer, which
  -- would walk from bob after the extra turn and hand back to alice --
  -- silently eating the ordinary turn the rules never touched.
  Spec.it s "CR 500.7 an opponent's extra turn does not consume their ordinary one" $ do
    (gs, held) <- boardOf 1
    case held of
      [spell] -> do
        let resolved = castAndResolveWith (aimPlayer S.bob) spell gs
        Spec.assertEqWith s "bob's turn was created" (takersOf resolved) [S.bob]
        Spec.assertEqWith
          s
          "bob's extra turn, then bob's own, and only then alice's"
          (turnTakers 3 resolved)
          [S.bob, S.bob, S.alice]
      _ -> Spec.assertFailure s "warpBoard should deal exactly one Time Warp"
  -- THE PROVING CASE for the last sentence of CR 500.7: "the most
  -- recently created turn will be taken first." Two Time Warps resolve
  -- in alice's turn, the first naming BOB and the second naming ALICE,
  -- so alice's turn is the more recent one and goes first.
  --
  -- Discriminating in both directions, which is why the two are aimed
  -- this way round rather than the other: a QUEUE would deal bob's turn
  -- out first and give [bob, alice, bob, alice] -- which is also exactly
  -- what an engine with NO extra turns at all produces, since the
  -- seating order alternates. Only the stack reading answers alice
  -- first.
  Spec.it s "CR 500.7 two extra turns are a stack: most recently created first" $ do
    (gs, held) <- boardOf 2
    case held of
      [first_, second] -> do
        let resolved =
              castAndResolveWith (aimPlayer S.alice) second $
                castAndResolveWith (aimPlayer S.bob) first_ gs
        Spec.assertEqWith s "pending, most recent at the head" (takersOf resolved) [S.alice, S.bob]
        Spec.assertEqWith
          s
          "alice's extra turn, then bob's, then bob's ordinary one"
          (turnTakers 4 resolved)
          [S.alice, S.bob, S.bob, S.alice]
      _ -> Spec.assertFailure s "warpBoard should deal exactly two Time Warps"

-- Run `n` whole turns. `runTurn` stops at the handoff, so the board it hands
-- back is positioned AT the next turn's untap step with that step not yet run --
-- which is why each assertion below is read after the turn it is about has been
-- run by one more call, not after the call that begins it.
runTurns :: Int -> GameState.GameState -> GameState.GameState
runTurns n gs = if n <= 0 then gs else runTurns (n - 1) (fst (runTurn S.identityAnswer gs))

-- alice in her precombat main phase holding Final Fortune and Time Warp, with
-- exactly {R}{R} and {3}{U}{U} to cast them, both libraries stocked for the
-- turns these run.
fortuneBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId, ObjectId)
fortuneBoard island mountain fortune warp piker =
  let (fortuneId, withFortune) = S.addHandCard fortune S.alice (S.landsFor mountain S.alice 2 (S.landsInPlay island 5))
      (warpId, withWarp) = S.addHandCard warp S.alice withFortune
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard piker pid g1)) g [1 .. (10 :: Int)]
   in ( (stock (stock withWarp S.alice) S.bob)
          { GameState.turn =
              (GameState.turn (stock (stock withWarp S.alice) S.bob))
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice,
                  TurnRegisters.remaining = S.phasesAfter Phase.PrecombatMain
                }
          },
        fortuneId,
        warpId
      )

-- CR 603.7a / 500.7: a delayed ability armed for "that turn", the extra turn
-- the same spell created, through Final Fortune ({R}{R} Instant, "Take an extra
-- turn after this one. At the beginning of that turn's end step, you lose the
-- game.") and Chance for Glory, which adds indestructible to the same text.
thatTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
thatTurnSpec s registry = Spec.describe s "ThatExtraTurn" $ do
  let boardOf = do
        island <- S.printingOf s registry "Island"
        mountain <- S.printingOf s registry "Mountain"
        fortune <- S.printingOf s registry "Final Fortune"
        warp <- S.printingOf s registry "Time Warp"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (fortuneBoard island mountain fortune warp piker)
  Spec.it s "CR 603.7a whole card: Final Fortune's controller loses at its extra turn's end step" $ do
    (gs, fortune, _) <- boardOf
    let resolved = castAndResolve fortune gs
        atExtra = runTurns 1 resolved
        afterExtra = runTurns 1 atExtra
    Spec.assertBool s (S.alice `elem` Game.stillPlaying atExtra) "alice survives the turn she cast it in"
    Spec.assertEqWith s "the extra turn is turn 2, and alice's" (TurnRegisters.turnNumber (GameState.turn atExtra), TurnRegisters.activePlayer (GameState.turn atExtra)) (2, S.alice)
    Spec.assertEqWith s "alice lost during it" (Game.stillPlaying afterExtra) [S.bob]
    Spec.assertEqWith s "at its end step" (TurnRegisters.phase (GameState.turn afterExtra)) (Phase.Ending EndingStep.EndStep)
  -- THE PROVING CASE. Final Fortune creates extra turn A; Time Warp, cast
  -- after it in the same turn, creates extra turn B, which CR 500.7 takes
  -- first. "That turn" is A: a "your next turn" reading would settle on B
  -- and lose one turn early.
  Spec.it s "CR 500.7 the loss waits for Final Fortune's own extra turn, not a later-created one" $ do
    (gs, fortune, warp) <- boardOf
    let resolved = castAndResolveWith (aimPlayer S.alice) warp (castAndResolve fortune gs)
        atWarpTurn = runTurns 1 resolved
        atFortuneTurn = runTurns 1 atWarpTurn
        afterFortuneTurn = runTurns 1 atFortuneTurn
    Spec.assertEqWith s "Time Warp's turn is turn 2, and alice's" (TurnRegisters.turnNumber (GameState.turn atWarpTurn), TurnRegisters.activePlayer (GameState.turn atWarpTurn)) (2, S.alice)
    Spec.assertBool s (S.alice `elem` Game.stillPlaying atFortuneTurn) "alice survives Time Warp's turn, end step and all"
    Spec.assertEqWith s "Final Fortune's is turn 3, and alice's" (TurnRegisters.turnNumber (GameState.turn atFortuneTurn), TurnRegisters.activePlayer (GameState.turn atFortuneTurn)) (3, S.alice)
    Spec.assertEqWith s "and alice loses at its end step" (Game.stillPlaying afterFortuneTurn) [S.bob]

-- Casts the first castable spell offered and passes otherwise, deferring every
-- other prompt to S.identityAnswer. This is the CR 725.1f discriminator: under a
-- correct implementation alice never gets a priority window with an empty stack
-- in her precombat main phase, so her sorcery stays in hand; an implementation
-- that rewrites the schedule but grants the window anyway lets her cast it.
castingFirst :: Prompt.Prompt r -> r
castingFirst p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast {} -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  _ -> S.identityAnswer p

-- The objects in one player's zone, by the name of the card each is a copy of --
-- Nothing for an object that is a copy of no card, which is how a stray ability
-- filed into a zone shows up rather than throwing.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- alice in her precombat main phase with priority, nine untapped Islands (Time
-- Stop's {4}{U}{U} and Divination's {2}{U} together), a Sinister Gnarlbark, and
-- both spells in hand; bob with one Mountain and a Burst Lightning ALREADY ON THE
-- STACK aimed at alice.
--
-- Every element earns its place:
--
--   * Sinister Gnarlbark's only ability is "at the beginning of your end step,
--     draw a card and blight 1" -- unconditional, and observable as a library
--     count and a -1/-1 counter rather than as a flag. It is CR 725.1e's
--     negative.
--   * bob's Burst Lightning is a SECOND object on the stack, owned by the other
--     seat, so CR 725.1b's exile cannot be confused with the resolving spell's
--     own CR 608.2n move to a graveyard.
--   * bob's activated Brothers of Fire ability is a THIRD object on the stack,
--     and the one that is not represented by a card: CR 725.1b's second sentence
--     says it ceases to exist, so it must NOT show up in bob's exile.
--   * Divination is a SORCERY, so it is uncastable while bob's spell sits on the
--     stack and becomes castable only in a priority window CR 725.1f forbids.
--   * The precombat main phase leaves more than one entry between here and the
--     cleanup step, so an implementation that advanced once would not pass.
--
-- Both libraries are stocked with lands so no draw decks anyone out (CR 704.5b),
-- and alice's hand stays under seven so CR 514.1's discard moves nothing the
-- assertions read.
timeStopBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId)
timeStopBoard island mountain gnarlbark timeStop divination burst brothers =
  let (tree, gs1) = S.addPermanent gnarlbark S.alice (S.landsFor mountain S.bob 4 (S.landsInPlay island 9))
      (shaman, gs2) = S.addPermanent brothers S.bob gs1
      (_, gs3) = S.addHandCard timeStop S.alice gs2
      (divine, gs4) = S.addHandCard divination S.alice gs3
      (bolt, gs5) = S.addHandCard burst S.bob gs4
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard mountain pid g1)) g [1 .. (10 :: Int)]
      staged =
        (stock (stock gs5 S.alice) S.bob)
          { GameState.turn =
              (GameState.turn (stock (stock gs5 S.alice) S.bob))
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.bob,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.remaining = afterPrecombatMain
                }
          }
      cast = snd (Engine.runGamePure (aimPlayer S.alice) staged (S.cast S.bob bolt))
      activated = case assaultAbility brothers of
        Nothing -> error "Parun.TurnSpec: Brothers of Fire should have an activated ability"
        Just ability -> snd (Engine.runGamePure (aimPlayer S.alice) cast (Activate.activateAbility S.bob shaman ability))
   in ( activated
          { GameState.turn =
              (GameState.turn activated)
                { TurnRegisters.priority = Just S.alice,
                  TurnRegisters.passed = Set.empty
                }
          },
        tree,
        divine
      )

-- CR 725.1, end to end, through the pool's simplest producer: Time Stop
-- ({4}{U}{U} Instant, "End the turn.").
endTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
endTurnSpec s registry = Spec.describe s "EndTheTurn" $ do
  let board = do
        island <- S.printingOf s registry "Island"
        mountain <- S.printingOf s registry "Mountain"
        gnarlbark <- S.printingOf s registry "Sinister Gnarlbark"
        timeStop <- S.printingOf s registry "Time Stop"
        divination <- S.printingOf s registry "Divination"
        burst <- S.printingOf s registry "Burst Lightning"
        brothers <- S.printingOf s registry "Brothers of Fire"
        pure (timeStopBoard island mountain gnarlbark timeStop divination burst brothers, S.printingName timeStop, S.printingName burst)
  -- CR 725.1d/725.1e: the schedule assertions, in their own case so no zone
  -- assertion can absorb a mutation to the jump.
  Spec.it s "CR 725.1d ending the turn skips straight to the cleanup step" $ do
    ((gs, _, _), _, _) <- board
    let phases = snd (runTurn castingFirst gs)
    Spec.assertEqWith s "the postcombat main phase and the end step never ran" phases [Phase.PrecombatMain, Phase.Ending EndingStep.Cleanup]
  -- CR 725.1f: no player gets priority during this process, and CR 725.1d has
  -- ended the step, so the window a sorcery would need never opens. The one
  -- assertion that separates a correct implementation from one that rewrites the
  -- schedule and settles anyway.
  Spec.it s "CR 725.1f no player gets priority once the turn has ended" $ do
    ((gs, _, divine), _, _) <- board
    let after = fst (runTurn castingFirst gs)
    Spec.assertEqWith s "alice's Divination is still in her hand" (fmap Object.zone (Game.lookupObject divine after)) (Just Zone.Hand)
  -- CR 725.1a, through Ultima ({3}{W}{W} Sorcery, "Destroy all artifacts and
  -- creatures. End the turn."): Doomed Traveler's dies ability triggers during the
  -- first sentence and is still pending when the second begins, so it ceases to
  -- exist and no Spirit is ever made.
  Spec.it s "CR 725.1a a trigger pending when the turn ends ceases to exist" $ do
    plains <- S.printingOf s registry "Plains"
    ultima <- S.printingOf s registry "Ultima"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (_, gs1) = S.addPermanent traveler S.alice (S.landsInPlay plains 5)
        (_, gs2) = S.addHandCard ultima S.alice gs1
        staged =
          gs2
            { GameState.turn =
                (GameState.turn gs2)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.priority = Just S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.remaining = afterPrecombatMain
                  }
            }
        after = fst (runTurn castingFirst staged)
    Spec.assertEqWith s "the turn ended" (TurnRegisters.turnNumber (GameState.turn after)) (TurnRegisters.turnNumber (GameState.turn staged) + 1)
    Spec.assertEqWith s "Doomed Traveler died" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName traveler)]
    Spec.assertEqWith s "alice controls no Spirit token" (namesIn Zone.Battlefield S.alice after) (replicate 5 (Just (S.printingName plains)))

-- alice, active and mid-combat at the DECLARE BLOCKERS step with her Goblin
-- Piker already attacking, holding Mandate of Peace ({1}{W} Instant, "Cast this
-- spell only during combat. / Your opponents can't cast spells this turn. / End
-- the combat phase.") and a Burst Lightning; bob holds a Burst Lightning of his
-- own and has a third one ALREADY ON THE STACK. Returns the board, alice's Jade
-- Statue, her attacker, her Mandate and the two spare Burst Lightnings.
--
-- Every element earns its place:
--
--   * Jade Statue is ANIMATED, by its own {2} ability, after attackers were
--     declared -- so it is a 3/6 Golem "until end of combat" (CR 500.5a) and is
--     NOT in combat. It is the pool's only producer of that duration, and the
--     one quantity that tells CR 725.2d's expiry from its schedule rewrite.
--   * The attacker is what CR 725.2d's "remove all creatures from combat" acts
--     on, and bob's Piker blocks it, so the removal has a subject on each side.
--   * bob's stacked Burst Lightning is a SECOND object on the stack, owned by
--     the other seat, so CR 725.2b's exile cannot be confused with the resolving
--     spell's own CR 608.2n move to a graveyard. It is genuinely CAST, aimed at
--     alice, so "nothing on the stack resolved" is a claim a life total can
--     falsify rather than one an empty binding map satisfies for free.
--   * The two hand-held Burst Lightnings are the "your opponents can't cast
--     spells this turn" clause's pair: after the combat phase ends alice may
--     still cast hers and bob may not cast his, which is what proves CR 725.2d's
--     expiry swept by EQUALITY on the combat phase rather than everything.
--   * The declare blockers step leaves two combat steps between here and the
--     postcombat main phase, so an implementation that advanced once would not
--     pass.
--
-- Four Plains pay the Jade Statue's {2} and then Mandate of Peace's {1}{W}; a
-- Mountain each pays for a Burst Lightning. Both libraries are stocked so no
-- draw decks anyone out (CR 704.5b).
mandateBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId, ObjectId, ObjectId, ObjectId)
mandateBoard plains mountain piker statue mandate burst =
  let (base, ours, _) = S.combatBoardOf [piker] [piker]
      attacker = case ours of
        a : _ -> a
        [] -> error "Parun.TurnSpec: combatBoardOf should return one creature"
      (jade, gs1) = S.addPermanent statue S.alice base
      gs2 = S.landsFor mountain S.bob 2 (S.landsFor mountain S.alice 1 (S.landsFor plains S.alice 4 gs1))
      (spell, gs3) = S.addHandCard mandate S.alice gs2
      (hers, gs4) = S.addHandCard burst S.alice gs3
      (his, gs5) = S.addHandCard burst S.bob gs4
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard mountain pid g1)) g [1 .. (10 :: Int)]
      -- Attackers are declared BEFORE the Jade Statue is animated, so the Statue
      -- stays out of combat and CR 725.2d's expiry is observable on a permanent
      -- its removal-from-combat clause never touches.
      attacking = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer (stock (stock gs5 S.alice) S.bob)
      animated = case Face.activatedAbilities (S.combinedFace statue) of
        [] -> error "Parun.TurnSpec: Jade Statue should have an activated ability"
        ability : _ ->
          let activated = snd (Engine.runGamePure S.identityAnswer attacking (Activate.activateAbility S.alice jade ability))
           in snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
      (bolt, gs6) = S.addHandCard burst S.bob animated
      staged = snd (Engine.runGamePure (aimPlayer S.alice) gs6 (S.cast S.bob bolt))
   in (staged, jade, attacker, spell, hers, his)

-- Casts Mandate of Peace the first time it is offered and passes otherwise,
-- leaving every combat decision to S.aggressiveAnswer. Pinned to the OBJECT
-- rather than to "the first cast on offer": alice also holds a Burst Lightning
-- she could afford, and the survivor case below needs it unspent.
castingMandate :: ObjectId -> Prompt.Prompt r -> r
castingMandate spell p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf spell) actions of
    h : _ -> h
    [] -> Action.Pass
  _ -> S.aggressiveAnswer p

-- The board as the postcombat main phase begins, WITHOUT running it -- the
-- moment CR 725.2d's expiry and the ordinary CR 500.5a sweep disagree. Read at
-- the end of the turn the two agree, CR 514.2 having dropped everything.
atPostcombatMain :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
atPostcombatMain = S.runToStep Phase.PostcombatMain

-- CR 725.2, end to end, through the one card rule 725.2 says ends a combat
-- phase: Mandate of Peace.
endCombatPhaseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
endCombatPhaseSpec s registry = Spec.describe s "EndTheCombatPhase" $ do
  let board = do
        plains <- S.printingOf s registry "Plains"
        mountain <- S.printingOf s registry "Mountain"
        piker <- S.printingOf s registry "Goblin Piker"
        statue <- S.printingOf s registry "Jade Statue"
        mandate <- S.printingOf s registry "Mandate of Peace"
        burst <- S.printingOf s registry "Burst Lightning"
        pure (mandateBoard plains mountain piker statue mandate burst, S.printingName mandate, (S.printingName burst, S.printingName piker))
  -- CR 725.2d/725.2e: the schedule assertion, in its own case so no zone
  -- assertion can absorb a mutation to the jump. The combat damage and end of
  -- combat steps never run, which is the whole of CR 725.2e in parun -- an "at end
  -- of combat" trigger fires from Engine.startStep and nowhere else.
  Spec.it s "CR 725.2d ending the combat phase skips straight to the next phase" $ do
    ((gs, _, _, spell, _, _), _, _) <- board
    let phases = snd (runTurn (castingMandate spell) gs)
    Spec.assertEqWith s "the combat damage and end of combat steps never ran" phases [Phase.Combat CombatStep.DeclareBlockers, Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
  -- CR 725.2f: no player gets priority during the process. The seats that were
  -- offered an action in the declare blockers step, in order -- alice casts,
  -- alice passes, bob passes, and the process runs with the round closed. An
  -- implementation that rewrites the schedule and settles anyway hands alice a
  -- fourth window inside the combat phase it just ended.
  Spec.it s "CR 725.2f no player gets priority once the combat phase has ended" $ do
    ((gs, _, _, spell, _, _), _, _) <- board
    let recording :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        recording p = case p of
          Prompt.ChooseAction _ pid _ -> do
            State.modify' (<> [pid])
            pure (castingMandate spell p)
          _ -> pure (castingMandate spell p)
        seats = State.execState (Engine.runGame recording gs Engine.runStep) []
    Spec.assertEqWith s "alice cast, alice passed, bob passed, and nobody else was asked" seats [S.alice, S.alice, S.bob]
  -- CR 725.2g: outside a combat phase nothing happens at all -- not the exile,
  -- not the schedule rewrite. Reached by applying the effect directly, since the
  -- card's own rider correctly refuses the cast.
  Spec.it s "CR 725.2g ending the combat phase outside one does nothing" $ do
    ((gs, _, _, _, _, _), _, _) <- board
    let staged =
          gs
            { GameState.turn =
                (GameState.turn gs)
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.remaining = afterPrecombatMain
                  }
            }
        source = ObjectId.MkObjectId 0
        after = S.runPure S.identityAnswer staged (Resolve.applyEffect source source S.alice Map.empty Map.empty Effect.EndCombatPhase)
    Spec.assertEqWith s "the stack is untouched" (TurnRegisters.stack (GameState.turn after)) (TurnRegisters.stack (GameState.turn staged))
    Spec.assertEqWith s "the schedule is untouched" (TurnRegisters.remaining (GameState.turn after)) afterPrecombatMain
    Spec.assertEqWith s "and nobody lost life" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 20, Just 20)

-- alice, active and at her DECLARE BLOCKERS step with a Goblin Piker already
-- attacking, holding Synthetic Truncate the Fray; bob has a Wall of Stone to
-- block with. Returns the board, alice's Jade Statue, her attacker, bob's
-- blocker and the spell.
--
-- Every element earns its place:
--
--   * Jade Statue is ANIMATED here, by its own {2} ability and after attackers
--     were declared, so it is a 3/6 Golem "until end of combat" (CR 500.5a) and
--     is NOT itself in combat -- which keeps the expiry observable on a
--     permanent CR 511.3's removal from combat never touches.
--   * Goblin Piker (2/1) attacks and Wall of Stone (0/8) blocks, so the record
--     has a subject on each side and BOTH survive the combat damage step: a
--     creature that died would leave combat by CR 506.4 and empty half the
--     record under either reading.
--   * The declare blockers step leaves two steps between here and the end of
--     combat step, so nothing here depends on the skip being the very next
--     thing that happens.
--
-- Four Islands pay the Statue's {2} and then the spell's {1}{U}. Both libraries
-- are stocked so no draw decks anyone out (CR 704.5b).
truncateBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId, ObjectId, ObjectId, ObjectId)
truncateBoard island piker wall statue fray =
  let (base, ours, theirs) = S.combatBoardOf [piker] [wall]
      attacker = case ours of
        a : _ -> a
        [] -> error "Parun.TurnSpec: combatBoardOf should return one creature"
      blocker = case theirs of
        b : _ -> b
        [] -> error "Parun.TurnSpec: combatBoardOf should return one blocker"
      (jade, gs1) = S.addPermanent statue S.alice base
      gs2 = S.landsFor island S.alice 4 gs1
      (spell, gs3) = S.addHandCard fray S.alice gs2
      stock g pid = List.foldl' (\g1 _ -> snd (S.addLibraryCard piker pid g1)) g [1 .. (10 :: Int)]
      attacking = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer (stock (stock gs3 S.alice) S.bob)
      animated = case Face.activatedAbilities (S.combinedFace statue) of
        [] -> error "Parun.TurnSpec: Jade Statue should have an activated ability"
        ability : _ ->
          let activated = snd (Engine.runGamePure S.identityAnswer attacking (Activate.activateAbility S.alice jade ability))
           in snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
   in (animated, jade, attacker, blocker, spell)

-- CR 500.11 / CR 614.1b: a phase's LAST step skipped on its own. CR 725.2e is
-- the CR's own precedent for the split this asks for -- "even though the combat
-- phase ends, 'at end of combat' triggered abilities don't trigger because the
-- end of combat step is skipped" -- read there off a phase an effect ended, and
-- here off a step a replacement effect removed. Either way the phase ends and
-- the step does not happen, so the phase-grain work is owed and the step-grain
-- work is not (Engine.skipStep).
--
-- Synthetic Truncate the Fray ({1}{U} Instant, "Target player skips their next
-- end of combat step") is the producer, and it is SYNTHETIC because nothing
-- printed names that step: Scryfall o:/skips?.*end of combat/ and
-- o:"end of combat step" o:skip, 2026-08-27, no hit, and the nearest thing --
-- Fatespinner -- names the draw step, a main phase or the whole combat phase.
-- Nothing in the CR forbids the printing: CR 500.11 skips a step, CR 506.1 makes
-- the end of combat step one, and Fatigue is the same card one step-name over.
--
-- The skip itself is the case below; CR 511.3's removal from combat and CR
-- 500.5a's "until end of combat" expiry on the same board, and its control, are
-- data/scenarios/turn's.
skippedEndOfCombatSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
skippedEndOfCombatSpec s registry = Spec.describe s "SkippedEndOfCombat" $ do
  let board = do
        island <- S.printingOf s registry "Island"
        piker <- S.printingOf s registry "Goblin Piker"
        wall <- S.printingOf s registry "Wall of Stone"
        statue <- S.printingOf s registry "Jade Statue"
        fray <- S.printingOf s registry "Synthetic Truncate the Fray"
        pure (truncateBoard island piker wall statue fray)
      endOfCombat = Phase.Combat CombatStep.EndOfCombat
      began gs = length (filter (== GameEvent.StepBegan (StepBegan.MkStepBegan endOfCombat S.alice)) (S.eventsOf gs))
      cast = castAndResolveWith (aimPlayer S.alice)
  -- CR 614.6: a replaced event never happens, and the CR 603.2b record
  -- Engine.startStep writes is what says whether it did.
  Spec.it s "CR 614.10a the end of combat step never begins, and the phase ends anyway" $ do
    (gs, _, _, _, spell) <- board
    let after = atPostcombatMain S.aggressiveAnswer (cast spell gs)
    Spec.assertBool s (S.castable S.alice spell gs) "CR 117.1a the instant is castable in the declare blockers step"
    Spec.assertEqWith s "the end of combat step never began" (began after) 0
    Spec.assertEqWith s "CR 511.3 the combat phase is over regardless" (TurnRegisters.phase (GameState.turn after)) Phase.PostcombatMain
    Spec.assertEqWith s "having consumed exactly the steps of this turn" (TurnRegisters.remaining (GameState.turn after)) (Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup])

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Turn" $ do
  turnSpec s
  turnDataSpec s
  skipSpec s registry
  extraPhaseSpec s registry
  extraTurnSpec s registry
  thatTurnSpec s registry
  endTurnSpec s registry
  endCombatPhaseSpec s registry
  skippedEndOfCombatSpec s registry
