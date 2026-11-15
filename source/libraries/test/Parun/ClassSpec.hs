{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 717's Class cards, which need no engine subsystem of their own. A
-- class level bar is a keyword ability (CR 717.2) whose meaning rule 717.2a spells
-- out in full: the activated half is the card's activated ability written with
-- Keyword.ClassLevel on it, which adds rule 717.2a's two riders as
-- ActivationRestriction.OnlyIf and ActivationRestriction.SorcerySpeed
-- (Parun.Engine.Keyword.printedRiders), and the static half is an ordinary
-- StaticAbility with a CR 604.2 clause. The mark both halves read is
-- Object.classLevel, written by Effect.SetClassLevel and read by
-- Quantity.ClassLevel.
--
-- The "activate only if this Class is level N-1" rider is a RESTRICTION and not
-- ActivatedAbility.condition's grant gate, which would make the bar absent below
-- the level rather than present and prohibited (CR 602.5 prohibits activating an
-- ability the object still HAS). barsHadSpec below is what tells the two
-- readings apart.
--
-- So what this file exercises is that mark's lifecycle: Parun.Engine.Resolve's
-- SetClassLevel arm writes it, Parun.Engine.Quantity's ClassLevel arm reads it back
-- through Parun.Engine.Filter's view, Parun.Engine.ActivationRestriction's OnlyIf
-- arm gates the next bar's activation on it, and Parun.Engine.Projection.gatherStatic
-- gates the section's continuous effect on it.
--
-- Paladin Class, AFR 29, is the card under test for every group but the last,
-- and it was picked
-- for what its level-2 section is: "Creatures you control get +1/+1", a plain
-- layer 7c modification, so nothing but the level gate is under test. Its TOP
-- section -- "Spells your opponents cast during your turn cost {1} more to cast",
-- which CR 717.3 makes an ability the Class has at all times -- is transcribed
-- too, and topSectionSpec below is what proves it. Its LEVEL-3 section --
-- "Whenever you attack, until end of turn, target attacking creature gets +1/+1
-- for each other attacking creature and gains double strike" -- is transcribed
-- as well, and data/scenarios/class/ is what proves it. Nothing on the card is
-- omitted.
--
-- CR 717.1 is a frame rule with no rules meaning -- the striated text box and the
-- sideways type line -- so nothing here asserts about the layout, and
-- data/cards/paladin-class.json states none.
--
-- CR 717.2c's "to gain a Class level" needs Sorcerer Class, and
-- data/scenarios/class/ is what proves it; gainClassLevelSpec below keeps the
-- control, an activation no level bar indicates.
--
-- CR 717's "when this Class becomes level N" trigger needs a second card,
-- Stormchaser's Talent, BLB, and data/scenarios/class/ is what proves it.
module Parun.ClassSpec where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Action as Action
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action.Type
import qualified Parun.Types.ClassLevel as ClassLevel
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.TurnRegisters as TurnRegisters

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Class" $ do
  topSectionSpec s registry
  sectionSpec s registry
  ladderSpec s registry
  barsHadSpec s registry
  gainClassLevelSpec s registry

-- CR 717.3: the text above the first class level bar is an ability the Class has
-- at ALL times -- no bar precedes it, so no level gates it. Paladin Class prints
-- "Spells your opponents cast during your turn cost {1} more to cast", and the
-- clause under test is the "during your turn": a CR 604.2 condition on the
-- PLAYER-facing static carrier, reading CR 102.1's active player.
--
-- alice controls the Class and nothing else; bob and alice each hold a Lightning
-- Bolt ({R}). No lands and no mana: Cost.total answers about a cost rather than
-- about paying it, so affordability cannot enter any assertion here.
--
-- The two boards below differ in EXACTLY one thing, TurnRegisters.activePlayer, and
-- the Class is never levelled -- CR 717.3's section is not a level bar's, so a
-- level would be a second moving part with nothing to prove.
taxBoard :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
taxBoard paladinClass lightningBolt active =
  let (_, withClass) = S.addPermanent paladinClass S.alice (Setup.emptyGame S.bothPlayers)
      (bolt, withBolt) = S.addHandCard lightningBolt S.bob withClass
   in ( bolt,
        withBolt
          { GameState.turn =
              (GameState.turn withBolt)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = active,
                  TurnRegisters.priority = Just active
                }
          }
      )

-- What `pid` would pay in total to cast the object in their hand whose printed
-- mana cost is `printed`. PlayerEffectSpec's totalManaCost, duplicated rather
-- than hoisted: Parun.Support rebuilds every spec in the tree.
totalManaCost :: PlayerId.PlayerId -> ObjectId.ObjectId -> ManaCost.ManaCost -> GameState.GameState -> Maybe ManaCost.ManaCost
totalManaCost pid oid printed gs = Cost.Type.mana (Cost.total pid oid (Cost.Type.MkCost (Just printed) []) gs)

red :: ManaSymbol.ManaSymbol
red = ManaSymbol.OfType (ManaType.Colored Color.Red)

topSectionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
topSectionSpec s registry = Spec.describe s "Top text box section" $ do
  -- The gameplay-level assertion first, and it is the one the whole unit exists
  -- for: an opponent's spell is taxed on alice's turn and NOT on bob's own turn.
  -- A Class whose clause was dropped taxes on both, and a clause read from the
  -- TAXED player's perspective rather than the Class controller's taxes on
  -- neither -- so the pair discriminates both wrong readings, which no single
  -- board can.
  Spec.it s "CR 717.3 / CR 102.1 the tax applies only during the Class controller's turn" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (bolt, alicesTurn) = taxBoard paladinClass lightningBolt S.alice
        (bobsBolt, bobsTurn) = taxBoard paladinClass lightningBolt S.bob
    Spec.assertEqWith
      s
      "bob's {R} Bolt costs {1}{R} during alice's turn"
      (totalManaCost S.bob bolt (ManaCost.MkManaCost [red]) alicesTurn)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
    Spec.assertEqWith
      s
      "the same Bolt costs {R} during bob's own turn"
      (totalManaCost S.bob bobsBolt (ManaCost.MkManaCost [red]) bobsTurn)
      (Just (ManaCost.MkManaCost [red]))
  -- The scope half, which the case above cannot see: "your opponents" and "each
  -- player" agree on every board where only an opponent casts. alice casting on
  -- her own turn -- the one turn the condition is true -- is where they differ.
  Spec.it s "CR 717.3 the Class controller's own spell is untaxed on her own turn" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, withClass) = S.addPermanent paladinClass S.alice (Setup.emptyGame S.bothPlayers)
        (alicesBolt, gs) = S.addHandCard lightningBolt S.alice withClass
        alicesTurn =
          gs
            { GameState.turn =
                (GameState.turn gs)
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.priority = Just S.alice
                  }
            }
    Spec.assertEqWith
      s
      "alice's own {R} Bolt stays {R}"
      (totalManaCost S.alice alicesBolt (ManaCost.MkManaCost [red]) alicesTurn)
      (Just (ManaCost.MkManaCost [red]))

-- alice's board: the Class and one Goblin Piker on the battlefield, twelve
-- untapped Plains, in her own precombat main phase with priority. Twelve is more
-- than both bars together cost ({2}{W} then {4}{W}), so no assertion below can
-- turn on affordability -- which matters for the ladder case, whose whole point
-- is that a bar it CAN pay for is still not offered.
--
-- Goblin Piker is the creature because it is vanilla and 2/1: the two axes carry
-- different numbers, so a +1/+1 that landed on only one of them could not read as
-- the whole modification.
--
-- Both permanents are placed rather than cast: CR 717 says nothing about how a
-- Class arrives, and Paladin Class has no entry trigger.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
board paladinClass plains piker =
  let (classId, withClass) = S.addPermanent paladinClass S.alice (S.landsInPlay plains 12)
      (pikerId, withPiker) = S.addPermanent piker S.alice withClass
   in ( classId,
        pikerId,
        withPiker
          { GameState.turn =
              (GameState.turn withPiker)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

-- CR 717.2b's mark itself, which no characteristic reports. Nothing is a Class
-- that has never been levelled, which CR 717.2d then reads as level 1 -- the
-- default lives at Parun.Engine.Quantity's read and deliberately not here.
levelOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe ClassLevel.ClassLevel
levelOf oid gs = Game.lookupObject oid gs >>= Object.classLevel

-- The level bars alice may activate on this permanent RIGHT NOW, off the same
-- enumeration a player is offered. Counts activations of one source, so the
-- Plains' mana abilities and the Piker cannot be mistaken for one.
barsOffered :: ObjectId.ObjectId -> GameState.GameState -> Int
barsOffered oid gs =
  length
    ( do
        Action.Type.Activate o _ <- Action.legalActions S.alice gs
        Monad.guard (o == oid)
        pure ()
    )

-- Activate the first bar alice is OFFERED on this permanent and resolve it.
-- Offered rather than merely present: CR 717.2a leaves every bar on the Class at
-- every level and CR 602.5's riders are what pick one out, so the enumeration is
-- the only thing that knows which. Stack.resolveTop rather than the priority
-- loop: the narrowest path that shows the write, with no settle in between that
-- could sweep something.
gainLevel :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
gainLevel oid gs = case [ability | Action.Type.Activate o ability <- Action.legalActions S.alice gs, o == oid] of
  [] -> gs
  ability : _ ->
    let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
     in S.runPure S.identityAnswer activated Stack.resolveTop

-- CR 717.2a's static half: "As long as this Class is level N or greater, it has
-- [abilities]."
sectionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
sectionSpec s registry = Spec.describe s "Level bar section" $ do
  -- The two readings this case must tell apart: a level-gated section that is off
  -- until the level is gained, and one that is simply always on. The Piker's
  -- printed 2/1 is what the second would have destroyed.
  Spec.it s "CR 717.2d a Class with no level reads as level 1, so its level-2 section is off" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, pikerId, gs) = board paladinClass plains piker
    Spec.assertEqWith s "the Piker is its printed 2/1" (S.powerToughnessOf pikerId gs) (Just (2, 1))
    Spec.assertEqWith s "CR 717.2b: no level designation has been written" (levelOf classId gs) Nothing

-- CR 717.2b's designation, written straight onto the permanent.
atLevel :: ObjectId.ObjectId -> Natural.Natural -> GameState.GameState -> GameState.GameState
atLevel oid level gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.classLevel = Just (ClassLevel.MkClassLevel level)}) oid (GameState.objects gs)
    }

-- CR 717.2a's activated half: "[Cost]: This Class's level becomes N. Activate only
-- if this Class is level N-1 and only as a sorcery."
ladderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ladderSpec s registry = Spec.describe s "Level bar activation" $ do
  -- One bar at a time, in order, and then nothing. The falsifier for a missing
  -- "only if this Class is level N-1" is the last entry: with no such clause the
  -- level-2 bar stays offered forever, and the third activation would put the
  -- level back to 2.
  Spec.it s "CR 717.2a a bar is activatable only from level N-1" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, _, gs) = board paladinClass plains piker
        climb n = iterate (gainLevel classId) gs !! n
    Spec.assertEqWith
      s
      "the level after 0, 1, 2 and 3 activations"
      (fmap (levelOf classId . climb) [0, 1, 2, 3])
      [Nothing, Just (ClassLevel.MkClassLevel 2), Just (ClassLevel.MkClassLevel 3), Just (ClassLevel.MkClassLevel 3)]
    Spec.assertEqWith
      s
      "the bars offered at levels 1, 2 and 3"
      (fmap (barsOffered classId . climb) [0, 1, 2])
      [1, 1, 0]
  -- Two boards differing in exactly one thing: whose turn it is. Both hold the
  -- same twelve untapped Plains, so the bar is affordable on each, and CR 307.5's
  -- "it must be during the main phase of their turn" is the only thing that moves.
  Spec.it s "CR 717.2a a level bar is activatable only as a sorcery" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, _, gs) = board paladinClass plains piker
        bobsTurn =
          gs
            { GameState.turn =
                (GameState.turn gs)
                  { TurnRegisters.activePlayer = S.bob
                  }
            }
    Spec.assertEqWith s "offered on alice's own main phase" (barsOffered classId gs) 1
    Spec.assertEqWith s "not offered on bob's turn" (barsOffered classId bobsTurn) 0

-- CR 717.2a's rider is "Activate only if this Class is level N-1", and CR 602.5
-- makes that a prohibition on activating an ability the object HAS. The other
-- reading -- ActivatedAbility.condition, which GRANTS the bar only at level N-1 --
-- refuses the same activations, so the two come apart only where a card reads the
-- abilities an object has rather than what it may do (CR 602.1).
--
-- Paladin Class at level THREE is where they differ most sharply: its ladder is
-- spent, so under the grant gate neither bar is among its abilities and the Class
-- has none, while rule 717.2a leaves both bars on it, prohibited.
--
-- Synthetic Ability Audit is the reader -- "Creatures get +1/+0 for each permanent
-- with an activated ability that isn't a mana ability", which is
-- Filter.HasActivatedAbility (Just NonManaAbility) over the battlefield. Synthetic because no
-- printing asks that question of an enchantment: Tsabo's Web and Ravager Wurm ask
-- it of LANDS, Magewright's Stone of a creature, and Zirda, the Dawnwaker of the
-- CARDS in a starting deck, which Parun.Engine.Projection.View.viewOfCard answers
-- off the printed face with no gate to apply, so both readings agree there. The
-- Enigma Jewel's craft, "four or more nonlands with activated abilities", asks
-- Filter.HasActivatedAbility Nothing of a permanent, and is the real card that can
-- replace this one.
--
-- bob owns the Piker so that Paladin Class's own level-2 section -- "Creatures you
-- control get +1/+1" -- cannot reach it, leaving the audit the only thing that
-- moves a number; and the audit moves POWER only, so a modification landing on
-- both axes could not pass for it. No land is on the board, so nothing but the
-- Class can carry an activated ability of any kind.
barsHadSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
barsHadSpec s registry = Spec.describe s "Bars the Class has" $ do
  Spec.it s "CR 717.2a / CR 602.1 a Class has its level bars at every level, activatable or not" $ do
    audit <- S.printingOf s registry "Synthetic Ability Audit"
    paladinClass <- S.printingOf s registry "Paladin Class"
    piker <- S.printingOf s registry "Goblin Piker"
    let (topClass, topPiker, topBoard) = auditBoard audit paladinClass piker (Just 3)
        (_, freshPiker, freshBoard) = auditBoard audit paladinClass piker (Just 1)
        (_, lonePiker, loneBoard) = auditBoard audit paladinClass piker Nothing
    Spec.assertEqWith
      s
      "CR 717.2a: bob's Piker is 3/1, the audit counting a level-3 Class whose bars are all prohibited"
      (S.powerToughnessOf topPiker topBoard)
      (Just (3, 1))
    Spec.assertEqWith
      s
      "CR 717.2a: a level-1 Class, whose first bar IS activatable, is counted the same"
      (S.powerToughnessOf freshPiker freshBoard)
      (Just (3, 1))
    Spec.assertEqWith
      s
      "the same board without the Class: the audit counts nothing and the Piker is its printed 2/1"
      (S.powerToughnessOf lonePiker loneBoard)
      (Just (2, 1))
    Spec.assertEqWith s "CR 717.2b: the first board's Class is at level 3" (fmap (`levelOf` topBoard) topClass) (Just (Just (ClassLevel.MkClassLevel 3)))

-- alice's audit and bob's Piker, plus alice's Class at the given level when there
-- is one. The three boards differ in exactly one thing apiece: whether the Class
-- is there at all, and what level it is at. The level is WRITTEN rather than
-- climbed, there being no mana here to climb with, and the case asserts it.
auditBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe Natural.Natural -> (Maybe ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
auditBoard audit paladinClass piker level =
  let (_, withAudit) = S.addPermanent audit S.alice (Setup.emptyGame S.bothPlayers)
      (pikerId, withPiker) = S.addPermanent piker S.bob withAudit
   in case level of
        Nothing -> (Nothing, pikerId, withPiker)
        Just n ->
          let (classId, withClass) = S.addPermanent paladinClass S.alice withPiker
           in (Just classId, pikerId, atLevel classId n withClass)

-- CR 717.2c: "to gain a Class level" means "to activate an ability indicated by
-- a class level bar". Sorcerer Class, AFR 233, prints it: at level 2 creatures
-- its controller controls have "{T}: Add {U} or {R}. Spend this mana only to
-- cast an instant or sorcery spell or to gain a Class level." The clause is
-- asked of the ABILITY being activated (Keyword.ClassLevel, which the card
-- writes on each bar), not of its source.
--
-- alice's only mana is the granted ability on five creatures: Shivan Dragon and
-- four Goblin Pikers. The level-3 bar costs {3}{U}{R}, all five; Shivan
-- Dragon's firebreathing costs {R}, and the Mountain board differs from the
-- other in that one land and nothing else.
sorcererBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
sorcererBoard sorcererClass shivan piker =
  let (classId, withClass) = S.addPermanent sorcererClass S.alice (Setup.emptyGame S.bothPlayers)
      (shivanId, withShivan) = S.addPermanent shivan S.alice withClass
      withPikers = iterate (snd . S.addPermanent piker S.alice) withShivan !! 4
   in ( classId,
        shivanId,
        (atLevel classId 2 withPikers)
          { GameState.turn =
              (GameState.turn (atLevel classId 2 withPikers))
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      )

gainClassLevelSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
gainClassLevelSpec s registry = Spec.describe s "Gaining a Class level" $ do
  -- Sorcerer Class's mana refuses an activation no class level bar indicates;
  -- data/scenarios/class/cr-717-2c-*.json is the control.
  Spec.it s "CR 106.6 the same mana does not pay Shivan Dragon's firebreathing" $ do
    sorcererClass <- S.printingOf s registry "Sorcerer Class"
    shivan <- S.printingOf s registry "Shivan Dragon"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, shivanId, gs) = sorcererBoard sorcererClass shivan piker
    Spec.assertEqWith s "CR 106.6 firebreathing is not offered" (barsOffered shivanId gs) 0
