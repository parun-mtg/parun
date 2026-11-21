{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Parun.Engine.Daytime (CR 732, "Day and Night", and CR 702.145's
-- daybound and nightbound), the CR 502.2 turn-based action Parun.Engine.Engine
-- runs from it, the CR 117.5 settle arm beside it, Parun.Types.Daytime, and
-- GameState's daytime field and Game.spellsCastLastTurn.
--
-- Also CR 702.145b's FIRST static ability -- "if it is night and this permanent
-- is represented by a double-faced card, it enters transformed" -- which is a
-- replacement effect (CR 614.1d) ranked by CR 616.1d,
-- Parun.Types.EntryRewrite's EntersTransformed, minted from rule 702.145b by
-- Parun.Engine.Keyword.mintedReplacementsFor and applied by Parun.Engine.Event's
-- arm. NOT CR 701.27j, which governs a resolving double-faced spell and is the
-- stack road alone -- the case citations below are on that road and say so. See
-- entrySpec, whose fixture is Infestation Expert // Infested Werewolf, and the
-- synthetic Dusklit Conjurer for an instant back face (CR 701.27j, 304.4).
--
-- Also CR 702.145b's third static ability and CR 702.145e's second -- "this
-- permanent can't transform except due to its daybound/nightbound ability" --
-- which is Parun.Engine.Event.restrictsTransform read by Parun.Engine.Resolve's
-- `turnOver`. See data/scenarios/daytime.
--
-- Gameplay-level throughout. Tovolar, Dire Overlord // Tovolar, the Midnight
-- Scourge is the whole fixture, and one card supplies every half of the rule: a
-- daybound front face (so a board with it on it becomes day, CR 702.145d), an
-- upkeep trigger that says "it becomes night" (CR 732.1), and a nightbound back
-- face for the night to turn it over onto (CR 702.145c). Russet Wolves is here
-- only to reach the trigger's "three or more Wolves and/or Werewolves";
-- data/scenarios/daytime adds Moonmist, Forest, Humility and Daybreak Ranger //
-- Nightfall Predator.
--
-- Tovolar's second sentence -- "Then transform any number of Human Werewolves
-- you control" -- is modeled and proved by data/scenarios/daytime's CR 608.2d
-- scenarios. It needs a
-- SECOND card for a rules reason rather than a fixture one: CR 702.145b's third
-- static ability makes the clause inert on every daybound Human Werewolf,
-- Tovolar himself included, so Daybreak Ranger // Nightfall Predator -- a Human
-- Werewolf with no daybound -- is the only permanent the clause can reach. His
-- combat-damage trigger IS modeled, and is read by
-- data/scenarios/counter-keyword-trigger rather than here.
--
-- Moonmist's second sentence is modeled by its card file and proved by
-- Parun.ReplacementSpec's Moonmist group, not here: it is a CR 615.1 prevention
-- effect and has nothing to do with day or night.
module Parun.DaytimeSpec where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Daytime as Daytime
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

-- The face this permanent is showing, by name (CR 709.4a). What "transformed"
-- means to a reader: Object.face is the one field CR 701.27a writes, and every
-- characteristic follows it.
faceNameOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe CardName.CardName
faceNameOf oid gs = fmap Face.name (Game.faceOf oid gs)

frontName :: CardName.CardName
frontName = CardName.MkCardName (Text.pack "Tovolar, Dire Overlord")

backName :: CardName.CardName
backName = CardName.MkCardName (Text.pack "Tovolar, the Midnight Scourge")

-- CR 117.5's settle, which is where CR 702.145c/d/f/g are checked. Not
-- S.settleSba: that runs the state-based actions alone, and these rules are
-- explicitly not state-based actions.
settle :: GameState.GameState -> GameState.GameState
settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

-- CR 502: the untap step's turn-based actions, which is where CR 502.2's
-- day/night check lives.
untapStep :: GameState.GameState -> GameState.GameState
untapStep gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- Alice's board: Tovolar and `wolves` Russet Wolves, settled. Nothing has
-- happened yet, so the game still has neither designation (CR 732.1).
tovolarBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
tovolarBoard tovolar wolf wolves =
  let (tovolarId, gs) = S.addPermanent tovolar S.alice (Setup.emptyGame S.bothPlayers)
   in (tovolarId, foldr (\_ g -> snd (S.addPermanent wolf S.alice g)) gs [1 .. wolves])

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Daytime" $ do
  untapCheckSpec s registry
  entrySpec s registry

expertFront :: CardName.CardName
expertFront = CardName.MkCardName (Text.pack "Infestation Expert")

expertBack :: CardName.CardName
expertBack = CardName.MkCardName (Text.pack "Infested Werewolf")

-- Every face alice's battlefield shows that belongs to Infestation Expert, found
-- by name rather than by an ObjectId: CR 400.7 makes the resolving spell a NEW
-- object, so the id the cast returned is not the permanent's.
--
-- A LIST rather than the first match, so a board that somehow grew two of them
-- fails loudly instead of answering about one.
expertFaces :: GameState.GameState -> [CardName.CardName]
expertFaces gs = do
  oid <- Game.zoneMembers Zone.Battlefield S.alice gs
  name <- Maybe.maybeToList (faceNameOf oid gs)
  Monad.guard (name == expertFront || name == expertBack)
  pure name

-- alice controls Tovolar and five Forests, with Infestation Expert in hand.
-- `spells` is what the previous turn's active player cast, which is the whole of
-- what decides the designation the spell will enter under: the settle makes it
-- day (CR 702.145d), and CR 502.2's untap check then turns it to night on
-- nothing, or leaves it day on one.
--
-- S.handOne leaves the board in a main phase with alice holding priority, and
-- `untapStep` takes the phase it runs CR 502.2 for as an argument rather than
-- moving the board to it, so that stands. Nothing here is about timing anyway:
-- S.cast drives Parun.Engine.Cast directly.
--
-- Tovolar is on the board only to give it a designation at all -- CR 702.145d
-- needs a permanent with daybound -- and five Forests is exactly Infestation
-- Expert's {4}{G}, so the cast can be paid without a decision.
expertBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Natural.Natural -> (ObjectId.ObjectId, GameState.GameState)
expertBoard tovolar forest expert spells =
  let (_, withTovolar) = S.addPermanent tovolar S.alice (S.landsInPlay forest 5)
      (gs, spellId) = S.handOne expert withTovolar
   in (spellId, untapStep (S.afterCasting spells (settle gs)))

-- alice casts Infestation Expert and it resolves. Answered in two steps because
-- the assertion between them is the point: the permanent is on the battlefield
-- and its enters-the-battlefield trigger has not resolved yet.
castExpert :: ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, GameState.GameState)
castExpert spellId gs =
  let cast = S.runPure S.castAnswer gs (S.cast S.alice spellId)
      entered = S.runPure S.castAnswer cast Stack.resolveTop
      triggered = S.runPure S.castAnswer (settle entered) Stack.resolveTop
   in (entered, triggered)

entrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entrySpec s registry = Spec.describe s "EntersTransformed" $ do
  -- CR 701.27j through CR 702.145b's FIRST static ability: "if it is night and
  -- this permanent is represented by a double-faced card, it enters transformed."
  -- Infestation Expert is cast with its front face up (CR 712.8) and reaches the
  -- battlefield showing its back face.
  --
  -- THE FACE IS THE DISCRIMINATOR, and it is asserted at a moment that tells the
  -- competing readings apart. Three of them put a permanent on this board showing
  -- Infested Werewolf:
  --
  --   * it entered transformed -- CR 701.27j, the rule;
  --   * it entered front face up and the CR 702.145c sweep turned it over
  --     (Parun.Engine.Daytime.turnDue, which is what this engine did before);
  --   * it was never front face up at all, a back-face cast (CR 712.8a) -- which
  --     this board rules out by casting the card from hand, where CR 712.6a shows
  --     only the front face.
  --
  -- The first two are separated by asserting on `entered`, the board the spell's
  -- resolution leaves, which is BEFORE the settle the sweep runs in. Its by-day
  -- negative is data/scenarios/daytime's CR 702.145b scenario.
  Spec.it s "CR 701.27j/702.145b a daybound spell cast at night enters transformed" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    expert <- S.printingOf s registry "Infestation Expert"
    let (spellId, board) = expertBoard tovolar forest expert 0
        (entered, _) = castExpert spellId board
    Spec.assertEqWith s "it is night when the spell resolves" (GameState.daytime board) (Just Daytime.Night)
    Spec.assertEqWith s "the permanent is showing its back face already" (expertFaces entered) [expertBack]
  -- CR 701.27j's second sentence, on the synthetic Dusklit Conjurer: daybound
  -- makes it enter transformed, its back face is an instant, so it goes to its
  -- owner's graveyard instead of the battlefield. Its by-day control is
  -- data/scenarios/daytime's CR 712.10 scenario.
  Spec.it s "CR 701.27j a spell entering transformed onto an instant back face goes to the graveyard" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    conjurer <- S.printingOf s registry "Synthetic Dusklit Conjurer"
    let (spellId, board) = expertBoard tovolar forest conjurer 0
        (entered, _) = castExpert spellId board
    Spec.assertEqWith s "it is night when the spell resolves" (GameState.daytime board) (Just Daytime.Night)
    Spec.assertEqWith s "neither face is on the battlefield" (conjurerFaces entered) []
    Spec.assertEqWith s "the card is in alice's graveyard" (cardsIn Zone.Graveyard conjurer entered) 1
    Spec.assertEqWith s "and the stack is empty" (TurnRegisters.stack (GameState.turn entered)) []
  -- CR 701.27j's copy clause: Double Major copies the spell, the copy enters
  -- transformed too, and neither it nor the original reaches the battlefield.
  Spec.it s "CR 701.27j a copy of that spell doesn't enter the battlefield either" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    conjurer <- S.printingOf s registry "Synthetic Dusklit Conjurer"
    doubleMajor <- S.printingOf s registry "Double Major"
    let (_, withTovolar) = S.addPermanent tovolar S.alice (S.landsFor island S.alice 1 (S.landsInPlay forest 6))
        (withConjurer, spellId) = S.handOne conjurer withTovolar
        (majorId, withMajor) = S.addHandCard doubleMajor S.alice withConjurer
        board = untapStep (S.afterCasting 0 (settle withMajor))
        cast1 = S.runPure S.castAnswer board (S.cast S.alice spellId)
    case Maybe.listToMaybe (TurnRegisters.stack (GameState.turn cast1)) of
      Nothing -> Spec.assertFailure s "the spell never reached the stack"
      Just conjurerSpell -> do
        let cast2 = S.runPure (pinTarget (Recipient.ToObject conjurerSpell)) cast1 (S.cast S.alice majorId)
            after = resolveSettled (resolveSettled (resolveSettled cast2))
        Spec.assertEqWith s "it is night when the spells resolve" (GameState.daytime board) (Just Daytime.Night)
        Spec.assertEqWith s "neither the copy nor the original is on the battlefield" (conjurerFaces after) []
        Spec.assertEqWith s "the card is in alice's graveyard" (cardsIn Zone.Graveyard conjurer after) 1
        Spec.assertEqWith s "and the stack is empty" (TurnRegisters.stack (GameState.turn after)) []
  -- Off the stack road CR 701.27j is silent and CR 304.4 answers: Exhume puts
  -- the card onto the battlefield at night, daybound makes it an instant as it
  -- enters, and an instant that would enter remains in its previous zone.
  Spec.it s "CR 304.4 put onto the battlefield at night, that card stays in the graveyard" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    swamp <- S.printingOf s registry "Swamp"
    conjurer <- S.printingOf s registry "Synthetic Dusklit Conjurer"
    exhume <- S.printingOf s registry "Exhume"
    let (_, withTovolar) = S.addPermanent tovolar S.alice (S.landsInPlay swamp 2)
        (buriedId, withBuried) = S.addGraveyardCard conjurer S.alice withTovolar
        (withExhume, exhumeId) = S.handOne exhume withBuried
        board = untapStep (S.afterCasting 0 (settle withExhume))
        after = resolveSettled (S.runPure S.castAnswer board (S.cast S.alice exhumeId))
    Spec.assertEqWith s "it is night when Exhume resolves" (GameState.daytime board) (Just Daytime.Night)
    Spec.assertEqWith s "neither face is on the battlefield" (conjurerFaces after) []
    Spec.assertEqWith s "the same object is still in alice's graveyard (CR 400.7)" (fmap Object.zone (Game.lookupObject buriedId after)) (Just Zone.Graveyard)
    Spec.assertEqWith s "and Exhume resolved" (TurnRegisters.stack (GameState.turn after)) []

conjurerFront :: CardName.CardName
conjurerFront = CardName.MkCardName (Text.pack "Synthetic Dusklit Conjurer")

conjurerBack :: CardName.CardName
conjurerBack = CardName.MkCardName (Text.pack "Synthetic Dusk's Reprisal")

-- Every face of Dusklit Conjurer on the battlefield, token copies included, by
-- PROJECTED name (CR 707.2), since a copy's card is not the printing's.
conjurerFaces :: GameState.GameState -> [CardName.CardName]
conjurerFaces gs = do
  oid <- Set.toList (GameState.battlefield gs)
  name <- Set.toList (Projection.namesOf oid gs)
  Monad.guard (name == conjurerFront || name == conjurerBack)
  pure name

-- How many cards of this printing alice owns in `zone`.
cardsIn :: Zone.Zone -> Printing.Printing -> GameState.GameState -> Int
cardsIn zone printing gs = length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.nameOf (Printing.card printing))) (Game.zoneMembers zone S.alice gs))

-- Resolve the top of the stack, then settle, so CR 704.5e removes a resolved
-- copy between resolutions.
resolveSettled :: GameState.GameState -> GameState.GameState
resolveSettled gs = settle (S.runPure S.castAnswer gs Stack.resolveTop)

-- Answer every target prompt with this recipient alone.
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (== recipient) offered) asked
  _ -> S.identityAnswer p

untapCheckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
untapCheckSpec s registry = Spec.describe s "UntapCheck" $ do
  -- CR 502.2's first sentence: day, and the previous turn's active player cast
  -- no spells during it, so it becomes night -- and CR 702.145c turns Tovolar
  -- over as it does.
  Spec.it s "CR 502.2 day and no spells last turn becomes night" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    let (tovolarId, board) = tovolarBoard tovolar tovolar 0
        after = untapStep (S.afterCasting 0 (settle board))
    Spec.assertEqWith s "night" (GameState.daytime after) (Just Daytime.Night)
    Spec.assertEqWith s "and Tovolar turned over" (faceNameOf tovolarId after) (Just backName)
  -- The discriminator: one spell is not "didn't cast any spells", so the same
  -- step leaves it day. An implementation that changed the designation every
  -- untap step passes the case above and fails this one.
  Spec.it s "CR 502.2 one spell last turn keeps it day" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    let (tovolarId, board) = tovolarBoard tovolar tovolar 0
        after = untapStep (S.afterCasting 1 (settle board))
    Spec.assertEqWith s "still day" (GameState.daytime after) (Just Daytime.Day)
    Spec.assertEqWith s "and still front face up" (faceNameOf tovolarId after) (Just frontName)
  -- CR 732.2's input, taken through the rules rather than written: alice casts a
  -- Lightning Bolt, the turn is handed on, and the count the next untap step will
  -- read is the one her turn ended with. The event log it is folded from is
  -- cleared by that same handoff, which files it under her (History.pastTurns).
  Spec.it s "CR 732.2 the handoff records what the previous turn's active player cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, spellId) = S.handOne lightningBolt (S.landsInPlay mountain 1)
        cast = S.runPure S.castAnswer gs (S.cast S.alice spellId)
        handed = Engine.beginTurnOf S.bob cast
    Spec.assertEqWith s "nothing was on the books before the turn ended" (Game.spellsCastLastTurn gs) 0
    Spec.assertEqWith s "one spell rode across the handoff" (Game.spellsCastLastTurn handed) 1
    Spec.assertEqWith s "and the log it was folded from is gone" (Seq.null (TurnRegisters.events (GameState.turn handed))) True
