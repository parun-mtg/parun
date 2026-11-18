{-# LANGUAGE GADTs #-}

-- Covers Parun.Engine.Game, Parun.Engine.Engine, and Parun.Engine.Action: zones and changeZone, legal
-- actions, object facts, engine steps, and engine-rule integration (priority
-- rounds, the CR 103.8a draw skip, CR 514.2 discard, CR 704.5b deck-out).
module Parun.GameSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Engine.Action as Action
import qualified Parun.Engine.Ante as Ante
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Damage as Damage
import qualified Parun.Engine.Departure as Departure
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Mint as Mint
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Engine.Turn as Turn
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.Action as A
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Parun.Types.Activator as Activator
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Bookkeeping as Bookkeeping
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardIdentity as CardIdentity
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Concession as Concession
import qualified Parun.Types.Copyability as Copyability
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Departure as Departure.Type
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.MulliganDecision as MulliganDecision
import qualified Parun.Types.MulliganOffer as MulliganOffer
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OutsideObject as OutsideObject
import qualified Parun.Types.OwnershipChange as OwnershipChange
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Response as Response
import qualified Parun.Types.Result as Result
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.Sickness as Sickness
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Source as Source
import qualified Parun.Types.Status as Status
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Teams as Teams
import qualified Parun.Types.Timestamp as Timestamp
import qualified Parun.Types.TurnKind as TurnKind
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

objectFactSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
objectFactSpec s registry = Spec.describe s "ObjectFacts" $ do
  Spec.it s "a Piker's power and toughness are 2 and 1" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "power" (Projection.powerOf oid gs) (Just 2)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf oid gs) (Just 1)

  Spec.it s "CR 111.3 a token's characteristics are read through cardOf" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        goblinCard = Printing.card piker
        (tokId, gs) = S.addToken goblinCard S.alice base
    Spec.assertEqWith s "cardOf returns the token's embedded card" (Game.cardOf tokId gs) (Just goblinCard)
    Spec.assertEqWith s "the token is on the battlefield" (Set.member tokId (GameState.battlefield gs)) True

  Spec.it s "CR 112.1 isSpell is True for a spell on the stack, False off it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (spellId, gs1) = S.spellOnStack piker S.alice base
        (permId, gs2) = S.addPermanent piker S.bob gs1
        tokenCard = Printing.card piker
        (tokId, gs3) = S.addToken tokenCard S.bob gs2
    Spec.assertBool s (Game.isSpell spellId gs3) "a card on the stack is a spell"
    Spec.assertBool s (not (Game.isSpell permId gs3)) "a battlefield permanent is not a spell"
    Spec.assertBool s (not (Game.isSpell tokId gs3)) "a token is not a spell"

  Spec.it s "a Mountain has no power or toughness" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> do
        Spec.assertEqWith s "power" (Projection.powerOf oid gs) Nothing
        Spec.assertEqWith s "toughness" (Projection.toughnessOf oid gs) Nothing

  Spec.it s "controllerOf is the owner while nothing can change control" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "controller" (Projection.controllerOf oid gs) (Just S.bob)

  Spec.it s "an unknown id has no facts" $ do
    let gs = Setup.emptyGame S.bothPlayers
        missing = ObjectId.MkObjectId 999
    Spec.assertEqWith s "power" (Projection.powerOf missing gs) Nothing
    Spec.assertEqWith s "controller" (Projection.controllerOf missing gs) Nothing

-- The battlefield after changeZone moves the sole Mountain onto it. Loaded
-- fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
afterMountainMoved :: Printing.Printing -> GameState.GameState
afterMountainMoved mountain =
  S.runPure S.identityAnswer (S.oneMountainState mountain Phase.PrecombatMain) (Event.changeZone Settle.none (ObjectId.MkObjectId 0) Zone.Battlefield)

gameSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gameSpec s registry = Spec.describe s "Game" $ do
  Spec.it s "CR 514.1 a short or doubled discard answer still discards down to the maximum hand size" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let gs0 = List.foldl' (\g _ -> snd (S.addHandCard bears S.alice g)) (Setup.emptyGame S.bothPlayers) [1 .. 9 :: Int]
        held = Game.zoneMembers Zone.Hand S.alice gs0
        after chosen = S.handSize S.alice (snd (Engine.runGamePure (discarding chosen) gs0 (Engine.discardToHandSize S.alice)))
    Spec.assertEqWith s "none named" (after []) 7
    Spec.assertEqWith s "one named twice" (after (take 1 held <> take 1 held)) 7
  Spec.it s "changeZone preserves object count" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "count" (Game.objectCount (afterMountainMoved mountain)) 1

  Spec.it s "changeZone drops the old id" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "old gone" (Game.lookupObject (ObjectId.MkObjectId 0) (afterMountainMoved mountain)) Nothing

  Spec.it s "the moved object is on the battlefield, owner preserved" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith
      s
      "moved"
      (Game.lookupObject (ObjectId.MkObjectId 1) (afterMountainMoved mountain))
      ( Just
          -- changeZone draws a fresh timestamp; oneMountainState's
          -- nextTimestamp starts at 1 (object 0 already holds 0).
          (Game.cardObject (ObjectId.MkObjectId 0) S.alice S.oneMountainPrintingId Zone.Battlefield (Timestamp.MkTimestamp 1))
            { -- CR 400.7: changeZone clears any singled-out face along with
              -- every other per-incarnation field.
              Object.face = Nothing
            }
      )

  Spec.it s "CR 400.7 changeZone forgets a spell's bindings" $ do
    mountain <- S.printingOf s registry "Mountain"
    let base = S.oneMountainState mountain Phase.PrecombatMain
        slot = SlotName.MkSlotName (Text.pack "target")
        stamped =
          base
            { GameState.objects =
                Map.adjust
                  (\o -> o {Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.alice))) Nothing Seq.empty})
                  (ObjectId.MkObjectId 0)
                  (GameState.objects base)
            }
        moved = S.runPure S.identityAnswer stamped (Event.changeZone Settle.none (ObjectId.MkObjectId 0) Zone.Battlefield)
        landed = Map.elems (Map.filter (\o -> Object.zone o == Zone.Battlefield) (GameState.objects moved))
    Spec.assertEqWith s "reset" (fmap Object.bindings landed) [Map.empty]

  Spec.it s "CR 613.7d changeZone stamps the new incarnation with a fresh timestamp" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs) = S.addPermanent piker S.bob (S.landsInPlay mountain 1)
        before = Bookkeeping.nextTimestamp (GameState.bookkeeping gs)
        movedState = S.runPure S.identityAnswer gs (Event.changeZone Settle.none oid Zone.Graveyard)
        movedId = case Game.zoneMembers Zone.Graveyard S.bob movedState of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        stamp = fmap Object.timestamp (Game.lookupObject movedId movedState)
    Spec.assertEqWith s "the incarnation carries the pre-move next timestamp" stamp (Just before)
    Spec.assertBool s (Bookkeeping.nextTimestamp (GameState.bookkeeping movedState) > before) "the counter advanced"

  Spec.it s "emptyGame starts the timestamp counter at zero" $ do
    Spec.assertEqWith s "zero" (Bookkeeping.nextTimestamp (GameState.bookkeeping (Setup.emptyGame S.bothPlayers))) (Timestamp.MkTimestamp 0)

  Spec.it s "a fresh game has no continuous effects" $ do
    Spec.assertEqWith s "empty" ((StoredEffects.continuousEffects . GameState.stored) (Setup.emptyGame S.bothPlayers)) []

  Spec.it s "a vanilla printing declares no static abilities" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith s "empty" (Face.staticAbilities (S.combinedFace piker)) []

actionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
actionSpec s registry = Spec.describe s "Action" $ do
  Spec.it s "a land in hand is playable in a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (A.Play (ObjectId.MkObjectId 0) Nothing `elem` Action.legalActions S.alice (S.oneMountainState mountain Phase.PrecombatMain)) "play"

  Spec.it s "passing is always legal" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (A.Pass `elem` Action.legalActions S.alice (S.oneMountainState mountain Phase.PrecombatMain)) "pass"

  -- The DEFAULT window, which CR 702.8a lifts for the card the keyword is on and
  -- for no other -- a Mountain prints no flash, so this is the case the flash
  -- group below is measured against.
  Spec.it s "no land play outside a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "only pass" (Action.legalActions S.alice (S.oneMountainState mountain (Phase.Beginning BeginningStep.Upkeep))) [A.Pass]

  -- CR 305.1 / 116.2a: a land play needs an EMPTY STACK as well as a main phase
  -- of the player's own turn -- the same three conjuncts CR 307.5's window has,
  -- which is why the gate asks Turn.sorcerySpeedWindow rather than keeping its
  -- own near-copy. The empty-stack board is the falsifier: a gate that offered
  -- nothing at all would satisfy the second assertion for the wrong reason.
  Spec.it s "CR 305.1 no land play while a spell is on the stack" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.oneMountainState mountain Phase.PrecombatMain
        (_, withSpell) = S.spellOnStack piker S.alice base
    Spec.assertBool s (A.Play (ObjectId.MkObjectId 0) Nothing `elem` Action.legalActions S.alice base) "playable while the stack is empty"
    Spec.assertEqWith s "only pass while a spell is on it" (Action.legalActions S.alice withSpell) [A.Pass]

  -- CR 305.2b: with the normal allowance of one already spent, no further play
  -- is legal. The raised-allowance half of CR 305.2 is Parun.PlayerEffectSpec's
  -- Exploration and Azusa group.
  Spec.it s "CR 305.2b no second land after one is played" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = (S.oneMountainState mountain Phase.PrecombatMain) {GameState.landsPlayed = Map.singleton S.alice 1}
    Spec.assertEqWith s "only pass" (Action.legalActions S.alice gs) [A.Pass]

  -- CR 702.8a says "you may PLAY this card any time you could cast an instant",
  -- and CR 601.1a makes playing a card either casting it or playing it as a
  -- land -- so the keyword moves CR 116.2a's window as well as CR 117.1a's.
  -- Teferi, Mage of Zhalfir's "creature cards you own that aren't on the
  -- battlefield have flash" is the printed route to it: Dryad Arbor is a Land
  -- Creature, so a copy in a hand is in Teferi's set and Cast.flashOn reads the
  -- CR 613.1f grant off the projection.
  --
  -- A PAIR of boards differing in exactly one thing: whether Teferi is on the
  -- battlefield. Same Arbor in the same hand, same upkeep, same seat active. The
  -- UPKEEP is what makes the pair discriminating -- in a main phase the Arbor
  -- goes onto the battlefield either way -- and priorityLoop advances no step,
  -- so alice never reaches a main phase inside it.
  --
  -- Teferi's third clause is on the card and reaches nothing here: it is scoped
  -- to his controller's opponents, alice both controls him and takes the action,
  -- and the action is a land PLAY, which CR 305.1 makes a special action rather
  -- than a cast.
  Spec.it s "CR 702.8a/116.2a a land card with flash is played during its controller's upkeep" $ do
    dryadArbor <- S.printingOf s registry "Dryad Arbor"
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    let (arborId, withTeferi) = arborBoard dryadArbor (Just teferi)
        bare = snd (arborBoard dryadArbor Nothing)
        play gs = S.runPure S.playLandAnswer gs Engine.priorityLoop
        after = play withTeferi
    Spec.assertEqWith s "the Arbor is on the battlefield" (S.countOnBattlefieldByName (S.printingName dryadArbor) S.alice after) 1
    Spec.assertEqWith s "and without Teferi it never left her hand" (S.countOnBattlefieldByName (S.printingName dryadArbor) S.alice (play bare)) 0
    Spec.assertEqWith s "it was still the upkeep" (TurnRegisters.phase (GameState.turn after)) (Phase.Beginning BeginningStep.Upkeep)
    Spec.assertBool s (Projection.hasKeyword Keyword.Flash arborId withTeferi) "the card in hand projects flash"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flash arborId bare)) "and does not without Teferi"

-- Dryad Arbor in alice's hand during alice's UPKEEP with alice holding
-- priority, and Teferi, Mage of Zhalfir on the battlefield or not. The Arbor has
-- no mana cost, so affordability is identical either way and the only thing that
-- varies is the printing this takes.
arborBoard :: Printing.Printing -> Maybe Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
arborBoard dryadArbor mTeferi =
  let (gs0, arborId) = S.handOne dryadArbor (Setup.emptyGame S.bothPlayers)
      gs1 = maybe gs0 (\teferi -> snd (S.addPermanent teferi S.alice gs0)) mTeferi
   in ( arborId,
        gs1
          { GameState.turn =
              (GameState.turn gs1)
                { TurnRegisters.phase = Phase.Beginning BeginningStep.Upkeep
                }
          }
      )

goldfishResult :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Result.Result, GameState.GameState)
goldfishResult s registry = do
  matchup <- S.landsOnly (S.printingOf s registry)
  pure (Engine.runMatchPure S.identityAnswer GameSettings.plain matchup)

landState :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
landState s registry = do
  matchup <- S.shortRedRed (S.printingOf s registry)
  pure (S.opening 6 1 S.playLandAnswer matchup)

engineSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
engineSpec s registry = Spec.describe s "Engine" $ do
  -- The goldfish game is eight Mountains a seat, over at the second turn's
  -- draw; the land game stops after six turns.
  Spec.it s "goldfish game ends with the starting player winning, conserving cards" $ do
    (result, gs) <- goldfishResult s registry
    Spec.assertEqWith s "winner" result (Result.Won S.alice)
    Spec.assertEqWith s "objects" (Game.objectCount gs) 16

  Spec.it s "playing lands fills the battlefield, conserving cards, at most one land per turn (CR 305.2)" $ do
    gs <- landState s registry
    Spec.assertBool s (not (null (Game.zoneMembers Zone.Battlefield S.alice gs))) "non-empty"
    Spec.assertEqWith s "objects" (Game.objectCount gs) 60
    -- With CR 305.2's normal one land play per turn a player can never have
    -- more lands out than turns begun (Turn.turnsBegun). NORMAL is load-bearing
    -- now that a continuous effect can raise the number: this bound holds for
    -- the red-red matchup because no card in it grants an extra land play, and
    -- Parun.PlayerEffectSpec is where the raised number is measured.
    Spec.assertBool
      s
      ( Natural.length (Game.zoneMembers Zone.Battlefield S.alice gs) <= Turn.turnsBegun TurnKind.AnyTurn S.alice gs
          && Natural.length (Game.zoneMembers Zone.Battlefield S.bob gs) <= Turn.turnsBegun TurnKind.AnyTurn S.bob gs
      )
      "no double land plays"

-- Run setup, then a scripted tweak, then whatever steps the scenario needs.
scenario :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Game.Type.Game () -> m GameState.GameState
scenario s registry steps = do
  matchup <- S.redRed (S.printingOf s registry)
  pure . snd . Engine.runGamePure S.identityAnswer (Setup.emptyGame (fmap fst matchup)) $ do
    Setup.newGame S.performer matchup
    steps

-- Alice starts, so her turn-1 draw step is skipped.
aliceFirstDraw :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
aliceFirstDraw s registry = scenario s registry wholeDrawStep

-- The draw step as Engine.runStep takes it, not just its turn-based actions:
-- CR 103.8a skips the whole step (Engine.skipsDraw).
wholeDrawStep :: Game.Type.Game ()
wholeDrawStep = do
  State.modify' $ \gs ->
    gs
      { GameState.turn =
          (GameState.turn gs)
            { TurnRegisters.phase = Phase.Beginning BeginningStep.DrawStep,
              TurnRegisters.remaining = Seq.singleton Phase.PrecombatMain
            }
      }
  Engine.runStep

-- Bob is not the starting player, so his draw happens normally.
bobFirstDraw :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
bobFirstDraw s registry = scenario s registry $ do
  State.modify' $ \gs ->
    gs
      { GameState.turn =
          (GameState.turn gs)
            { TurnRegisters.activePlayer = S.bob,
              TurnRegisters.turnNumber = 2
            }
      }
  S.drawStep

bobAfterCleanup :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
bobAfterCleanup s registry = scenario s registry $ do
  State.modify' $ \gs ->
    gs
      { GameState.turn =
          (GameState.turn gs)
            { TurnRegisters.activePlayer = S.bob,
              TurnRegisters.turnNumber = 2
            }
      }
  S.drawStep
  Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)

deckedOut :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
deckedOut s registry = scenario s registry $ do
  State.modify' $ \gs ->
    gs
      { GameState.library = Map.insert S.alice Seq.empty (GameState.library gs),
        GameState.turn =
          (GameState.turn gs)
            { TurnRegisters.turnNumber = 3
            }
      }
  S.drawStep
  Engine.settleForPriority

librarySize :: PlayerId.PlayerId -> GameState.GameState -> Int
librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)

ruleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ruleSpec s registry = Spec.describe s "Rules" $ do
  Spec.it s "CR 103.8a starting player skips first draw" $ do
    gs <- aliceFirstDraw s registry
    Spec.assertEqWith s "hand" (S.handSize S.alice gs) 7
    Spec.assertEqWith s "library" (librarySize S.alice gs) 53

  Spec.it s "CR 103.8a only the starting player skips" $ do
    gs <- bobFirstDraw s registry
    Spec.assertEqWith s "hand" (S.handSize S.bob gs) 8
    Spec.assertEqWith s "library" (librarySize S.bob gs) 52

  Spec.it s "CR 103.8c/800.7 nobody skips the first draw in a three-player game" $ do
    -- CR 103.8a's skip is a TWO-player rule: "In a two-player game, the
    -- player who plays first skips the draw step ... of their first turn."
    -- CR 103.8c: "In all other multiplayer games, no player skips the draw
    -- step of their first turn." Alice heads the three-seat order and is
    -- active on turn 1.
    --
    -- Neither fixture has a library, so the attempted draw is observable as
    -- drewFromEmpty -- Event.drawCard flags a draw from an empty library.
    -- turnNumber is 1 from Setup.emptyGame, which is load-bearing: it is
    -- skipsDraw's first conjunct.
    let afterThree = S.runPure S.identityAnswer S.threePlayerGame wholeDrawStep
        afterTwo = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) wholeDrawStep
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty afterThree)) "CR 103.8c: alice draws on turn one at three seats"
    Spec.assertBool s (not (Set.member S.alice (GameState.drewFromEmpty afterTwo))) "CR 103.8a: alice still skips at two seats"
    -- CR 121.4's attempt draws no card, so it is not one of CR 121.1's draws: the
    -- per-turn tally that CR 702.94a's "the first card you've drawn this turn"
    -- is read off stays empty even on the seat that tried.
    Spec.assertEqWith s "an empty-library draw counts as no draw" (TurnRegisters.drawsThisTurn (GameState.turn afterThree)) Map.empty

  Spec.it s "CR 514.2 discard to hand size" $ do
    gs <- bobAfterCleanup s registry
    Spec.assertEqWith s "hand" (S.handSize S.bob gs) 7

  Spec.it s "CR 704.5b deck-out loses" $ do
    gs <- deckedOut s registry
    Spec.assertEqWith
      s
      "alice departed"
      (fmap Player.status (Map.lookup S.alice (GameState.players gs)))
      (Just (Status.Departed Departure.Type.Lost))

  Spec.it s "CR 704.5b the survivor wins" $ do
    gs <- deckedOut s registry
    Spec.assertEqWith s "bob won" (GameState.result gs) (Just (Result.Won S.bob))

  Spec.it s "CR 400.7/728.2 gameplay: a restart puts Painter's Servant into a library with its chosen colour forgotten" $ do
    -- The card-driven proof for Setup.startGameFromCards' hand-written zone
    -- move. Painter's Servant is CAST rather than placed, because CR 614.1c's
    -- colour choice happens only on the entry path (Event.runEntry) -- a
    -- Servant put straight onto the battlefield has chosenColors = Set.empty and
    -- this test would assert nothing. S.identityAnswer answers ChooseColor with
    -- white, which is all this needs: WHICH colour it chose does not matter,
    -- only that it chose one.
    --
    -- Then bob restarts the game with Karn's ultimate and CR 728.2 rebuilds
    -- every library from the existing cards. The Servant is a new object in that
    -- library (CR 400.7), so the colour it chose is gone.
    karn <- S.printingOf s registry "Karn Liberated"
    paintersServant <- S.printingOf s registry "Painter's Servant"
    mountain <- S.printingOf s registry "Mountain"
    let base = S.landsInPlay mountain 2
        (inHand, handId) = S.handOne paintersServant base
        -- alice's bulk pool is built BEFORE the cast, and deliberately so. Both
        -- players need >= 7 owned cards or CR 728.3's short-deck loss ends the
        -- rebuilt game, but the Servant also has to still be IN alice's library
        -- afterwards: a DRAWN Servant goes through Event.changeZone and comes
        -- out clean no matter what startGameFromCards did, which is the vacuous
        -- pass this test exists to avoid. startGameFromCards orders each
        -- library by object id and the opening hand comes off the front, so
        -- minting these mountains first leaves the Servant's battlefield
        -- incarnation -- whose id the cast mints last -- at the back, behind
        -- the seven cards drawn. The library assertion below is what keeps that
        -- reasoning honest if it ever stops holding.
        bulked = addManyG mountain 24 S.alice inHand
        castServant = snd (Engine.runGamePure S.identityAnswer bulked (S.cast S.alice handId))
        painted = snd (Engine.runGamePure S.identityAnswer castServant Stack.resolveTop)
        (karnId, withKarn) = S.addPermanent karn S.bob painted
        withRestart = S.addCounter CounterKind.Loyalty 14 karnId withKarn
        filled = addManyG mountain 7 S.bob withRestart
        gStart =
          filled
            { GameState.turn =
                (GameState.turn filled)
                  { TurnRegisters.activePlayer = S.bob,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.bob
                  }
            }
        after = snd (Engine.runGamePure restartAnswer gStart Engine.priorityLoop)
        -- The Servant's BATTLEFIELD incarnation: the cast minted new ids at
        -- each hop (CR 400.7), so `handId` is not it. startGameFromCards keeps
        -- the id it rebuilds, so this same id is what to look for afterwards.
        entered = Set.toList (Set.difference (GameState.battlefield painted) (GameState.battlefield bulked))
    case entered of
      [servantId] -> case (Game.lookupObject servantId gStart, Game.lookupObject servantId after) of
        -- The discriminator: the Servant really did make a CR 614.1c choice
        -- going in, and the object that carried it is still the same object,
        -- sitting in a library, coming out.
        (Just before, Just rebuilt) -> do
          Spec.assertBool s (not (Map.null (Object.chosenColors before))) "CR 614.1c: the cast Servant chose a colour as it entered"
          Spec.assertEqWith s "CR 728.2: the rebuilt Servant is the same object, in a library" (Object.zone rebuilt) Zone.Library
          Spec.assertEqWith s "CR 400.7: the Servant in the library forgot the colour it chose" (Object.chosenColors rebuilt) Map.empty
        (_, Nothing) -> Spec.assertFailure s "the opening draw took the Servant out of the library; raise alice's pool"
        _ -> Spec.assertFailure s "the fixture lost the Servant before the restart"
      _ -> Spec.assertFailure s "Painter's Servant did not reach the battlefield"
    Spec.assertEqWith s "CR 728.2: the restart really ran (the battlefield is empty)" (Set.null (GameState.battlefield after)) True
    Spec.assertEqWith s "the game did not end, so the rebuild is the live state" (GameState.result after) Nothing

  Spec.it s "CR 730.2/730.3/730.5: playSubgame runs a nested game, bob decks, cards funnel back" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        -- alice: 8 library cards; bob: 3 (fewer than seven -> loses, CR 730.3)
        g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 3 S.bob (addManyG mountain 8 S.alice g0)))
        (result, after) = Engine.runGamePure S.identityAnswer g1 Engine.playSubgame
        libCount pid = length (Game.zoneMembers Zone.Library pid after)
    Spec.assertEqWith s "CR 730.3: bob has fewer than 7 cards, so alice wins the subgame" result (Result.Won S.alice)
    Spec.assertEqWith s "CR 730.5: alice's cards funnel back into her main-game library" (libCount S.alice) 8
    Spec.assertEqWith s "CR 730.5: bob's cards funnel back into his main-game library" (libCount S.bob) 3
    Spec.assertEqWith s "the main game resumes with no result recorded" (GameState.result after) Nothing

  -- #136 / CR 730.2: "Randomly determine which player goes first." The
  -- interpreter supplies the randomness (Prompt.RandomFirstPlayer); the
  -- engine only asks. Both players get libraries of EXACTLY seven, so each
  -- opening hand (CR 103.5) empties its library without drawing from empty:
  -- nobody loses during setup. The starting player then skips their first
  -- draw (CR 103.8a), so the OTHER player is the one who draws from an empty
  -- library on turn 2 and decks (CR 704.5b) -- the subgame's winner is
  -- exactly whoever the roll started. Flipping the answer flips the winner,
  -- which is what makes the determination observable rather than cosmetic.
  Spec.it s "CR 730.2: the subgame's first player comes from the roll; the answer decides who wins" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 7 S.bob (addManyG mountain 7 S.alice g0)))
        winnerWhenStarting starter = fst (Engine.runGamePure (firstPlayerAnswer starter) g1 Engine.playSubgame)
    Spec.assertEqWith s "alice starts, skips her first draw, and bob decks on turn 2" (winnerWhenStarting S.alice) (Result.Won S.alice)
    Spec.assertEqWith s "bob starts, skips his first draw, and alice decks on turn 2" (winnerWhenStarting S.bob) (Result.Won S.bob)

  Spec.it s "CR 730.2: a lone player is not asked -- the determination is forced" $ do
    -- Where the rules leave nothing to determine, don't prompt: with one
    -- player in the turn order, every roll yields the same starter. alice's
    -- library is empty, so her opening draw decks her (CR 704.5b) and the
    -- subgame ends during setup -- long enough to record an ask if one were
    -- made, which is what the transcript is inspected for.
    let g0 = Setup.emptyGame (S.alice NonEmpty.:| [])
        (_, log_) = Replay.record S.identityAnswer g0 Engine.playSubgame
        isRoll r = case r of
          Response.DeterminedFirstPlayer _ -> True
          _ -> False
    Spec.assertEqWith s "no first-player roll was recorded" (length (filter isRoll log_)) 0

  Spec.it s "CR 103.1/730.2: the subgame's turn order is rotated to begin with the starting player" $ do
    -- Not just activePlayer: Engine.skipsDraw (CR 103.8a) reads the HEAD of
    -- the turn order, so a subgame that set one without the other would hand
    -- the skip to the wrong player.
    let sub = Setup.subgameStateFrom S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "bob is the subgame's active player" (TurnRegisters.activePlayer (GameState.turn sub)) S.bob
    Spec.assertEqWith s "the subgame turn order begins with bob" (GameState.seating sub) [S.bob, S.alice]

  Spec.it s "CR 730.4: the subgame sees the main game's cards as outside it, minus the ones it took" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let g0 = poolToLibraryG S.alice (addManyG mountain 3 S.alice (Setup.emptyGame S.bothPlayers))
        (pikerId, g1) = S.addPermanent piker S.alice g0
        sub = Setup.subgameStateFrom S.alice g1
    Spec.assertEqWith s "the battlefield card is outside the subgame" (Map.member pikerId (GameState.outsideObjects sub)) True
    Spec.assertEqWith s "CR 730.2: the library cards moved INTO the subgame, so they are not outside it" (length (GameState.outsideObjects sub)) 1
    Spec.assertEqWith s "and it is hers" (fmap OutsideObject.owner (Map.lookup pikerId (GameState.outsideObjects sub))) (Just S.alice)

  Spec.it s "CR 730.2/730.4 #147: a departed player is not offered as the subgame's first player, and has no library for the subgame to churn" $ do
    -- Three seats; bob has left. CR 800.4a's first clause took his cards with
    -- him, so what this case now pins is (a) the CR 730.2 roll offers only the
    -- players still in the game, and (b) the subgame round trip does not
    -- resurrect a departed player's library. The full-roster span in
    -- Setup.subgameStateFrom and Setup.funnelBack still has to match; see the
    -- comments at both sites.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.threePlayers
        g1 = poolToLibraryG S.carol (poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 7 S.carol (addManyG mountain 3 S.bob (addManyG mountain 7 S.alice g0)))))
        parent = S.departs Departure.Type.Conceded S.bob g1
        ((_, after), rolls) = State.runState (Engine.runGame subgameRosterAnswer parent Engine.playSubgame) []
    Spec.assertEqWith s "the roll offers only the players still in the game" rolls [[S.alice, S.carol]]
    Spec.assertEqWith s "CR 800.4a: bob's cards left the game with him" (Game.zoneMembers Zone.Library S.bob parent) []
    Spec.assertEqWith s "and the subgame round trip does not give them back" (Game.zoneMembers Zone.Library S.bob after) []
    Spec.assertEqWith s "alice's library came back whole" (length (Game.zoneMembers Zone.Library S.alice after)) 7
    Spec.assertEqWith s "and carol's" (length (Game.zoneMembers Zone.Library S.carol after)) 7
    Spec.assertEqWith s "the main game is not decided by the subgame" (GameState.result after) Nothing

  -- The CR 730.1b board played for ante: the subgame antes one card from each
  -- library (CR 407.2), bob's 6 and carol's 4 cannot fill a 7-card hand, both
  -- leave (CR 800.4a: three seats), and CR 800.4n keeps their ante cards for
  -- alice, who wins. alice has 9 because CR 103.8c has her draw on turn 1.
  -- Then CR 730.5 sends every card to its owner's main-game library: alice's 9
  -- and the two she won, and the rest of bob's and carol's -- once each.
  Spec.it s "CR 407.2/730.5 gameplay: a Shahrazad subgame's ante goes to its winner's main-game library" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    shahrazad <- S.printingOf s registry "Shahrazad"
    let after = castShahrazad forAnte mountain plains shahrazad 9
        libraryOf pid = length (Game.zoneMembers Zone.Library pid after)
    Spec.assertEqWith s "CR 407.2/730.5 alice's main-game library holds the two ante cards she won" (libraryOf S.alice) 11
    Spec.assertEqWith s "CR 730.5 bob's library is one short: his ante card is alice's now" (libraryOf S.bob) 5
    Spec.assertEqWith s "and carol's" (libraryOf S.carol) 3
    Spec.assertEqWith s "CR 108.3 the report lists both" (List.sort (changesOf after)) [(S.bob, S.alice), (S.carol, S.alice)]
    Spec.assertEqWith s "the main game did not end" (GameState.result after) Nothing
  -- CR 407.3 / 800.4a / 730.5: a Shahrazad subgame played for ante, in which
  -- alice casts Darkpact on bob's ante card (darkpactAnswer), so it goes to
  -- her subgame library, and then concedes. The card leaves the subgame with
  -- her; bob outlasts carol, wins, and takes the subgame's ante (CR 407.2).
  -- It comes home to alice's main-game library, and the report lists it.
  Spec.it s "CR 730.5 gameplay: a card that left a subgame with its new owner goes to their main-game library" $ do
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    plains <- S.printingOf s registry "Plains"
    shahrazad <- S.printingOf s registry "Shahrazad"
    darkpact <- S.printingOf s registry "Darkpact"
    let after = castDarkpactInShahrazad mountain swamp plains shahrazad darkpact
        libraryOf pid = length (Game.zoneMembers Zone.Library pid after)
    Spec.assertEqWith s "CR 730.5 alice's main-game library holds one card bob began the game with" (length (filter (\oid -> fmap CardIdentity.startingOwner (Game.lookupObject oid after >>= Object.identity) == Just S.bob) (Game.zoneMembers Zone.Library S.alice after))) 1
    Spec.assertEqWith s "CR 407.2 / 730.5 the libraries: alice's ten and bob's card, bob's eleven and the three ante cards he won, carol's nine" (fmap libraryOf [S.alice, S.bob, S.carol]) [11, 14, 9]
    Spec.assertEqWith s "CR 108.3 the report lists it, from bob to alice, beside the ante bob won" (List.sort (changesOf after)) [(S.alice, S.bob), (S.alice, S.bob), (S.bob, S.alice), (S.carol, S.bob)]
  -- The drawn board (CR 104.4a): all three leave, nobody is paid, and each
  -- library comes back whole, its ante card in it.
  Spec.it s "CR 104.4a/730.5 gameplay: a drawn Shahrazad subgame pays out no ante card" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    shahrazad <- S.printingOf s registry "Shahrazad"
    let after = castShahrazad forAnte mountain plains shahrazad 3
    Spec.assertEqWith s "CR 730.5 each library is whole again" (fmap (\pid -> length (Game.zoneMembers Zone.Library pid after)) [S.alice, S.bob, S.carol]) [3, 6, 4]
    Spec.assertEqWith s "CR 407.2 and no card changed owner" (Ante.ownershipChanges after) Seq.empty
  -- The won board with alice and bob on one team (CR 104.2c): bob decked in
  -- the subgame and still wins, and alice, asked, hands each ante card to him
  -- (shahrazadAnswer): alice's own and carol's change owner, and bob's stays his.
  Spec.it s "CR 104.2c gameplay: a Shahrazad subgame won by a team pays the ante to the winner it names" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    shahrazad <- S.printingOf s registry "Shahrazad"
    let teamed gs = gs {GameState.match = (GameState.match gs) {MatchScope.settings = (MatchScope.settings (GameState.match gs)) {GameSettings.teams = Teams.MkTeams (Map.fromList [(S.alice, TeamId.MkTeamId 0), (S.bob, TeamId.MkTeamId 0), (S.carol, TeamId.MkTeamId 1)])}}}
        after = castShahrazad (forAnte . teamed) mountain plains shahrazad 9
    Spec.assertEqWith s "CR 407.2 alice's and carol's ante cards are bob's" (List.sort (changesOf after)) [(S.alice, S.bob), (S.carol, S.bob)]
    Spec.assertEqWith s "CR 730.5 and they went to his main-game library" (fmap (\pid -> length (Game.zoneMembers Zone.Library pid after)) [S.alice, S.bob, S.carol]) [8, 8, 3]

  -- CR 614.1c with CR 730.1a: a subgame started by an as-enters effect, which
  -- never reaches the stack. Three seats and a winner who is not the caster, so
  -- "the winner draws" cannot pass as "you draw". The two cases are the same
  -- board with one dial, bob's library: 8 cards survive the subgame's opening
  -- hand (CR 730.3) and 3 do not, leaving a draw that binds nobody.
  Spec.it s "CR 614.1c / 730.1b gameplay: an as-enters subgame's winner draws a card" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    threshold <- S.printingOf s registry "Synthetic Threshold of Legends"
    let after = castThreshold island mountain threshold 8
    Spec.assertEqWith s "CR 730.1b: bob won the subgame and drew a card in the main game" (S.handSize S.bob after) 1
    Spec.assertEqWith s "alice, who cast it but lost, drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "nor did carol" (S.handSize S.carol after) 0
    Spec.assertEqWith s "CR 730.5: bob's draw came from his funnelled-back library" (length (Game.zoneMembers Zone.Library S.bob after)) 7
    Spec.assertEqWith s "the main game did not end" (GameState.result after) Nothing

  Spec.it s "CR 614.1c / 730.1b gameplay: a drawn as-enters subgame has no winner to draw" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    threshold <- S.printingOf s registry "Synthetic Threshold of Legends"
    let after = castThreshold island mountain threshold 3
    Spec.assertEqWith s "bob's 3 cards cannot fill an opening hand either, so nobody won and he drew nothing" (S.handSize S.bob after) 0
    Spec.assertEqWith s "and his library is whole" (length (Game.zoneMembers Zone.Library S.bob after)) 3
    Spec.assertEqWith s "the drawn subgame did not decide the main game (CR 730.1a)" (GameState.result after) Nothing

  -- CR 605.4a with CR 730.1a: a subgame started by a triggered mana ability,
  -- which never reaches the stack and runs while alice pays for a spell. CR
  -- 605.1b has no library clause, so the ability stays a mana ability. Three
  -- seats and the same dial as the as-enters pair above: bob's library.
  Spec.it s "CR 605.4a / 730.1b gameplay: a triggered mana ability's subgame winner draws a card" $ do
    after <- castThroughWellspring s registry 8
    Spec.assertEqWith s "CR 730.1b: bob won the subgame and drew a card in the main game" (S.handSize S.bob after) 1
    Spec.assertEqWith s "alice, whose land was tapped, lost and drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "nor did carol" (S.handSize S.carol after) 0
    Spec.assertEqWith s "CR 730.5: bob's draw came from his funnelled-back library" (length (Game.zoneMembers Zone.Library S.bob after)) 7
    merfolk <- S.printingOf s registry "Merfolk of the Pearl Trident"
    Spec.assertEqWith s "CR 730.5: the main game resumed, and the spell the mana paid for resolved" (S.countOnBattlefieldByName (S.printingName merfolk) S.alice after) 1
    Spec.assertEqWith s "the main game did not end" (GameState.result after) Nothing

  Spec.it s "CR 605.4a / 730.1b gameplay: a drawn mana-ability subgame has no winner to draw" $ do
    after <- castThroughWellspring s registry 3
    Spec.assertEqWith s "bob's 3 cards cannot fill an opening hand either, so nobody won and he drew nothing" (S.handSize S.bob after) 0
    Spec.assertEqWith s "and his library is whole" (length (Game.zoneMembers Zone.Library S.bob after)) 3
    Spec.assertEqWith s "the drawn subgame did not decide the main game (CR 730.1a)" (GameState.result after) Nothing

  -- CR 730.1a #137: "the spell or ability that created the subgame" -- the rule
  -- names both kinds of object, and Shahrazad and Sindbad (Unknown Event,
  -- Creature -- Human, type line and Oracle text checked against
  -- api.scryfall.com) is the printing that reaches the ability half: "whenever
  -- this creature deals combat damage to an opponent, ... players play a Magic
  -- subgame ... Each player who doesn't win the subgame loses half their life,
  -- rounded up." Scryfall o:subgame include:extras (2026-08-19) returns five
  -- cards, and the other four -- Shahrazad, Enter the Dungeon, The Countdown Is
  -- at One, Tug of War -- are Sorceries; a sixth that played one from an ability
  -- would refute nothing here, but would be the better-ranked producer if it were
  -- black border (docs/design.md section 6).
  --
  -- THREE SEATS, and the subgame's winner is CAROL -- neither the ability's
  -- controller nor the player the combat damage was dealt to, so "the player
  -- CR 730.1b excludes" is a different seat from either of the two the trigger
  -- itself names.
  --
  -- THREE DISTINCT LIFE TOTALS and three distinct library sizes: who wins the
  -- subgame is decided by the libraries (CR 730.3), and the halving is read
  -- against each payer's own total. 20, 10 (13 less the Sindbad's 3) and 9 halve
  -- to 10, 5 and 5-rounded-up-from-4.5 -- so a subgame that bound no winner
  -- charges carol 5 and leaves her on 4, which her printed 9 cannot be confused
  -- with.
  --
  -- The second ability, "{T}: Draw a card and reveal it. If it isn't a land
  -- card, discard it.", IS transcribed; Parun.ZoneChangeSpec is where it is
  -- proven.
  --
  -- The intervening "if there haven't been any subgames this match" (CR 603.4)
  -- IS transcribed, over MatchScope.subgamesThisMatch; the case below proves it,
  -- on this board with the tally already at 1.
  --
  -- "An opponent" is not transcribed either, and that one IS a rules equivalence:
  -- CR 508.1a lets only the active player's creatures attack, CR 506.2 and CR
  -- 506.2a make the defending player one of the attacking player's opponents, and
  -- CR 510.1b assigns an unblocked creature's damage to what it is attacking, so
  -- combat damage a creature deals to a player is always dealt to its
  -- controller's opponent (Parun.KeywordTriggerSpec's Questing Beast case makes
  -- the same reading of the same printed phrase).
  --
  -- The trigger playing its subgame on this board is the scenario
  -- game/cr-730-1a-730-1b-gameplay-a-triggered-ability-plays-the-subgame-and-cr-730-1b-s-winner-is-bound.json.
  --
  -- CR 603.4 over CR 100.6a: the SAME board as that scenario with exactly one
  -- thing different -- the match has already had a subgame -- so Shahrazad and
  -- Sindbad's intervening "if there haven't been any subgames this match" is
  -- false and the ability does not trigger at all. Same combat, same damage,
  -- same mana and the same three libraries, so nothing but the tally can be
  -- what stops it.
  Spec.it s "CR 603.4/100.6a gameplay: Shahrazad and Sindbad does not trigger once the match has had a subgame" $ do
    mountain <- S.printingOf s registry "Mountain"
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    let (before, placed, after) = sindbadCombat mountain sindbad 1
        isRoll r = case r of
          Response.DeterminedFirstPlayer _ -> True
          _ -> False
        rolls = length (filter isRoll (snd (Replay.record sindbadAnswer placed Engine.priorityLoop)))
    Spec.assertEqWith s "the fixture's own tally: one subgame already played this match" (MatchScope.subgamesThisMatch (GameState.match before)) 1
    -- THE proving assertion, at gameplay level: nobody paid the CR 730.1b half,
    -- because no subgame was played. bob is on 10, the combat damage alone.
    Spec.assertEqWith s "CR 603.4: no subgame, so no player halves their life" (S.lifeOf S.alice after, S.lifeOf S.bob after, S.lifeOf S.carol after) (Just 20, Just 10, Just 9)
    Spec.assertEqWith s "CR 730.2: no subgame determined a first player" rolls 0
    Spec.assertEqWith s "CR 100.6a: the tally stands where it started" (MatchScope.subgamesThisMatch (GameState.match after)) 1
    Spec.assertEqWith s "and nothing was left on the stack" (TurnRegisters.stack (GameState.turn after)) []

  Spec.it s "CR 730.1a #153: a question names the game it came from, so a subgame's is not a main-game one" $ do
    -- The same cast-a-subgame fixture as the two cases around this one, run
    -- through the Asked seam instead of the Prompt one. `survivorId` is a
    -- creature on the MAIN battlefield, and CR 730.2 puts nothing but library
    -- cards and CR 730.2c's commanders into the subgame -- so it is a fact that
    -- holds of one game and not the other, which is exactly what an untagged
    -- question could not tell you. This board designates no commander.
    mountain <- S.printingOf s registry "Mountain"
    syntheticSubgame <- S.printingOf s registry "Synthetic Subgame"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 3 S.bob (addManyG mountain 8 S.alice g0)))
        (survivorId, g2) = S.addPermanent mountain S.alice g1
        (_spellId, g3) = S.addHandCard syntheticSubgame S.alice g2
        gStart =
          g3
            { GameState.turn =
                (GameState.turn g3)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        tags = reverse (State.execState (Engine.runGameAsked (recordAskedGame survivorId) gStart Engine.priorityLoop) [])
        (mainAsks, subAsks) = List.partition (\tag -> askDepth tag == 0) tags
    Spec.assertBool s (not (null mainAsks)) "the main game asked something"
    Spec.assertBool s (not (null subAsks)) "and so did the subgame -- otherwise the split below is vacuous"
    Spec.assertEqWith s "no question came from deeper than one subgame" (filter (\tag -> askDepth tag > 1) tags) []
    Spec.assertEqWith
      s
      "CR 730.2: no subgame question is about a board holding the main game's creature"
      (filter askedGameHasSurvivor subAsks)
      []
    Spec.assertEqWith
      s
      "and every one of them still reaches the main game, which does hold it"
      (filter (not . mainGameHasSurvivor) subAsks)
      []
    Spec.assertEqWith
      s
      "a main-game question is about the main game, and it holds the creature"
      (filter (not . askedGameHasSurvivor) mainAsks)
      []

  Spec.it s "CR 730.6 gameplay: a subgame nests a subgame; nesting terminates and the main game resumes" $ do
    -- alice's MAIN-GAME library feeds level 1's library: one nested
    -- synthetic-subgame sorcery + 13 Mountains (14 total). The level-1 opening
    -- hand draws 7 (the sorcery + 6 Mountains), leaving exactly 7 Mountains in
    -- her level-1 library -- enough that she does NOT deck when level 2's
    -- opening hand draws from it (CR 730.2 pulls a subgame's library from its
    -- parent's library). bob's library is exactly 7 Mountains: his level-1
    -- opening hand consumes all seven (no immediate CR 704.5b loss -- every
    -- draw still succeeds), leaving his level-1 library EMPTY, so he decks at
    -- his own level-1 draw step (turn 2) -- real level-1 play, not an instant
    -- SBA loss before anyone gets priority. Sized this way (rather than the
    -- brief's flat "3 Mountains decks instantly") because an immediate deck-out
    -- during subgame setup fires before ANY priorityLoop grants alice priority
    -- (Sba.losesNow reads GameState.drewFromEmpty, set during the opening-hand
    -- draw itself), which would leave alice no window to cast the nested
    -- sorcery at all and collapse this gate to a flat (non-nested) subgame.
    --
    -- CR 730.1a's isolation means a subgame's INTERNAL choices leave no trace
    -- in the parent's GameState -- but the interpreter TRANSCRIPT (every
    -- Response, recorded by Parun.Engine.Replay.record) is a top-level observable, and
    -- it DOES discriminate nesting depth: each subgame level's setup
    -- (subgameStateFrom -> startGameFromCards) shuffles every player's
    -- library once, and playSubgame's CR 730.5 funnel-back reshuffles the
    -- parent's library once per player -- 2 Response.Shuffled entries per
    -- level, per funnel. A flat (single-level) subgame gate contributes 4
    -- (2 setup + 2 funnel-back); this two-level gate contributes 8 (2 levels
    -- x (2 setup + 2 funnel-back)), so asserting the count is a genuine
    -- nesting regression test, not just a termination guard.
    syntheticSubgame <- S.printingOf s registry "Synthetic Subgame"
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        (_nestedId, g1) = libraryCard syntheticSubgame S.alice g0
        g2 = poolToLibraryG S.bob (addToLibraryG mountain 13 S.alice (addManyG mountain 7 S.bob g1))
        (_spellId, g3) = S.addHandCard syntheticSubgame S.alice g2
        gStart =
          g3
            { GameState.turn =
                (GameState.turn g3)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        ((_, after), log_) = Replay.record subgameAnswer gStart Engine.priorityLoop
        isShuffled r = case r of
          Response.Shuffled _ -> True
          _ -> False
        shuffles = length (filter isShuffled log_)
    -- If nesting had not terminated, runGamePure/Replay.record would not return.
    Spec.assertEqWith s "CR 730.6: the top-level main game resumed with no result" (GameState.result after) Nothing
    Spec.assertEqWith s "CR 730.1b: bob lost 3 to the level-1 subgame's follow-on" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "the top-level subgame spell left the stack" (TurnRegisters.stack (GameState.turn after)) []
    Spec.assertEqWith s "CR 730.6: two nested subgame levels each shuffle on setup and funnel-back (measured; a flat gate yields 4)" shuffles 8
    -- CR 100.6a: a subgame started INSIDE a subgame is still a subgame of this
    -- match, so the resumed top-level game counts both (Setup.funnelBack).
    Spec.assertEqWith s "CR 730.6/100.6a: both levels are on the match's books" (MatchScope.subgamesThisMatch (GameState.match after)) 2
    Spec.assertEqWith s "CR 730.6: the main game resumes as the match's first game of three" (MatchScope.gameNumber (GameState.match after), MatchScope.gamesThisMatch (GameState.match after)) (1, 3)

  Spec.it s "a subgame replays deterministically (the reason Prompt.PlaySubgame was rejected, CR 730 / M0's determinism criterion)" $ do
    -- A Prompt would run the subgame INSIDE the answer function, below
    -- Replay.record's interposition point, so its inner choices could never
    -- be recovered from the recorded transcript. Round-tripping this nested
    -- gate's fixture through record -> replay and comparing the final
    -- GameState (derives Eq) is the test that would fail if that design had
    -- been taken instead.
    syntheticSubgame <- S.printingOf s registry "Synthetic Subgame"
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        (_nestedId, g1) = libraryCard syntheticSubgame S.alice g0
        g2 = poolToLibraryG S.bob (addToLibraryG mountain 13 S.alice (addManyG mountain 7 S.bob g1))
        (_spellId, g3) = S.addHandCard syntheticSubgame S.alice g2
        gStart =
          g3
            { GameState.turn =
                (GameState.turn g3)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        ((_, after), log_) = Replay.record subgameAnswer gStart Engine.priorityLoop
        ((_, replayed), desync) = Replay.replay log_ gStart Engine.priorityLoop
    Spec.assertEqWith s "a subgame replays deterministically (the reason PlaySubgame is not a Prompt, CR 730 / M0 determinism)" replayed after
    Spec.assertEqWith s "and the transcript answered every prompt" desync Nothing

  Spec.it s "M5.6b gate: three-player setup -- a free first mulligan (CR 103.5c) and no first-turn draw skip (CR 103.8c)" $ do
    -- A real newGame at three seats: build and shuffle three 60-card
    -- libraries, draw opening hands, run the CR 103.5 round loop with every
    -- player taking exactly one mulligan, then run turn one's draw step.
    --
    -- CR 103.5c: the first mulligan does not count toward the number of cards
    -- bottomed, so every player keeps seven and nothing goes back to the
    -- library -- today each keeps six and bottoms one. CR 103.8c: the
    -- starting player does not skip the first draw -- today alice skips.
    matchup <- S.threeWayMirror (S.printingOf s registry)
    let g0 = Setup.emptyGame (fmap fst matchup)
        afterSetup = snd (Engine.runGamePure mulliganOnceAnswer g0 (Setup.newGame S.performer matchup))
        afterDraw = snd (Engine.runGamePure mulliganOnceAnswer afterSetup S.drawStep)
    Spec.assertEqWith s "alice's free mulligan bottomed nothing" (S.handSize S.alice afterSetup) 7
    Spec.assertEqWith s "bob's did not either" (S.handSize S.bob afterSetup) 7
    Spec.assertEqWith s "nor carol's" (S.handSize S.carol afterSetup) 7
    Spec.assertEqWith s "so alice's library is 60 less her seven, with nothing bottomed back" (librarySize S.alice afterSetup) 53
    Spec.assertEqWith s "CR 103.8c: alice draws on turn one" (S.handSize S.alice afterDraw) 8
    Spec.assertEqWith s "and her library is one lower for it" (librarySize S.alice afterDraw) 52
    Spec.assertEqWith s "the draw step is alice's alone" (S.handSize S.bob afterDraw) 7

-- #133 / CR 104.3a. Concede is a special action, not a card, so the gate is
-- gameplay-level. The central case is CR 724.6: a Mindslaver controller may not
-- concede for the player they control, but that player may still concede
-- themselves -- which is why Prompt.Concede carries no Decider.

-- Concedes for exactly one player, continues for everyone else, and otherwise
-- passes. The PlayerId the prompt carries is the TRUE player: if the engine ever
-- routed this through Decide.deciderFor, this answerer would concede for the
-- wrong person and the CR 724.6 test below would fail.
concedeAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
concedeAnswer who p = case p of
  Prompt.Concede asked -> if asked == who then Concession.Concedes else Concession.Continues
  _ -> S.identityAnswer p

-- Records the PlayerId of every Concede ask, in order, and concedes for `who`.
-- Prompt.Concede is asked of the priority holder before anything else, so the
-- recorded order IS the order priority moved in. Delegating through a wildcard
-- keeps this answerer out of the -Werror exhaustiveness net.
concedeOrderAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
concedeOrderAnswer who p = case p of
  Prompt.Concede asked -> do
    State.modify' (\seen -> seen <> [asked])
    pure (if asked == who then Concession.Concedes else Concession.Continues)
  _ -> pure (S.identityAnswer p)

concedeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
concedeSpec s _ = Spec.describe s "concede (CR 104.3a)" $ do
  Spec.it s "the conceding player departs as Conceded, not Lost" $ do
    let gs =
          (Setup.emptyGame S.bothPlayers)
            { GameState.turn =
                (GameState.turn (Setup.emptyGame S.bothPlayers))
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice
                  }
            }
        after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
    Spec.assertEqWith s "reason recorded" (fmap Player.status (Map.lookup S.alice (GameState.players after))) (Just (Status.Departed Departure.Type.Conceded))

  -- #144, and the proof that the elision is NOT cosmetic latency. CR 104.3a
  -- says a player may concede "at any time"; Engine.priorityLoop offers the
  -- ask only at a priority the conceder themselves holds, and CR 117.5 /
  -- CR 704.3 put a state-based action check in front of every priority
  -- grant. So the two are in a race, and the race decides the game.
  --
  -- alice is at 0 life and it is her turn, with nothing yet settled. bob
  -- wants out.
  --
  --   * conceding BEFORE the settle: bob leaves immediately (CR 104.3a),
  --     alice's opponents have all left, and CR 104.2a hands her the win --
  --     "immediately and overrides all effects that would preclude that
  --     player from winning the game", her own 0 life included.
  --   * conceding at bob's NEXT priority: settleForPriority runs first,
  --     CR 104.3b takes alice at 0 life, and bob wins before he is ever
  --     asked.
  --
  -- Same start state, two different winners, differing only in WHEN the
  -- concession lands. This pins both arms rather than picking one: no set
  -- of poll points a pull channel can offer is "any time", so widening the
  -- poll would move this race rather than end it.
  Spec.it s "#144 conceding before the settle window and after it name different winners" $ do
    let base =
          (Setup.emptyGame S.bothPlayers)
            { GameState.turn =
                (GameState.turn (Setup.emptyGame S.bothPlayers))
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice
                  }
            }
        gs = base {GameState.players = Map.adjust (\p -> p {Player.life = 0}) S.alice (GameState.players base)}
        -- The engine as it stands. concedeOrderAnswer records every Concede
        -- ask, so the test does not have to take the narrowness of the
        -- window on trust: bob is asked nothing at all.
        ((_, atOwnPriority), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.bob) gs Engine.runStep) []
        -- The counterfactual, reached through the same door the engine
        -- uses (Departure.leaveGame IS priorityLoop's concede arm): bob's
        -- concession lands before the settle instead of after it.
        conceded = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.bob)
        beforeSettle = S.runPure (concedeAnswer S.bob) conceded Engine.runStep
    Spec.assertEqWith s "the settle got there first: nobody was ever asked" asks []
    Spec.assertEqWith s "CR 104.3b takes alice first, so bob wins" (GameState.result atOwnPriority) (Just (Result.Won S.bob))
    Spec.assertEqWith s "CR 104.2a: had bob got out first, alice wins" (GameState.result beforeSettle) (Just (Result.Won S.alice))
    Spec.assertBool s (GameState.result atOwnPriority /= GameState.result beforeSettle) "the race decides the game, not its timing"

-- M5.6a / CR 800.4: the turn order and priority control flow around a departure
-- that does NOT end the game. Most cases here need three seats: at two players
-- a departure ends the game before the next thing happens, which is exactly why
-- these defects survived five milestones. One case below is the exception on
-- purpose -- it needs exactly two, to reach a state three seats cannot.
--
-- S.identityAnswer returns [] for Prompt.DeclareAttackers, so it cannot tell a
-- SKIPPED prompt from an ANSWERED one that legally attacks with nothing.
-- Records the PlayerId of every DeclareAttackers ask and attacks with
-- everything offered. The ASK is the observable, not the resulting attackers
-- map: once CR 800.4a removes a departed player's permanents there is nothing
-- for them to attack with, so an empty attackers map alone is no longer
-- evidence that the CR 703.4i guard fired (a two-player board sidesteps that
-- narrowing entirely -- see the load-bearing case below). No new Prompt
-- constructor lands in this phase, so the wildcard keeps this answerer out of
-- the -Werror exhaustiveness net.
declareAttackersAskAnswer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
declareAttackersAskAnswer p = case p of
  Prompt.DeclareAttackers _ pid candidates -> do
    State.modify' (\seen -> seen <> [pid])
    pure candidates
  _ -> pure (S.identityAnswer p)

-- The single activated ability of a printing. Total: the fallback is
-- unreachable in this fixture. Duplicated per this suite's convention of
-- group-local helpers (CostSpec, ActivateSpec, ReplacementSpec each carry
-- their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] [] Nothing (Face.spell (S.combinedFace p)) [] (Activator.Related PlayerRelation.You) Nothing Nothing Nothing Copyability.Copyable Nothing

-- Records every player asked Prompt.Concede, in order -- the
-- concedeOrderAnswer shape -- and drives alice through exactly one Activate
-- of Greed's ability, then Pass for everyone (including alice, on any later
-- ask: by then she can no longer afford the cost, so Greed's Activate is not
-- offered). Prompt.Concede is asked of the priority holder before anything
-- else, so the recorded order IS the order priority moved in -- including a
-- stale holder who has already departed, which is exactly what the CR 800.4a
-- guard below is for.
greedThenPassAnswer :: ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
greedThenPassAnswer greedId ability p = case p of
  Prompt.Concede asked -> do
    State.modify' (\seen -> seen <> [asked])
    pure Concession.Continues
  Prompt.ChooseAction _ pid actions ->
    pure (if pid == S.alice && List.elem (A.Activate greedId ability) actions then A.Activate greedId ability else A.Pass)
  _ -> pure (S.identityAnswer p)

turnOrderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnOrderSpec s registry = Spec.describe s "TurnOrder (CR 800.4)" $ do
  Spec.it s "CR 800.4k the walk is a loop: two consecutive departed seats are both skipped" $ do
    -- Proves the walk keeps walking rather than skipping exactly one seat.
    -- With bob and carol both gone, alice takes consecutive turns.
    let gone =
          S.departs
            Departure.Type.Conceded
            S.carol
            (S.departs Departure.Type.Conceded S.bob S.threePlayerGame)
        after = S.runPure S.identityAnswer gone Engine.handoffTurn
    Spec.assertEqWith s "alice takes the next turn too" (TurnRegisters.activePlayer (GameState.turn after)) S.alice
    Spec.assertEqWith s "and it is a new turn" (TurnRegisters.turnNumber (GameState.turn after)) 2

  Spec.it s "the seat walk terminates when every seat has departed" $ do
    -- Totality, pinned. A game with no survivors already has a Result, so
    -- this is unreachable while the game is running -- but the walk is
    -- bounded by the seat count and falls back rather than looping or
    -- reaching for a partial head.
    let gone =
          S.departs
            Departure.Type.Conceded
            S.carol
            ( S.departs
                Departure.Type.Conceded
                S.bob
                (S.departs Departure.Type.Conceded S.alice S.threePlayerGame)
            )
        after = S.runPure S.identityAnswer gone Engine.handoffTurn
    Spec.assertEqWith s "the active player is unchanged" (TurnRegisters.activePlayer (GameState.turn after)) S.alice
    Spec.assertEqWith s "no turn began" (TurnRegisters.turnNumber (GameState.turn after)) 1

  Spec.it s "CR 800.4b a pending control whose decider has left is not promoted" $ do
    -- bob Mindslavered carol; bob then left the game. When carol's turn
    -- begins, she is NOT controlled: "If a player would be controlled by a
    -- player who has left the game, they aren't."
    let armed = S.threePlayerGame {GameState.pendingControl = Map.singleton S.carol (pure (S.turnControlRow S.bob))}
        gone = S.departs Departure.Type.Conceded S.bob armed
        after = S.runPure S.identityAnswer gone Engine.handoffTurn
    Spec.assertEqWith s "CR 800.4a's second clause already cleared it at bob's departure" (GameState.pendingControl gone) Map.empty
    Spec.assertEqWith s "carol's turn began" (TurnRegisters.activePlayer (GameState.turn after)) S.carol
    Spec.assertEqWith s "she is uncontrolled" (GameState.control after) Map.empty
    Spec.assertEqWith s "and the stale entry is gone" (GameState.pendingControl after) Map.empty

  Spec.it s "CR 800.4b the promotion guard stands on its own: an entry armed AFTER the departure is still not promoted" $ do
    -- CR 800.4a's second clause now clears pendingControl at the moment its
    -- decider leaves, so the sibling case above is satisfied by two rules at
    -- once. This one isolates CR 800.4b's last sentence -- "If a player would
    -- be controlled by a player who has left the game, they aren't" -- by
    -- arming the entry after bob has already gone, a state CR 800.4a makes
    -- unreachable in play and which therefore only the promotion guard can
    -- answer for.
    let gone = S.departs Departure.Type.Conceded S.bob S.threePlayerGame
        armed = gone {GameState.pendingControl = Map.singleton S.carol (pure (S.turnControlRow S.bob))}
        after = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "carol's turn began" (TurnRegisters.activePlayer (GameState.turn after)) S.carol
    Spec.assertEqWith s "she is uncontrolled" (GameState.control after) Map.empty
    Spec.assertEqWith s "and the stale entry is gone" (GameState.pendingControl after) Map.empty

  Spec.it s "CR 724.1b a pending control whose decider is still playing IS promoted" $ do
    -- The control assertion: the same board with bob still in the game. If
    -- this passed either way, the case above would prove nothing.
    let armed = S.threePlayerGame {GameState.pendingControl = Map.singleton S.bob (pure (S.turnControlRow S.alice))}
        after = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "bob's turn began" (TurnRegisters.activePlayer (GameState.turn after)) S.bob
    Spec.assertEqWith s "alice controls him" (GameState.control after) (S.turnControl S.alice S.bob)

  Spec.it s "CR 800.4a nextStillPlaying finds the successor of a player who has ALREADY departed" $ do
    -- The unit-level statement of #143's first half. Bob's seat is looked up
    -- in the FULL seating order, so his successor is carol -- not alice, the
    -- head of the filtered list.
    let gone = S.departs Departure.Type.Conceded S.bob S.threePlayerGame
    Spec.assertEqWith s "bob's successor is carol" (Engine.nextStillPlaying gone S.bob) S.carol
    Spec.assertEqWith s "carol's successor is alice (wraps)" (Engine.nextStillPlaying gone S.carol) S.alice
    Spec.assertEqWith s "alice's successor skips departed bob" (Engine.nextStillPlaying gone S.alice) S.carol

  Spec.it s "CR 800.4a/117.4 priority after a concede goes to the next seat, and the pass cycle restarts" $ do
    -- Both halves of #143 in one exact list, which is why it is asserted in
    -- full rather than with `take`:
    --
    --   * The THIRD ask proves the seat lookup. Alice passes, bob concedes at
    --     his own priority, and CR 800.4a's last sentence hands priority to
    --     CAROL -- the seat after bob's. Before the fix, nextStillPlaying
    --     could not find already-departed bob in the filtered order and fell
    --     through to alice, the head of the list.
    --   * The FOURTH ask proves the `passes` reset. CR 117.4 needs "all
    --     players pass without taking any actions in between"; bob's
    --     concession is an action, so alice's earlier pass no longer counts
    --     and BOTH survivors must pass again. Without the reset the list is
    --     three asks long -- carol's single pass would reach
    --     `passes >= length (stillPlaying gs)` and end the step.
    let gs =
          S.threePlayerGame
            { GameState.turn =
                (GameState.turn S.threePlayerGame)
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice
                  }
            }
        ((_, after), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.bob) gs Engine.priorityLoop) []
    Spec.assertEqWith s "alice, then bob, then CAROL, then alice again" asks [S.alice, S.bob, S.carol, S.alice]
    Spec.assertEqWith s "bob departed by conceding" (fmap Player.status (Map.lookup S.bob (GameState.players after))) (Just (Status.Departed Departure.Type.Conceded))
    Spec.assertEqWith s "the game continues" (GameState.result after) Nothing

  Spec.it s "CR 800.4j the active player having left does not stop the turn: priority starts at the next seat" $ do
    -- Alice left during her own turn. The turn continues to its completion
    -- WITHOUT an active player: alice is never asked, bob is asked first.
    let gone =
          S.departs
            Departure.Type.Conceded
            S.alice
            ( S.threePlayerGame
                { GameState.turn =
                    (GameState.turn S.threePlayerGame)
                      { TurnRegisters.phase = Phase.PrecombatMain
                      }
                }
            )
        ((_, after), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.carol) gone Engine.priorityLoop) []
    Spec.assertEqWith s "priorityHolder skips the departed active player" (Engine.priorityHolder gone) S.bob
    Spec.assertEqWith s "bob is asked first, alice never" (take 2 asks) [S.bob, S.carol]
    Spec.assertBool s (notElem S.alice asks) "alice was never asked"
    Spec.assertEqWith s "the turn still belongs to alice's seat" (TurnRegisters.activePlayer (GameState.turn after)) S.alice

  Spec.it s "CR 800.4j after a resolution, priority returns to the next seat, not the departed active player" $ do
    -- The second site: the post-resolution re-grant. Carol's Goblin Piker is
    -- on the stack, alice (the active player) has left. Bob and carol pass,
    -- the Piker resolves, and priority is re-granted to BOB. Without
    -- priorityHolder the third ask would be alice's.
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, withSpell) = S.spellOnStack piker S.carol S.threePlayerGame
        gone =
          S.departs
            Departure.Type.Conceded
            S.alice
            ( withSpell
                { GameState.turn =
                    (GameState.turn withSpell)
                      { TurnRegisters.phase = Phase.PrecombatMain
                      }
                }
            )
        ((_, after), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.alice) gone Engine.priorityLoop) []
    Spec.assertEqWith s "bob, carol, then bob AGAIN -- never alice" asks [S.bob, S.carol, S.bob, S.carol]
    Spec.assertEqWith s "the spell resolved" (length (TurnRegisters.stack (GameState.turn after))) 0
    Spec.assertEqWith s "carol's Piker is on the battlefield" (S.creaturesInPlay S.carol after) 1

  Spec.it s "CR 800.4j/703.4d a departed active player does not draw" $ do
    -- With no library, a draw flags drewFromEmpty -- so "did not draw" is
    -- directly observable without building a deck.
    let base =
          S.threePlayerGame
            { GameState.turn =
                (GameState.turn S.threePlayerGame)
                  { TurnRegisters.phase = Phase.Beginning BeginningStep.DrawStep,
                    TurnRegisters.turnNumber = 2
                  }
            }
        gone = S.departs Departure.Type.Conceded S.alice base
        after = S.runPure S.identityAnswer gone S.drawStep
        control = S.runPure S.identityAnswer base S.drawStep
    Spec.assertBool s (not (Set.member S.alice (GameState.drewFromEmpty after))) "a departed active player never attempts the draw"
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty control)) "but a playing one does -- the guard is what did it"

  Spec.it s "CR 800.4j/305.2 a departed active player's turn opens with nothing reset" $ do
    -- landsPlayed is the observable part of the turn's opening that needs no
    -- permanents: it is cleared for the active player as each turn begins.
    let base =
          S.threePlayerGame
            { GameState.turn =
                (GameState.turn S.threePlayerGame)
                  { TurnRegisters.phase = Phase.Beginning BeginningStep.Untap
                  },
              GameState.landsPlayed = Map.singleton S.alice 1
            }
        gone = S.departs Departure.Type.Conceded S.alice base
        after = S.runPure S.identityAnswer gone Engine.openTurn
        control = S.runPure S.identityAnswer base Engine.openTurn
    Spec.assertEqWith s "nothing was reset" (GameState.landsPlayed after) (Map.singleton S.alice 1)
    Spec.assertEqWith s "a playing active player's land play IS cleared" (GameState.landsPlayed control) Map.empty

  Spec.it s "CR 800.4j/703.4i the declare-attackers guard is load-bearing at two seats, where the game loop cannot reach this state" $ do
    -- At two seats Departure.continuesAfterDeparture is False (CR 800.1's
    -- "more than two players"), so NONE of CR 800.4a's four clauses run:
    -- alice keeps her Piker, and Projection.controls still names her its
    -- controller -- checked directly below, the mirror of the three-seat
    -- case's Nothing. That state is unreachable through actual play: CR
    -- 104.2a ends a two-player game the instant alice leaves it, so
    -- Engine.priorityLoop and Engine.runStep can never call
    -- runTurnBasedActions for her again. This case calls it directly
    -- instead, the same way the CR 800.4b sibling further above isolates
    -- the promotion guard for a state CR 800.4a makes unreachable in play.
    -- Deleting the hasActive guard here does not just change what is
    -- asked -- her Piker actually attacks.
    --
    -- The stated defending player is what keeps that load-bearing: a board at
    -- the declare attackers step with Combat.defenders still empty cannot
    -- attack at all (no attack is possible), so deleting the guard would leave
    -- this case green for the wrong reason. alice is active, so bob defends
    -- (CR 506.2's second sentence).
    piker <- S.printingOf s registry "Goblin Piker"
    let seated =
          (Setup.emptyGame S.bothPlayers)
            { GameState.turn =
                (GameState.turn (Setup.emptyGame S.bothPlayers))
                  { TurnRegisters.phase = Phase.Combat CombatStep.DeclareAttackers,
                    TurnRegisters.combat = (TurnRegisters.combat (GameState.turn (Setup.emptyGame S.bothPlayers))) {Combat.Type.defenders = [S.bob]}
                  }
            }
        (pikerId, board) = S.addPermanent piker S.alice seated
        gone = S.departs Departure.Type.Conceded S.alice board
        ((_, after), askedAfter) = State.runState (Engine.runGame declareAttackersAskAnswer gone (Engine.runTurnBasedActions (Phase.Combat CombatStep.DeclareAttackers))) []
    Spec.assertEqWith s "she still controls her Piker -- CR 800.4a never ran at two seats" (Projection.controls S.alice gone) [pikerId]
    Spec.assertEqWith s "the guard is what keeps her from being asked, not an empty board" askedAfter []
    Spec.assertBool s (Map.null (Combat.Type.attackers (TurnRegisters.combat (GameState.turn after)))) "and nothing attacked"

  Spec.it s "CR 800.4a a player who departs paying a cost is not asked again" $ do
    -- Alice controls Greed and one Swamp at exactly 2 life. She activates
    -- Greed ({B}, Pay 2 life: Draw a card -- CR 119.4 makes paying her last
    -- 2 life legal), which leaves her at 0 life. Every Play/Cast/Activate
    -- arm writes TurnRegisters.priority BEFORE settleForPriority runs the SBA
    -- pass that departs her (CR 704.5a), so without the CR 800.4a guard the
    -- next `loop` iteration would find `priority = Just alice` and ask the
    -- departed alice again. With the guard it re-derives the holder via
    -- nextStillPlaying and asks bob, the next seat still in the game,
    -- instead.
    swamp <- S.printingOf s registry "Swamp"
    greed <- S.printingOf s registry "Greed"
    let (_, withSwamp) = S.addPermanent swamp S.alice S.threePlayerGame
        (greedId, withGreed) = S.addPermanent greed S.alice withSwamp
        gs =
          withGreed
            { GameState.turn =
                (GameState.turn withGreed)
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.priority = Just S.alice
                  },
              GameState.players = Map.adjust (\p -> p {Player.life = 2}) S.alice (GameState.players withGreed)
            }
        ((_, after), asks) = State.runState (Engine.runGame (greedThenPassAnswer greedId (theAbility greed)) gs Engine.priorityLoop) []
    Spec.assertEqWith s "alice activates Greed, then BOB is asked -- not alice again" (take 2 asks) [S.alice, S.bob]
    Spec.assertBool s (notElem S.alice (drop 1 asks)) "alice is never asked again after departing"
    Spec.assertEqWith
      s
      "alice departed by paying her last 2 life"
      (fmap Player.status (Map.lookup S.alice (GameState.players after)))
      (Just (Status.Departed Departure.Type.Lost))

-- #219: Action.legalActions computes what is legal; the priority loop must not
-- act on anything else. Every gate the enumeration applies -- the controller
-- check, CR 302.6's tap-sickness gate, cost payability, CR 307.5 timing -- is
-- only as strong as this.
trustedActionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trustedActionSpec s registry = Spec.describe s "TrustedActions" $ do
  -- CR 302.6: a summoning-sick creature's {T} ability cannot be activated.
  -- legalActions already refuses to offer it; this pins that NAMING it anyway
  -- does not work either.
  Spec.it s "#219 an activation that was never offered is refused" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addPermanent prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        -- Summoning sick, so the ability is genuinely illegal to activate.
        sick =
          g0
            { GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) srcId (GameState.objects g0),
              GameState.turn =
                (GameState.turn g0)
                  { TurnRegisters.priority = Just S.alice
                  }
            }
        ability = theAbility prodigalSorcerer
        offered = Action.legalActions S.alice sick
        after = snd (State.evalState (Engine.runGame (illegalActivationAnswer srcId ability) sick Engine.priorityLoop) False)
    Spec.assertBool s (notElem (A.Activate srcId ability) offered) "the engine really does not offer it"
    Spec.assertEqWith s "and naming it puts nothing on the stack" (TurnRegisters.stack (GameState.turn after)) []
    Spec.assertEqWith s "the Sorcerer is not tapped" (fmap Object.tapped (Game.lookupObject srcId after)) (Just TapState.Untapped)

  -- The control: the SAME interpreter, the same board, but settled -- so the
  -- activation is legal and must go through. Without this, a guard that
  -- refused every activation would pass the test above.
  Spec.it s "#219 the same activation IS honoured once it is legal" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addPermanent prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.alice)
        ready =
          settled
            { GameState.turn =
                (GameState.turn settled)
                  { TurnRegisters.priority = Just S.alice
                  }
            }
        ability = theAbility prodigalSorcerer
        after = snd (State.evalState (Engine.runGame (illegalActivationAnswer srcId ability) ready Engine.priorityLoop) False)
    Spec.assertBool s (elem (A.Activate srcId ability) (Action.legalActions S.alice ready)) "it is offered now"
    Spec.assertEqWith s "so the Sorcerer taps" (fmap Object.tapped (Game.lookupObject srcId after)) (Just TapState.Tapped)

-- The intern table: a printing goes in, an id names it, and an id nothing
-- minted names nothing. Minting only through Game.intern is what makes a
-- dangling PrintingId unconstructible, which is why the table is game-local
-- rather than a reference into the registry -- whose pool cannot cover a token
-- or an emblem (CR 111.6 / 114.3).
printingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printingSpec s registry = Spec.describe s "PrintingTable" $ do
  Spec.it s "intern reads back the printing it was given" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (pid, gs) = Game.intern mountain (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "printingOf" (Game.printingOf pid gs) (Just mountain)

  -- The property Parun.Engine.Commander.isCommander rests on: a designation and
  -- an object naming one printing name one id, however many times either was
  -- interned.
  Spec.it s "intern is idempotent for a printing already in the table" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (a, gs1) = Game.intern mountain (Setup.emptyGame S.bothPlayers)
        (b, gs2) = Game.intern mountain gs1
    Spec.assertEqWith s "same id" a b
    Spec.assertEqWith s "and no second entry" (Map.size (Bookkeeping.printings (GameState.bookkeeping gs2))) 1

  Spec.it s "intern mints a distinct id for a distinct printing" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (a, gs1) = Game.intern mountain (Setup.emptyGame S.bothPlayers)
        (b, gs2) = Game.intern piker gs1
    Spec.assertBool s (a /= b) "ids differ"
    Spec.assertEqWith s "two entries" (Map.size (Bookkeeping.printings (GameState.bookkeeping gs2))) 2

  Spec.it s "an unminted id names nothing" $
    Spec.assertEqWith
      s
      "printingOf"
      (Game.printingOf (PrintingId.MkPrintingId 99) (Setup.emptyGame S.bothPlayers))
      Nothing

  Spec.it s "an object built by Setup answers cardOf with its deck entry's card" $ do
    mountain <- S.printingOf s registry "Mountain"
    let deck = Deck.fromCards (Map.singleton mountain 2)
        gs = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
    case Game.zoneMembers Zone.Library S.alice gs of
      [] -> Spec.assertFailure s "fixture should have two Mountains"
      oid : _ ->
        Spec.assertEqWith
          s
          "cardOf"
          (fmap S.nameOf (Game.cardOf oid gs))
          (Just (S.printingName mountain))

  -- The copies of a 4-of share one entry because the DECK groups them, not
  -- because intern deduplicates -- Setup.createDeck is what makes that true, and
  -- Parun.Engine.Commander's designation check depends on it.
  Spec.it s "two objects from one deck entry name one printing" $ do
    mountain <- S.printingOf s registry "Mountain"
    let deck = Deck.fromCards (Map.singleton mountain 2)
        gs = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
        ids =
          Set.fromList
            ( Maybe.mapMaybe
                ( \obj -> case Object.source obj of
                    Source.OfCard p -> Just p
                    _ -> Nothing
                )
                (Map.elems (GameState.objects gs))
            )
    Spec.assertEqWith s "distinct printing ids" (Set.size ids) 1

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Game" $ do
  gameSpec s registry
  printingSpec s registry
  actionSpec s registry
  objectFactSpec s registry
  engineSpec s registry
  ruleSpec s registry
  restartReentrySpec s registry
  cleanupStepSpec s registry
  concedeSpec s registry
  turnOrderSpec s registry
  trustedActionSpec s registry
  lastChoiceSpec s registry
  mandatoryLoopSpec s
  mandatoryLoopBoardSpec s registry

-- CR 104.4b at gameplay level, out of two printings. Life and Limb makes every
-- Saproling a Forest land and every Forest a Saproling creature (CR 613.1d,
-- layer 4; CR 205.1b keeps the types it already had), so each 1/1 Saproling
-- token Sporemound mints is itself a land entering under alice's control, and
-- Sporemound's "whenever a land you control enters" triggers again (CR 603.6a).
-- Nothing in the cycle is optional and nothing in it makes progress: a loop of
-- mandatory actions, repeating a sequence of events with no way to stop.
--
-- Aether Flash is on the board to keep the loop in a STEADY STATE rather than to
-- drive it: each Saproling is a 1/1, so 2 damage buries it on arrival (CR
-- 704.5g) and no permanent accumulates. Without it the tokens pile up as
-- untapped Forest lands, and the loop would then rest on CR 302.6 refusing their
-- {T} forever instead of on there being nothing to tap.
mandatoryLoopBoardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mandatoryLoopBoardSpec s registry = Spec.describe s "a mandatory loop (CR 104.4b)" $ do
  Spec.it s "a loop nobody can interrupt is a draw" $ do
    flash <- S.printingOf s registry "Aether Flash"
    limb <- S.printingOf s registry "Life and Limb"
    sporemound <- S.printingOf s registry "Sporemound"
    forest <- S.printingOf s registry "Forest"
    let result = runConcedingAt 10000 (loopBoard flash limb sporemound forest Nothing)
    Spec.assertEqWith s "CR 104.4b" result Result.Drawn

  Spec.it s "a loop containing an optional action is not" $ do
    -- CR 104.4b's second sentence. The board differs by an untapped Mountain and
    -- a Lightning Bolt in alice's hand it pays for, so her menu carries both CR
    -- 605.3a's mana ability and a Cast every time she gets priority -- either
    -- alone would do it. That resets the marker each round,
    -- so the loop -- otherwise identical, and still running -- is never declared
    -- a draw. The game ends only because alice eventually concedes (CR 104.3a),
    -- which is the only way to get a terminating test out of a game the rules say
    -- does not end.
    flash <- S.printingOf s registry "Aether Flash"
    limb <- S.printingOf s registry "Life and Limb"
    sporemound <- S.printingOf s registry "Sporemound"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let result = runConcedingAt 200 (loopBoard flash limb sporemound forest (Just (mountain, bolt)))
    Spec.assertEqWith s "alice conceded, so bob won -- no draw" result (Result.Won S.bob)

  -- Lich's Mirror's ruling: alice controls bob's Mirror at ten poison counters.
  -- Every check would lose her the game, and the Mirror, not being hers, never
  -- shuffles itself away, so it replaces the loss on every pass -- an
  -- involuntary loop, which the ruling and CR 104.4b call a draw. Driven
  -- through Engine.settleForPriority, the state-based action recursion the loop
  -- runs in, seeded near the limit for loopBoard's reason.
  Spec.it s "a borrowed Lich's Mirror resetting forever is a draw" $ do
    mirror <- S.printingOf s registry "Lich's Mirror"
    island <- S.printingOf s registry "Island"
    let (mirrorId, gs1) = S.addPermanent mirror S.bob (Setup.emptyGame S.bothPlayers)
        stocked = iterate (snd . S.addLibraryCard island S.alice) (S.giveControl mirrorId S.alice gs1) !! 8
        board =
          Game.overBookkeeping
            (\bookkeeping -> bookkeeping {Bookkeeping.nextTimestamp = Timestamp.MkTimestamp (Departure.mandatoryLoopLimit - 10), Bookkeeping.lastChoice = Timestamp.MkTimestamp 0})
            ( Game.overTurn
                ( \registers ->
                    registers
                      { TurnRegisters.phase = Phase.PrecombatMain,
                        TurnRegisters.remaining = Seq.empty
                      }
                )
                (S.addPlayerCounter PlayerCounterKind.Poison 10 S.alice stocked)
            )
        after = S.runPure S.identityAnswer board Engine.settleForPriority
    Spec.assertEqWith s "CR 104.4b" (GameState.result after) (Just Result.Drawn)

  -- The same loop with nothing to move: alice owns no card at all, so the reset
  -- shuffles nothing and draws from an empty library, minting no timestamp, and
  -- only Departure.checkMandatoryLoop's pass count can call it.
  Spec.it s "a borrowed Lich's Mirror moving nothing is a draw" $ do
    mirror <- S.printingOf s registry "Lich's Mirror"
    let (mirrorId, gs1) = S.addPermanent mirror S.bob (Setup.emptyGame S.bothPlayers)
        board =
          Game.overTurn
            ( \registers ->
                registers
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.remaining = Seq.empty
                  }
            )
            (S.addPlayerCounter PlayerCounterKind.Poison 10 S.alice (S.giveControl mirrorId S.alice gs1))

        after = S.runPure S.identityAnswer board Engine.settleForPriority
    Spec.assertEqWith s "CR 104.4b" (GameState.result after) (Just Result.Drawn)

-- alice, active, in her precombat main phase: bob's Aether Flash and Life and
-- Limb, alice's Sporemound, and a Forest of alice's ENTERING (so CR 603.6a's
-- event is there for Sporemound's landfall trigger to see). Empty libraries and
-- nothing scheduled after this phase, so the loop is the only thing that can
-- happen and nothing on the board can end the game. `castable`,
-- if any, is an untapped land for alice plus a spell in her hand it pays for --
-- both halves, since Cast.castableSpells offers a spell only when its cost can be
-- paid.
--
-- The seed Forest enters TAPPED (CR 107.5). S.addPermanent settles what it
-- places, and Life and Limb makes a Forest a 1/1 creature, so an untapped one
-- would offer alice CR 605.3a's mana ability at her first priority and move
-- Bookkeeping.lastChoice. That is a speed measure, not what makes the loop
-- mandatory: Aether Flash buries the Forest before a second priority, so
-- dropping the tap only postpones the draw by the thousand events the marker
-- then has to climb again, and the case stays green. The TOKENS need no such
-- care either: CR 302.6 refuses the {T} of a creature its controller has not
-- controlled since their turn began.
--
-- The two static permanents sit under bob so that alice -- the active player,
-- whose menu the loop reads first -- controls nothing with an activatable
-- ability.
--
-- Seeded ten events short of the limit rather than starting from zero: the
-- mechanism is the same at any gap, and this keeps the test at a handful of
-- cycles instead of a few hundred.
loopBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe (Printing.Printing, Printing.Printing) -> GameState.GameState
loopBoard flash limb sporemound forest castable =
  let base = Setup.emptyGame S.bothPlayers
      (_, gs1) = S.addPermanent flash S.bob base
      (_, gs2) = S.addPermanent limb S.bob gs1
      (_, gs3) = S.addPermanent sporemound S.alice gs2
      gs4 = case castable of
        Nothing -> gs3
        Just (land, spell) ->
          let (_, withLand) = S.addPermanent land S.alice gs3
           in snd (S.addHandCard spell S.alice withLand)
      (forestId, gs5) = S.entersWithTrigger forest S.alice gs4
      seeded =
        gs5
          { GameState.turn =
              (GameState.turn gs5)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.remaining = Seq.empty
                },
            GameState.bookkeeping =
              (GameState.bookkeeping gs5)
                { Bookkeeping.nextTimestamp = Timestamp.MkTimestamp (Departure.mandatoryLoopLimit - 10),
                  Bookkeeping.lastChoice = Timestamp.MkTimestamp 0
                }
          }
   in S.tapObject forestId seeded

-- Play the game out, with alice conceding once she has been asked `limit` times.
-- The concession is a backstop for the test that expects NO draw: without it that
-- game would run forever, which is exactly what CR 104.4b says should happen to a
-- loop containing an optional action.
runConcedingAt :: Int -> GameState.GameState -> Result.Result
runConcedingAt limit gs =
  let answer :: Prompt.Prompt r -> State.State Int r
      answer p = case p of
        Prompt.Concede pid
          | pid == S.alice -> do
              asked <- State.get
              State.put (asked + 1)
              pure (if asked >= limit then Concession.Concedes else Concession.Continues)
        _ -> pure (S.identityAnswer p)
      ((result, _), _) = State.runState (Engine.runGame answer gs Engine.playGame) 0
   in result

-- CR 104.4b, the guard itself. The gameplay proof that a real loop reaches it is
-- the "a mandatory loop (CR 104.4b)" group; these four pin the boundary, which a
-- board-level test cannot do precisely.
mandatoryLoopSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
mandatoryLoopSpec s = Spec.describe s "the mandatory-loop guard (CR 104.4b)" $ do
  Spec.it s "one event short of the limit is not a draw" $ do
    let after = S.runPure S.identityAnswer (atGap (Departure.mandatoryLoopLimit - 1)) (Departure.checkMandatoryLoop 0)
    Spec.assertEqWith s "still being played" (GameState.result after) Nothing

  Spec.it s "the limit is a draw" $ do
    let after = S.runPure S.identityAnswer (atGap Departure.mandatoryLoopLimit) (Departure.checkMandatoryLoop 0)
    Spec.assertEqWith s "CR 104.4b" (GameState.result after) (Just Result.Drawn)

  -- The second measure: passes in a row that acted with nobody asked, for a
  -- loop that mints no timestamp.
  Spec.it s "one pass short of the limit is not a draw" $ do
    let after = S.runPure S.identityAnswer (atGap 0) (Departure.checkMandatoryLoop (Departure.mandatoryLoopLimit - 1))
    Spec.assertEqWith s "still being played" (GameState.result after) Nothing

  Spec.it s "the pass limit is a draw" $ do
    let after = S.runPure S.identityAnswer (atGap 0) (Departure.checkMandatoryLoop Departure.mandatoryLoopLimit)
    Spec.assertEqWith s "CR 104.4b" (GameState.result after) (Just Result.Drawn)

  Spec.it s "a game that already ended keeps the result it had" $ do
    -- A draw must never overwrite a win: the guard fires on a gap only a loop
    -- should reach, and a won game is not looping.
    let won = (atGap Departure.mandatoryLoopLimit) {GameState.result = Just (Result.Won S.alice)}
        after = S.runPure S.identityAnswer won (Departure.checkMandatoryLoop 0)
    Spec.assertEqWith s "alice still won" (GameState.result after) (Just (Result.Won S.alice))

  Spec.it s "playGame draws instead of looping" $ do
    -- Proves the guard is WIRED, not merely correct: playGame's loop head is what
    -- reads it. The board is empty, so nothing else can end this game.
    let (result, _) = Engine.runGamePure S.identityAnswer (atGap Departure.mandatoryLoopLimit) Engine.playGame
    Spec.assertEqWith s "CR 104.4b" result Result.Drawn

-- An otherwise-idle two-player game in which `n` events have happened since any
-- player was last offered a choice.
atGap :: Natural -> GameState.GameState
atGap n =
  (Setup.emptyGame S.bothPlayers)
    { GameState.bookkeeping = (GameState.bookkeeping (Setup.emptyGame S.bothPlayers)) {Bookkeeping.nextTimestamp = Timestamp.MkTimestamp n, Bookkeeping.lastChoice = Timestamp.MkTimestamp 0}
    }

-- CR 104.4b's "loops that contain an optional action": what makes an action
-- optional is that the player had more than one answer to give. Bookkeeping.lastChoice
-- is how the engine remembers when that last happened, and Parun.Engine.Game.choose
-- is the only thing that advances it.
lastChoiceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastChoiceSpec s registry = Spec.describe s "lastChoice (CR 104.4b)" $ do
  Spec.it s "a priority round whose only action is Pass leaves it alone" $ do
    mountain <- S.printingOf s registry "Mountain"
    let after = S.runPure S.identityAnswer (choicelessBoard mountain True) Engine.priorityLoop
    Spec.assertEqWith s "no player was offered a choice" (Bookkeeping.lastChoice (GameState.bookkeeping after)) (Timestamp.MkTimestamp 0)

  Spec.it s "CR 605.3a an untapped land's own mana ability is that optional action" $ do
    -- The board differs by ONE BIT from the one above: the same lone Mountain,
    -- untapped. CR 605.3a lets alice activate its mana ability while she holds
    -- priority, so Action.legalActions offers it beside the Pass and CR 104.4b's
    -- loop is no longer made of mandatory actions alone.
    mountain <- S.printingOf s registry "Mountain"
    let board = choicelessBoard mountain False
        after = S.runPure S.identityAnswer board Engine.priorityLoop
    Spec.assertEqWith s "the menu is Pass and the mana ability, nothing else" (length (Action.legalActions S.alice board)) 2
    Spec.assertBool s (Bookkeeping.lastChoice (GameState.bookkeeping after) > Timestamp.MkTimestamp 0) "alice was offered a choice"

-- alice, active, in her precombat main phase with an empty stack, nothing
-- scheduled after it, and one Mountain. Her hand is empty, so her menu is
-- exactly [Pass] while that Mountain is tapped.
choicelessBoard :: Printing.Printing -> Bool -> GameState.GameState
choicelessBoard mountain tapped =
  let base = Setup.emptyGame S.bothPlayers
      (mountainId, gs1) = S.addPermanent mountain S.alice base
      gs2 = if tapped then S.tapObject mountainId gs1 else gs1
   in gs2
        { GameState.turn =
            (GameState.turn gs2)
              { TurnRegisters.phase = Phase.PrecombatMain,
                TurnRegisters.remaining = Seq.empty
              },
          GameState.bookkeeping = (GameState.bookkeeping gs2) {Bookkeeping.lastChoice = Timestamp.MkTimestamp 0}
        }

-- An interpreter that names an action it was never offered. CR-wise this is not
-- a rules question at all: Action.legalActions computes what is legal, and the
-- priority loop must not act on anything else (#219). parun's boundary is an
-- interpreter that may be an LLM, and a plausible-looking illegal action is
-- exactly what such an interpreter produces.
-- Stateful, and it must be: an answerer that returns Activate for EVERY
-- ChooseAction never lets the priority loop terminate. This one names the
-- activation once and passes forever after, which is also the honest model of an
-- interpreter that tries something illegal and then gives up.
illegalActivationAnswer :: ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Prompt.Prompt r -> State.State Bool r
illegalActivationAnswer oid ability p = case p of
  Prompt.ChooseAction {} -> do
    tried <- State.get
    if tried
      then pure A.Pass
      else do
        State.put True
        pure (A.Activate oid ability)
  -- Aim Prodigal Sorcerer's ping at a PLAYER. S.identityAnswer takes the lowest
  -- recipient, which is the Sorcerer itself -- and a 1/1 pinging itself dies, so
  -- the board this test wants to inspect would not exist by the time it looked.
  Prompt.ChooseTargets _ _ _ sets ->
    pure (S.preferring isPlayerRecipient sets)
  _ -> pure (S.identityAnswer p)

-- The lowest ToPlayer recipient in a legal set, falling back to the lowest of
-- anything when the set holds no player.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToPlaneswalker _ -> False
  Recipient.ToBattle _ -> False
  Recipient.ToObject _ -> False
  Recipient.ToPile _ -> False

isCastAction :: A.Action -> Bool
isCastAction a = case a of
  A.Cast {} -> True
  _ -> False

-- Does this Activate action fire a restart? Karn Liberated has three loyalty
-- abilities, so "the first Activate" would be its +4 rather than its ultimate --
-- the answer has to be PINNED to the ability under test, not taken by position.
isRestartActivation :: A.Action -> Bool
isRestartActivation a =
  let isRestart e = case e of
        Effect.RestartGame _ -> True
        _ -> False
   in case a of
        A.Activate _ ability -> any isRestart (foldMap Mode.allEffects (Modal.modes (ActivatedAbility.modal ability)))
        _ -> False

-- CR 728 gate strategy. Whoever has priority activates the restart -- Karn
-- Liberated's ultimate, which only bob's Karn offers -- and otherwise passes.
-- Karn's loyalty pays for it exactly once, and after the restart Karn is a card
-- in a library, so no player can activate anything and everyone passes to
-- termination. Non-ChooseAction prompts (Shuffle during the rebuild, etc.)
-- delegate to identityAnswer.
restartAnswer :: Prompt.Prompt r -> r
restartAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isRestartActivation actions of
      activation : _ -> activation
      [] -> A.Pass
  _ -> S.identityAnswer p

-- CR 730 gate strategy. Whoever has priority casts the only castable spell on the
-- board -- the synthetic subgame sorcery ({0}, in alice's hand) -- and otherwise
-- passes. Inside the subgame the libraries are Mountains (lands are PLAYED, not
-- cast), so no cast is available there and everyone passes to termination (bob
-- decks) -- except the CR 730.6 nested gate, where the level-1 subgame's
-- library also holds a castable nested synthetic-subgame sorcery, and the same
-- cast-if-available strategy descends into it. It takes the question and not
-- the game it came from (#153), so the one answerer serves every level.
-- Non-ChooseAction prompts (Shuffle during setup, etc.) delegate to
-- identityAnswer.
subgameAnswer :: Prompt.Prompt r -> r
subgameAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isCastAction actions of
      cast : _ -> cast
      [] -> A.Pass
  _ -> S.identityAnswer p

-- Attack bob with everything and never block, so the Sindbad connects with a
-- player who is neither its controller nor the subgame's winner. Inside the
-- subgame the same answerer keeps every opening hand and takes no action, which
-- is all CR 730.3 needs: two of the three libraries cannot fill a 7-card hand.
sindbadAnswer :: Prompt.Prompt r -> r
sindbadAnswer p = case p of
  Prompt.ChooseDefender {} -> S.bob
  Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.bob) (NonEmpty.toList options))
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- The Shahrazad and Sindbad combat board the two CR 730.1b cases share: three
-- seats, three distinct life totals and three distinct library sizes, with the
-- match's CR 100.6a subgame tally as the one dial between them. Returns the
-- board, the state the trigger was placed from (what Replay.record re-runs) and
-- the state the priority loop leaves.
sindbadCombat :: Printing.Printing -> Printing.Printing -> Natural -> (GameState.GameState, GameState.GameState, GameState.GameState)
sindbadCombat mountain sindbad subgamesSoFar =
  let (gs0, _, _, _) = S.threePlayerCombat [sindbad] [] []
      -- Straight onto each library, never through poolToLibraryG: that sweeps
      -- every object its player owns off the battlefield, which would take the
      -- attacking Sindbad with it.
      stocked = addToLibraryG mountain 8 S.carol (addToLibraryG mountain 4 S.bob (addToLibraryG mountain 6 S.alice gs0))
      atLife pid n gs = gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}
      living = atLife S.carol 9 (atLife S.bob 13 (atLife S.alice 20 stocked))
      before = living {GameState.match = (GameState.match living) {MatchScope.subgamesThisMatch = subgamesSoFar}}
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) sindbadAnswer before
      fought = S.runPure sindbadAnswer atDamage (Damage.dealCombatDamage Settle.none)
      placed = S.runPure sindbadAnswer fought Engine.settleForPriority
      after = S.runPure sindbadAnswer placed Engine.priorityLoop
   in (before, placed, after)

-- #153: what one question said about the game that raised it. A depth and two
-- booleans rather than the GameStates themselves, so a failure prints something
-- a person can read.
data AskTag = MkAskTag
  { askDepth :: Int,
    askedGameHasSurvivor :: Bool,
    mainGameHasSurvivor :: Bool
  }
  deriving (Eq, Ord, Show)

-- Answers as subgameAnswer does, tagging each question with the game it came
-- from. The distinguishing fact is whether that game's battlefield holds
-- `survivor`, a main-game creature that CR 730.2 keeps out of the subgame.
recordAskedGame :: ObjectId.ObjectId -> Asked.Asked GameState.GameState r -> State.State [AskTag] r
recordAskedGame survivor asked = do
  let holds gs = Set.member survivor (GameState.battlefield gs)
  State.modify'
    ( MkAskTag
        { askDepth = length (Asked.enclosing asked),
          askedGameHasSurvivor = holds (Asked.game asked),
          mainGameHasSurvivor = holds (Asked.mainGame asked)
        }
        :
    )
  pure (subgameAnswer (Asked.prompt asked))

-- #136 / CR 730.2: hands the subgame's first-player roll a fixed answer, so a
-- test can play the same fixture with each player starting. Every other prompt
-- delegates to identityAnswer (which would answer this one with the head of the
-- order -- the pre-#136 behaviour).
firstPlayerAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
firstPlayerAnswer starter p = case p of
  Prompt.RandomFirstPlayer _ -> starter
  _ -> S.identityAnswer p

-- Take exactly one mulligan, then keep. CR 103.5c's free first mulligan is only
-- observable if someone actually takes one.
mulliganOnceAnswer :: Prompt.Prompt r -> r
mulliganOnceAnswer p = case p of
  Prompt.DeclareMulligan _ _ offer -> if MulliganOffer.taken offer < 1 then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Records the candidate list of every Prompt.RandomFirstPlayer, rolling the first
-- candidate, and reverses every shuffle. The reversal is what makes "whose
-- library was shuffled" observable at all: S.identityAnswer's Shuffle arm is the
-- identity, so a library that was shuffled looks exactly like one that was not.
subgameRosterAnswer :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
subgameRosterAnswer p = case p of
  Prompt.RandomFirstPlayer order -> do
    State.modify' (<> [NonEmpty.toList order])
    pure (NonEmpty.head order)
  Prompt.Shuffle ids -> pure (reverse ids)
  _ -> pure (S.identityAnswer p)

addManyG :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addManyG mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addPermanent mountain pid g)) gs (replicate n ())

-- Put one printing into a player's library as a fresh object; return its id.
-- Mirrors S.addHandCard, then relocates hand -> library.
libraryCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
libraryCard printing pid gs =
  let (oid, gs1) = S.addHandCard printing pid gs
      onLibrary o = o {Object.zone = Zone.Library}
   in ( oid,
        gs1
          { GameState.objects = Map.adjust onLibrary oid (GameState.objects gs1),
            GameState.hand = Map.adjust (Seq.filter (/= oid)) pid (GameState.hand gs1),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs1)
          }
      )

-- Append n Mountains to a player's library.
addToLibraryG :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addToLibraryG mountain n pid gs =
  List.foldl' (\g _ -> snd (libraryCard mountain pid g)) gs (replicate n ())

-- Move every object this player owns onto their library (mirror of
-- SetupSpec.poolToLibrary, adapted to GameSpec's imports): used to craft a
-- pre-shuffled library of a known size for a subgame/restart gate.
poolToLibraryG :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
poolToLibraryG pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (flip (Map.adjust onLibrary)) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }

-- #134 / CR 728.4. A restart that resolves inside a LIVE Engine.runStep replaces
-- the game underneath the frames that are still running: the resolution, the
-- priority loop, and the step itself. Setup.restartGame leaves the rebuilt state
-- positioned just before turn 1's untap step with no player holding priority, so
-- every one of those frames has to unwind without touching it. The two things
-- that must NOT happen are a further priority grant ("No player has priority")
-- and Engine.advance, which would pop the FRESH `remaining` and skip turn 1's
-- untap step entirely.

-- A player's `n` owned cards, so the rebuilt game can deal a 7-card opening hand
-- without tripping the CR 728.3 short-deck loss.
ownedCards :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
ownedCards mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addPermanent mountain pid g)) gs [1 .. n]

-- alice is active in her precombat main phase (a step that grants priority) with
-- bob's RestartGame ability already on the stack. Both players pass, the ability
-- resolves, and the restart fires mid-step.
restartOnStack :: Printing.Printing -> GameState.GameState
restartOnStack mountain =
  let g0 = Setup.emptyGame S.bothPlayers
      g1 = ownedCards mountain 10 S.alice g0
      g2 = ownedCards mountain 10 S.bob g1
      (abilId, g3) = Game.freshObjectId g2
      (ts, g4) = Game.freshTimestamp g3
      ability =
        ActivatedAbility.MkActivatedAbility
          { ActivatedAbility.costChoices = [],
            ActivatedAbility.cost =
              Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
            ActivatedAbility.modal =
              Mint.oneMode (Seq.singleton (Effect.RestartGame Nothing)),
            ActivatedAbility.maximumX = [],
            ActivatedAbility.definedX = Nothing,
            ActivatedAbility.restrictions = [],
            ActivatedAbility.activator = Activator.Related PlayerRelation.You,
            ActivatedAbility.condition = Nothing,
            ActivatedAbility.name = Nothing,
            ActivatedAbility.keyword = Nothing,
            ActivatedAbility.copyability = Copyability.Copyable,
            ActivatedAbility.chosenPlayer = Nothing
          }
      abilObj =
        ( Object.newSettled
            S.bob
            ( Source.OfAbility
                ActivatedAbilitySource.MkActivatedAbilitySource
                  { ActivatedAbilitySource.source = ObjectId.MkObjectId 0,
                    ActivatedAbilitySource.ability = ability,
                    ActivatedAbilitySource.delayed = Map.empty,
                    ActivatedAbilitySource.acquisition = Acquisition.Own
                  }
            )
            Zone.Stack
            ts
        )
          { Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))
          }
   in g4
        { GameState.objects = Map.insert abilId abilObj (GameState.objects g4),
          GameState.turn =
            (GameState.turn g4)
              { TurnRegisters.stack = abilId : TurnRegisters.stack (GameState.turn g4),
                TurnRegisters.activePlayer = S.alice,
                TurnRegisters.phase = Phase.PrecombatMain,
                TurnRegisters.remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
              }
        }

-- Run under a prompt-counting interpreter: how many times a player was asked to
-- act is exactly how CR 728.4's "No player has priority" is observed.
runCountingActions :: GameState.GameState -> Game.Type.Game a -> (GameState.GameState, Int)
runCountingActions gs act =
  let answer :: Prompt.Prompt r -> State.State Int r
      answer p = do
        case p of
          Prompt.ChooseAction {} -> State.modify' (+ 1)
          _ -> pure ()
        pure (S.identityAnswer p)
      ((_, gs1), n) = State.runState (Engine.runGame answer gs act) 0
   in (gs1, n)

restartReentrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
restartReentrySpec s registry = Spec.describe s "restart re-entry (CR 728.4)" $ do
  Spec.it s "no player receives priority after the restart resolves" $ do
    -- alice passes, bob passes, the ability resolves: two ChooseAction prompts
    -- and no more. A third means the priority loop kept running on a game that
    -- no longer exists.
    mountain <- S.printingOf s registry "Mountain"
    let (_, asked) = runCountingActions (restartOnStack mountain) Engine.runStep
    Spec.assertEqWith s "exactly the two passes that resolved the ability" asked 2

  Spec.it s "the next step runs the rebuilt turn 1's untap step" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (afterRestart, _) = runCountingActions (restartOnStack mountain) Engine.runStep
        (afterUntap, _) = runCountingActions afterRestart Engine.runStep
    Spec.assertEqWith s "the untap step ran and handed on to upkeep" (TurnRegisters.phase (GameState.turn afterUntap)) (Phase.Beginning BeginningStep.Upkeep)
    Spec.assertEqWith s "still turn 1" (TurnRegisters.turnNumber (GameState.turn afterUntap)) 1

  Spec.it s "a live playGame survives the restart and plays the new game to a result" $ do
    -- An end-to-end liveness guard, not a discriminating one: the loop did
    -- not wedge before the fix either, it just played a turn 1 with no untap
    -- step. The two tests above and the Karn restart scenarios under
    -- data/scenarios/game/ are what actually catch that. This one pins the
    -- surrounding claim -- playGame keeps looping across the
    -- rebuild and returns the REBUILT game's result (CR 728.1: no player
    -- wins, loses or draws the game that was restarted). Terminating: the
    -- restart is a hand-built stack object, not a card in any library, so it
    -- cannot fire again and the rebuilt game decks out like any other.
    mountain <- S.printingOf s registry "Mountain"
    let (result, _) = Engine.runGamePure S.identityAnswer (restartOnStack mountain) Engine.playGame
    Spec.assertBool s (case result of Result.Won _ -> True; Result.TeamWon _ -> True; Result.Drawn -> True) "the new game reached a result"

-- alice is the active player in her cleanup step with `n` cards in hand and
-- nothing else scheduled; `others` are put onto the battlefield under bob's
-- control. Nothing is in either library, so no draw can happen and the only
-- event of the step is CR 514.1's discard.
cleanupBoard :: Printing.Printing -> Int -> [Printing.Printing] -> GameState.GameState
cleanupBoard filler n others =
  let base = List.foldl' (\g p -> snd (S.addPermanent p S.bob g)) (Setup.emptyGame S.bothPlayers) others
      full = List.foldl' (\g _ -> snd (S.addHandCard filler S.alice g)) base [1 .. n]
   in full
        { GameState.turn =
            (GameState.turn full)
              { TurnRegisters.activePlayer = S.alice,
                TurnRegisters.turnNumber = 1,
                TurnRegisters.phase = Phase.Ending EndingStep.Cleanup,
                TurnRegisters.remaining = Seq.empty
              }
        }

-- CR 514.3a's extra cleanup step and its priority round.
--
-- Megrim, {2}{B} Enchantment: "Whenever an opponent discards a card, this
-- enchantment deals 2 damage to that player." bob controls it, so CR 109.5 fixes
-- its "you" as bob and "an opponent" as alice. alice is the active player with
-- eight cards in hand, so CR 514.1's turn-based discard is an opponent's discard
-- and a triggered ability is waiting DURING the cleanup step -- CR 514.3a's
-- condition exactly.
--
-- The discriminator is alice's life, read after ONE Engine.runStep. Under CR
-- 514.3 alone the trigger was placed by the settle in Engine.advance and then
-- sat on the stack across the handoff, to resolve at bob's first priority: alice
-- still on 20, bob active, turn 2. Under CR 514.3a it resolves inside alice's own
-- cleanup step: alice on 18, alice still active, and a second cleanup step
-- current.
cleanupStepSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cleanupStepSpec s registry = Spec.describe s "extra cleanup step (CR 514.3a)" $ do
  -- The termination argument at Engine.cleanupException, pinned: the chain is
  -- two cleanup steps because CR 514.1 finds the hand already at its maximum
  -- the second time round and so has no discard to fire the Megrim with.
  Spec.it s "CR 514.3a the second cleanup step finds nothing waiting and ends the turn" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    megrim <- S.printingOf s registry "Megrim"
    let after = S.runPure S.identityAnswer (cleanupBoard piker 8 [megrim]) Engine.runStep
        (again, asked) = runCountingActions after Engine.runStep
    Spec.assertEqWith s "nobody was asked to act in the second cleanup step" asked 0
    Spec.assertEqWith s "the turn handed off" (TurnRegisters.activePlayer (GameState.turn again)) S.bob
    Spec.assertEqWith s "to turn 2" (TurnRegisters.turnNumber (GameState.turn again)) 2
    Spec.assertEqWith s "starting at the untap step" (TurnRegisters.phase (GameState.turn again)) Turn.firstPhase
    Spec.assertEqWith s "with a fresh schedule, not a third cleanup" (TurnRegisters.remaining (GameState.turn again)) Turn.laterPhases
    Spec.assertEqWith s "and alice took Megrim's 2 damage once, not twice" (S.lifeOf S.alice again) (Just 18)

  -- CR 514.3: "Normally, no player receives priority during the cleanup step,
  -- so no spells can be cast and no abilities can be activated." The same
  -- board one card apart: without the Megrim nothing is waiting, and the
  -- exception must not fire.
  Spec.it s "CR 514.3 a cleanup step with nothing waiting grants no priority" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (after, asked) = runCountingActions (cleanupBoard piker 8 []) Engine.runStep
    Spec.assertEqWith s "nobody was asked to act" asked 0
    Spec.assertEqWith s "the turn handed off at once" (TurnRegisters.activePlayer (GameState.turn after)) S.bob
    Spec.assertEqWith s "to turn 2" (TurnRegisters.turnNumber (GameState.turn after)) 2

  -- The count, not just the fact: one round of passes resolves the trigger,
  -- and a second empties the stack and ends the step (CR 514.3a's "once the
  -- stack is empty and all players pass in succession").
  Spec.it s "CR 514.3a the exception grants a real priority round" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    megrim <- S.printingOf s registry "Megrim"
    let (_, asked) = runCountingActions (cleanupBoard piker 8 [megrim]) Engine.runStep
    Spec.assertEqWith s "two passes to resolve the trigger, two to end the step" asked 4

-- The CR 730.1b board played for ante (CR 407.1); the subgame inherits it.
-- Ante.ownershipChanges as (starting owner, owner) pairs, in its order.
changesOf :: GameState.GameState -> [(PlayerId.PlayerId, PlayerId.PlayerId)]
changesOf gs = [(OwnershipChange.startingOwner c, OwnershipChange.owner c) | c <- Foldable.toList (Ante.ownershipChanges gs)]

forAnte :: GameState.GameState -> GameState.GameState
forAnte gs = gs {GameState.match = (GameState.match gs) {MatchScope.settings = (MatchScope.settings (GameState.match gs)) {GameSettings.ante = True}}}

-- Shahrazad's board: three seats on distinct life totals, alice holding Shahrazad and
-- two untapped Plains to cast it with, and libraries of `aliceLibrary`, 6 and 4
-- Mountains. Runs one pass of the priority loop -- long enough for alice to cast
-- and the spell to resolve -- and hands back the main game afterwards.
--
-- bob's 6 and carol's 4 are both under seven, so both deck on the subgame's
-- opening hand (CR 730.3); `aliceLibrary` is the one dial, and it decides whether
-- the subgame has a survivor. The Plains go on AFTER poolToLibraryG, which sweeps
-- every object a player owns into their library.
castShahrazad :: (GameState.GameState -> GameState.GameState) -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
castShahrazad staged mountain plains shahrazad aliceLibrary =
  let g0 = staged (Setup.emptyGame S.threePlayers)
      g1 = addManyG mountain aliceLibrary S.alice (addManyG mountain 6 S.bob (addManyG mountain 4 S.carol g0))
      g2 = poolToLibraryG S.carol (poolToLibraryG S.bob (poolToLibraryG S.alice g1))
      g3 = S.landsFor plains S.alice 2 g2
      (_spellId, g4) = S.addHandCard shahrazad S.alice g3
      atLife pid n gs = gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}
      g5 = atLife S.carol 9 (atLife S.bob 13 (atLife S.alice 20 g4))
      gStart =
        g5
          { GameState.turn =
              (GameState.turn g5)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.alice
                }
          }
   in snd (Engine.runGamePure shahrazadAnswer gStart Engine.priorityLoop)

-- The Shahrazad board played for ante with Darkpact on top of alice's
-- 12-card library and 11 Swamps below it; bob has 12 Mountains and carol 10.
-- Each subgame library antes its bottom card (darkpactAnswer), so alice draws
-- Darkpact and casts it off her third Swamp on turn 7, carol decks on turn 9,
-- and bob is left to win.
castDarkpactInShahrazad :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
castDarkpactInShahrazad mountain swamp plains shahrazad darkpact =
  let g0 = forAnte (Setup.emptyGame S.threePlayers)
      g1 = addManyG swamp 11 S.alice (addManyG darkpact 1 S.alice g0)
      g2 = addManyG mountain 12 S.bob (addManyG mountain 10 S.carol g1)
      g3 = poolToLibraryG S.carol (poolToLibraryG S.bob (poolToLibraryG S.alice g2))
      g4 = S.landsFor plains S.alice 2 g3
      (_spellId, g5) = S.addHandCard shahrazad S.alice g4
      gStart =
        g5
          { GameState.turn =
              (GameState.turn g5)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.alice
                }
          }
   in snd (Engine.runGameAskedPure darkpactAnswer gStart Engine.priorityLoop)

-- Casts and plays lands as S.castAnswer does, antes each library's bottom
-- card, aims Darkpact at bob's ante card, and has alice concede the subgame
-- once she owns a card bob began it with.
darkpactAnswer :: Asked.Asked GameState.GameState r -> r
darkpactAnswer asked =
  let gs = Asked.game asked
      bobs recipient = maybe False (\oid -> fmap Object.owner (Game.lookupObject oid gs) == Just S.bob) (Recipient.objectOf recipient)
      tookBobs = any (\c -> OwnershipChange.startingOwner c == S.bob && OwnershipChange.owner c == S.alice) (Ante.ownershipChanges gs)
   in case Asked.prompt asked of
        Prompt.RandomObject candidates -> NonEmpty.last candidates
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, recipients) -> Set.filter bobs recipients) sets
        Prompt.Concede pid
          | pid == S.alice && not (null (Asked.enclosing asked)) && tookBobs -> Concession.Concedes
        p -> S.castAnswer p

-- subgameAnswer, naming the LAST candidate, pinned by position, wherever a
-- winning team is asked who takes an ante card (CR 407.2 / 104.2c).
shahrazadAnswer :: Prompt.Prompt r -> r
shahrazadAnswer p = case p of
  Prompt.ChoosePlayer _ _ _ offered -> NonEmpty.last offered
  _ -> subgameAnswer p

-- The Synthetic Threshold of Legends board the two CR 614.1c cases share: alice
-- casts it off three Islands; alice's library holds 3 cards, carol's 4 and bob's
-- `bobLibrary`, so only bob can survive the subgame's opening hand (CR 730.3).
castThreshold :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
castThreshold island mountain threshold bobLibrary =
  let g0 = Setup.emptyGame S.threePlayers
      g1 = addManyG mountain 3 S.alice (addManyG mountain bobLibrary S.bob (addManyG mountain 4 S.carol g0))
      g2 = poolToLibraryG S.carol (poolToLibraryG S.bob (poolToLibraryG S.alice g1))
      g3 = S.landsFor island S.alice 3 g2
      (_spellId, g4) = S.addHandCard threshold S.alice g3
      gStart =
        g4
          { GameState.turn =
              (GameState.turn g4)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.alice
                }
          }
   in snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)

-- The Synthetic Wellspring of Legends board the two CR 605.4a cases share:
-- alice's lone Island carries the Aura and pays for a Merfolk of the Pearl
-- Trident; the libraries are castThreshold's, `bobLibrary` the dial.
castThroughWellspring :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
castThroughWellspring s registry bobLibrary = do
  island <- S.printingOf s registry "Island"
  mountain <- S.printingOf s registry "Mountain"
  wellspring <- S.printingOf s registry "Synthetic Wellspring of Legends"
  merfolk <- S.printingOf s registry "Merfolk of the Pearl Trident"
  let g0 = Setup.emptyGame S.threePlayers
      g1 = addManyG mountain 3 S.alice (addManyG mountain bobLibrary S.bob (addManyG mountain 4 S.carol g0))
      g2 = S.landsFor island S.alice 1 (poolToLibraryG S.carol (poolToLibraryG S.bob (poolToLibraryG S.alice g1)))
  case Game.zoneMembers Zone.Battlefield S.alice g2 of
    [islandId] -> do
      let (auraId, g3) = S.addPermanent wellspring S.alice g2
          -- ToObject, as Parun.ManaSpec's wildGrowthBoard attaches its Aura.
          g4 = S.attachTo auraId (Recipient.ToObject islandId) g3
          (_spellId, g5) = S.addHandCard merfolk S.alice g4
          gStart =
            g5
              { GameState.turn =
                  (GameState.turn g5)
                    { TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.priority = Just S.alice
                    }
              }
      pure (snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop))
    _ -> Spec.assertFailure s "fixture should give alice exactly one Island" >> pure g2

-- Answers a discard with `chosen`, and everything else as S.castAnswer does.
discarding :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
discarding chosen prompt = case prompt of
  Prompt.ChooseDiscard {} -> chosen
  _ -> S.castAnswer prompt
