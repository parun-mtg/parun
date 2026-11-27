{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Parun.Engine.Resolve over the effects that move an object between zones or
-- change a life total: library position, drawing, losing life, the life-total
-- exchanges and the zone exchanges. The machinery is Parun.ResolveSpec.
module Parun.ZoneChangeSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Departure as Departure
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Resolve.Effect as Resolve
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as A
import qualified Parun.Types.AnyNumberOf as AnyNumberOf
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Departure as Departure.Type
import qualified Parun.Types.DiscardCause as DiscardCause
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GraveyardArrangement as GraveyardArrangement
import qualified Parun.Types.GraveyardOrder as GraveyardOrder
import qualified Parun.Types.LifeChange as LifeChange
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.Response as Response
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.Sickness as Sickness
import qualified Parun.Types.StepBegan as StepBegan
import qualified Parun.Types.Timestamp as Timestamp
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChange as ZoneChange

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- atBobAnswer with Library of Leng's CR 614.1a "may" answered as given.
lengAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
lengAnswer decision p = case p of
  Prompt.ChooseRedirect {} -> decision
  _ -> atBobAnswer p

-- CR 401.4's owner, answering Prompt.ArrangeLibraryArrivals: bob reverses the
-- order he is offered when `reversing`, and keeps it otherwise; any other seat
-- asked keeps it, so asking the wrong player leaves the move order.
arrangedBy :: Bool -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [Natural]
arrangedBy reversing pid oids
  | reversing && pid == S.bob = reverse (zipWith const [0 ..] oids)
  | otherwise = zipWith const [0 ..] oids

-- Library of Leng under bob, who holds a Forest and an Island over a library
-- of one Goblin Piker; alice casts Mind Rot at him and he puts both discards on
-- top. Two cards are his whole hand, so CR 609.3 takes both unasked, in hand
-- order: handCards adds at the front, so the Island goes first and the Forest
-- lands on it. Returns the resolved state.
lengMindRotBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
lengMindRotBoard s registry reversing = do
  swamp <- S.printingOf s registry "Swamp"
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  mindRot <- S.printingOf s registry "Mind Rot"
  leng <- S.printingOf s registry "Library of Leng"
  let base = S.landsInPlay swamp 3
      (_, withLeng) = S.addPermanent leng S.bob base
      (_, stocked) = S.addLibraryCard piker S.bob withLeng
      withHand = handCards island S.bob 1 (handCards forest S.bob 1 stocked)
      (gs, spellId) = S.handOne mindRot withHand
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeLibraryArrivals _ pid _ oids -> arrangedBy reversing pid oids
        _ -> lengAnswer OptionalDecision.Exercises p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Wheel of Sun and Moon, alice's, enchanting bob, who controls a Goblin Piker
-- and an Ogre Sentry over a library of one Typhoid Rats; alice casts Day of
-- Judgment, and both creatures go to the bottom of bob's library. Returns the
-- resolved state.
wheelJudgmentBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
wheelJudgmentBoard s registry reversing = do
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  rats <- S.printingOf s registry "Typhoid Rats"
  wheel <- S.printingOf s registry "Wheel of Sun and Moon"
  judgment <- S.printingOf s registry "Day of Judgment"
  let base = S.landsInPlay plains 4
      (wheelId, withWheel) = S.addPermanent wheel S.alice base
      enchanting = S.attachTo wheelId (Recipient.ToPlayer S.bob) withWheel
      (_, withPiker) = S.addPermanent piker S.bob enchanting
      (_, withSentry) = S.addPermanent sentry S.bob withPiker
      (_, stocked) = S.addLibraryCard rats S.bob withSentry
      (gs, spellId) = S.handOne judgment stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeLibraryArrivals _ pid _ oids -> arrangedBy reversing pid oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Wheel of Sun and Moon, alice's, enchanting bob, over a library of Plains,
-- Island, Swamp, Mountain and Forest from the top, then a Goblin Piker; alice
-- casts Tome Scour at bob, and the five milled cards go to the bottom of that
-- same library one by one. Returns the resolved state.
wheelScourBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
wheelScourBoard s registry reversing = do
  island <- S.printingOf s registry "Island"
  wheel <- S.printingOf s registry "Wheel of Sun and Moon"
  scour <- S.printingOf s registry "Tome Scour"
  pile <- traverse (S.printingOf s registry) ["Goblin Piker", "Forest", "Mountain", "Swamp", "Island", "Plains"]
  let base = S.landsInPlay island 1
      (wheelId, withWheel) = S.addPermanent wheel S.alice base
      enchanting = S.attachTo wheelId (Recipient.ToPlayer S.bob) withWheel
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) enchanting pile
      (gs, spellId) = S.handOne scour stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeLibraryArrivals _ pid _ oids -> arrangedBy reversing pid oids
        _ -> atBobAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Wheel of Sun and Moon enchanting alice, over her library of Plains, Island,
-- Goblin Piker and Swamp from the top; she casts Curate and surveils both top
-- cards into her graveyard, Plains first, so both go to the bottom; then she
-- draws the Piker, and Curate itself follows them under (CR 608.2n). She
-- reverses the arrangement she is offered when
-- `reversing`. Returns the resolved state.
wheelCurateBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
wheelCurateBoard s registry reversing = do
  island <- S.printingOf s registry "Island"
  wheel <- S.printingOf s registry "Wheel of Sun and Moon"
  curate <- S.printingOf s registry "Curate"
  pile <- traverse (S.printingOf s registry) ["Swamp", "Goblin Piker", "Island", "Plains"]
  let base = S.landsInPlay island 2
      (wheelId, withWheel) = S.addPermanent wheel S.alice base
      enchanting = S.attachTo wheelId (Recipient.ToPlayer S.alice) withWheel
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) enchanting pile
      (gs, spellId) = S.handOne curate stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseSurveil _ _ looked -> (looked, [])
        Prompt.ArrangeLibraryArrivals _ pid _ oids
          | reversing && pid == S.alice -> reverse (zipWith const [0 ..] oids)
          | otherwise -> zipWith const [0 ..] oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- CR 404.3's owner, answering Prompt.ArrangeGraveyardArrivals: bob gives
-- `answer` the batch he is offered; any other seat asked takes any order, so
-- asking the wrong player leaves the move order.
graveyardBy :: ([ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement) -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement
graveyardBy answer pid oids
  | pid == S.bob = answer oids
  | otherwise = GraveyardArrangement.AnyOrder

-- Reverse the offered batch.
reversedBatch :: [ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement
reversedBatch oids = GraveyardArrangement.InOrder (reverse (zipWith const [0 ..] oids))

-- bob controls Volrath's Shapeshifter over a library of Ogre Sentry, Plains,
-- Island, Swamp and Mountain from the top, then a Goblin Piker, with his
-- graveyard order set to `order`, or left at the game's default; alice casts Tome Scour at him, so the Sentry
-- is milled first and the Mountain last, and bob gives `answer` any
-- arrangement he is offered. Returns the resolved state and the Shapeshifter.
shapeshifterScourBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe GraveyardOrder.GraveyardOrder -> ([ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement) -> m (GameState.GameState, ObjectId.ObjectId)
shapeshifterScourBoard s registry order arrange = do
  island <- S.printingOf s registry "Island"
  shapeshifter <- S.printingOf s registry "Volrath's Shapeshifter"
  scour <- S.printingOf s registry "Tome Scour"
  pile <- traverse (S.printingOf s registry) ["Goblin Piker", "Mountain", "Swamp", "Island", "Plains", "Ogre Sentry"]
  let base = maybe id (Game.setGraveyardOrder S.bob) order (S.landsInPlay island 1)
      (shifterId, withShifter) = S.addPermanent shapeshifter S.bob base
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withShifter pile
      (gs, spellId) = S.handOne scour stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeGraveyardArrivals _ pid oids -> graveyardBy arrange pid oids
        _ -> atBobAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop), shifterId)

-- bob controls a Goblin Piker and an Ogre Sentry, and arranges what he is
-- offered with `answer`, his graveyard order Matters; alice casts Day of
-- Judgment. Returns the resolved state.
judgmentBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> ([ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement) -> m GameState.GameState
judgmentBoard s registry arrange = do
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  judgment <- S.printingOf s registry "Day of Judgment"
  let base = Game.setGraveyardOrder S.bob GraveyardOrder.Matters (S.landsInPlay plains 4)
      (_, withPiker) = S.addPermanent piker S.bob base
      (_, withSentry) = S.addPermanent sentry S.bob withPiker
      (gs, spellId) = S.handOne judgment withSentry
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeGraveyardArrivals _ pid oids -> graveyardBy arrange pid oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- alice, her graveyard order Matters, casts Curate over her library of Plains,
-- Island and Goblin Piker from the top and surveils both top cards into her
-- graveyard, Plains first. Any graveyard arrangement she is offered she
-- reverses. Returns the resolved state.
curateBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
curateBoard s registry = do
  island <- S.printingOf s registry "Island"
  curate <- S.printingOf s registry "Curate"
  pile <- traverse (S.printingOf s registry) ["Goblin Piker", "Island", "Plains"]
  let base = Game.setGraveyardOrder S.alice GraveyardOrder.Matters (S.landsInPlay island 2)
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) base pile
      (gs, spellId) = S.handOne curate stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseSurveil _ _ looked -> (looked, [])
        Prompt.ArrangeGraveyardArrivals _ _ oids -> reversedBatch oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Three seats: alice controls Pulmonic Sliver; carol owns a Lymph Sliver bob
-- controls, over a library of one Goblin Piker each for bob and carol. The
-- Lymph Sliver is destroyed; bob answers the redirect as given and any other
-- seat asked answers the opposite. Returns the state and the two printings the
-- assertions name.
pulmonicBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> OptionalDecision.OptionalDecision -> m (GameState.GameState, Printing.Printing, Printing.Printing)
pulmonicBoard s registry decision = do
  pulmonic <- S.printingOf s registry "Pulmonic Sliver"
  lymph <- S.printingOf s registry "Lymph Sliver"
  piker <- S.printingOf s registry "Goblin Piker"
  let (_, withPulmonic) = S.addPermanent pulmonic S.alice S.threePlayerGame
      (lymphId, withLymph) = S.addPermanent lymph S.carol withPulmonic
      stolen = S.giveControl lymphId S.bob withLymph
      (_, g1) = S.addLibraryCard piker S.bob stolen
      (_, g2) = S.addLibraryCard piker S.carol g1
      opposite = if decision == OptionalDecision.Exercises then OptionalDecision.Declines else OptionalDecision.Exercises
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseRedirect _ pid _ _ -> if pid == S.bob then decision else opposite
        _ -> S.identityAnswer p
      after = S.runPure answer g2 (Event.destroy Settle.none Regenerability.Regenerable [lymphId])
  pure (after, lymph, piker)

-- Add k cards of a printing to pid's hand (each a fresh Hand-zone object).
handCards :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
handCards printing pid k gs =
  let addOne g =
        let (printingId, gP) = Game.intern printing g
            (oid, g1) = Game.freshObjectId gP
            obj =
              (Game.cardObject oid pid printingId Zone.Hand (Timestamp.MkTimestamp 0))
                { Object.sickness = Sickness.Settled pid
                }
         in g1
              { GameState.objects = Map.insert oid obj (GameState.objects g1),
                GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand g1)
              }
   in List.foldl' (\g _ -> addOne g) gs [1 .. k]

-- Put k cards of a printing into pid's library, each on top of the last, for a
-- draw to find.
stockLibrary :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
stockLibrary printing pid k gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. k]

-- alice's upkeep begins, settled to the point where any trigger it woke is on
-- the stack (CR 603.3b) waiting to resolve.
settleAtAlicesUpkeep :: GameState.GameState -> GameState.GameState
settleAtAlicesUpkeep gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          ( gs
              { GameState.turn =
                  (GameState.turn gs)
                    { TurnRegisters.phase = upkeep,
                      TurnRegisters.activePlayer = S.alice
                    }
              }
          )
   in snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)

-- Who drew, in the order they drew, read off the turn-scoped event log. CR
-- 121.1 makes a draw one library-to-hand move, and a library and a hand each
-- belong to one player, so the moved card's owner is the drawer. Any OTHER route
-- from library to hand would count here too; no fixture below has one.
drawersOf :: GameState.GameState -> [PlayerId.PlayerId]
drawersOf gs =
  let drawer zc =
        if ZoneChange.from zc == Zone.Library && ZoneChange.to zc == Zone.Hand
          then fmap Object.owner (Game.lookupObject (ZoneChange.object zc) gs)
          else Nothing
   in Maybe.mapMaybe drawer (S.zoneChangesOf gs)

-- Shahrazad and Sindbad on alice's battlefield, untapped and settled so its {T}
-- is payable (CR 302.6), over a library whose TOP card is `top` and a hand
-- holding `handLand` and `handSpell` -- two cards named neither `top` nor each
-- other, so any of the three is identifiable in the result by name alone.
--
-- `filler` sits under the drawn card twice over, which is what keeps CR 104.3c
-- out of the fixture: alice still has a library after the draw.
sindbadBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
sindbadBoard sindbad top filler handLand handSpell =
  let (sindbadId, onBoard) = S.addPermanent sindbad S.alice (Setup.emptyGame S.bothPlayers)
      -- S.addLibraryCard puts each card ON TOP of the last, so `top` goes in
      -- after the filler.
      stocked = snd (S.addLibraryCard top S.alice (stockLibrary filler S.alice 2 onBoard))
   in (sindbadId, snd (S.addHandCard handSpell S.alice (snd (S.addHandCard handLand S.alice stocked))))

-- Activate the permanent's one activated ability for alice and resolve it. LOUD
-- rather than quiet when the card offers a different number of them: a helper
-- that silently did nothing is how this test would stay green while activating
-- no ability at all.
activateSole :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateSole oid gs = case Activatable.abilitiesFor oid gs of
  [ability] ->
    let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
     in S.runPure S.identityAnswer activated Stack.resolveTop
  other -> error ("Parun.ZoneChangeSpec: expected exactly one activated ability, got " <> show (length other))

zoneChangeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
zoneChangeSpec s registry = Spec.describe s "ZoneChange" $ do
  -- CR 121.2c: "If more than one player is instructed to draw cards, the
  -- active player performs all of their draws first, then each other player
  -- in turn order does the same." The seat order the players map answers in
  -- is not that order, so this needs an active player who is not the first
  -- seat: alice casts an INSTANT on BOB's turn, which makes seat order
  -- [alice, bob, carol] and turn order [bob, carol, alice] disagree.
  --
  -- The draws are read back off the turn-scoped event log -- the same log a
  -- trigger scans (CR 603.2) -- because that is where the order of the
  -- individual draws is observable; the hand sizes alone are order-blind.
  Spec.it s "CR 121.2c Vision Skeins draws for the active player first, then in turn order" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let -- S.landsInPlay builds its own two-seat game, so the {1}{U} goes on
        -- a three-seat board one Island at a time.
        withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        -- handOne hands alice the turn along with the card, so bob takes the
        -- turn back. Cast.castSpell gates neither timing nor priority, but
        -- the fixture is a legal board regardless: Vision Skeins is an
        -- INSTANT, which alice may cast on bob's turn.
        gs =
          gs0
            { GameState.turn =
                (GameState.turn gs0)
                  { TurnRegisters.activePlayer = S.bob
                  }
            }
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "bob (active) draws both of his, then carol, then the caster"
      (drawersOf after)
      [S.bob, S.bob, S.carol, S.carol, S.alice, S.alice]
    Spec.assertEqWith s "and everyone holds two" (fmap (\pid -> S.handSize pid after) [S.alice, S.bob, S.carol]) [2, 2, 2]
  -- The card that proves Effect.Draw's `Relative Opponent` arm (#276), and
  -- the one shape no "you draw" card can stand in for: Master of the Feast's
  -- trigger is a DRAWBACK, drawing for everyone except the player who
  -- controls it (CR 109.5 makes "your upkeep" that controller's).
  Spec.it s "CR 121.1 Master of the Feast's upkeep trigger draws for the opponent, not its controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addPermanent masterOfTheFeast S.alice (Setup.emptyGame S.bothPlayers)
        withLibs = stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board)
        onStack = settleAtAlicesUpkeep withLibs
        after = snd (Engine.runGamePure S.identityAnswer onStack Stack.resolveTop)
    Spec.assertBool s (not (null (TurnRegisters.stack (GameState.turn onStack)))) "the upkeep trigger really reached the stack"
    Spec.assertEqWith s "bob drew" (S.handSize S.bob after) 1
    Spec.assertEqWith s "alice, who controls it, did not" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and alice's library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  -- The discriminator, and it needs a THIRD seat: at two players an
  -- `Opponent` arm that reached only ONE opponent is indistinguishable from
  -- one that reaches them all. CR 806.1: in a Free-for-All the players
  -- compete as individuals, so every other player is an opponent -- CR 102.3's
  -- teammates are the one exception, and this board is played between no teams
  -- -- and both of them draw.
  Spec.it s "CR 806.1 at three seats each opponent draws off Master of the Feast, and only opponents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addPermanent masterOfTheFeast S.alice S.threePlayerGame
        withLibs = stockLibrary piker S.carol 1 (stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board))
        after = snd (Engine.runGamePure S.identityAnswer (settleAtAlicesUpkeep withLibs) Stack.resolveTop)
    -- A drawer whose library was empty would draw no card and so record no
    -- zone change; this is what keeps the list below honest about that.
    Spec.assertEqWith s "no draw outran a library" (GameState.drewFromEmpty after) Set.empty
    Spec.assertEqWith s "both opponents drew, and the controller did not" (drawersOf after) [S.bob, S.carol]
  -- CR 102.1: "A player is one of the people in the game", so once CR 800.4a
  -- takes carol out, `Relative AnyPlayer` stops naming her (#279). It needs three
  -- seats twice over: CR 800.4 says only a multiplayer game -- CR 800.1's,
  -- one that BEGAN with more than two players -- continues after a
  -- departure, and a two-seat game would already have ended under CR 104.2a
  -- with nothing left to resolve.
  --
  -- drewFromEmpty is what makes this observable rather than merely tidy.
  -- CR 800.4a took carol's library out of the game with her, so a draw aimed
  -- at her finds it empty and Event.drawCard writes her seat into that set --
  -- engine state recorded for someone who is not in the game.
  Spec.it s "CR 800.4a Vision Skeins does not draw for a player who has left the game" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        gs = S.departs Departure.Type.Conceded S.carol gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the two players still in the game drew, in APNAP order" (drawersOf after) [S.alice, S.alice, S.bob, S.bob]
    Spec.assertEqWith s "and nothing was drawn against carol's departed library" (GameState.drewFromEmpty after) Set.empty
  -- The card that proves Effect.Draw's SLOT (#1899): Shahrazad and Sindbad's
  -- "{T}: Draw a card and reveal it. If it isn't a land card, discard it."
  -- (Unknown Event; Oracle text and type line checked against
  -- api.scryfall.com). Every draw above answers "who" and "how many"; this one
  -- is the first that has to answer WHICH CARD, because the reveal and the
  -- discard both say "it" -- CR 121.1 puts the card in the hand, and the two
  -- later instructions name that card there.
  --
  -- The pair below differ in EXACTLY ONE THING, the top card of the library, so
  -- what the second board shows is the condition reading the bound card rather
  -- than a board assembled to pass. A binding that named the wrong card would
  -- reverse both: the nonland board would bury a card the draw never touched,
  -- and the land board -- whose hand holds a nonland the whole time -- would
  -- discard THAT instead of leaving the graveyard empty. A binding that named
  -- nothing empties the first board's graveyard.
  Spec.it s "CR 121.1 / 701.20a Shahrazad and Sindbad discards the nonland card it just drew, and only that card" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    let (sindbadId, board) = sindbadBoard sindbad piker plains island divination
        after = activateSole sindbadId board
    Spec.assertEqWith
      s
      "the Goblin Piker it drew is what went to the graveyard"
      (namesIn Zone.Graveyard S.alice after)
      [Just (S.printingName piker)]
    Spec.assertEqWith
      s
      "and the two cards the hand already held, one of them a nonland, are untouched"
      (List.sort (namesIn Zone.Hand S.alice after))
      (List.sort [Just (S.printingName island), Just (S.printingName divination)])
    Spec.assertEqWith
      s
      "CR 701.20a it was shown to the table as it was drawn"
      (S.revealsOf after)
      [(S.alice, Set.singleton (S.printingName piker))]
  Spec.it s "CR 121.1 the same ability keeps a LAND it drew, so the clause condition reads the bound card" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    let (sindbadId, board) = sindbadBoard sindbad mountain plains island divination
        after = activateSole sindbadId board
    Spec.assertEqWith
      s
      "the Mountain it drew joined the hand and stayed there"
      (List.sort (namesIn Zone.Hand S.alice after))
      (List.sort [Just (S.printingName mountain), Just (S.printingName island), Just (S.printingName divination)])
    Spec.assertEqWith s "and nothing was discarded" (namesIn Zone.Graveyard S.alice after) []
    Spec.assertEqWith
      s
      "CR 701.20a the reveal happens either way"
      (S.revealsOf after)
      [(S.alice, Set.singleton (S.printingName mountain))]
  -- CR 121.6c: the card Plagiarize has alice draw in place of bob's draw is
  -- drawn as a result of the replacement, so bob's "reveal it" does not reach it.
  -- The pair differ in EXACTLY ONE THING, whether alice cast Plagiarize at bob
  -- before he activated; bob's library tops with a Goblin Piker and alice's with
  -- Divination, so a reveal of either is identifiable by name.
  Spec.it s "CR 121.6c a draw Plagiarize redirects binds nothing for Shahrazad and Sindbad's reveal" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    plagiarize <- S.printingOf s registry "Plagiarize"
    piker <- S.printingOf s registry "Goblin Piker"
    divination <- S.printingOf s registry "Divination"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    let (sindbadId, withSindbad) = S.addPermanent sindbad S.bob (Setup.emptyGame S.bothPlayers)
        stocked = stockLibrary divination S.alice 2 (snd (S.addLibraryCard piker S.bob (stockLibrary plains S.bob 2 withSindbad)))
        withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) stocked [1 .. (4 :: Int)]
        (board, plagiarizeId) = S.handOne plagiarize withMana
        activateForBob gs = case Activatable.abilitiesFor sindbadId gs of
          [ability] ->
            let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.bob sindbadId ability)
             in S.runPure S.identityAnswer activated Stack.resolveTop
          other -> error ("Parun.ZoneChangeSpec: expected exactly one activated ability, got " <> show (length other))
        plagiarized =
          let cast = S.runPure atBobAnswer board (S.cast S.alice plagiarizeId)
           in S.runPure S.identityAnswer cast Stack.resolveTop
        after = activateForBob plagiarized
        control = activateForBob board
    Spec.assertEqWith s "nothing is revealed: alice's Divination was drawn as a result of the replacement" (S.revealsOf after) []
    Spec.assertEqWith
      s
      "and on the board without Plagiarize, bob reveals the Goblin Piker he drew"
      (S.revealsOf control)
      [(S.bob, Set.singleton (S.printingName piker))]
    Spec.assertEqWith s "alice drew the Divination in bob's place" (namesIn Zone.Hand S.alice after) [Just (S.printingName divination)]
    Spec.assertEqWith s "and bob drew nothing" (namesIn Zone.Hand S.bob after) []
  -- CR 401.4: "if an effect puts two or more cards in a specific position in a
  -- library at the same time, the owner of those cards may arrange them in any
  -- order" -- Library of Leng's own ruling says the same. Mind Rot's two
  -- discards are one event (CR 608.2f), each redirected onto the top of bob's
  -- library, so bob is asked their order. The pair differs in bob's answer
  -- alone: kept, the Forest moved last is on top; reversed, the Island is, and
  -- it is what bob draws next.
  Spec.it s "CR 401.4 bob orders the two Mind Rot discards Library of Leng puts on top of his library" $ do
    reversed <- lengMindRotBoard s registry True
    kept <- lengMindRotBoard s registry False
    let name = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "reversed: the Island is on top, over the Forest" (namesIn Zone.Library S.bob reversed) [name "Island", name "Forest", name "Goblin Piker"]
    Spec.assertEqWith s "kept: the Forest is on top, over the Island" (namesIn Zone.Library S.bob kept) [name "Forest", name "Island", name "Goblin Piker"]
    Spec.assertEqWith s "and his graveyard is empty" (namesIn Zone.Graveyard S.bob reversed) []
  -- The same rule through the destroy funnel: Wheel of Sun and Moon sends Day of
  -- Judgment's two victims to the bottom of bob's library together, and bob
  -- orders them from the bottom up.
  Spec.it s "CR 401.4 bob orders the two creatures Wheel of Sun and Moon puts under his library" $ do
    reversed <- wheelJudgmentBoard s registry True
    kept <- wheelJudgmentBoard s registry False
    let name = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "reversed: the Goblin Piker is on the bottom" (namesIn Zone.Library S.bob reversed) [name "Typhoid Rats", name "Ogre Sentry", name "Goblin Piker"]
    Spec.assertEqWith s "kept: the Ogre Sentry is on the bottom" (namesIn Zone.Library S.bob kept) [name "Typhoid Rats", name "Goblin Piker", name "Ogre Sentry"]
  -- And through the mill funnel, which moves its cards one at a time outside
  -- any event bracket: Tome Scour's five cards reach the bottom together all
  -- the same (CR 701.17a, one instruction), so bob orders them.
  Spec.it s "CR 401.4 bob orders the five cards Wheel of Sun and Moon puts under his library from Tome Scour" $ do
    reversed <- wheelScourBoard s registry True
    kept <- wheelScourBoard s registry False
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "reversed: the Plains, milled first, is on the bottom" (namesIn Zone.Library S.bob reversed) (names ["Goblin Piker", "Forest", "Mountain", "Swamp", "Island", "Plains"])
    Spec.assertEqWith s "kept: the Forest, milled last, is on the bottom" (namesIn Zone.Library S.bob kept) (names ["Goblin Piker", "Plains", "Island", "Swamp", "Mountain", "Forest"])
  -- And through a surveil's graveyard half (CR 701.25a), the same one-by-one
  -- road.
  Spec.it s "CR 401.4 alice orders the two cards she surveils under her library with Wheel of Sun and Moon" $ do
    reversed <- wheelCurateBoard s registry True
    kept <- wheelCurateBoard s registry False
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "reversed: the Plains, put first, is under the Island" (namesIn Zone.Library S.alice reversed) (names ["Swamp", "Island", "Plains", "Curate"])
    Spec.assertEqWith s "kept: the Island, put last, is under the Plains" (namesIn Zone.Library S.alice kept) (names ["Swamp", "Plains", "Island", "Curate"])
    Spec.assertEqWith s "and she drew the Goblin Piker" (namesIn Zone.Hand S.alice reversed) (names ["Goblin Piker"])
  -- CR 404.3: "if an effect or rule puts two or more cards into the same
  -- graveyard at the same time, the owner of those cards may arrange them in
  -- any order", and CR 404.1 makes the top observable: Volrath's Shapeshifter
  -- has the full text of the top card of bob's graveyard. Tome Scour mills
  -- five at once (CR 701.17a), so bob, whose graveyard order Matters, is asked
  -- -- he and not alice, who cast it.
  Spec.it s "CR 404.3 bob puts the milled Ogre Sentry on top of his graveyard, and Volrath's Shapeshifter becomes it" $ do
    (reversed, shifterId) <- shapeshifterScourBoard s registry (Just GraveyardOrder.Matters) reversedBatch
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Shapeshifter is a 3/3 Ogre Sentry" (Projection.namesOf shifterId reversed, S.powerToughnessOf shifterId reversed) (Set.singleton (CardName.MkCardName (Text.pack "Ogre Sentry")), Just (3, 3))
    Spec.assertEqWith s "the Ogre Sentry is on top" (namesIn Zone.Graveyard S.bob reversed) (names ["Mountain", "Swamp", "Island", "Plains", "Ogre Sentry"])
  -- The same board with bob's standing setting left at a new game's default:
  -- the order does not matter to him, so he is not asked and the cards keep the order they
  -- moved in, his reversing answer never given.
  Spec.it s "CR 404.3 bob, to whom graveyard order does not matter, is not asked" $ do
    (unasked, shifterId) <- shapeshifterScourBoard s registry Nothing reversedBatch
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Mountain, milled last, is on top, and the Shapeshifter is its printed 0/1" (Projection.namesOf shifterId unasked, S.powerToughnessOf shifterId unasked) (Set.singleton (CardName.MkCardName (Text.pack "Volrath's Shapeshifter")), Just (0, 1))
    Spec.assertEqWith s "in the order they moved" (namesIn Zone.Graveyard S.bob unasked) (names ["Ogre Sentry", "Plains", "Island", "Swamp", "Mountain"])
  -- Asked, bob may answer that this batch's order does not matter to him; it
  -- then keeps the order it moved in.
  Spec.it s "CR 404.3 answering any order keeps the order the cards moved in" $ do
    (anyOrder, shifterId) <- shapeshifterScourBoard s registry (Just GraveyardOrder.Matters) (const GraveyardArrangement.AnyOrder)
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Mountain is on top" (namesIn Zone.Graveyard S.bob anyOrder) (names ["Ogre Sentry", "Plains", "Island", "Swamp", "Mountain"])
    Spec.assertEqWith s "and the Shapeshifter is its printed 0/1" (S.powerToughnessOf shifterId anyOrder) (Just (0, 1))
  -- The same rule through the destroy funnel: Day of Judgment's two victims
  -- reach bob's graveyard at once, and he orders them.
  Spec.it s "CR 404.3 bob orders the two creatures Day of Judgment puts into his graveyard" $ do
    reversed <- judgmentBoard s registry reversedBatch
    kept <- judgmentBoard s registry (GraveyardArrangement.InOrder . zipWith const [0 ..])
    Spec.assertEqWith s "reversed is kept, the other way up" (namesIn Zone.Graveyard S.bob reversed) (reverse (namesIn Zone.Graveyard S.bob kept))
    Spec.assertEqWith s "two cards" (length (namesIn Zone.Graveyard S.bob kept)) 2
  -- A surveil's answer already orders the cards it puts into the graveyard
  -- (Prompt.ChooseSurveil), so alice is not asked again: her reversing
  -- answerer leaves the Plains, put first, under the Island, and Curate on top
  -- (CR 608.2n).
  Spec.it s "CR 404.3 a surveil's graveyard cards are not arranged a second time" $ do
    after <- curateBoard s registry
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Plains, then the Island, then Curate" (namesIn Zone.Graveyard S.alice after) (names ["Plains", "Island", "Curate"])
  -- CR 118.12: Tweeze's "You may discard a card. If you do, draw a card" makes
  -- the discard a cost paid on resolution, and Library of Leng's ruling: "you
  -- can't use the Library of Leng ability ... when you discard a card as a cost,
  -- because costs aren't effects". Alice controls the Library and takes every
  -- "may" and every redirect she is offered; were the redirect offered, the
  -- Forest would go on top of her library and be the card she draws.
  Spec.it s "CR 118.12 Library of Leng does not reach Tweeze's discard, a cost" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    tweeze <- S.printingOf s registry "Tweeze"
    leng <- S.printingOf s registry "Library of Leng"
    let (_, withLeng) = S.addPermanent leng S.alice (S.landsInPlay mountain 3)
        (_, stocked) = S.addLibraryCard piker S.alice withLeng
        (withSpell, spellId) = S.handOne tweeze stocked
        (_, gs) = S.addHandCard forest S.alice withSpell
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          _ -> lengAnswer OptionalDecision.Exercises p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
        name = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "the Forest is in alice's graveyard beside Tweeze" (List.sort (namesIn Zone.Graveyard S.alice after)) (List.sort [name "Forest", name "Tweeze"])
    Spec.assertEqWith s "and she drew the Goblin Piker, not the Forest" (namesIn Zone.Hand S.alice after) [name "Goblin Piker"]
    Spec.assertEqWith s "bob took the 3 damage" (S.lifeOf S.bob after) (Just 17)
  -- The row's other gate, its ruling's "you can't use the Library of Leng
  -- ability ... when you discard a card as a cost, because costs aren't
  -- effects": a DiscardCause.Ordinary discard is never offered the row, so an
  -- answerer that would take it is never asked and the card reaches the
  -- graveyard.
  Spec.it s "CR 701.9a Library of Leng does not reach a discard no effect caused" $ do
    swamp <- S.printingOf s registry "Swamp"
    leng <- S.printingOf s registry "Library of Leng"
    let (_, withLeng) = S.addPermanent leng S.bob (S.landsInPlay swamp 3)
        (swampId, gs) = S.addHandCard swamp S.bob withLeng
        after = S.runPure (lengAnswer OptionalDecision.Exercises) gs (Event.discard Settle.none DiscardCause.Ordinary S.bob swampId)
    Spec.assertEqWith s "bob's swamp is in his graveyard" (namesIn Zone.Graveyard S.bob after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "and not in his library" (namesIn Zone.Library S.bob after) []
  -- Pulmonic Sliver's GRANTED "may" row (CR 614.1a), on a stolen Sliver: alice
  -- controls Pulmonic, carol owns Lymph Sliver, bob controls it. CR 109.5 makes
  -- bob, the controller of the Sliver the ability is on, the one asked; CR 400.3
  -- sends the card to carol's library. The pair differs in the answerer's flip
  -- alone: bob's answer is as given and every other seat answers the opposite,
  -- so asking alice or carol lands the card in the wrong zone in both cases.
  Spec.it s "CR 109.5 / 400.3 Pulmonic Sliver's granted redirect asks the Sliver's controller and tops its owner's library" $ do
    (after, lymph, piker) <- pulmonicBoard s registry OptionalDecision.Exercises
    Spec.assertEqWith s "lymph sliver is on top of carol's library" (namesIn Zone.Library S.carol after) [Just (S.printingName lymph), Just (S.printingName piker)]
    Spec.assertEqWith s "and bob's library is untouched" (namesIn Zone.Library S.bob after) [Just (S.printingName piker)]
  Spec.it s "CR 614.1a Pulmonic Sliver's granted redirect declined leaves the Sliver in its owner's graveyard" $ do
    (after, lymph, piker) <- pulmonicBoard s registry OptionalDecision.Declines
    Spec.assertEqWith s "lymph sliver is in carol's graveyard" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName lymph)]
    Spec.assertEqWith s "and carol's library is untouched" (namesIn Zone.Library S.carol after) [Just (S.printingName piker)]
  -- The Wheel's own relation, CR 303.4b's enchanted player
  -- (ControllerRelation.NamedBySource Enchanted, judged by Replacement.relationHolds),
  -- as the whole card: cast at bob, then one card of each seat's headed for its
  -- owner's graveyard. THREE seats, because two cannot part the relation from
  -- its siblings: alice controls the Wheel, so Related You would take her card, and
  -- carol is her opponent, so Opponents would take carol's. Bob's library is
  -- stocked so the bottom is a position; the reveal (CR 701.20a) is asserted off
  -- the log, and is bob's, who holds the card.
  Spec.it s "CR 303.4b / 614.1a Wheel of Sun and Moon reroutes only the enchanted player's cards" $ do
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wheel <- S.printingOf s registry "Wheel of Sun and Moon"
    let lands = S.landsFor forest S.alice 2 S.threePlayerGame
        (staged, spellId) = S.handOne wheel lands
        cast = snd (Engine.runGamePure atBobAnswer staged (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        (_, stocked) = S.addLibraryCard piker S.bob resolved
        (bobs, g1) = S.addHandCard swamp S.bob stocked
        (alices, g2) = S.addHandCard swamp S.alice g1
        (carols, g3) = S.addHandCard swamp S.carol g2
        after = S.runPure S.identityAnswer g3 (mapM_ (\oid -> Event.changeZone Settle.none oid Zone.Graveyard) [bobs, alices, carols])
        enchanting = fmap Object.attachedTo (filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after)))
    Spec.assertEqWith s "bob's card went to the bottom of bob's library" (namesIn Zone.Library S.bob after) [Just (S.printingName piker), Just (S.printingName swamp)]
    Spec.assertEqWith s "and not to bob's graveyard" (namesIn Zone.Graveyard S.bob after) []
    Spec.assertEqWith s "alice's own card reached her graveyard -- she is not the enchanted player" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "carol's card reached her graveyard -- an opponent is not the enchanted player" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "bob revealed the card on its way" (S.revealsOf after) [(S.bob, Set.singleton (S.printingName swamp))]
    Spec.assertEqWith s "the Wheel is attached to bob" enchanting [Just (Recipient.ToPlayer S.bob)]
  -- Noxious Toad's "each opponent discards a card", with no slot at all. bob
  -- controls the Toad at a three-seat table on alice's turn, so "each opponent"
  -- is two seats and excludes the controller, and CR 101.4's order starts from
  -- the ACTIVE player: alice, then carol, where turn order from the controller
  -- would ask carol first.
  Spec.it s "CR 701.9 / 101.4 Noxious Toad has each opponent discard, in APNAP order" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    sentry <- S.printingOf s registry "Ogre Sentry"
    rats <- S.printingOf s registry "Typhoid Rats"
    toad <- S.printingOf s registry "Noxious Toad"
    let (toadId, placed) = S.addPermanent toad S.bob S.threePlayerGame
        gs = S.markDamage toadId 1 (handCards rats S.carol 2 (handCards sentry S.bob 2 (handCards piker S.alice 2 placed)))
        answer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        answer p = case p of
          Prompt.ChooseDiscard _ victim held _ _ -> do
            State.modify' (<> [victim])
            pure (take 1 held)
          _ -> pure (S.identityAnswer p)
        (after, asked) =
          flip State.runState [] $ do
            -- CR 704.5g kills the Toad, and settling puts its CR 603.6c dies
            -- trigger on the stack.
            (_, died) <- Engine.runGame answer gs Engine.settleForPriority
            (_, resolved) <- Engine.runGame answer died Stack.resolveTop
            pure resolved
    Spec.assertEqWith s "alice discarded one of her two" (S.handSize S.alice after) 1
    Spec.assertEqWith s "carol discarded one of her two" (S.handSize S.carol after) 1
    Spec.assertEqWith s "bob, the controller, kept both" (S.handSize S.bob after) 2
    Spec.assertEqWith s "alice's graveyard holds her own piker" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "carol's graveyard holds her own rats" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName rats)]
    Spec.assertEqWith s "CR 101.4: asked in turn order from the active player, not the controller" asked [S.alice, S.carol]

drawCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
drawCardSpec s registry = Spec.describe s "DrawCard" $ do
  Spec.it s "CR 121.2 drawCard moves the top library card to hand" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, withCard) = S.addLibraryCard piker S.alice base
        after = S.runPure S.identityAnswer withCard (Event.drawCard Settle.none S.alice)
    Spec.assertEqWith s "one card in hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice after) []

loseLifeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
loseLifeSpec s registry = Spec.describe s "LoseLife" $ do
  -- Both cases are Sign in Blood, the card that proves the opcode (#273): its
  -- two clauses share one target slot, so the player who draws is the player
  -- who loses life, and neither is aimed at the caster.
  -- The last assertion is the falsifier for a life loss spelled as damage.
  -- CR 119.2 makes damage a CAUSE of life loss, not a synonym for it, so
  -- this records no damage event for CR 614/615's replacement and
  -- prevention, infect's CR 120.3b diversion or CR 704.5h's deathtouch scan
  -- to read.
  Spec.it s "CR 119.3 Sign in Blood makes the player it targets draw two and lose two life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let base = S.landsInPlay swamp 2
        withLib = stockLibrary piker S.bob 3 base
        (gs, spellId) = S.handOne signInBlood withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        isDamage ev = case ev of
          GameEvent.DamageDealt _ -> True
          _ -> False
    Spec.assertEqWith s "bob drew two" (S.handSize S.bob after) 2
    Spec.assertEqWith s "and lost two life" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "alice, who cast it, lost none" (S.lifeOf S.alice after) (S.lifeOf S.alice gs)
    Spec.assertBool s (not (any isDamage (S.eventsOf after))) "no damage was dealt (CR 119.2)"

-- The life events the whole step logged, by player and amount. CR 701.12c makes
-- the exchange a GAIN and a LOSS rather than two assignments, so this is what a
-- "whenever you gain life" trigger would have to read.
lifeGains :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeGains gs = Maybe.mapMaybe (\ev -> case ev of GameEvent.LifeGained (LifeChange.MkLifeChange pid n _) -> Just (pid, n); _ -> Nothing) (S.eventsOf gs)

lifeLosses :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeLosses gs = Maybe.mapMaybe (\ev -> case ev of GameEvent.LifeLost (LifeChange.MkLifeChange pid n _) -> Just (pid, n); _ -> Nothing) (S.eventsOf gs)

-- CR 701.12g. The life-for-power and life-for-toughness exchanges are the
-- scenarios under data/scenarios/zone-change/cr-701-12g-*.json and
-- cr-119-7-*.json.
exchangeValuesSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
exchangeValuesSpec s registry = Spec.describe s "ExchangeValues" $ do
  -- Serene Master, a 0/2, blocks alice's Hill Giant, a 3/3, and its trigger
  -- targets the Giant (CR 509.1g's BlockPartnerOfSource): the powers swap until
  -- end of combat (CR 511.2), so the Master deals 3 and takes 0.
  Spec.it s "CR 701.12g Serene Master swaps powers with the creature it blocks until end of combat" $ do
    master <- S.printingOf s registry "Serene Master"
    giant <- S.printingOf s registry "Hill Giant"
    let (board, giants, masters) = S.combatBoardOf [giant] [master]
        -- Aims AWAY from the Giant whenever anything else is offered, so a filter
        -- letting the Master itself in is caught aiming there.
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> S.preferring (\r -> notElem r (fmap Recipient.ToCreature giants)) sets
          _ -> S.aggressiveAnswer p
        after = S.runCombat answer board
    case (giants, masters) of
      ([giantId], [masterId]) -> do
        Spec.assertBool s (not (Set.member giantId (GameState.battlefield after))) "the Giant died to the Master's borrowed 3 power"
        Spec.assertEqWith s "CR 511.2 the Master survived the Giant's borrowed 0 power and is a 0/2 again after combat" (S.powerToughnessOf masterId after) (Just (0, 2))
      _ -> Spec.assertFailure s "expected one creature a side"

-- CR 119.5: "If an effect sets a player's life total to a specific number, the
-- player gains or loses the necessary amount of life to end up with the new
-- total." So a set is NOT a third kind of life event: it is a gain or a loss,
-- whichever the arithmetic makes it, and everything that watches gaining or
-- losing life sees it. The whole group exists to hold that reading in place.
--
-- Magister Sphinx, {4}{W}{U}{B} Artifact Creature -- Sphinx 5/5 with flying,
-- "When this creature enters, target player's life total becomes 10." The
-- literal, so the SAME card is a gain at one seat and a loss at another -- and
-- the first two cases below are one board differing in nothing but which seat
-- the trigger names. Arbiter of Knollridge's fold and Biorhythm's per-seat
-- count are data/scenarios/zone-change/cr-119-5-*.json.
--
-- Three seats at 4, 27 and 13 -- distinct, and one above 10 and two below, so the
-- Sphinx cases tell a gain from a loss. Only the CR 119.9 case changes a
-- starting total, moving carol to 10 so that the seat the trigger names is
-- already there.
--
-- The watchers are what make the claim about EVENTS rather than about totals.
-- Ajani's Pridemate ("whenever you gain life, put a +1/+1 counter on this
-- creature") is on the board for the gain side and Mindcrank ("whenever an
-- opponent loses life, that player mills that many cards") for the loss side, so
-- every case asserts which of the two fired -- and a set that wrote Player.life
-- directly would leave both silent while every life total still came out right.
setLifeTotalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
setLifeTotalSpec s registry =
  let -- Cast, then settle-and-resolve until the stack runs dry: the spell, then
      -- CR 603.6a's entry trigger, then whatever the life change itself
      -- triggered. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears TurnRegisters.events out from under lifeGains and lifeLosses. Six
      -- passes is more than the deepest case needs, and settling or resolving an
      -- empty stack is a no-op.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- Pinned, not searched: the trigger's target slot takes `who` and nothing
      -- else, so a mutation cannot be repaired by an answerer that goes looking
      -- for a legal option. Three players and a count of one, so there is a real
      -- choice to pin.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer who) sets
        _ -> S.identityAnswer p
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- The three seats, alice holding the mana and both watchers, and every
      -- library stocked so Mindcrank has something to mill and CR 104.3c never
      -- fires. `lands` is the mana base the spell needs; everything else is
      -- shared by all four cases.
      setBoard lands pridemate mindcrank filler spell aliceLife bobLife carolLife =
        let withLands = List.foldl' (\board (printing, n) -> S.landsFor printing S.alice n board) S.threePlayerGame lands
            (aliceMate, withAliceMate) = S.addPermanent pridemate S.alice withLands
            (bobMate, withBobMate) = S.addPermanent pridemate S.bob withAliceMate
            (_, withCrank) = S.addPermanent mindcrank S.alice withBobMate
            stocked = List.foldl' (\board pid -> stockLibrary filler pid 30 board) withCrank [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players stocked)))}
            (gs, spellId) = S.handOne spell lifed
         in (aliceMate, bobMate, spellId, gs)
      sphinxBoard aliceLife bobLife carolLife = do
        plains <- S.printingOf s registry "Plains"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sphinx <- S.printingOf s registry "Magister Sphinx"
        pure (setBoard [(plains, 5), (island, 1), (swamp, 1)] pridemate mindcrank piker sphinx aliceLife bobLife carolLife)
   in Spec.describe s "SetLifeTotal" $ do
        -- The gain direction. alice is BELOW 10, so reaching it is a gain of 6 --
        -- and her Pridemate sees it, which is the whole CR 119.5 claim.
        Spec.it s "CR 119.5 Magister Sphinx sets a total UPWARD, and that is a life gain" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.alice) spellId gs
          Spec.assertEqWith s "alice reached 10" (S.lifeOf S.alice after) (Just 10)
          Spec.assertEqWith s "bob, untargeted, keeps his 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly 6" (lifeGains after) [(S.alice, 6)]
          Spec.assertEqWith s "and as no loss at all" (lifeLosses after) []
          Spec.assertEqWith s "alice's Pridemate saw the gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: it was not his life" (countersOn bobMate after) (Just 0)
          Spec.assertEqWith s "and Mindcrank stayed silent: nobody lost life" (graveyardSize S.bob after) 0
        -- The control twin, differing in ONE thing: the seat the trigger names.
        -- bob is ABOVE 10, so the identical card is a LOSS of 17 -- Mindcrank
        -- fires and the Pridemate does not, which is the pair that tells a set
        -- from a gain.
        Spec.it s "CR 119.5 the control: the same card set DOWNWARD is a life loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.bob) spellId gs
          Spec.assertEqWith s "bob came down to 10" (S.lifeOf S.bob after) (Just 10)
          Spec.assertEqWith s "alice, untargeted, keeps her 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a loss of exactly 17" (lifeLosses after) [(S.bob, 17)]
          Spec.assertEqWith s "and as no gain at all" (lifeGains after) []
          Spec.assertEqWith s "Mindcrank milled bob for exactly 17" (graveyardSize S.bob after) 17
          Spec.assertEqWith s "alice's Pridemate stayed silent" (countersOn aliceMate after) (Just 0)
          Spec.assertEqWith s "and so did bob's: losing life is not gaining it" (countersOn bobMate after) (Just 0)
        -- CR 119.9's own last sentence, on the set: carol is ALREADY at 10, so
        -- the necessary amount is 0, no life event occurs, and neither watcher
        -- may fire. The Sphinx really entered, so a spell that did nothing cannot
        -- pass this by leaving the board alone.
        Spec.it s "CR 119.9 setting a total to the number it already holds is neither a gain nor a loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 10
          let after = castAndTrigger (aimedAt S.carol) spellId gs
          Spec.assertEqWith s "carol is still at 10" (S.lifeOf S.carol after) (Just 10)
          Spec.assertEqWith s "no gain" (lifeGains after) []
          Spec.assertEqWith s "no loss" (lifeLosses after) []
          Spec.assertEqWith s "no Pridemate counter anywhere" (fmap (\oid -> countersOn oid after) [aliceMate, bobMate]) [Just 0, Just 0]
          Spec.assertEqWith s "and Mindcrank milled nobody" (graveyardSize S.carol after) 0
          Spec.assertEqWith s "and the Sphinx really entered, so a spell that never resolved cannot pass this" (Set.size (GameState.battlefield after)) (Set.size (GameState.battlefield gs) + 1)

-- Reverse the Sands, {6}{W}{W} Sorcery: "Redistribute any number of players'
-- life totals. (Each of those players gets one life total back.)" CR 119.7 and
-- CR 119.8 name the action; every seat's new total is CR 119.5's gain or loss,
-- which setLifeTotalSpec above holds in place for one recipient. The choice is
-- the resolving controller's (CR 608.2c-d, and the card's own ruling "you
-- choose which player gets which life total when the spell resolves"). The
-- permutations a game can answer are data/scenarios/zone-change/; what stays
-- here is an answer the scenario runner refuses before the engine sees it, and
-- the offer and the elision, driven through Resolve.applyEffect directly.
redistributeLifeTotalsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
redistributeLifeTotalsSpec s registry =
  let -- setLifeTotalSpec's driver: cast, then settle-and-resolve until the stack
      -- runs dry, so the spell and everything its life changes triggered all
      -- resolve. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears TurnRegisters.events out from under lifeGains and lifeLosses.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- PINNED, not searched: the answer is exactly these pairs whatever the
      -- prompt offers, so a mutation to the engine's own handling cannot be
      -- repaired by an answerer that goes hunting for a legal permutation.
      assigning :: [(PlayerId.PlayerId, PlayerId.PlayerId)] -> Prompt.Prompt r -> r
      assigning pairs p = case p of
        Prompt.ChooseRedistribution {} -> Map.fromList pairs
        _ -> S.identityAnswer p
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- alice holds the mana and casts; every seat has a Pridemate, bob has the
      -- Mindcrank, and every library is stocked deep enough that no mill
      -- reaches CR 104.3c.
      sandsBoard = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sands <- S.printingOf s registry "Reverse the Sands"
        let withLands = S.landsFor plains S.alice 8 S.threePlayerGame
            (_, g1) = S.addPermanent pridemate S.alice withLands
            (_, g2) = S.addPermanent pridemate S.bob g1
            (_, g3) = S.addPermanent pridemate S.carol g2
            (_, g4) = S.addPermanent mindcrank S.bob g3
            stocked = List.foldl' (\board pid -> stockLibrary piker pid 40 board) g4 [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players stocked)))}
            (gs, spellId) = S.handOne sands lifed
        pure (spellId, gs)
      -- The board a refused answer is checked against: nothing moved, and
      -- nothing was logged.
      assertUntouched after = do
        Spec.assertEqWith s "alice keeps her 27" (S.lifeOf S.alice after) (Just 27)
        Spec.assertEqWith s "bob keeps his 4" (S.lifeOf S.bob after) (Just 4)
        Spec.assertEqWith s "carol keeps her 13" (S.lifeOf S.carol after) (Just 13)
        Spec.assertEqWith s "no gain" (lifeGains after) []
        Spec.assertEqWith s "no loss" (lifeLosses after) []
        -- Load-bearing: the spent sorcery and nothing else is in alice's
        -- graveyard, so the spell really RESOLVED and no Mindcrank mill followed.
        -- Without it the case below would pass just as well on a spell that
        -- fizzled.
        Spec.assertEqWith s "the spell resolved, and milled nobody doing it" (graveyardSize S.alice after) 1
   in Spec.describe s "RedistributeLifeTotals" $ do
        -- #222, an outsider: dave is not in this game. This answer IS a
        -- permutation -- its keys and its values are the same two seats -- so
        -- only the candidate check refuses it. The scenario runner refuses an
        -- answer outside the offer before the engine sees it, so this stays here.
        Spec.it s "#222 an answer naming a player who is not in the game is refused" $ do
          (spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.dave), (S.dave, S.alice)]) spellId gs)
        -- CR 102.1: the offer is the players IN the game, not the keys of
        -- GameState.players, which keep a departed seat's row. Driven through
        -- Resolve.applyEffect rather than a cast, the narrowest path that raises
        -- the prompt at all.
        Spec.it s "CR 102.1 every player in the game is offered, beside the total they hold, and a departed seat is not" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, g0) = S.addPermanent piker S.alice S.threePlayerGame
              at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
              gs = g0 {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players g0)))}
              recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Integer)] r
              recording p = case p of
                Prompt.ChooseRedistribution _ _ offered -> do
                  State.put offered
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              offerOf g = List.sort (State.execState (Engine.runGame recording g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) [])
              gone = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.carol)
          Spec.assertEqWith s "all three seats, each beside its own total" (offerOf gs) [(S.alice, 27), (S.bob, 4), (S.carol, 13)]
          Spec.assertEqWith s "carol conceded, so she is no longer a candidate" (offerOf gone) [(S.alice, 27), (S.bob, 4)]
        -- Where the rules leave nothing to ask, do not ask. One candidate admits
        -- only the identity and no candidate not even that, so both are the same
        -- assignment however they are answered; two candidates is a real choice.
        Spec.it s "one remaining player leaves only the identity, so no prompt is raised" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, gs) = S.addPermanent piker S.alice S.threePlayerGame
              countingAnswer :: Prompt.Prompt r -> State.State Int r
              countingAnswer p = case p of
                Prompt.ChooseRedistribution {} -> do
                  State.modify (+ 1)
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) 0
              leaves pid g = S.runPure S.identityAnswer g (Departure.leaveGame Departure.Type.Conceded pid)
              two = leaves S.carol gs
              one = leaves S.bob two
          Spec.assertEqWith s "three seats: a real decision" (asks gs) 1
          Spec.assertEqWith s "two seats: still a real decision, the swap being legal" (asks two) 1
          Spec.assertEqWith s "one seat: only the identity, so nothing to ask" (asks one) 0

-- Randomness under CR 400.7's zone change: Elkin Lair {3}{R} World
-- Enchantment (Homelands; Oracle text checked against api.scryfall.com,
-- 2026-09-18) -- "At the beginning of each player's upkeep, that player exiles a
-- card at random from their hand. The player may play that card this turn. At
-- the beginning of the next end step, if the player hasn't played the card, they
-- put it into their graveyard."
--
-- The pool's only card moving a card at random OUT of a hand (Scryfall
-- o:/(exiles?|puts?|returns?|shuffles?) .{0,20}cards? at random from (your|their|his or her|that player.s|target player.s|each player.s) hand/,
-- 2026-09-11, one hit), so Effect.MoveToZone's gather asking randomCards
-- is what this card is here to exercise.
--
-- TWO SEATS, and ALICE controls the enchantment while BOB takes the upkeep:
-- "that player" and "the resolving controller" are the same seat on a one-seat
-- board, and alice holds a card of her own so a gather reading the controller's
-- hand exiles the wrong one rather than nothing.
--
-- THREE distinct printings in bob's hand, and the PAIR of legs is the proof: "at
-- random" is not a property of one outcome, since the engine does not roll, so
-- the two legs differ in the index the interpreter answered with and in nothing
-- else. Exiling the head of the hand unasked passes the second and fails the
-- first.
elkinLairSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
elkinLairSpec s registry =
  let -- S.addHandCard puts its card at the FRONT of the hand, so the list is
      -- stocked in reverse and index 0 is the first card named.
      board lair mine theirs =
        let (_, withLair) = S.addPermanent lair S.alice (Setup.emptyGame S.bothPlayers)
            withAlices = List.foldl' (\g q -> snd (S.addHandCard q S.alice g)) withLair (reverse mine)
         in List.foldl' (\g q -> snd (S.addHandCard q S.bob g)) withAlices (reverse theirs)
      -- BOB's upkeep, stamped and recorded -- the half @Relative AnyPlayer@ buys,
      -- since under @Relative You@ the trigger would not fire here at all.
      runBobsUpkeep :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      runBobsUpkeep answer gs = S.runPure answer (bobsStepBegins (Phase.Beginning BeginningStep.Upkeep) answer gs) Engine.priorityLoop
      -- A step of bob's begun and its triggers put on the stack, none resolved.
      bobsStepBegins :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      bobsStepBegins step answer gs =
        let began =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan step S.bob))
                ( gs
                    { GameState.turn =
                        (GameState.turn gs)
                          { TurnRegisters.phase = step,
                            TurnRegisters.activePlayer = S.bob
                          }
                    }
                )
         in S.runPure answer began Engine.settleForPriority
      endStep = Phase.Ending EndingStep.EndStep
      -- Pinned by INDEX into the offer rather than read off the prompt's fields:
      -- an answerer that hunted for "a legal card" would go on answering legally
      -- after a mutation broke which card the engine honours.
      rolling :: Int -> Prompt.Prompt r -> r
      rolling i p = case p of
        Prompt.RandomObject offered -> case List.drop (min i (length (NonEmpty.toList offered) - 1)) (NonEmpty.toList offered) of
          h : _ -> h
          [] -> NonEmpty.head offered
        _ -> S.identityAnswer p
      named n = Just (CardName.MkCardName (Text.pack n))
      bobsCards = ["Goblin Piker", "Bog Wraith", "Bird Maiden"]
   in Spec.describe s "ElkinLair" $ do
        -- The proving case.
        Spec.it s "CR 400.7 the card exiled is the one randomness named, not the first in hand" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          bolt <- S.printingOf s registry "Lightning Bolt"
          ps <- traverse (S.printingOf s registry) bobsCards
          let after = runBobsUpkeep (rolling 2) (board lair [bolt] ps)
          Spec.assertEqWith s "the LAST card of bob's hand is in exile" (namesIn Zone.Exile S.bob after) [named "Bird Maiden"]
          Spec.assertEqWith s "and the two randomness passed over stay in hand" (namesIn Zone.Hand S.bob after) [named "Goblin Piker", named "Bog Wraith"]
          Spec.assertEqWith s "CR 603.2's \"that player\" is bob: alice's own card is untouched" (namesIn Zone.Hand S.alice after) [named "Lightning Bolt"]
          Spec.assertEqWith s "and the trigger resolved" (length (TurnRegisters.stack (GameState.turn after))) 0
        -- The other half of the pair: the same board, the same everything, one
        -- different answer.
        Spec.it s "CR 400.7 the same board with a different roll exiles a different card" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          bolt <- S.printingOf s registry "Lightning Bolt"
          ps <- traverse (S.printingOf s registry) bobsCards
          let after = runBobsUpkeep (rolling 0) (board lair [bolt] ps)
          Spec.assertEqWith s "the FIRST card this time" (namesIn Zone.Exile S.bob after) [named "Goblin Piker"]
          Spec.assertEqWith s "and the other two stay in hand" (namesIn Zone.Hand S.bob after) [named "Bog Wraith", named "Bird Maiden"]
        -- The THIRD clause: "at the beginning of the next end step, if the player
        -- hasn't played the card, they put it into their graveyard". Three boards
        -- differing only in what bob did with the card between: nothing, cast it
        -- (CR 601.2a), or played it as a land (CR 305.1). Two Mountains each, so
        -- the unplayed Goblin Piker was affordable and stayed unplayed.
        Spec.it s "CR 603.4 an unplayed card goes to the graveyard at the next end step" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let withMana g = snd (S.addPermanent mountain S.bob (snd (S.addPermanent mountain S.bob g)))
              upkept = runBobsUpkeep (rolling 0) (withMana (board lair [] [piker]))
              after = S.runPure S.identityAnswer (bobsStepBegins endStep S.identityAnswer upkept) Engine.priorityLoop
          Spec.assertEqWith s "the Goblin Piker bob never played is in his graveyard" (namesIn Zone.Graveyard S.bob after) [named "Goblin Piker"]
          Spec.assertEqWith s "and no longer in exile" (namesIn Zone.Exile S.bob upkept, namesIn Zone.Exile S.bob after) ([named "Goblin Piker"], [])

-- "Until the beginning of your next upkeep" (Duration.UntilNextStepOf).
-- Oracle text checked against api.scryfall.com, 2026-09-26:
--
-- Grinning Totem {4} Artifact -- "{2}, {T}, Sacrifice this artifact: Search
-- target opponent's library for a card and exile it. Then that player
-- shuffles. Until the beginning of your next upkeep, you may play that card. At
-- the beginning of your next upkeep, if you haven't played it, put it into its
-- owner's graveyard."
--
-- Elkin Bottle {3} Artifact -- "{3}, {T}: Exile the top card of your library.
-- Until the beginning of your next upkeep, you may play that card."
--
-- The answerer casts the exiled Lightning Bolt the moment alice is offered it,
-- so what the permission allows is read off bob's life.
nextUpkeepSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
nextUpkeepSpec s registry =
  let -- alice in her main phase with priority, the rest of her turn scheduled.
      mainPhase gs =
        gs
          { GameState.turn =
              (GameState.turn gs)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.remaining = S.phasesAfter Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      stock printing pid g = List.foldl' (\h _ -> snd (S.addLibraryCard printing pid h)) g [1 :: Int .. 4]
      -- Whole steps, stopping BEFORE the first state `stop` holds of.
      runUntil :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState -> Bool) -> GameState.GameState -> GameState.GameState
      runUntil answer stop =
        let go n g =
              if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || stop g
                then g
                else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
         in go 64
      -- The rest of alice's turn and all of bob's, passing throughout.
      untilAlicesNextTurn g0 = runUntil S.identityAnswer (\g -> TurnRegisters.activePlayer (GameState.turn g) == S.alice && TurnRegisters.turnNumber (GameState.turn g) > TurnRegisters.turnNumber (GameState.turn g0)) g0
      -- Then alice's turn up to and including the step `target`.
      through :: (forall r. Prompt.Prompt r -> r) -> Phase.Phase -> GameState.GameState -> GameState.GameState
      through answer target g = snd (Engine.runGamePure answer (runUntil answer (\h -> TurnRegisters.phase (GameState.turn h) == target) g) Engine.runStep)
      -- Casts `boltId` at bob whenever alice is offered it, and passes otherwise.
      castingBolt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      castingBolt boltId p = case p of
        Prompt.ChooseAction _ _ actions -> Maybe.fromMaybe A.Pass (List.find (\a -> case a of A.Cast oid _ _ -> oid == boltId; _ -> False) actions)
        _ -> atBobAnswer p
      activated source board = case Activatable.abilitiesFor source board of
        [ability] -> Just (S.runPure atBobAnswer (S.runPure atBobAnswer board (Activate.activateAbility S.alice source ability)) Stack.resolveTop)
        _ -> Nothing
   in Spec.describe s "UntilNextStepOf" $ do
        -- CR 500.11: Eon Hub skips every upkeep, so the duration outlasts the
        -- start of alice's next turn and she may still cast the card then.
        Spec.it s "CR 611.2a Elkin Bottle's card stays castable into alice's next turn when Eon Hub skips her upkeep" $ do
          ps <- traverse (S.printingOf s registry) ["Elkin Bottle", "Eon Hub", "Mountain", "Island", "Lightning Bolt"]
          case ps of
            [bottle, hub, mountain, island, bolt] -> do
              let (bottleId, withBottle) = S.addPermanent bottle S.alice (stock island S.bob (stock island S.alice (Setup.emptyGame S.bothPlayers)))
                  withHub = snd (S.addPermanent hub S.bob withBottle)
                  withLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) withHub [1 :: Int .. 3]
                  (_, board) = S.addLibraryCard bolt S.alice withLands
              case activated bottleId (mainPhase board) of
                Just exiled -> case Game.zoneMembers Zone.Exile S.alice exiled of
                  [boltId] -> do
                    let drawn = through (castingBolt boltId) (Phase.Beginning BeginningStep.DrawStep) (untilAlicesNextTurn exiled)
                    Spec.assertEqWith s "alice cast the Lightning Bolt on her next turn" (S.lifeOf S.bob drawn) (Just 17)
                  _ -> Spec.assertFailure s "Elkin Bottle should exile exactly one card"
                Nothing -> Spec.assertFailure s "one activated ability"
            _ -> Spec.assertFailure s "five printings"

-- CR 701.9a's "discarded this way" over Discard.These, the random discard's
-- look-back. Cragganwick Cremator {2}{R}{R} Creature -- Giant Shaman 5/4 -- "When
-- this creature enters, discard a card at random. If you discard a creature card
-- this way, this creature deals damage equal to that card's power to target
-- player or planeswalker." (checked against api.scryfall.com, 2026-09-28).
-- Aether Rift's cases are data/scenarios/zone-change/cr-701-9*-aether-rift-*.json
-- and cr-118-12a-carol-pays-*.json.
discardedThisWaySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
discardedThisWaySpec s registry = Spec.describe s "CR 701.9a discarded this way" $ do
  let named = Just . CardName.MkCardName . Text.pack
  -- Cragganwick Cremator: two boards differing in the one card alice holds once
  -- the Cremator is cast. The Piker's power, 2, is not the Cremator's own 5.
  let cremate held = do
        mountain <- S.printingOf s registry "Mountain"
        cremator <- S.printingOf s registry "Cragganwick Cremator"
        heldCard <- S.printingOf s registry held
        let lands = S.landsFor mountain S.alice 4 S.threePlayerGame
            (withCremator, crematorId) = S.handOne cremator lands
            (_, ready) = S.addHandCard heldCard S.alice withCremator
            cast = S.runPure atBobAnswer ready (S.cast S.alice crematorId)
            resolved = S.runPure atBobAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
        pure (resolved, S.runPure atBobAnswer resolved Stack.resolveTop)
  Spec.it s "CR 701.9a Cragganwick Cremator deals the discarded creature card's power" $ do
    (onStack, after) <- cremate "Goblin Piker"
    Spec.assertBool s (not (null (TurnRegisters.stack (GameState.turn onStack)))) "the enters trigger really reached the stack"
    Spec.assertEqWith s "bob took the Piker's 2" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "the Piker is in alice's graveyard" (namesIn Zone.Graveyard S.alice after) [named "Goblin Piker"]
  Spec.it s "CR 701.9a Cragganwick Cremator deals nothing for a noncreature card" $ do
    (onStack, after) <- cremate "Lightning Bolt"
    Spec.assertBool s (not (null (TurnRegisters.stack (GameState.turn onStack)))) "the enters trigger really reached the stack"
    Spec.assertEqWith s "bob took nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "the Bolt is in alice's graveyard" (namesIn Zone.Graveyard S.alice after) [named "Lightning Bolt"]

-- CR 107.1c / 701.9b: "discard any number of" matching cards -- the discarding
-- player picks the subset, none included, out of the matching cards in their own
-- hand (Parun.Types.Discard's AnyNumber arm), and what moved is bound for the
-- rest of the resolution to count.
--
-- A test-local answerer rather than the Board harness, whose vocabulary has no
-- resolution-time subset choice: each leg pins the subset by position in the
-- offer, so a fallback that happened to agree cannot answer for the engine.
anyNumberDiscardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
anyNumberDiscardSpec s registry =
  let -- Cast the card and run the priority loop out under one answerer, keeping
      -- every response so "never asked" and "asked, answered nothing" differ.
      runCast :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, [Response.Response])
      runCast answer (withSpell, spell) =
        let ((_, afterCast), castResponses) = Replay.record answer withSpell (S.cast S.alice spell)
            ((_, after), loopResponses) = Replay.record answer afterCast Engine.priorityLoop
         in (after, castResponses <> loopResponses)
      -- Accepts every printed "may" and discards the offered cards `pick` keeps.
      discarding :: ([ObjectId.ObjectId] -> [ObjectId.ObjectId]) -> (forall q. Prompt.Prompt q -> q) -> Prompt.Prompt r -> r
      discarding pick fallback p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseAnyNumber _ _ _ AnyNumberOf.ToDiscard offered _ _ -> Set.fromList (pick offered)
        _ -> fallback p
      -- The rest of alice's hand, added AFTER S.handOne, which replaces it.
      withHand fill (gs, spell) = (fill gs, spell)
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      onBattlefield name gs = List.find (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just name) (Set.toList (GameState.battlefield gs))
      -- Aims the reflexive trigger at the one creature named, FILTERING the offer.
      atWall :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      atWall wallId p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (\r -> Recipient.objectOf r == Just wallId) offered) sets
        _ -> S.identityAnswer p
      askedForTargets :: [Response.Response] -> Bool
      askedForTargets = any (\r -> case r of Response.ChoseTargets _ -> True; _ -> False)
      sortedNames zone pid gs = List.sort (namesIn zone pid gs)
   in Spec.describe s "CR 107.1c discard any number of cards" $ do
        -- Borborygmos and Fblthp {2}{G}{U}{R}: "whenever Borborygmos and Fblthp
        -- enters or attacks, draw a card, then you may discard any number of land
        -- cards. When you discard one or more cards this way, Borborygmos and
        -- Fblthp deals twice that much damage to target creature."
        --
        -- alice holds three Mountains and a Goblin Piker and draws a Piker, so the
        -- offer is the three lands alone; bob's Wall of Stone is a 0/8, alive
        -- under every amount below, so its marked damage IS the reading. Three
        -- legs over one board, differing only in the subset: all three lands, one,
        -- none. Twice three and twice one separate the count from a literal and
        -- from the Times; none is CR 603.12's "when you discard one or more"
        -- staying unarmed.
        let borborygmosBoard = do
              borborygmos <- S.printingOf s registry "Borborygmos and Fblthp"
              forest <- S.printingOf s registry "Forest"
              island <- S.printingOf s registry "Island"
              mountain <- S.printingOf s registry "Mountain"
              swamp <- S.printingOf s registry "Swamp"
              piker <- S.printingOf s registry "Goblin Piker"
              wall <- S.printingOf s registry "Wall of Stone"
              let lands = List.foldl' (\g land -> snd (S.addPermanent land S.alice g)) (Setup.emptyGame S.bothPlayers) [forest, island, mountain, swamp, swamp]
                  (wallId, withWall) = S.addPermanent wall S.bob lands
                  staged = withHand (handCards piker S.alice 1 . handCards mountain S.alice 3) (S.handOne borborygmos (stockLibrary piker S.alice 3 withWall))
              pure (wallId, staged, S.printingName mountain, S.printingName piker)
        Spec.it s "CR 603.12 Borborygmos and Fblthp deals twice the lands discarded to the target" $ do
          (wallId, staged, mountain, piker) <- borborygmosBoard
          let (after, _) = runCast (discarding id (atWall wallId)) staged
          Spec.assertEqWith s "the wall took twice three" (fmap Object.damage (Game.lookupObject wallId after)) (Just 6)
          Spec.assertEqWith s "every land in hand was discarded" (sortedNames Zone.Graveyard S.alice after) (replicate 3 (Just mountain))
          Spec.assertEqWith s "and the pikers were never offered" (sortedNames Zone.Hand S.alice after) (replicate 2 (Just piker))
        Spec.it s "CR 107.1c Borborygmos and Fblthp discards only the chosen subset" $ do
          (wallId, staged, mountain, piker) <- borborygmosBoard
          let (after, _) = runCast (discarding (take 1) (atWall wallId)) staged
          Spec.assertEqWith s "the wall took twice one" (fmap Object.damage (Game.lookupObject wallId after)) (Just 2)
          Spec.assertEqWith s "one land discarded" (sortedNames Zone.Graveyard S.alice after) [Just mountain]
          Spec.assertEqWith s "the other two kept" (sortedNames Zone.Hand S.alice after) [Just piker, Just piker, Just mountain, Just mountain]
        Spec.it s "CR 603.12 Borborygmos and Fblthp discarding none arms no reflexive trigger" $ do
          (wallId, staged, _, _) <- borborygmosBoard
          let (after, responses) = runCast (discarding (const []) (atWall wallId)) staged
          Spec.assertEqWith s "the wall took nothing" (fmap Object.damage (Game.lookupObject wallId after)) (Just 0)
          Spec.assertBool s (not (askedForTargets responses)) "and no reflexive trigger asked for a target"
          Spec.assertEqWith s "nothing discarded" (namesIn Zone.Graveyard S.alice after) []
          Spec.assertBool s (any (\r -> case r of Response.ChoseAnyNumberToDiscard _ -> True; _ -> False) responses) "though the choice was put to alice"
        -- Nantuko Cultivator {3}{G}: "when this creature enters, you may discard
        -- any number of land cards. Put that many +1/+1 counters on this creature
        -- and draw that many cards." Two of three lands, so "that many" is
        -- neither the hand nor the offer.
        Spec.it s "CR 107.1c Nantuko Cultivator counts and draws the lands discarded" $ do
          cultivator <- S.printingOf s registry "Nantuko Cultivator"
          forest <- S.printingOf s registry "Forest"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let lands = List.foldl' (\g land -> snd (S.addPermanent land S.alice g)) (Setup.emptyGame S.bothPlayers) [forest, swamp, swamp, swamp]
              staged = withHand (handCards mountain S.alice 3) (S.handOne cultivator (stockLibrary piker S.alice 3 lands))
              (after, _) = runCast (discarding (take 2) S.identityAnswer) staged
              cultivatorId = onBattlefield (S.printingName cultivator) after
          Spec.assertEqWith s "two counters" (cultivatorId >>= \oid -> countersOn oid after) (Just 2)
          Spec.assertEqWith s "two cards drawn beside the kept land" (sortedNames Zone.Hand S.alice after) [Just (S.printingName piker), Just (S.printingName piker), Just (S.printingName mountain)]
          Spec.assertEqWith s "two lands discarded" (namesIn Zone.Graveyard S.alice after) (replicate 2 (Just (S.printingName mountain)))
        -- Mind Maggots {3}{B}: "when this creature enters, discard any number of
        -- creature cards. For each card discarded this way, put two +1/+1
        -- counters on this creature." Every offered card taken, so the land in
        -- hand staying put is the filter's doing.
        Spec.it s "CR 107.1c Mind Maggots offers only creature cards and puts two counters per discard" $ do
          maggots <- S.printingOf s registry "Mind Maggots"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let staged = withHand (handCards mountain S.alice 1 . handCards piker S.alice 2) (S.handOne maggots (S.landsInPlay swamp 4))
              (after, _) = runCast (discarding id S.identityAnswer) staged
              maggotsId = onBattlefield (S.printingName maggots) after
          Spec.assertEqWith s "four counters" (maggotsId >>= \oid -> countersOn oid after) (Just 4)
          Spec.assertEqWith s "both creature cards discarded" (namesIn Zone.Graveyard S.alice after) (replicate 2 (Just (S.printingName piker)))
          Spec.assertEqWith s "the land kept" (namesIn Zone.Hand S.alice after) [Just (S.printingName mountain)]
        -- Flux {2}{U}: "each player discards any number of cards, then draws that
        -- many cards. Draw a card." Three seats discarding one, three and none, so
        -- "that many" is each drawer's own: the union of four read by every seat
        -- would give alice five, bob four and carol four.
        Spec.it s "CR 107.1c Flux draws each player the number THEY discarded" $ do
          flux <- S.printingOf s registry "Flux"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          sentry <- S.printingOf s registry "Ogre Sentry"
          rats <- S.printingOf s registry "Typhoid Rats"
          let stocked = List.foldl' (\g pid -> stockLibrary piker pid 6 g) (S.landsFor island S.alice 3 S.threePlayerGame) [S.alice, S.bob, S.carol]
              staged = withHand (handCards rats S.carol 2 . handCards sentry S.bob 4 . handCards piker S.alice 2) (S.handOne flux stocked)
              perSeat :: Prompt.Prompt r -> r
              perSeat p = case p of
                Prompt.ChooseAnyNumber _ victim _ AnyNumberOf.ToDiscard offered _ _
                  | victim == S.alice -> Set.fromList (take 1 offered)
                  | victim == S.bob -> Set.fromList (take 3 offered)
                  | otherwise -> Set.empty
                _ -> S.identityAnswer p
              (after, _) = runCast perSeat staged
              drew pid = 6 - length (Game.zoneMembers Zone.Library pid after)
          Spec.assertEqWith s "alice drew her one and Flux's one" (drew S.alice) 2
          Spec.assertEqWith s "bob drew his three" (drew S.bob) 3
          Spec.assertEqWith s "carol drew none" (drew S.carol) 0
          Spec.assertEqWith s "bob discarded three sentries" (namesIn Zone.Graveyard S.bob after) (replicate 3 (Just (S.printingName sentry)))
          Spec.assertEqWith s "carol kept both rats" (namesIn Zone.Hand S.carol after) (replicate 2 (Just (S.printingName rats)))
        -- Steal the Show {2}{R}: "choose one or both -- target player discards any
        -- number of cards, then draws that many cards; Steal the Show deals damage
        -- equal to the number of instant and sorcery cards in your graveyard to
        -- target creature or planeswalker." Both modes: bob discards two of three,
        -- and alice's graveyard holds three instants and sorceries beside a land,
        -- so the 0/8 wall's damage is neither the discard count nor the graveyard.
        Spec.it s "CR 700.2 Steal the Show's discard draws that many and its damage counts instants and sorceries" $ do
          steal <- S.printingOf s registry "Steal the Show"
          mountain <- S.printingOf s registry "Mountain"
          bolt <- S.printingOf s registry "Lightning Bolt"
          divination <- S.printingOf s registry "Divination"
          sentry <- S.printingOf s registry "Ogre Sentry"
          rats <- S.printingOf s registry "Typhoid Rats"
          wall <- S.printingOf s registry "Wall of Stone"
          let (wallId, withWall) = S.addPermanent wall S.bob (S.landsInPlay mountain 3)
              graveyard = List.foldl' (\g p -> snd (S.addGraveyardCard p S.alice g)) withWall [bolt, bolt, divination, mountain]
              staged = withHand (handCards sentry S.bob 3) (S.handOne steal (stockLibrary rats S.bob 4 graveyard))
              bothAtBob :: Prompt.Prompt r -> r
              bothAtBob p = case p of
                Prompt.ChooseModes _ _ _ offered _ -> Seq.fromList (Set.toList offered)
                Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (\r -> Recipient.playerOf r == Just S.bob || Recipient.objectOf r == Just wallId) offered) sets
                Prompt.ChooseAnyNumber _ _ _ AnyNumberOf.ToDiscard offered _ _ -> Set.fromList (take 2 offered)
                _ -> S.identityAnswer p
              (after, _) = runCast bothAtBob staged
          Spec.assertEqWith s "the wall took three" (fmap Object.damage (Game.lookupObject wallId after)) (Just 3)
          Spec.assertEqWith s "bob drew two" (length (Game.zoneMembers Zone.Library S.bob after)) 2
          Spec.assertEqWith s "bob holds his kept sentry and two drawn rats" (sortedNames Zone.Hand S.bob after) (List.sort [Just (S.printingName sentry), Just (S.printingName rats), Just (S.printingName rats)])

-- Random discards that need more than the discard itself, and a repeat loop.
-- Checked against api.scryfall.com, 2026-09-28:
--
-- Rowdy Crew {2}{R}{R} Creature -- Human Pirate 3/3, trample -- "When this
-- creature enters, draw three cards, then discard two cards at random. If two
-- cards that share a card type are discarded this way, put two +1/+1 counters on
-- this creature."
--
-- Rites of Initiation {R} Instant -- "Discard any number of cards at random.
-- Creatures you control get +1/+0 until end of turn for each card discarded this
-- way."
--
-- Kindle the Carnage {1}{R}{R} Sorcery -- "Discard a card at random. If you do,
-- Kindle the Carnage deals damage equal to that card's mana value to each
-- creature. You may repeat this process any number of times."
randomDiscardProcessSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
randomDiscardProcessSpec s registry = Spec.describe s "CR 701.9b random discards that share, announce or repeat" $ do
  -- Rites of Initiation off one Mountain. alice holds three other cards and
  -- controls a Goblin Piker (2/1); bob controls a Llanowar Elves (1/1), which
  -- "creatures you control" must not reach.
  let rites number = do
        mountain <- S.printingOf s registry "Mountain"
        ritesCard <- S.printingOf s registry "Rites of Initiation"
        piker <- S.printingOf s registry "Goblin Piker"
        elves <- S.printingOf s registry "Llanowar Elves"
        held <- traverse (S.printingOf s registry) ["Hill Giant", "Lightning Bolt", "Mountain"]
        let lands = S.landsFor mountain S.alice 1 S.threePlayerGame
            (pikerId, withPiker) = S.addPermanent piker S.alice lands
            (elvesId, withElves) = S.addPermanent elves S.bob withPiker
            (withRites, ritesId) = S.handOne ritesCard withElves
            stocked = List.foldl' (\gs p -> snd (S.addHandCard p S.alice gs)) withRites held
            named_ :: Natural
            named_ = number
            answer :: Prompt.Prompt r -> r
            answer p = case p of
              Prompt.ChooseNumber {} -> named_
              _ -> S.identityAnswer p
            cast = S.runPure answer stocked (S.cast S.alice ritesId)
            ((_, after), transcript) = Replay.record answer cast Stack.resolveTop
            numbers = filter (\r -> case r of Response.ChoseNumber _ -> True; _ -> False) transcript
        pure (fmap fst (S.powerToughnessOf pikerId after), fmap fst (S.powerToughnessOf elvesId after), S.handSize S.alice after, numbers)
  Spec.it s "CR 107.1c Rites of Initiation discards the two cards alice names and pumps her creature by two" $ do
    (pikerPower, elvesPower, handLeft, numbers) <- rites 2
    Spec.assertEqWith s "the Piker gets +2/+0" pikerPower (Just 4)
    Spec.assertEqWith s "one card left in alice's hand" handLeft 1
    Spec.assertEqWith s "bob's Elves are untouched" elvesPower (Just 1)
    Spec.assertEqWith s "CR 608.2d alice named the number" numbers [Response.ChoseNumber 2]
  -- Five named, three held: CR 609.3 discards the three, and the bonus counts
  -- the cards discarded rather than the number named.
  Spec.it s "CR 609.3 Rites of Initiation counts the cards discarded, not the number named" $ do
    (pikerPower, _, handLeft, _) <- rites 5
    Spec.assertEqWith s "the Piker gets +3/+0" pikerPower (Just 5)
    Spec.assertEqWith s "alice's hand is empty" handLeft 0
  Spec.it s "CR 107.1c Rites of Initiation with zero named discards nothing" $ do
    (pikerPower, _, handLeft, _) <- rites 0
    Spec.assertEqWith s "the Piker is unchanged" pikerPower (Just 2)
    Spec.assertEqWith s "alice keeps all three cards" handLeft 3
  -- Kindle the Carnage off three Mountains. alice holds a Goblin Piker (mana
  -- value 2) and a Hill Giant (4), and randomness takes the first card it is
  -- offered, the Piker; bob controls a Platinum Emperion (8/8), which survives all of it.
  -- `runs` is how many times alice runs the process before declining.
  let kindle runs = do
        mountain <- S.printingOf s registry "Mountain"
        kindleCard <- S.printingOf s registry "Kindle the Carnage"
        emperion <- S.printingOf s registry "Platinum Emperion"
        held <- traverse (S.printingOf s registry) ["Hill Giant", "Goblin Piker"]
        let lands = S.landsFor mountain S.alice 3 S.threePlayerGame
            (emperionId, withEmperion) = S.addPermanent emperion S.bob lands
            (withKindle, kindleId) = S.handOne kindleCard withEmperion
            stocked = List.foldl' (\gs p -> snd (S.addHandCard p S.alice gs)) withKindle held
            -- Pinned by the run count the prompt carries, so each ask is told
            -- apart.
            limit :: Natural
            limit = runs
            answer :: Prompt.Prompt r -> r
            answer p = case p of
              Prompt.ChooseRepeat _ _ _ done -> if done < limit then OptionalDecision.Exercises else OptionalDecision.Declines
              _ -> S.identityAnswer p
            cast = S.runPure answer stocked (S.cast S.alice kindleId)
            ((_, after), transcript) = Replay.record answer cast Stack.resolveTop
            repeats = filter (\r -> case r of Response.ChoseRepeat _ -> True; _ -> False) transcript
        pure (S.damageOf emperionId after, S.handSize S.alice after, length repeats)
  Spec.it s "CR 608.2d Kindle the Carnage run once deals the one discarded card's mana value" $ do
    (damage, handLeft, asked) <- kindle 1
    Spec.assertEqWith s "the Emperion took the Piker's 2" damage (Just 2)
    Spec.assertEqWith s "the Hill Giant stays in alice's hand" handLeft 1
    Spec.assertEqWith s "alice was asked once" asked 1
  Spec.it s "CR 608.2d Kindle the Carnage repeated deals each discarded card's mana value" $ do
    (damage, handLeft, asked) <- kindle 2
    Spec.assertEqWith s "the Emperion took 2 then 4" damage (Just 6)
    Spec.assertEqWith s "alice's hand is empty" handLeft 0
    Spec.assertEqWith s "alice was asked after each run" asked 2
  -- A third run finds an empty hand: it discards nothing, so it deals nothing,
  -- rather than the Hill Giant's 4 again.
  Spec.it s "CR 608.2d a Kindle the Carnage run over an empty hand deals nothing" $ do
    (damage, handLeft, asked) <- kindle 3
    Spec.assertEqWith s "the Emperion took only 2 then 4" damage (Just 6)
    Spec.assertEqWith s "alice's hand is empty" handLeft 0
    Spec.assertEqWith s "alice was asked after each of three runs" asked 3

-- Ad Nauseam {3}{B}{B} Instant -- "Reveal the top card of your library and put
-- that card into your hand. You lose life equal to its mana value. You may
-- repeat this process any number of times."
--
-- Trade Secrets {1}{U}{U} Sorcery -- "Target opponent draws two cards, then you
-- draw up to four cards. That opponent may repeat this process as many times as
-- they choose."
repeatProcessSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
repeatProcessSpec s registry = Spec.describe s "CR 608.2d processes a player may repeat" $ do
  -- Trade Secrets off three Islands, aimed at carol rather than bob, so "that
  -- opponent" is not the one APNAP order reaches first. Whoever is asked to
  -- repeat answers by seat: carol takes a second run and declines a third, and
  -- anyone else declines outright. alice names six each time, past the four the
  -- card allows.
  Spec.it s "CR 608.2d Trade Secrets' target opponent chooses to repeat, and alice draws at most four" $ do
    island <- S.printingOf s registry "Island"
    spell <- S.printingOf s registry "Trade Secrets"
    filler <- S.printingOf s registry "Mountain"
    let lands = S.landsFor island S.alice 3 S.threePlayerGame
        (withSpell, spellId) = S.handOne spell lands
        stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard filler pid g)) gs [1 :: Int .. 12]
        stocked = stock S.carol (stock S.alice withSpell)
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.carol) . snd) sets
          Prompt.ChooseRepeat _ pid _ done -> if pid == S.carol && done < 2 then OptionalDecision.Exercises else OptionalDecision.Declines
          Prompt.ChooseNumber {} -> 6
          _ -> S.identityAnswer p
        cast = S.runPure answer stocked (S.cast S.alice spellId)
        ((_, after), transcript) = Replay.record answer cast Stack.resolveTop
        numbers = filter (\r -> case r of Response.ChoseNumber _ -> True; _ -> False) transcript
    Spec.assertEqWith s "carol drew two cards in each of two runs" (S.handSize S.carol after) 4
    Spec.assertEqWith s "alice drew four, not six, in each of two runs" (S.handSize S.alice after) 8
    Spec.assertEqWith s "bob drew nothing" (S.handSize S.bob after) 0
    Spec.assertEqWith s "CR 608.2d alice named each run's number" numbers [Response.ChoseNumber 6, Response.ChoseNumber 6]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Resolve" $ do
  zoneChangeSpec s registry
  discardedThisWaySpec s registry
  randomDiscardProcessSpec s registry
  repeatProcessSpec s registry
  anyNumberDiscardSpec s registry
  elkinLairSpec s registry
  nextUpkeepSpec s registry
  drawCardSpec s registry
  loseLifeSpec s registry
  exchangeValuesSpec s registry
  setLifeTotalSpec s registry
  redistributeLifeTotalsSpec s registry
