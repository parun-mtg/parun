{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 705 FLIPPING A COIN -- Parun.Types.FlipCoin (both of its tallies),
-- Parun.Types.CoinReading, Parun.Engine.Coin, Parun.Engine.Event.flipOneCoin and
-- the CR 614.1a replacement over it (Parun.Types.InstructionR, Krark's Thumb),
-- Parun.Types.StatedFlip, Effect.FlipCoin's arm in Parun.Engine.Resolve, and the
-- Parun.Types.Prompt / Parun.Types.Response pairs the call and the flip are
-- externalised through. The transcript legs live in
-- Parun.ReplaySpec with the other randomness prompts.
--
-- NOT the flip's GameEvent, which Parun.CardTriggerSpec's FlipsCoin group
-- proves with Karplusan Minotaur: what a trigger sees is a rule 603 question
-- rather than a rule 705 one, and Winter Sky watches nothing.
--
-- NOT rule 705.2's first sentence as an ENTRY REPLACEMENT (Molten Sentry), which
-- is proved in Parun.ReplacementSpec beside the rest of the CR 614.1c family. It
-- appears here twice: in the CR 705.3 group, because rule 705.3 is the one rule
-- both roads have to obey and Parun.Engine.Event.flipOneCoin is the one road they
-- share, and
-- as an EFFECT in the face-reading group below (Odds), which is the same
-- sentence on the other road.
--
-- Its own module rather than a group in Parun.DiceSpec: CR 705 and CR 706 are
-- different rules sharing no type, no prompt and no effect. A coin has no size,
-- no results table and no modifier; a die has no call and no winner.
--
-- Winter Sky's CR 705.2 legs -- the whole (face, call) truth table, and who
-- calls and when -- are data/scenarios/coin/'s; the fixtures below are
-- documented where they are built.
module Parun.CoinSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Engine.Cast as Cast
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.TurnRegisters as TurnRegisters

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Coin" $ do
  faceReadingSpec s registry
  missesSpec s registry
  chaosSpec s registry

-- The `i`th element, or a fallback past the end. The fallbacks are the answers
-- that LOSE a flip -- a tails coin against a call of heads -- so a run that
-- flipped more coins than the case pinned cannot inflate the tally.
atIndex :: [CoinFace.CoinFace] -> CoinFace.CoinFace -> Int -> CoinFace.CoinFace
atIndex xs fallback i = Maybe.fromMaybe fallback (Maybe.listToMaybe (drop i xs))

-- CR 705.2's FIRST sentence as an effect rather than an entry replacement: Odds
-- (the left half of Odds // Ends, {U}{R} Instant, "Flip a coin. If it comes up
-- heads, counter target instant or sorcery spell. If it comes up tails, copy
-- that spell and you may choose new targets for the copy"; name, cost, type
-- line and Oracle text read off the card_faces array at api.scryfall.com
-- 2026-09-01). Its two branches read the FACE, and no player wins or loses.
--
-- Lightning Bolt is the spell it answers, so the two branches are three damage
-- apart in each direction: countered leaves bob at 20, copied takes him to 14.
-- Two seats are enough -- the copy keeps the Bolt's target, so no third seat has
-- a role.
--
-- Three lands, exactly both costs: two Mountains and an Island.
oddsBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
oddsBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  bolt <- S.printingOf s registry "Lightning Bolt"
  odds <- S.printingOf s registry "Odds"
  let lands = S.landsFor island S.alice 1 (S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers))
      (withBolt, boltId) = S.handOne bolt lands
      (oddsId, board) = S.addHandCard odds S.alice withBolt
  pure (boltId, oddsId, board)

-- Answer a ChooseTargets by FILTERING the offered set down to one recipient,
-- never by building one: CR 608.2b re-reads what was chosen, and a hand-built
-- Recipient.ToObject of the same permanent is a different recipient that the
-- re-read drops with no error.
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (== recipient) offered) sets
  _ -> S.identityAnswer p

-- alice casts Lightning Bolt at bob and then -- CR 117.3c, still holding
-- priority -- Odds at the Bolt, and the whole stack resolves under a coin pinned
-- to `face`.
afterOdds :: CoinFace.CoinFace -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
afterOdds face (boltId, oddsId, board) =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.FlipCoin -> face
        -- CR 707.10c's re-target prompt, which the copy raises because Odds says
        -- "you may choose new targets for the copy". Pinned back onto bob rather
        -- than left to the fallback, so the copy deals its damage where the Bolt
        -- would have.
        Prompt.ChooseTargets {} -> pinTarget (Recipient.ToPlayer S.bob) p
        _ -> S.identityAnswer p
      cast1 = S.runPure (pinTarget (Recipient.ToPlayer S.bob)) board (Cast.castSpell S.manaPerformer S.alice boltId boltName Facing.FaceUp)
      drain n g = if n <= (0 :: Int) || null (TurnRegisters.stack (GameState.turn g)) then g else drain (n - 1) (S.runPure answer g Stack.resolveTop)
   in case TurnRegisters.stack (GameState.turn cast1) of
        [] -> cast1
        boltSpell : _ ->
          let cast2 = S.runPure (pinTarget (Recipient.ToObject boltSpell)) cast1 (Cast.castSpell S.manaPerformer S.alice oddsId oddsName Facing.FaceUp)
           in S.settleSba (drain 8 cast2)

-- How many CR 705.2 CALLS the whole run asked. Not readable off the board, so it
-- takes a State-counting answerer.
oddsCalls :: CoinFace.CoinFace -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState) -> Int
oddsCalls face (boltId, oddsId, board) =
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.CallCoin {} -> do
          State.modify' (+ 1)
          pure (S.identityAnswer p)
        Prompt.FlipCoin -> pure face
        _ -> pure (S.identityAnswer p)
      drain :: Int -> GameState.GameState -> State.State Int GameState.GameState
      drain n g =
        if n <= 0 || null (TurnRegisters.stack (GameState.turn g))
          then pure g
          else do
            (_, next) <- Engine.runGame counting g Stack.resolveTop
            drain (n - 1) next
      cast1 = S.runPure (pinTarget (Recipient.ToPlayer S.bob)) board (Cast.castSpell S.manaPerformer S.alice boltId boltName Facing.FaceUp)
   in case TurnRegisters.stack (GameState.turn cast1) of
        [] -> 0
        boltSpell : _ ->
          let cast2 = S.runPure (pinTarget (Recipient.ToObject boltSpell)) cast1 (Cast.castSpell S.manaPerformer S.alice oddsId oddsName Facing.FaceUp)
           in State.execState (drain 8 cast2) 0

boltName :: CardName.CardName
boltName = CardName.MkCardName (Text.pack "Lightning Bolt")

oddsName :: CardName.CardName
oddsName = CardName.MkCardName (Text.pack "Odds")

faceReadingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
faceReadingSpec s registry = Spec.describe s "FlipCoin read for its face (CR 705.2)" $ do
  Spec.it s "CR 705.2 an effect that cares only about the face reads the face" $ do
    board <- oddsBoard s registry
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation, and on the TAILS leg, which is the one leg the win/lose reading
    -- cannot reach the same answer on: Replay.defaultAnswer calls and flips
    -- heads, so a flip read as a WIN would counter the Bolt on both legs.
    Spec.assertEqWith
      s
      "CR 705.2: tails copies the Bolt, so bob takes it twice"
      (S.lifeOf S.bob (afterOdds CoinFace.Tails board))
      (Just 14)
    -- The pair, one thing different: the same board and the same answers with
    -- the coin the other way up.
    Spec.assertEqWith
      s
      "CR 705.2: heads counters the Bolt, so bob takes nothing"
      (S.lifeOf S.bob (afterOdds CoinFace.Heads board))
      (Just 20)
  Spec.it s "CR 705.2 no call is made for a flip nobody wins" $ do
    board <- oddsBoard s registry
    -- "No player wins or loses a coin flip for this kind of effect", so there is
    -- nothing to call and nothing for a CR 724 controller to usurp. Not readable
    -- off the board -- both legs above leave the same board whether or not a
    -- call was asked and thrown away.
    Spec.assertEqWith s "CR 705.2: Odds asks no call" (oddsCalls CoinFace.Tails board) 0

-- CR 705.2's OTHER tally: Mutalith Vortex Beast ({4}{U}{R} Creature -- Mutant
-- Beast 6/6, Trample, "Warp Vortex -- When this creature enters, flip a coin for
-- each opponent you have. For each flip you win, draw a card. For each flip you
-- lose, this creature deals 3 damage to that player."; Oracle text via
-- api.scryfall.com 2026-09-02). Its ruling ties each flip to one opponent
-- ("deals 3 damage to the appropriate player for each lost flip"), so the card
-- is a ForEach over the opponents with a one-coin flip in the body, then a loop
-- apiece for the draws and the damage (CR 608.2e), each reading that opponent's
-- own flip (Parun.Types.Binding's perMember).
--
-- THREE SEATS, because the card ranges over opponents and two flips are what
-- separates the lost tally from the won one: with bob's flip lost and carol's
-- won, the losses reaching bob and NOT carol is a reading neither the wins
-- tally nor the coin count produces. Distinct life totals (20 and 16) so no
-- numeric coincidence can make the wrong seat read right. Two cards in alice's
-- library, since the won flip draws.
--
-- The Beast enters via S.entersWithTrigger rather than a cast: its six mana buy
-- nothing here, and the trigger placed off the hand-built enters event is the
-- same ability a cast would place.
mutalithBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
mutalithBoard s registry = do
  beast <- S.printingOf s registry "Mutalith Vortex Beast"
  maiden <- S.printingOf s registry "Bird Maiden"
  let stocked = snd (S.addLibraryCard maiden S.alice (snd (S.addLibraryCard maiden S.alice S.threePlayerGame)))
  pure (snd (S.entersWithTrigger beast S.alice (atLife S.carol 16 stocked)))

-- Every call is heads; the faces are pinned by index, in the order the loop
-- visits the opponents (APNAP from alice: bob, then carol). A pure answerer
-- cannot tell the two flips apart, so this one counts them.
mutalithAnswer :: [CoinFace.CoinFace] -> Prompt.Prompt r -> State.State Int r
mutalithAnswer faces p = case p of
  Prompt.CallCoin {} -> pure CoinFace.Heads
  Prompt.FlipCoin -> do
    i <- State.get
    State.put (i + 1)
    pure (atIndex faces CoinFace.Tails i)
  _ -> pure (S.identityAnswer p)

-- Place the enters trigger and resolve it under the pinned faces.
afterMutalith :: [CoinFace.CoinFace] -> GameState.GameState -> GameState.GameState
afterMutalith faces board =
  let run :: GameState.GameState -> Game.Type.Game a -> GameState.GameState
      run g game = snd (State.evalState (Engine.runGame (mutalithAnswer faces) g game) 0)
   in S.settleSba (run (run board (Engine.placePendingTriggers [])) Stack.resolveTop)

missesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
missesSpec s registry = Spec.describe s "FlipCoin tallies the flips lost (CR 705.2)" $ do
  Spec.it s "CR 705.2 a lost flip reaches its loser and a won one does not" $ do
    board <- mutalithBoard s registry
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation. Bob's coin is tails against a call of heads (lost), carol's is
    -- heads (won): three damage to bob and none to carol. The losses bound to
    -- the WON tally would deal it to carol instead; bound to the coin COUNT it
    -- would deal it to both; left unbound it would reach nobody.
    Spec.assertEqWith
      s
      "CR 705.2: the lost flip deals 3 to bob and the won one deals nothing to carol"
      (let settled = afterMutalith [CoinFace.Tails, CoinFace.Heads] board in (S.lifeOf S.bob settled, S.lifeOf S.carol settled))
      (Just 17, Just 16)
    -- Supporting: the won flip drew its card and the lost one did not.
    Spec.assertEqWith
      s
      "CR 705.2: one flip won, so one card drawn"
      (S.handSize S.alice (afterMutalith [CoinFace.Tails, CoinFace.Heads] board))
      1
  Spec.it s "CR 705.2 each lost flip is paid once and none when none is lost" $ do
    board <- mutalithBoard s registry
    -- Both lost: both opponents take 3, and nothing is drawn.
    Spec.assertEqWith
      s
      "CR 705.2: two flips lost, so 3 damage to each opponent"
      (let settled = afterMutalith [CoinFace.Tails, CoinFace.Tails] board in (S.lifeOf S.bob settled, S.lifeOf S.carol settled, S.handSize S.alice settled))
      (Just 17, Just 13, 0)
    -- The pair, one thing different: both won, so nobody is damaged and two
    -- cards are drawn.
    Spec.assertEqWith
      s
      "CR 705.2: no flip lost, so no damage and two cards drawn"
      (let settled = afterMutalith [CoinFace.Heads, CoinFace.Heads] board in (S.lifeOf S.bob settled, S.lifeOf S.carol settled, S.handSize S.alice settled))
      (Just 20, Just 16, 2)
  -- CR 608.2e: the flips are one sentence and the draws the next, so every
  -- coin is flipped before the first draw. Darkblast's dredge is the read:
  -- CR 702.52a offers it at the draw, and the answerer notes how many coins
  -- had been flipped by then. Bob's flip is won, so taking the opponents one
  -- at a time would offer it after one flip.
  Spec.it s "CR 608.2e every coin is flipped before the first draw" $ do
    beast <- S.printingOf s registry "Mutalith Vortex Beast"
    maiden <- S.printingOf s registry "Bird Maiden"
    darkblast <- S.printingOf s registry "Darkblast"
    let stocked = foldr (\_ g -> snd (S.addLibraryCard maiden S.alice g)) (snd (S.addGraveyardCard darkblast S.alice S.threePlayerGame)) [1 :: Int .. 4]
        board = snd (S.entersWithTrigger beast S.alice stocked)
        answer :: Prompt.Prompt r -> State.State (Int, [Int]) r
        answer p = case p of
          Prompt.CallCoin {} -> pure CoinFace.Heads
          Prompt.FlipCoin -> do
            State.modify' (\(flipped, seen) -> (flipped + 1, seen))
            State.gets (\(flipped, _) -> if flipped == 1 then CoinFace.Heads else CoinFace.Tails)
          Prompt.ChooseDredge {} -> do
            State.modify' (\(flipped, seen) -> (flipped, seen <> [flipped]))
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        run :: GameState.GameState -> Game.Type.Game a -> State.State (Int, [Int]) GameState.GameState
        run g game = fmap snd (Engine.runGame answer g game)
        offers = snd (State.execState (run board (Engine.placePendingTriggers []) >>= \placed -> run placed Stack.resolveTop) (0, []))
    Spec.assertEqWith s "CR 608.2e the one draw's dredge is offered after both flips" offers [2]

-- CR 608.2c: each branch of Game of Chaos is two instructions in written order,
-- so two events, and CR 603.4's board for the first does not hold the second.
-- The log is the read: life totals can't show it and no pool trigger's "if" does.
chaosSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chaosSpec s registry = Spec.describe s "Game of Chaos's life changes (CR 608.2c)" $ do
  let declining :: CoinFace.CoinFace -> Prompt.Prompt r -> r
      declining face p = case p of
        Prompt.ChooseRepeat {} -> OptionalDecision.Declines
        _ -> flipAnswer face CoinFace.Heads p
      groupsAfter face = do
        chaos <- S.printingOf s registry "Game of Chaos"
        mountain <- S.printingOf s registry "Mountain"
        let (board, chaosId) = S.handOne chaos (S.landsFor mountain S.alice 3 (Setup.emptyGame S.bothPlayers))
            after = S.runPure (declining face) board (S.cast S.alice chaosId >> Stack.resolveTop)
            lifeChange logged = case LoggedEvent.event logged of
              GameEvent.LifeGained _ -> Just (LoggedEvent.group logged)
              GameEvent.LifeLost _ -> Just (LoggedEvent.group logged)
              _ -> Nothing
        pure (Maybe.mapMaybe lifeChange (Foldable.toList (TurnRegisters.events (GameState.turn after))))
      twoEvents face = do
        groups <- groupsAfter face
        Spec.assertEqWith s "one life change each, in groups of their own" (length groups, Set.size (Set.fromList groups)) (2, 2)
  Spec.it s "CR 608.2c / 603.4 a won flip's gain and loss are two events" (twoEvents CoinFace.Heads)
  Spec.it s "CR 608.2c / 603.4 a lost flip's loss and gain are two events" (twoEvents CoinFace.Tails)
