{-# LANGUAGE GADTs #-}

-- Covers: CR 701.48 LEARN -- Parun.Engine.Learn and Effect.Learn's arm in
-- Parun.Engine.Resolve.Effect.
--
-- Cram Session ({1}{B/G} Sorcery, "You gain 4 life. Learn.") is the fixture: the
-- keyword action is its whole second clause and states nothing the rulebook does
-- not, so every assertion below is about rule 701.48a.
--
-- alice holds a Hill Giant to discard, has a Plains to draw, and owns TWO cards
-- outside the game -- Airbending Lesson (an Instant -- Lesson) and Sign in Blood
-- (a sorcery, no Lesson). The branches on that board alone are scenarios, under
-- data/scenarios/learn; the cases here add a card to it.
module Parun.LearnSpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.LearnMode as LearnMode
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Zone as Zone

-- alice: two Swamps, Cram Session and a Hill Giant in hand, a Plains in her
-- library, and Airbending Lesson plus Sign in Blood outside the game.
--
-- The pool is written onto the player directly, Parun.OutsideTheGameSpec's
-- posture: CR 103.2a's road from a Deck is that spec's subject, and this one is
-- about what rule 701.48a does with a pool that exists.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
board swamp cram giant plains lesson sorcery =
  let (_, g1) = S.addHandCard giant S.alice (S.landsInPlay swamp 2)
      (spell, g2) = S.addHandCard cram S.alice g1
      (_, g3) = S.addLibraryCard plains S.alice g2
      (lessonId, g4) = Game.intern lesson g3
      (sorceryId, g5) = Game.intern sorcery g4
      stock p = p {Player.outsideTheGame = Map.fromList [(lessonId, 1), (sorceryId, 1)]}
   in (spell, g5 {GameState.players = Map.adjust stock S.alice (GameState.players g5)})

-- The rule 701.48a branch this run takes, pinned by index rather than searched:
-- an answerer that took whatever was legal would repair itself after a mutation.
learning :: Maybe LearnMode.LearnMode -> Prompt.Prompt r -> r
learning mode p = case p of
  Prompt.ChooseLearn {} -> mode
  _ -> S.castAnswer p

-- alice casts Cram Session and it resolves, under that branch.
learned :: Maybe LearnMode.LearnMode -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
learned mode spell gs =
  let cast = S.runPure (learning mode) gs (S.cast S.alice spell)
   in S.runPure (learning mode) cast Stack.resolveTop

-- The printings of the cards in a zone, so a case can say WHICH card is there.
printingsIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Printing.Printing]
printingsIn zone pid gs = Maybe.mapMaybe (\oid -> Game.printingOfObject oid gs) (Game.zoneMembers zone pid gs)

-- What rule 701.48a put into her graveyard, which is her graveyard WITHOUT Cram
-- Session: CR 608.2n puts the resolved sorcery there itself, and that is a fact
-- about the fixture rather than about learning.
discardedBy :: Printing.Printing -> GameState.GameState -> [Printing.Printing]
discardedBy cram gs = List.sort (filter (/= cram) (printingsIn Zone.Graveyard S.alice gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Learn" $ do
  -- CR 121.2a: the first branch's draw is an instruction to draw a card, so
  -- Quantum Riddler's "if you would draw one or more cards" reaches it once the
  -- discard has left alice's hand empty. Drawing through Event.drawCard, around
  -- the instruction, draws the Plains alone and the assertion reddens.
  Spec.it s "CR 121.2a Quantum Riddler makes the first branch's draw two cards" $ do
    swamp <- S.printingOf s registry "Swamp"
    cram <- S.printingOf s registry "Cram Session"
    giant <- S.printingOf s registry "Hill Giant"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    lesson <- S.printingOf s registry "Airbending Lesson"
    sorcery <- S.printingOf s registry "Sign in Blood"
    riddler <- S.printingOf s registry "Quantum Riddler"
    let (spell, bare) = board swamp cram giant plains lesson sorcery
        (_, stocked) = S.addLibraryCard island S.alice bare
        (_, before) = S.addPermanent riddler S.alice stocked
        after = learned (Just LearnMode.DiscardAndDraw) spell before
    Spec.assertEqWith s "she drew both library cards" (List.sort (printingsIn Zone.Hand S.alice after)) (List.sort [plains, island])
    Spec.assertEqWith s "after discarding the Hill Giant" (discardedBy cram after) [giant]
  -- The first branch's board with a second card in her hand, so the discard is
  -- a choice. S.addHandCard puts the Ornithopter in FRONT of the Hill Giant, so
  -- the answer below, the last card offered, names the Giant: a learn that took
  -- the offered set's front would discard the Ornithopter instead.
  Spec.it s "CR 701.48a with two cards in hand the learner chooses which to discard" $ do
    swamp <- S.printingOf s registry "Swamp"
    cram <- S.printingOf s registry "Cram Session"
    giant <- S.printingOf s registry "Hill Giant"
    plains <- S.printingOf s registry "Plains"
    lesson <- S.printingOf s registry "Airbending Lesson"
    sorcery <- S.printingOf s registry "Sign in Blood"
    flier <- S.printingOf s registry "Ornithopter"
    let (spell, bare) = board swamp cram giant plains lesson sorcery
        (_, before) = S.addHandCard flier S.alice bare
        taking p = case p of
          Prompt.ChooseCardInHand _ _ _ cards -> NonEmpty.last cards
          _ -> learning (Just LearnMode.DiscardAndDraw) p
        cast = S.runPure taking before (S.cast S.alice spell)
        after = S.runPure taking cast Stack.resolveTop
    Spec.assertEqWith s "CR 701.48a the chosen Hill Giant is in her graveyard" (discardedBy cram after) [giant]
    Spec.assertEqWith s "and the Ornithopter stayed in her hand beside the drawn Plains" (List.sort (printingsIn Zone.Hand S.alice after)) (List.sort [flier, plains])
