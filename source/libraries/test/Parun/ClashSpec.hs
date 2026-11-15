{-# LANGUAGE GADTs #-}

-- Covers: CR 701.30 CLASH -- Effect.Clash's arm and the clash procedure in
-- Parun.Engine.Resolve.Effect, plus Prompt.ChooseClash.
--
-- Pulling Teeth ({1}{B} Sorcery, "Clash with an opponent. If you win, target
-- player discards two cards. Otherwise, that player discards a card.") is the
-- fixture: the keyword action is its first clause, and the two discards are rule
-- 701.30d's outcome read back out of the slot.
--
-- THREE SEATS, because "an opponent" and "target player" are different players
-- here: alice casts, clashes with bob, and targets CAROL. A two-player board
-- would collapse the two and a discard would prove nothing about which one the
-- clash chose.
--
-- The outcome, the APNAP questions and the public reveal are proved in
-- data/scenarios/clash; the case here reads visibility at a prompt, which no
-- scenario check observes.
module Parun.ClashSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Visibility as Visibility.Type
import qualified Parun.Types.Zone as Zone

-- The top card of a player's library as a list, so a case can compare it without
-- a partial head over an empty library.
topOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
topOf pid gs = take 1 (Game.zoneMembers Zone.Library pid gs)

-- alice: two Swamps and Pulling Teeth in hand, and a library whose TOP is
-- `aliceTop` over one more card -- the card beneath is what makes top-and-bottom
-- two different places, so the decision is a real one. bob's library is the same
-- shape, and carol holds three cards, one more than the larger discard so that
-- "discards two" is a choice rather than her whole hand.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
board swamp teeth aliceTop bobTop filler =
  let g0 = S.landsFor swamp S.alice 2 S.threePlayerGame
      (spell, g1) = S.addHandCard teeth S.alice g0
      (_, g2) = S.addLibraryCard filler S.alice g1
      (_, g3) = S.addLibraryCard aliceTop S.alice g2
      (_, g4) = S.addLibraryCard filler S.bob g3
      (_, g5) = S.addLibraryCard bobTop S.bob g4
      stock gs _ = snd (S.addHandCard filler S.carol gs)
   in (spell, Foldable.foldl' stock g5 [1 :: Int, 2, 3])

-- Everything but the clash decisions: CR 701.30b's opponent, taken from the
-- offered set rather than built -- with three seats it is a real choice, and the
-- case is about bob rather than carol -- and carol as the discard's target.
casting :: Prompt.Prompt r -> r
casting p = case p of
  Prompt.ChooseOpponent _ _ _ _ opponents -> Maybe.fromMaybe (NonEmpty.head opponents) (List.find (S.bob ==) (NonEmpty.toList opponents))
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((==) (Just S.carol) . Recipient.playerOf) sets
  _ -> S.castAnswer p

spec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Clash" $ do
  -- What carol, who clashed with nobody, sees of the two revealed cards at her
  -- discard prompt, after alice bottomed hers and bob kept his on top. Pulling
  -- Teeth is still on the stack, so the reveal's window (CR 701.20a,
  -- Event.revealResolving) is open for both: the pair differs in one decision.
  -- Bob's card was not put anywhere (CR 701.30a's "may then put that card on the
  -- bottom"), so it is not reordered.
  Spec.it s "CR 701.20d a card the clash bottoms stops being revealed and becomes a new object" $ do
    swamp <- S.printingOf s registry "Swamp"
    teeth <- S.printingOf s registry "Pulling Teeth"
    giant <- S.printingOf s registry "Hill Giant"
    bolt <- S.printingOf s registry "Lightning Bolt"
    plains <- S.printingOf s registry "Plains"
    let (spell, before) = board swamp teeth giant bolt plains
        revealedTops = topOf S.alice before <> topOf S.bob before
        isRevealedName g oid = fmap S.nameOf (Game.cardOf oid g) `elem` [Just (S.printingName giant), Just (S.printingName bolt)]
        -- At each prompt that matters, carol's view of every library card named
        -- like a revealed one, and those cards' ids.
        watching :: Asked.Asked GameState.GameState r -> State.State [(String, [ObjectId.ObjectId], [Visibility.Type.Visibility])] r
        watching asked = do
          let g = Asked.game asked
              shown = filter (isRevealedName g) (foldMap (\pid -> Game.zoneMembers Zone.Library pid g) [S.alice, S.bob])
              note :: String -> State.State [(String, [ObjectId.ObjectId], [Visibility.Type.Visibility])] ()
              note label = State.modify' (<> [(label, shown, fmap (\oid -> Visibility.sees S.carol oid g) shown)])
          -- Pinned by seat: alice bottoms, bob keeps his on top.
          case Asked.prompt asked of
            Prompt.ChooseClash _ pid _ _ _ _
              | pid == S.alice -> note "clash" >> pure LibraryPosition.Bottom
              | otherwise -> pure LibraryPosition.Top
            p@(Prompt.ChooseDiscard _ pid _ _ _) | pid == S.carol -> note "discard" >> pure (S.identityAnswer p)
            p -> pure (casting p)
        ((_, after), seen) = State.runState (Engine.runGameAsked watching before (S.cast S.alice spell >> Stack.resolveTop)) []
        discarded = lookup "discard" [(label, (ids, views)) | (label, ids, views) <- seen]
    -- THE gameplay reading, and first: carol no longer sees alice's bottomed
    -- card, and still sees bob's, which stayed where it was.
    Spec.assertEqWith s "CR 701.20d at carol's discard prompt she sees bob's kept card and not alice's bottomed one" (fmap snd discarded) (Just [Visibility.Type.Obscured, Visibility.Type.Visible])
    -- Alice's card is a new object (CR 701.20d); bob's is the object it was.
    Spec.assertEqWith s "CR 701.20d only the bottomed card is a new object" (fmap (filter (`elem` revealedTops) . fst) discarded) (Just (topOf S.bob before))
    -- The control, off the same run: before the cards moved, carol saw both.
    Spec.assertEqWith s "CR 701.20a at alice's clash prompt carol sees both revealed cards" (lookup "clash" [(label, views) | (label, _, views) <- seen]) (Just [Visibility.Type.Visible, Visibility.Type.Visible])
    Spec.assertEqWith s "CR 701.30d carol discarded two" (length (Game.zoneMembers Zone.Graveyard S.carol after)) 2
