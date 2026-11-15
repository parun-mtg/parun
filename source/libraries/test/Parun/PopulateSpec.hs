{-# LANGUAGE GADTs #-}

-- Covers: CR 701.36 POPULATE -- Parun.Engine.Populate and Effect.Populate's arm
-- in Parun.Engine.Resolve.Effect.
--
-- Wake the Reflections ({W} Sorcery, "Populate.") is the fixture: the keyword
-- action is its whole text, so every assertion below is about rule 701.36a.
--
-- THE BOARD SHAPE that makes the cases discriminating. Rule 701.36a's candidate
-- has to be a creature, a token, and one this player controls, and the board
-- carries a counterexample to each: alice controls a Goblin Piker TOKEN (the
-- candidate), a NONTOKEN Hill Giant, and bob controls an Ornithopter token of
-- his own. A populate that read any one of the three qualities wrongly copies
-- something the assertions below name by printing. The board's other cases are
-- data/scenarios/populate/.
module Parun.PopulateSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt

-- alice: two Plains and Wake the Reflections in hand, a nontoken Hill Giant and
-- a Goblin Piker token. bob: an Ornithopter token.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
board plains wake piker giant flier =
  let (_, g1) = S.addPermanent giant S.alice (S.landsInPlay plains 2)
      g2 = snd (S.addToken (Printing.card piker) S.alice g1)
      (_, g3) = S.addToken (Printing.card flier) S.bob g2
      (spell, g4) = S.addHandCard wake S.alice g3
   in (spell, g4)

-- `S.castAnswer`, but answering the populate's Prompt.ChoosePermanent with the
-- LAST token offered. Pinned by position, so a populate that ignored the answer
-- and took the offered set's front copies the other token.
choosingLast :: Prompt.Prompt r -> r
choosingLast p = case p of
  Prompt.ChoosePermanent _ _ _ candidates _ -> NonEmpty.last candidates
  _ -> S.castAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Populate" $ do
  -- Two creature tokens of different printings, so WHICH one was copied is
  -- readable by name. The Ornithopter token is minted after the Piker, so it is
  -- the last of the ascending candidates and the answer below names it.
  Spec.it s "CR 701.36a with two creature tokens the populating player chooses which is copied" $ do
    plains <- S.printingOf s registry "Plains"
    wake <- S.printingOf s registry "Wake the Reflections"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    flier <- S.printingOf s registry "Ornithopter"
    let (spell, bare) = board plains wake piker giant flier
        (_, before) = S.addToken (Printing.card flier) S.alice bare
        cast = S.runPure choosingLast before (S.cast S.alice spell)
        after = S.runPure choosingLast cast Stack.resolveTop
    Spec.assertEqWith s "CR 701.36a the chosen Ornithopter token was copied" (S.countOnBattlefieldByName (S.printingName flier) S.alice after) 2
    Spec.assertEqWith s "and the Goblin Piker token beside it was not" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
