{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 719 and CR 702.160 end to end: Parun.Types.Keyword's Prototype arm and
-- the Parun.Types.Prototype frame it carries, the candidate cost
-- Parun.Engine.Cost.candidateCostsFor offers beside the printed one, the stamp
-- Parun.Engine.Cast.stampPrototyped writes when CR 601.2b settles on it, the move
-- Parun.Engine.Event.changeZoneAttaching lets it cross, and the swap
-- Parun.Engine.Projection.View.withPrototype makes in the seed characteristics.
--
-- Gameplay-level throughout. Autonomous Assembler is the fixture: a {5} colourless
-- 4/5 Artifact Creature -- Assembly-Worker whose inset frame reads "Prototype
-- {1}{W} -- 2/2", plus vigilance and "{1}, {T}: Put a +1\/+1 counter on target
-- Assembly-Worker you control".
--
-- The card is chosen for CR 719.3b's colour clause: it is the only prototype
-- printing whose inset cost is a single colour over a colourless printed cost, so
-- one assertion tells "the cost was swapped" from "the characteristics were". Its
-- numbers are non-degenerate too -- 4\/5 against 2\/2, mana value 5 against 2 --
-- so no reading coincides with another.
--
-- CR 719.3b's characteristics, cast prototyped and cast normally, and CR 719.5's
-- kept abilities are data/scenarios/prototype/'s.
module Parun.PrototypeSpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.CandidateCost as CandidateCost
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.Zone as Zone

-- The inset cost (CR 719.3), written out so every board names the same symbols
-- the card file does.
insetCost :: [ManaSymbol.ManaSymbol]
insetCost = [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.White)]

-- CR 601.2b's announcement answered by NAMING a cost rather than an index: an
-- answerer that searched the offered list for a legal option would find the right
-- one again after a mutation.
castingFor :: [ManaSymbol.ManaSymbol] -> Prompt.Prompt r -> r
castingFor wanted p = case p of
  Prompt.ChooseCost _ _ _ candidates _ ->
    Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just (ManaCost.MkManaCost wanted)) . Cost.Type.mana . CandidateCost.cost) candidates)
  _ -> S.identityAnswer p

-- The answerer every case below casts with: Autonomous Assembler's inset {1}{W}.
prototyping :: Prompt.Prompt r -> r
prototyping = castingFor insetCost

-- The permanent that printing became, found by NAME rather than by the
-- characteristics under test: a cast that resolved and one that is still on the
-- stack must both be found, or the assertions would compare two Nothings.
assemblerOn :: Printing.Printing -> GameState.GameState -> Maybe ObjectId.ObjectId
assemblerOn printing gs =
  List.find
    (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Prototype" $ do
  -- CR 719.4: "in every zone except the stack or the battlefield, and while on the
  -- stack or the battlefield when not cast as a prototyped spell, a prototype card
  -- has only its normal characteristics."
  --
  -- The HAND is the zone that separates rule 719.4 from an implementation that
  -- read the inset frame off the card wherever it lay; the GRAVEYARD is the half
  -- that separates it from one that stamped the choice and never cleared it. Both
  -- readings are of the same card in the same game, one having been cast
  -- prototyped and died.
  Spec.it s "CR 719.4 in hand and in a graveyard the Assembler has only its printed characteristics" $ do
    assembler <- S.printingOf s registry "Autonomous Assembler"
    plains <- S.printingOf s registry "Plains"
    let (board, spellId) = S.handOne assembler (S.landsInPlay plains 5)
    Spec.assertEqWith
      s
      "CR 719.4: in hand it is a colourless 4/5"
      ((,) (Projection.colorsOf spellId board) (S.powerToughnessOf spellId board))
      (Set.empty, Just (4, 5))
    let cast = S.runPure prototyping board (S.cast S.alice spellId)
        resolved = S.settleSba (S.runPure prototyping cast Stack.resolveTop)
    case assemblerOn assembler resolved of
      Nothing -> Spec.assertFailure s "the prototyped spell did not resolve onto the battlefield"
      Just oid -> do
        let dead = S.settleSba (S.runPure prototyping resolved (Event.destroy (pure ()) Regenerability.Regenerable [oid]))
            inGraveyard = Game.zoneMembers Zone.Graveyard S.alice dead
        Spec.assertEqWith
          s
          "CR 719.4: the card in its owner's graveyard is a colourless 4/5 again"
          (fmap (\card -> (Projection.colorsOf card dead, S.powerToughnessOf card dead)) inGraveyard)
          [(Set.empty, Just (4, 5))]
