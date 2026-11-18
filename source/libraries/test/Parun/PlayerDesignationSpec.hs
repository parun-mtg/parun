-- Covers: Parun.Engine.PlayerDesignation (CR 702.131 ascend), the settle pass
-- Parun.Engine.Engine runs it from, and Parun.Types.Player's designations
-- field. Ascend's and storied's grants and thresholds are the scenarios under
-- data/scenarios/player-designation; what stays here destroys a permanent no
-- move reaches.
--
-- Gameplay-level: the mark is read through Skymarcher Aspirant's flying, whose
-- printed "as long as you have" clause is gated on it, and the field itself is
-- read only after that.
module Parun.PlayerDesignationSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerDesignation as PlayerDesignation
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.Settle as Settle

spec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry =
  Spec.describe s "PlayerDesignation" $
    ascendSpec s registry

ascendSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ascendSpec s registry = Spec.describe s "Ascend" $ do
  -- CR 702.131a/b's "for the rest of the game": the mark does not track the count
  -- that granted it. Alice reaches ten, then a Swamp is destroyed and the board
  -- settles again at nine -- the scenario
  -- cr-702-131b-nine-permanents-grant-nothing is what says nine alone would never
  -- have granted it.
  Spec.it s "CR 702.131b the blessing survives falling back below ten" $ do
    aspirant <- S.printingOf s registry "Skymarcher Aspirant"
    swamp <- S.printingOf s registry "Swamp"
    let (aspirantId, board) = S.addPermanent aspirant S.alice (S.landsInPlay swamp 9)
        blessed = settle board
        victims = take 1 (filter (/= aspirantId) (Set.toAscList (GameState.battlefield blessed)))
        shrunk = settle (S.runPure S.identityAnswer blessed (Event.destroy Settle.none Regenerability.Regenerable victims))
    Spec.assertEqWith s "alice now controls nine permanents" (Set.size (GameState.battlefield shrunk)) 9
    Spec.assertBool s (flies aspirantId shrunk) "the Aspirant still flies"
    Spec.assertEqWith s "alice still has the city's blessing" (marksOf S.alice shrunk) (Just (Set.singleton PlayerDesignation.CitysBlessing))

-- CR 117.5's settle, which is where CR 702.131b's check runs -- it is not a
-- state-based action, so Parun.Support's settleSba would not reach it.
settle :: GameState.GameState -> GameState.GameState
settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

marksOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe (Set.Set PlayerDesignation.PlayerDesignation)
marksOf pid gs = fmap Player.designations (Map.lookup pid (GameState.players gs))

flies :: ObjectId.ObjectId -> GameState.GameState -> Bool
flies oid gs = Map.member Keyword.Flying (Projection.keywordsOf oid gs)
