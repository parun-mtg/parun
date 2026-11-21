module Parun.Engine.Limit where

import qualified Data.Map.Strict as Map
import qualified Parun.Extra.Int as Int
import Parun.Types.Action (Action)
import qualified Parun.Types.Action as Action
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.MatchScope as MatchScope

-- | Whether the game holds as many objects as its host's limit allows
-- (Parun.Types.Limits.objects). Map.size is constant-time, so asking at every
-- priority grant costs nothing.
reached :: GameState -> Bool
reached gs = Int.toNaturalSaturating (Map.size (GameState.objects gs)) >= Limits.objects (MatchScope.limits (GameState.match gs))

-- | A priority menu as the host's limits leave it: Pass alone once the game
-- has `reached` its object limit, which refuses an action before it starts
-- rather than stopping one halfway (see docs/decisions.md, "What a game does
-- at a host limit"). Parun.LimitSpec proves it.
offered :: GameState -> [Action] -> [Action]
offered gs actions = if reached gs then filter (== Action.Pass) actions else actions

-- | A game under these limits rather than the ones it was built with.
under :: Limits.Limits -> GameState -> GameState
under limits gs = gs {GameState.match = (GameState.match gs) {MatchScope.limits = limits}}
