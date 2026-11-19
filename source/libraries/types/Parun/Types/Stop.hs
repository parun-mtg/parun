{-# LANGUAGE GADTs #-}

module Parun.Types.Stop where

import Numeric.Natural (Natural)
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Timed as Timed

-- | Where a scenario's replay stopped (Parun.Scenario.replay).
data Stop where
  -- | About to take an entry, having taken that many, while answering the question asked.
  MkBefore :: Natural -> Timed.Timed -> Asked.Asked GameState.GameState r -> Stop
  -- | Past every entry: the state the run ended in, its final checks passed.
  MkEnded :: GameState.GameState -> Stop

-- | The game as the stop finds it.
state :: Stop -> GameState.GameState
state stop = case stop of
  MkBefore _ _ asked -> Asked.game asked
  MkEnded gs -> gs
