module Parun.Types.ChooseInstead where

import qualified Parun.Types.Condition as Condition

-- | CR 700.2's instruction replaced while a condition holds: "Choose one. If
-- this spell was cast using teamwork, choose both instead." (Widow's Bite).
-- `ordinary` is the printed instruction, `instead` the one that replaces it.
--
-- Parametric in the selection so Parun.Types.ModeSelection can hold one of
-- itself without a module cycle.
data ChooseInstead selection = MkChooseInstead
  { condition :: Condition.Condition,
    ordinary :: selection,
    instead :: selection
  }
  deriving (Eq, Ord, Show)
