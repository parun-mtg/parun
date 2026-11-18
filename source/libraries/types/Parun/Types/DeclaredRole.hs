module Parun.Types.DeclaredRole where

import qualified Parun.Types.AttackTargetKind as AttackTargetKind

-- | CR 508.1a / 509.1a: what a creature was declared as earlier this turn, as
-- Parun.Types.Filter's DeclaredThisTurn asks it.
data DeclaredRole
  = -- | CR 508.1a: declared as an attacker; with Just, one whose CR 508.1b
    -- announcement named this kind of thing (War Historian's "attacked a
    -- battle this turn").
    Attacked (Maybe AttackTargetKind.AttackTargetKind)
  | -- | CR 509.1a: declared as a blocker.
    Blocked
  deriving (Eq, Ord, Show)
