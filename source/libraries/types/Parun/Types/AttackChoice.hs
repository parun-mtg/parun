module Parun.Types.AttackChoice where

import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.PlayerId as PlayerId

-- | What an effect leaves open about CR 508.4's choice, which is a question
-- about the EFFECT rather than about the board, so it is settled by
-- Parun.Engine.Resolve.Common's read of Parun.Types.EntryAttack and passed
-- down. No card writes it; Parun.Types.DeferredLot holds it.
data AttackChoice
  = -- | CR 508.4's main clause: the controller chooses among every legal subject.
    Any
  | -- | CR 508.4's parenthetical (CR 702.49c): the effect said what it attacks.
    Specified AttackTarget.AttackTarget
  | -- | CR 702.116a: the controller still chooses, among that player and the
    -- planeswalkers they control.
    Under PlayerId.PlayerId
  deriving (Eq, Ord, Show)
