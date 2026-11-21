module Parun.Types.BlockChoice where

import qualified Parun.Types.ObjectId as ObjectId

-- | What an effect leaves open about CR 509.4's choice, AttackChoice's twin:
-- settled by Parun.Engine.Resolve.Common's read of Parun.Types.EntryBlock.
data BlockChoice
  = -- | CR 509.4's main clause: the controller chooses the attacker.
    AnyAttacker
  | -- | CR 509.4's parenthetical: the effect named this attacker.
    SpecifiedAttacker ObjectId.ObjectId
  deriving (Eq, Ord, Show)
