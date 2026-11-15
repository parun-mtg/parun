module Parun.Types.StackPlacement where

import qualified Parun.Types.Label as Label
import qualified Parun.Types.Placement as Placement

-- | One spell a scenario's board places on the stack (CR 112.1), with its
-- owner, since the stack is no seat's zone (CR 405.1).
data StackPlacement = MkStackPlacement
  { owner :: Label.Label,
    placement :: Placement.Placement
  }
  deriving (Eq, Ord, Show)
