module Parun.Types.ChangeTargets where

import qualified Parun.Types.ChangeCount as ChangeCount
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.RecipientRef as RecipientRef

-- | CR 115.7a / 115.7b: change `count` of the targets of each spell or ability
-- on the stack `objects` names, freely or to a recipient `onto` names.
data ChangeTargets = MkChangeTargets
  { -- | The spells and abilities whose targets change.
    objects :: ObjectRef.ObjectRef,
    -- | Every target or one (CR 115.7a / 115.7b).
    count :: ChangeCount.ChangeCount,
    -- | Nothing: each new target is one the resolving controller chooses
    -- (Deflection). Just: each is a permanent or a player this names (Mizzium
    -- Meddler's "to this creature", Ricochet's "to the player with the lowest
    -- result").
    onto :: Maybe RecipientRef.RecipientRef
  }
  deriving (Eq, Ord, Show)
