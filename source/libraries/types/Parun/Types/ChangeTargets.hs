module Parun.Types.ChangeTargets where

import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.RecipientRef as RecipientRef

-- | CR 115.7a / 115.7b: change the targets of each spell or ability on the
-- stack `objects` names, freely or to the one recipient `onto` names.
data ChangeTargets = MkChangeTargets
  { -- | The spells and abilities whose targets change.
    objects :: ObjectRef.ObjectRef,
    -- | Nothing: CR 115.7a, every target changes to one the resolving
    -- controller chooses (Deflection). Just: CR 115.7b, exactly one target, of
    -- the controller's choosing, changes to a permanent or a player this names
    -- (Mizzium Meddler's "change a target ... to this creature"). Ricochet's
    -- CR 115.7a "change the target ... to the player with the lowest result"
    -- is this too: its spell has one target, so the two changes coincide.
    onto :: Maybe RecipientRef.RecipientRef
  }
  deriving (Eq, Ord, Show)
