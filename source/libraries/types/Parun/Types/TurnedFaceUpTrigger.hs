module Parun.Types.TurnedFaceUpTrigger where

import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 708.7 through CR 603.2: which permanent turned face up, and who turned
-- it.
data TurnedFaceUpTrigger = MkTurnedFaceUpTrigger
  { -- | The bearer (Skirk Marauder) or a permanent the Filter admits (Aven
    -- Farseer).
    permanent :: ActingPermanent.ActingPermanent,
    -- | The player who turned it (Parun.Types.TurnedFaceUp's), against the
    -- bearer's controller: Growing Dread's "whenever you turn".
    player :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)

-- | "When this permanent is turned face up", by anyone. The codec's defaults.
defaultValue :: TurnedFaceUpTrigger
defaultValue =
  MkTurnedFaceUpTrigger
    { permanent = ActingPermanent.Self,
      player = PlayerRelation.AnyPlayer
    }

-- | "Whenever a permanent the Filter admits is turned face up", by anyone.
matching :: Filter.Filter Keyword.Keyword -> TurnedFaceUpTrigger
matching f = defaultValue {permanent = ActingPermanent.Matching f}
