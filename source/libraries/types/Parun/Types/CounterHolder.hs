module Parun.Types.CounterHolder where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 603.2: whose counters a counter trigger watches.
data CounterHolder
  = -- | The bearer, compared by id (CR 715.2b's "this Saga").
    Self
  | -- | A permanent the Filter admits, judged with last known information (CR
    -- 608.2h), and with a relation also a player it admits (CR 122.1; All Will
    -- Be One's "a permanent or player").
    Permanent (Filter.Filter Keyword.Keyword) (Maybe PlayerRelation.PlayerRelation)
  deriving (Eq, Ord, Show)
