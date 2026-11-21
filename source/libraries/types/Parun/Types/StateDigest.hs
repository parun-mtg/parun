module Parun.Types.StateDigest where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.PastActivation as PastActivation
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 733.3: a game state as Parun.Engine.Fragmented.digest leaves it for
-- comparison, its object ids and timestamps renumbered by rank (CR 400.7).
-- Ordered by a few cheap, id-free fields before the canonical text, so two
-- states that differ in who holds priority, how much is on the stack or the
-- battlefield, or what is in a mana pool -- most pairs a loop compares --
-- never render every object.
data StateDigest = MkStateDigest
  { -- | The cheap fields, compared first: the worked turn's priority holder,
    -- passes, stack size and step, then the object count, battlefield size,
    -- mana pools and hand sizes.
    cheap :: ((Maybe PlayerId.PlayerId, Set.Set PlayerId.PlayerId, Int, Phase.Phase), (Int, Int, Map.Map PlayerId.PlayerId Mana.Mana, Map.Map PlayerId.PlayerId Int)),
    -- | The state, less what the digest drops, rendered with every object id
    -- and timestamp replaced by its rank among those it holds. The only field
    -- compared bar `cheap`, and lazy, so a comparison `cheap` settles never
    -- renders it.
    canonical :: String,
    -- | The rank `canonical` gives each object id the state holds, so a
    -- choice made in this state can be compared under the same renaming.
    objectRanks :: Map.Map Natural.Natural Int,
    -- | The dropped TurnRegisters.events, carried uncompared so a recurrence can
    -- ask which of them the loop wrote.
    events :: Seq.Seq LoggedEvent.LoggedEvent,
    -- | The dropped TurnRegisters.activationsThisTurn, carried the same way.
    activations :: Seq.Seq PastActivation.PastActivation,
    -- | CR 807.4b: the same two logs of each parked Grand Melee turn, by its
    -- marker's number.
    parkedLogs :: Map.Map Natural.Natural (Seq.Seq LoggedEvent.LoggedEvent, Seq.Seq PastActivation.PastActivation)
  }
  deriving (Show)

instance Eq StateDigest where
  a == b = compare a b == EQ

instance Ord StateDigest where
  compare x y = compare (cheap x) (cheap y) <> compare (canonical x) (canonical y)
