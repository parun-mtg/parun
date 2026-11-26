module Parun.Types.BattlefieldCandidate where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | One permanent as CR 603.10's trigger scan needs to see it: who controlled it
-- (CR 603.3a), what it looked like, what was attached to it and its counters at
-- that moment. Parun.Engine.Event's
-- battlefieldCandidates builds these, and
-- Parun.Types.Snapshots.battlefieldWhenTriggered files them per event group with
-- the reasoning for why a sample is needed at all.
--
-- A record rather than a pair, so neither half can be taken for the other; both
-- were unnamed before, and the controller is the one the live board most often
-- gets wrong.
--
-- PARAMETRIC in what is carried beside the controller, because the scan narrows
-- it: the stored form holds projected characteristics, and the scan maps those
-- to the abilities that function on the battlefield. Neither field is strict --
-- Snapshots.battlefieldWhenTriggered's entries are thunks over the state at that
-- moment, and forcing one here would cost the projection the laziness exists to
-- avoid.
data BattlefieldCandidate a = MkBattlefieldCandidate
  { controller :: PlayerId.PlayerId,
    characteristics :: a,
    -- | CR 303.4b / 301.5a: the permanents attached to it (Game.attachments),
    -- so "enchanted creature" reads the attachment the event saw, not one CR
    -- 704.5m has since undone.
    attached :: Set.Set ObjectId.ObjectId,
    -- | CR 122.1 / 603.10: the counters on it then, so "enters with a counter"
    -- (CR 122.6) reads the entry and not a counter put on it since.
    counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | Maps what is carried beside the controller, which is what the trigger scan
-- does to narrow characteristics to abilities. Written out rather than derived,
-- so the module needs no extension, and it is the same mapping the pair this
-- replaced got from its own Functor.
instance Functor BattlefieldCandidate where
  fmap f candidate = candidate {characteristics = f (characteristics candidate)}
