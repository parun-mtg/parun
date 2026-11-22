module Parun.Types.EnteredZone where

import qualified Data.Set as Set
import qualified Parun.Types.EntryWindow as EntryWindow
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Zone as Zone

-- | CR 400.7: which zone change made the candidate the object it is now, and
-- when -- the payload of Parun.Types.Filter's EnteredThisTurn. Octomancer's
-- "entered the battlefield this turn" names only the destination; No Rest for
-- the Wicked's "put there from the battlefield this turn" names the origin as
-- well; Premature Burial's "entered since your last turn ended" a wider window;
-- Shaile, Dean of Radiance's "entered the battlefield under your control" the
-- controller it entered under.
data EnteredZone = MkEnteredZone
  { -- | The zone the candidate arrived in.
    to :: Zone.Zone,
    -- | The zones it may have come from; empty admits any.
    from :: Set.Set Zone.Zone,
    -- | The turns the arrival may have happened in.
    window :: EntryWindow.EntryWindow,
    -- | CR 110.2: how the player it entered the battlefield under stands to
    -- the perspective; Nothing admits any.
    --
    -- Unprinted: a relation beside another zone or window.
    -- Parun.Codec.EnteredZone refuses it.
    under :: Maybe PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)
