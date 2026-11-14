module Parun.Types.Behold where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.SubtypeFamily as SubtypeFamily
import qualified Parun.Types.Zone as Zone

-- | The payload of Parun.Types.CostComponent's Behold arm: CR 701.4a as a cost,
-- beholding exactly this many DISTINCT objects the Filter admits -- Caustic
-- Exhale's "a Dragon" is one, Kindle the Inner Flame's "three Elementals" three.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Parun.Types.CostComponent is. Parun.Types.ExileMaterials' field names, the
-- pool spanning two zones there as here.
data Behold keyword = MkBehold
  { count :: Natural.Natural,
    whichObjects :: Filter.Filter keyword,
    -- | CR 601.2h: a subtype of this family the payer chooses before beholding,
    -- written to Object.chosenSubtype for whichObjects' Filter.HasChosenSubtype
    -- and the spell's own to read (Celestial Reunion's "choose a creature type").
    -- A cast cost's alone: on an activation it would overwrite the source's own
    -- choice.
    chosenType :: Maybe SubtypeFamily.SubtypeFamily,
    -- | The zone whose cards the payer OWNS join the permanents they control:
    -- CR 701.4a's hand, each card revealed, or exile, chosen unrevealed (CR
    -- 118.8, Close Encounter's "a warped creature card you own in exile").
    cardZone :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
