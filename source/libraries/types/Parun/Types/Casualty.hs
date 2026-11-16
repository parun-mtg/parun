module Parun.Types.Casualty where

import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Types.Loyalty as Loyalty
import qualified Parun.Types.Supertype as Supertype

-- | The payload of Parun.Types.Keyword's Casualty arm: CR 702.153a's "Casualty
-- N", plus the copy exceptions (CR 707.9b) a card prints after it (Ob Nixilis,
-- the Adversary's "The copy isn't legendary and has starting loyalty X").
--
-- amount is Nothing for CR 107.3's X, announced at CR 601.2b.
--
-- copyLoses and copyLoyalty are Parun.Types.CopyException's RemoveSupertypes
-- and SetLoyalty, which Parun.Engine.Keyword mints onto the copy. Fields
-- rather than a list of CopyException, which imports Keyword and so cannot be
-- named here (Parun.Types.Equip's timesEachTurn's reason).
data Casualty = MkCasualty
  { amount :: Maybe Natural,
    copyLoses :: Set.Set Supertype.Supertype,
    copyLoyalty :: Maybe Loyalty.Loyalty
  }
  deriving (Eq, Ord, Show)
