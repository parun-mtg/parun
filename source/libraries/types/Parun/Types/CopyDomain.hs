module Parun.Types.CopyDomain where

import qualified Parun.Types.ExiledPile as ExiledPile
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Zone as Zone

-- | Where a Parun.Types.AsCopy choice looks for the object to copy.
data CopyDomain
  = -- | CR 707.5: every object in this zone, every player's alike (Clone's
    -- battlefield, Superior Spider-Man's graveyards).
    InZone Zone.Zone
  | -- | CR 607.2a / 702.167c: the cards in exile linked to the source (Dinosaur
    -- Headdress's "exiled creature card used to craft this Equipment").
    InPile ExiledPile.ExiledPile
  | -- | CR 608.2h / 707.2: the object a slot of the installing resolution
    -- names, as it last existed if it has left (Mystic Reflection's "the
    -- chosen creature").
    InSlot SlotName.SlotName
  deriving (Eq, Ord, Show)
