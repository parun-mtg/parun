module Parun.Types.AttachedToBound where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Zone as Zone

-- | CR 303.4b / 301.5a / 608.2h: the permanents attached to the object `slot` holds --
-- or, once it has left, those attached as it left -- that `filter` admits (Rhuk,
-- Hexgold Nabber's "all Equipment attached to that creature", Fumble's "that
-- were attached to it").
data AttachedToBound = MkAttachedToBound
  { slot :: SlotName.SlotName,
    filter :: Filter.Filter Keyword.Keyword,
    -- | Where they are looked for: the battlefield, or CR 400.7f's graveyard
    -- card each Aura attached to a departed host became -- Cass, Hand of
    -- Vengeance's "Aura cards that were attached to it from your graveyard".
    -- Parun.Codec.AttachedToBound refuses any other zone.
    zone :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
