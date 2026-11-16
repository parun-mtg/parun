module Parun.Types.AttachmentEnd where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | CR 701.3: which end of an attachment link the bearer of a trigger is.
data AttachmentEnd
  = -- | The permanent attached to (Bramble Elemental's "this creature").
    Host
  | -- | The permanent doing the attaching (Enormous Energy Blade's "this
    -- Equipment").
    Attachment
  | -- | CR 603.2: neither; the Filter admits the attachment (Eriette, the
    -- Beguiler's "an Aura you control").
    Neither (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
