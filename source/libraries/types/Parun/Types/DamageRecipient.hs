module Parun.Types.DamageRecipient where

import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PlayerRef as PlayerRef

-- | CR 120.1: one recipient a damage trigger admits.
data DamageRecipient
  = -- | A player the reference names: a relation read against CR 109.5's
    -- "you", or a slot of the creating effect (CR 603.7).
    Player PlayerRef.PlayerRef
  | -- | A permanent of the card type (CR 120.1a), its recipient tag unioned
    -- with its types (Parun.Engine.Projection.damagedCardTypes), that the
    -- Filter admits as it stood immediately after the damage (CR 603.10),
    -- Binding.eventAmount holding the damage dealt to it; @Filter.And []@
    -- qualifies nothing.
    Permanent CardType.CardType (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
