module Parun.Types.ManaInstead where

import qualified Parun.Types.ManaType as ManaType

-- | CR 106.12b: the type a ManaR row makes a permanent tapped for mana
-- produce instead of any other type.
data ManaInstead
  = -- | CR 106.1b: this type (Naked Singularity's "Plains produce {R}").
    OfType ManaType.ManaType
  | -- | CR 105.4: one color, chosen by the player tapping it (Pulse of Llanowar).
    AnyColor
  deriving (Eq, Ord, Show)
