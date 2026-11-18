module Parun.Types.RetainedCharacteristic where

-- | CR 707.9c: a characteristic a copy effect does not copy, the copy retaining
-- its own value instead.
data RetainedCharacteristic
  = -- | CR 707.9c / 105.2: colour (Vesuvan Doppelganger).
    Colors
  | -- | CR 707.9c / 205.4b: supertypes (Bombardment's ruling, "keep any
    -- supertypes they may have").
    Supertypes
  deriving (Bounded, Enum, Eq, Ord, Show)
