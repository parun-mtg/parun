module Parun.Types.CopyPowerToughness where

import qualified Parun.Types.CharacteristicPT as CharacteristicPT
import qualified Parun.Types.SetPowerToughness as SetPowerToughness

-- | CR 707.9: the power and toughness a copy exception gives the copy in place
-- of the copied object's. Either kind clears the copied object's
-- characteristic-defining P/T (CR 707.9d; docs/decisions.md, "Whether a copy
-- exception granting a power-and-toughness ability strips the copied one").
data CopyPowerToughness
  = -- | CR 707.9b: these numbers ("except it's 7/7").
    Numbers SetPowerToughness.SetPowerToughness
  | -- | CR 707.9a / 604.3a(2): a characteristic-defining ability setting them,
    -- applied in layer 7a (Colossal Clone Master's "it has 'This creature's
    -- power and toughness are each equal to ...'").
    Defined CharacteristicPT.CharacteristicPT
  deriving (Eq, Ord, Show)
