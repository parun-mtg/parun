module Parun.Types.ParityOperand where

import qualified Parun.Types.Parity as Parity

-- | Which parity Filter.ManaValueParity asks the candidate's mana value for.
data ParityOperand
  = -- | A printed parity: Void Winnower's "even mana values".
    Printed Parity.Parity
  | -- | CR 607.2d: the parity of the number the source last chose (Gollum, Riddle Master's "the chosen quality").
    ChosenParity
  deriving (Eq, Ord, Show)
