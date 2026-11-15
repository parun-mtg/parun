module Parun.Types.ManaValueIs where

import qualified Parun.Types.Reference as Reference

-- | CR 202.3: an object's projected mana value.
data ManaValueIs = MkManaValueIs
  { object :: Reference.Reference,
    value :: Integer
  }
  deriving (Eq, Ord, Show)
