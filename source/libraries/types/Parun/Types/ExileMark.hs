module Parun.Types.ExileMark where

-- | A status a keyword action stamps on a card it exiles, which a Filter reads
-- (Filter.HasExileMark).
data ExileMark
  = -- | CR 702.185b: a warped card in exile, read off Object.warped.
    Warped
  | -- | CR 702.143c: a foretold card, or a spell that was one before it was
    -- cast, read off Object.foretold.
    Foretold
  deriving (Bounded, Enum, Eq, Ord, Show)
