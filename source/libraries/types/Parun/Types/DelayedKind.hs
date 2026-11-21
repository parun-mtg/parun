module Parun.Types.DelayedKind where

-- | What an armed delayed triggered ability is, which a "boon" condition reads.
data DelayedKind
  = -- | CR 603.7: an ordinary delayed triggered ability.
    Ordinary
  | -- | A one-time boon: Tenacious Pup's ruling's single-use triggered ability
    -- with no source, which Underbridge Warlock's "if you have a boon" counts.
    OneTimeBoon
  deriving (Bounded, Enum, Eq, Ord, Show)
