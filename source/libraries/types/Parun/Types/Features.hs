module Parun.Types.Features where

-- | The generic features of one player's position the search bot
-- (Parun.Play.Search) scores a cut-off playout by: a player's counts, or the
-- weight of each count.
data Features = MkFeatures
  { -- | CR 119.1: the player's life total.
    life :: Integer,
    -- | CR 402.1: the cards in the player's hand.
    hand :: Integer,
    -- | CR 208.1: the total power of the creatures the player controls,
    -- each counted at no less than 0.
    power :: Integer,
    -- | CR 208.1: the total toughness of the creatures the player controls,
    -- each counted at no less than 0.
    toughness :: Integer,
    -- | CR 305: the lands the player controls.
    lands :: Integer,
    -- | CR 110.1: the permanents the player controls, lands and creatures
    -- included.
    permanents :: Integer
  }
  deriving (Eq, Show)
