module Parun.Types.Existence where

-- | CR 400.7: whether an id still names an object in the game.
data Existence
  = -- | One it does.
    Live
  | -- | CR 608.2h: one it no longer does, the object having changed zones or
    -- left the game, known by last known information (CR 113.7a).
    Departed
  deriving (Eq, Ord, Show)
