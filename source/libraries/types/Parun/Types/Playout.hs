module Parun.Types.Playout where

-- | Who answers every seat's prompts in the search bot's playouts
-- (Parun.Play.Search.rollout).
data Playout
  = -- | The random bot (Parun.Play.Game.bot).
    Random
  | -- | The greedy policy (Parun.Play.Search.greedy).
    Greedy
  | -- | The greedy policy, each target chosen by its effect's polarity
    -- (Parun.Play.Search.aimed).
    Aimed
  deriving (Bounded, Enum, Eq, Ord, Show)
