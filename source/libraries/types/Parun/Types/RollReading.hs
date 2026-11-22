module Parun.Types.RollReading where

-- | Which number of a die roll a Parun.Types.RollTrigger compares.
data RollReading
  = -- | CR 706.2: the result, after every modifier (Night Shift of the Living Dead's "roll a 6").
    FinalResult
  | -- | CR 706.2: the natural result, the face one roll came up before any modifier (Critical Hit's "natural 20").
    NaturalResult
  deriving (Bounded, Enum, Eq, Ord, Show)
