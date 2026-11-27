module Parun.Types.PlanarRollCause where

-- | What rolled the planar die, which CR 901.9's cost counts by.
data PlanarRollCause
  = -- | CR 901.9: the special action.
    SpecialAction
  | -- | CR 901.9: an effect, which CR 901.9's cost does not count.
    Instruction
  deriving (Bounded, Enum, Eq, Ord, Show)
