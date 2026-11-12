module Parun.Types.PairwiseFrame where

-- | CR 509.1b / 109.5: which object a pairwise restriction's blocker filter is
-- read from -- its "this creature" and its "you".
data PairwiseFrame
  = -- | The attacker each blocker is compared with (CR 701.54c).
    Attacker
  | -- | The permanent stating the restriction (Champion of Lambholt).
    Source
  deriving (Bounded, Enum, Eq, Ord, Show)
