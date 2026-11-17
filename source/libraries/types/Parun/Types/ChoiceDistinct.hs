module Parun.Types.ChoiceDistinct where

-- | CR 607.2d: whether a later choice of a quality may repeat an earlier one
-- of the same ability's ("choose two basic land types" may not; "choose a
-- land type and a basic land type" may).
data ChoiceDistinct
  = -- | Any option, an earlier choice's included (Vision Charm).
    MayRepeat
  | -- | Every option but the earlier choices' (Illusionary Terrain).
    Distinct
  deriving (Bounded, Enum, Eq, Ord, Show)
