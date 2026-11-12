module Parun.Types.KeywordCostPart where

-- | The part of a rule-702 keyword's cost that an effect can forbid a creature
-- to pay: what a Parun.Types.KeywordCostRestriction names.
data KeywordCostPart
  = -- | Tapping it to pay a crew cost: "can't crew Vehicles" (CR 702.122d).
    Crew
  | -- | Returning it to pay a beam me up cost: "can't be beamed up" (CR 702.196b).
    BeamMeUp
  deriving (Bounded, Enum, Eq, Ord, Show)
