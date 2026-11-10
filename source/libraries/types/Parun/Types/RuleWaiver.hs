module Parun.Types.RuleWaiver where

-- | The rule a Parun.Types.RulePermission lifts.
data RuleWaiver
  = -- | CR 702.3b: "can attack as though it didn't have defender".
    Defender
  | -- | CR 302.6's attack half: "can attack as though it had haste".
    AttackSickness
  | -- | CR 302.6's {T}/{Q} half: "you may activate abilities of creatures you control as though those creatures had haste".
    ActivationSickness
  deriving (Bounded, Enum, Eq, Ord, Show)
