module Parun.Types.RequiredBlockers where

-- | CR 509.1c: how many creatures blocking an attacker obey a group blocking
-- requirement (Parun.Types.RequirementArity.AnySubject).
data RequiredBlockers
  = -- | One or more: "must be blocked if able" (Gaea's Protector).
    AnyNumber
  | -- | Exactly one, as ONE requirement, so a second blocker un-obeys it
    -- (Nacatl War-Pride and its ruling).
    ExactlyOne
  deriving (Bounded, Enum, Eq, Ord, Show)
