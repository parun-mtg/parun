module Parun.Types.InterveningCheck where

-- | When a triggered ability's condition (Parun.Types.TriggeredAbility.intervening)
-- is checked.
data InterveningCheck
  = -- | When the trigger event occurs and again on resolution: CR 603.4's
    -- intervening "if", re-checked by CR 608.2a.
    OnTriggerAndResolution
  | -- | When the trigger event occurs only: "This ability triggers only if"
    -- (CR 603.2, 702.169c), an "if" CR 603.4's last sentence leaves its normal
    -- meaning.
    OnTriggerOnly
  deriving (Bounded, Enum, Eq, Ord, Show)
