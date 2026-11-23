module Parun.Types.CostStep where

-- | Which step of the game a Prompt.ChooseCost is asked at: what a decline of
-- it would take back (Parun.Play.Protocol.withdrawalOf).
data CostStep
  = -- | CR 601.2b / 602.2b: the cast's or activation's own announcement of
    -- which cost it is made for.
    Announcement
  | -- | Any other: a substitute inside CR 601.2h's payment, CR 118.12's choice
    -- on resolution, a special action's cost, CR 616.1's madness choice.
    Elsewhere
  deriving (Eq, Ord, Show)
