module Parun.Types.PhaseGrant where

-- | Whom an effect adding a phase says gets it.
data PhaseGrant
  = -- | The turn gets it ("there is an additional combat phase").
    ThereIs
  | -- | CR 500.10a: its controller gets it ("you get"), so it adds nothing on
    -- another player's turn.
    YouGet
  deriving (Bounded, Enum, Eq, Ord, Show)
