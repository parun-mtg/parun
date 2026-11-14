module Parun.Types.CountedAgainst where

-- | What Parun.Types.CostModifier.perEach is counted against, and whose "you"
-- it reads (CR 109.5, CR 601.2f).
data CountedAgainst
  = -- | The cost change's source, with its controller as "you": Animar, Soul
    -- of Elements' "for each +1/+1 counter on Animar".
    ChangeSource
  | -- | The spell or activation being paid for, with the player paying as
    -- "you": Hum of the Radix's "for each artifact its controller controls".
    PaidFor
  deriving (Bounded, Enum, Eq, Ord, Show)
