module Parun.Types.LifeGainCause where

-- | CR 119.10 / 609.1: which life gains a Parun.Types.LifeGainR row narrows to
-- by what caused them, read against the event's Parun.Types.Cause.
data LifeGainCause
  = -- | CR 119.10: any gain, whatever caused it (Boon Reflection's "if you would
    -- gain life").
    AnyCause
  | -- | CR 609.1: a gain caused by a spell or ability the gaining player
    -- controls (Rain of Gore); never a combat damage lifelink gain, which no
    -- spell or ability causes.
    ByOwnSpellOrAbility
  deriving (Bounded, Enum, Eq, Ord, Show)
