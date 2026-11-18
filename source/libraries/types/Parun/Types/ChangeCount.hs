module Parun.Types.ChangeCount where

-- | How many of a stack object's targets Parun.Types.ChangeTargets changes (CR
-- 115.7).
data ChangeCount
  = -- | Every target or none of them: CR 115.7a's "change the target(s)".
    Every
  | -- | Exactly one, of the chooser's choosing: CR 115.7b's "change a target".
    One
  deriving (Bounded, Enum, Eq, Ord, Show)
