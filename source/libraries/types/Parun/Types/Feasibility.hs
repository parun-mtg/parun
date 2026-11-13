module Parun.Types.Feasibility where

-- | Which rule asks whether an instruction can be carried out.
data Feasibility
  = -- | CR 608.2d: as an option a player may choose.
    AsOption
  | -- | CR 118.3: as the cost an "If you can't" hangs off (CR 118.12).
    AsCost
  deriving (Eq, Ord, Show)
