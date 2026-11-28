module Parun.Types.CostedSpecialAction where

import qualified Parun.Types.TurnUpProcedure as TurnUpProcedure

-- | Which CR 116.2 special action a Parun.Types.CostSubject.SpecialActions
-- modifier reaches. One constructor per special action a printed cost modifier
-- names.
data CostedSpecialAction
  = -- | CR 116.2m / 709.5e: paying an unlock cost.
    Unlock
  | -- | CR 116.2k / 702.170e: plotting a card.
    Plot
  | -- | CR 116.2h / 702.143a: foretelling a card.
    Foretell
  | -- | CR 116.2b / 708.7: turning a permanent face up by this procedure.
    TurnFaceUp TurnUpProcedure.TurnUpProcedure
  deriving (Eq, Ord, Show)
