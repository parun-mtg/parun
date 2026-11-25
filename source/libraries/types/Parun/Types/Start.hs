module Parun.Types.Start where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Parun.Types.Board as Board
import qualified Parun.Types.Entrant as Entrant
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.Label as Label

-- | Where a scenario's game begins.
data Start
  = -- | A board placed directly.
    FromBoard Board.Board
  | -- | CR 103: a new game set up from each player's deck, in seating
    -- order, each seat numbered by its place, under the options and format
    -- it was played with (CR 800.2, 100.2), the turn order beginning with the
    -- starting player named (CR 103.1), else the first.
    FromDecks GameSettings.GameSettings (NonEmpty.NonEmpty Entrant.Entrant) (Maybe Label.Label)
  deriving (Eq, Ord, Show)
