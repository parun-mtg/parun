module Parun.Types.Start where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Parun.Types.Board as Board
import qualified Parun.Types.Entrant as Entrant
import qualified Parun.Types.Format as Format

-- | Where a scenario's game begins.
data Start
  = -- | A board placed directly.
    FromBoard Board.Board
  | -- | CR 103: a new game set up from each player's deck, in turn order,
    -- under the format the decks were built for (CR 100.2), if any.
    FromDecks (Maybe Format.Format) (NonEmpty.NonEmpty Entrant.Entrant)
  deriving (Eq, Ord, Show)
