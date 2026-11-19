module Parun.Types.Request where

import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question

-- | One line to a client: what its seat sees, and what it is asked.
data Request = MkRequest
  { -- | Nothing before the game begins (CR 103.1's play or draw).
    view :: Maybe PlayerView.PlayerView,
    question :: Question.Question,
    -- | Why the previous answer to this question could not be read.
    problem :: Maybe Text.Text,
    -- | CR 730.1a: how many games the question's game is a subgame within, 0
    -- for the main game (Asked.enclosing).
    depth :: Natural.Natural
  }
  deriving (Eq, Show)
