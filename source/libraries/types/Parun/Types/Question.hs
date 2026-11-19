module Parun.Types.Question where

import qualified Data.Text as Text
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Reply as Reply

-- | CR 724.5: a prompt as the seat answering it is shown it.
data Question = MkQuestion
  { seat :: PlayerId.PlayerId,
    -- | The prompt's constructor name, or PlayOrDraw for CR 103.1's choice
    -- before the game begins.
    kind :: Text.Text,
    -- | What the prompt offers, in its answer's vocabulary.
    offer :: Reply.Reply,
    -- | CR 724.1: the player the seat makes this choice for, while it
    -- controls them; Nothing for the seat's own choice.
    for :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)
