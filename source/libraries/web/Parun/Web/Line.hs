-- | What an event on a web client page sends a seat: a line the page fixes,
-- or the line typed into the element the event came from.
module Parun.Web.Line where

import qualified Data.Text as Text

data Line
  = -- | This line, whatever the event.
    MkFixed Text.Text
  | -- | The element's value, as typed.
    MkTyped
  deriving (Eq, Show)

-- | The line an event sends, given the value of the element it came from.
resolve :: Line -> Text.Text -> Text.Text
resolve line value = case line of
  MkFixed fixed -> fixed
  MkTyped -> value
