-- | What an element of a view carries besides its children.
module Parun.Web.Attribute where

import qualified Data.Text as Text

-- | An attribute of an element.
data Attribute msg
  = -- | A DOM property, by name, and its value.
    MkProperty Text.Text Text.Text
  | -- | The message an event of this type sends, by the event's DOM name.
    MkOn Text.Text msg
  deriving (Eq, Show)

instance Functor Attribute where
  fmap f attribute = case attribute of
    MkProperty name value -> MkProperty name value
    MkOn event msg -> MkOn event $ f msg
