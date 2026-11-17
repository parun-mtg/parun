-- | One step of bringing the DOM from the previous view to the next. A path
-- is the child indices leading from the root to the node changed, read
-- against the DOM as the patches before it left it.
module Parun.Web.Patch where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Web.Html as Html

-- | A change to the DOM.
data Patch msg
  = -- | Replaces the node at the path.
    MkReplace (Seq.Seq Natural) (Html.Html msg)
  | -- | Sets a property of the element at the path.
    MkSetProperty (Seq.Seq Natural) Text.Text Text.Text
  | -- | Removes a property of the element at the path.
    MkRemoveProperty (Seq.Seq Natural) Text.Text
  | -- | Appends children to the element at the path.
    MkAppend (Seq.Seq Natural) (Seq.Seq (Html.Html msg))
  | -- | Keeps only the first so many children of the element at the path.
    MkTruncate (Seq.Seq Natural) Natural
  | -- | Rebuilds the children of the element at the path from its current
    -- ones, moved, and new ones; a current child left out is removed.
    MkArrange (Seq.Seq Natural) (Seq.Seq (Source msg))
  deriving (Eq, Show)

-- | Where a child of an arranged element comes from.
data Source msg
  = -- | The element's current child at this index, moved.
    MkKept Natural
  | -- | A new node.
    MkCreated (Html.Html msg)
  deriving (Eq, Show)
