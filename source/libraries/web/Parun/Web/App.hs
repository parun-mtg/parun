-- | The Elm architecture: a model, a pure update and a pure view. The page
-- runs one through dispatch, applying the patches it returns to the DOM; all
-- of that but the patching is pure, so an app is tested natively.
module Parun.Web.App where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch

-- | An app: how a message changes the model, and how a model looks.
data App model msg = MkApp
  { update :: msg -> model -> model,
    view :: model -> Html.Html msg
  }

-- | A running app: its model and the view the DOM shows.
data Running model msg = MkRunning
  { model :: model,
    shown :: Html.Html msg
  }
  deriving (Eq, Show)

start :: App model msg -> model -> Running model msg
start app m = MkRunning m $ view app m

-- | Updates the model with the message, and the patches that bring the DOM
-- to the new view.
step :: App model msg -> msg -> Running model msg -> (Running model msg, Seq.Seq (Patch.Patch msg))
step app msg running =
  let next = update app msg $ model running
      html = view app next
   in (MkRunning next html, Diff.diff (shown running) html)

-- | An event of this type at the end of the path, sent to the handler the
-- shown view has for it (Html.handler); Nothing where none handles it.
dispatch :: App model msg -> Text.Text -> Seq.Seq Natural -> Running model msg -> Maybe (Running model msg, Seq.Seq (Patch.Patch msg))
dispatch app event path running = case Html.handler event path $ shown running of
  Nothing -> Nothing
  Just msg -> Just $ step app msg running
