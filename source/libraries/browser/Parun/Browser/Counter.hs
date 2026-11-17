-- | A counter, the smallest app that proves the glue: a tap sends a message,
-- and the patched view shows the new count.
module Parun.Browser.Counter where

import qualified Data.Text as Text
import qualified Parun.Browser as Browser
import qualified Parun.Web.App as App
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html

start :: IO ()
start = Browser.run app (0 :: Integer)

app :: App.App Integer ()
app =
  App.MkApp
    { App.update = \() n -> n + 1,
      App.view = \n ->
        Html.element
          (Text.pack "main")
          []
          [ Html.element (Text.pack "p") [] [Html.text . Text.pack $ show n],
            Html.element (Text.pack "button") [Attribute.MkOn (Text.pack "click") ()] [Html.text $ Text.pack "+1"]
          ]
    }
