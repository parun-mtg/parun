module Parun.Web.AppSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.App as App
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.App" $ do
  Spec.describe s "dispatch" $ do
    Spec.it s "updates the model and patches the view" $ do
      Spec.assertEq s (fmap snd . App.dispatch counter click (Seq.fromList [0]) $ App.start counter 0) . Just $
        Seq.singleton (Patch.MkReplace (Seq.fromList [1, 0]) (Html.text $ Text.pack "1"))

    Spec.it s "keeps the new model" $ do
      Spec.assertEq s (fmap (App.model . fst) . App.dispatch counter click (Seq.fromList [0]) $ App.start counter 41) $ Just 42

    Spec.it s "does nothing for an event no handler takes" $ do
      Spec.assertEq s (fmap snd . App.dispatch counter click (Seq.fromList [1]) $ App.start counter 0) Nothing

click :: Text.Text
click = Text.pack "click"

counter :: App.App Integer ()
counter =
  App.MkApp
    { App.update = \() n -> n + 1,
      App.view = \n -> Html.element (Text.pack "div") [] [Html.element (Text.pack "button") [Attribute.MkOn click ()] [Html.text $ Text.pack "+"], Html.element (Text.pack "span") [] [Html.text . Text.pack $ show n]]
    }
