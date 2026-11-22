module Parun.Web.BarSpec where

import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Bar" $ do
  let tapping label page = Html.labelled (Text.pack "button") (Text.pack label) page >>= \path -> Html.handler (Text.pack "click") path page
  Spec.describe s "view" $ do
    Spec.it s "sends each section from its button" $ do
      let bar = Bar.view Nothing
      Spec.assertEq s (fmap (\section -> tapping (Bar.title section) bar) [minBound .. maxBound]) (fmap Just [minBound .. maxBound])

    Spec.it s "marks the section shown, and only it" $ do
      let page = Html.toText (Bar.view (Just Bar.MkDecks))
      Spec.assertEqWith s "current" (Text.isInfixOf (Text.pack "aria-current=\"page\">Decks</button>") page) True
      Spec.assertEqWith s "once" (Text.count (Text.pack "aria-current") page) 1

    Spec.it s "carries the settings button page.js opens the menu from" $ do
      Spec.assertEq s (Text.isInfixOf (Text.pack "aria-label=\"Settings\" aria-haspopup=\"true\" aria-controls=\"settings\" data-settings=\"\"") (Html.toText (Bar.view Nothing))) True

  Spec.describe s "framed" $ do
    Spec.it s "puts the bar above the screen, its taps sent as given" $ do
      let page = Bar.framed Just (Just Bar.MkHome) (Element.el "main" ["home"] [] [Element.button [] Nothing [Element.string "Here"]])
      Spec.assertEqWith s "order" (Text.isPrefixOf (Text.pack "<div class=\"framed\"><header class=\"topbar\">") (Html.toText page) && Text.isInfixOf (Text.pack "</header><main class=\"home\">") (Html.toText page)) True
      Spec.assertEqWith s "a section" (tapping "Replays" page) (Just (Just Bar.MkReplays))
      Spec.assertEqWith s "the screen's own" (tapping "Here" page) (Just Nothing)
