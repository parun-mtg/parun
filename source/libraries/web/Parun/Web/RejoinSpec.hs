module Parun.Web.RejoinSpec where

import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch
import qualified Parun.Web.Rejoin as Rejoin

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Rejoin" $ do
  Spec.describe s "offerIn" $ do
    Spec.it s "reads the host's code from its link, or the code alone" $ do
      Spec.assertEq s (fmap (Rejoin.offerIn . Text.pack) ["https://parun.example/play/#offer=AbC-_9", " AbC-_9\n", "https://parun.example/#offer=AbC-_9&x=1"]) (replicate 3 (Text.pack "AbC-_9"))

  Spec.describe s "view" $ do
    Spec.it s "shows the host the new link to send and a box for the code that comes back" $ do
      let shown = Html.toText (Rejoin.view (Rejoin.host (Text.pack "Player 2")) {Rejoin.code = Just (Text.pack "https://parun.example/#offer=xyz")})
      Spec.assertBool s (all ((`Text.isInfixOf` shown) . Text.pack) ["Player 2", "#offer=xyz", "id=\"rejoin-reply\""]) "the link and the reply box"

    Spec.it s "shows the guest a box for the host's new link, then the code to send back" $ do
      let asking = Html.toText (Rejoin.view Rejoin.guest)
          made = Html.toText (Rejoin.view Rejoin.guest {Rejoin.code = Just (Text.pack "reply-code")})
      Spec.assertBool s (Text.isInfixOf (Text.pack "id=\"rejoin-offer\"") asking && Text.isInfixOf (Text.pack "reply-code") made) "the link box, then the code"

    Spec.it s "builds the guest's code box afresh rather than setting the box the link was pasted into, whose typed text a value attribute would not replace" $ do
      let patches = Diff.diff (Rejoin.view Rejoin.guest {Rejoin.typed = Text.pack "pasted"}) (Rejoin.view Rejoin.guest {Rejoin.code = Just (Text.pack "reply-code")})
          setsValue patch = case patch of
            Patch.MkSetProperty _ name _ -> name == Text.pack "value"
            _ -> False
      Spec.assertBool s (not (any setsValue patches)) "no value set on a kept box"
