module Parun.Web.WireSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch
import qualified Parun.Web.Wire as Wire

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Wire" $ do
  Spec.describe s "patches" $ do
    Spec.it s "encodes a replaced element without its handlers" $ do
      Spec.assertEq
        s
        (Wire.patches . Seq.singleton . Patch.MkReplace (Seq.fromList [1, 0]) $ Html.element (Text.pack "button") [Attribute.MkProperty (Text.pack "class") (Text.pack "go"), Attribute.MkOn (Text.pack "click") ()] [Html.text $ Text.pack "<1>"])
        "[{\"op\":\"replace\",\"path\":[1,0],\"node\":{\"tag\":\"button\",\"props\":[[\"class\",\"go\"]],\"children\":[{\"text\":\"<1>\"}]}}]"

    Spec.it s "encodes the children of a keyed element in order" $ do
      Spec.assertEq
        s
        (Wire.patches . Seq.singleton . Patch.MkReplace Seq.empty $ (Html.keyed (Text.pack "ul") [] [(Text.pack "b", Html.text $ Text.pack "x"), (Text.pack "a", Html.text $ Text.pack "y")] :: Html.Html ()))
        "[{\"op\":\"replace\",\"path\":[],\"node\":{\"tag\":\"ul\",\"props\":[],\"children\":[{\"text\":\"x\"},{\"text\":\"y\"}]}}]"

    Spec.it s "encodes every other patch" $ do
      Spec.assertEq
        s
        ( Wire.patches $
            Seq.fromList
              [ Patch.MkSetProperty (Seq.fromList [0]) (Text.pack "value") (Text.pack "\"q\""),
                Patch.MkRemoveProperty Seq.empty (Text.pack "hidden"),
                Patch.MkAppend (Seq.fromList [2]) . Seq.singleton . Html.text $ Text.pack "z",
                Patch.MkTruncate (Seq.fromList [2]) 1,
                Patch.MkArrange (Seq.fromList [3]) $ Seq.fromList [Patch.MkKept 1, Patch.MkCreated . Html.text $ Text.pack "n"] :: Patch.Patch ()
              ]
        )
        "[{\"op\":\"set\",\"path\":[0],\"name\":\"value\",\"value\":\"\\\"q\\\"\"},{\"op\":\"remove\",\"path\":[],\"name\":\"hidden\"},{\"op\":\"append\",\"path\":[2],\"nodes\":[{\"text\":\"z\"}]},{\"op\":\"truncate\",\"path\":[2],\"count\":1},{\"op\":\"arrange\",\"path\":[3],\"sources\":[{\"kept\":1},{\"node\":{\"text\":\"n\"}}]}]"

  Spec.describe s "eventPath" $ do
    Spec.it s "reads the root" $ do
      Spec.assertEq s (Wire.eventPath Text.empty) $ Just Seq.empty

    Spec.it s "reads dotted indices" $ do
      Spec.assertEq s (Wire.eventPath $ Text.pack "1.0.12") . Just $ Seq.fromList [1, 0, 12]

    Spec.it s "rejects a negative index" $ do
      Spec.assertEq s (Wire.eventPath $ Text.pack "1.-1") Nothing

    Spec.it s "rejects an empty index" $ do
      Spec.assertEq s (Wire.eventPath $ Text.pack "1..2") Nothing
