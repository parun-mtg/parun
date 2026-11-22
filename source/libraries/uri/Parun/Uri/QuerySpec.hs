module Parun.Uri.QuerySpec where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as Char8
import qualified Parun.Extra.Builder as Builder
import qualified Parun.Spec as Spec
import qualified Parun.Uri.Query as Query

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Uri.Query" $ do
  let render = Builder.toString . Query.render . fmap (Bifunctor.bimap Char8.pack Char8.pack)
      parse = fmap (Bifunctor.bimap Char8.unpack Char8.unpack) . Query.parse . Char8.pack

  Spec.describe s "render" $ do
    Spec.it s "renders no fields as nothing" $ do
      Spec.assertEq s (render []) ""

    Spec.it s "renders each field between ampersands, a question mark first" $ do
      Spec.assertEq s (render [("scenario", "a/b c"), ("at", "3")]) "?scenario=a%2fb%20c&at=3"

    Spec.it s "renders a field with an empty value as its key" $ do
      Spec.assertEq s (render [("at", "0"), ("branch", "")]) "?at=0&branch"

  Spec.describe s "parse" $ do
    Spec.it s "reads each field, a key alone having the empty value" $ do
      Spec.assertEq s (parse "?scenario=a%2Fb+c&branch&at=3") [("scenario", "a/b c"), ("branch", ""), ("at", "3")]

    Spec.it s "reads a query without its question mark" $ do
      Spec.assertEq s (parse "a=1&&b=") [("a", "1"), ("b", "")]

    Spec.it s "reverses render" $ do
      let fields = [("decks", "saved/My deck+1&2"), ("x", "")]
      Spec.assertEq s (parse (render fields)) fields
