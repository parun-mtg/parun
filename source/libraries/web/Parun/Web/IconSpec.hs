module Parun.Web.IconSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Parun.Spec as Spec
import qualified Parun.Web.Icon as Icon

spec :: Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Web.Icon"
  . Spec.it s "the sprite in index.html draws exactly the icons"
  $ do
    page <- ByteString.readFile "source/web/index.html" >>= either (fail . show) pure . Encoding.decodeUtf8'
    let drawn = Set.fromList [name | rest <- drop 1 (Text.splitOn (Text.pack "<symbol id=\"") page), let name = Text.takeWhile (/= '"') rest, Text.pack "ui-" `Text.isPrefixOf` name]
    Spec.assertEq s drawn (Set.fromList (fmap Icon.iconId [minBound .. maxBound]))
