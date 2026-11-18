module Parun.Web.ManaSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Parun.Play.Render as Render
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlanarDieFace as PlanarDieFace
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Web.Mana" $ do
  Spec.it s "the sprite in index.html draws exactly the motifs" $ do
    page <- ByteString.readFile "source/web/index.html" >>= either (fail . show) pure . Encoding.decodeUtf8'
    let drawn = Set.fromList [Text.takeWhile (/= '"') rest | rest <- drop 1 (Text.splitOn (Text.pack "<symbol id=\"") page)]
    Spec.assertEq s drawn (Set.fromList (fmap Mana.motifId [minBound .. maxBound]))

  Spec.describe s "drawing" $ do
    let motif = Mana.MkMotif
        part tint = Mana.MkPart (Text.pack tint)
    Spec.it s "CR 107.4a a coloured symbol is its motif on its colour" $
      Spec.assertEq s (Mana.drawing (Text.pack "G")) (Mana.MkWhole (part "g" (motif Mana.MkGreen)))
    Spec.it s "CR 107.4b a number is a pill" $
      Spec.assertEq s (Mana.drawing (Text.pack "12")) (Mana.MkPill (Text.pack "12"))
    Spec.it s "CR 107.4e a hybrid symbol splits its circle between its halves" $ do
      Spec.assertEq s (Mana.drawing (Text.pack "W/U")) (Mana.MkSplit (part "w" (motif Mana.MkWhite)) (part "u" (motif Mana.MkBlue)))
      Spec.assertEq s (Mana.drawing (Text.pack "2/B")) (Mana.MkSplit (part "generic" (Mana.MkLetters (Text.pack "2"))) (part "b" (motif Mana.MkBlack)))
      Spec.assertEq s (Mana.drawing (Text.pack "C/R")) (Mana.MkSplit (part "c" (motif Mana.MkColorless)) (part "r" (motif Mana.MkRed)))
    Spec.it s "CR 107.4f a Phyrexian symbol is the Phyrexian motif on its colour, split for a hybrid" $ do
      Spec.assertEq s (Mana.drawing (Text.pack "R/P")) (Mana.MkWhole (part "r" (motif Mana.MkPhyrexian)))
      Spec.assertEq s (Mana.drawing (Text.pack "G/U/P")) (Mana.MkSplit (part "g" (motif Mana.MkPhyrexian)) (part "u" (motif Mana.MkPhyrexian)))
    Spec.it s "CR 107.4g the bare Phyrexian symbol has no colour" $
      Spec.assertEq s (Mana.drawing (Text.pack "H")) (Mana.MkWhole (part "generic" (motif Mana.MkPhyrexian)))
    Spec.it s "CR 107.18 a lone P is the pawprint" $
      Spec.assertEq s (Mana.drawing (Text.pack "P")) (Mana.MkWhole (part "generic" (motif Mana.MkPawprint)))
    Spec.it s "a half symbol is the left half of its colour's circle" $
      Spec.assertEq s (Mana.drawing (Text.pack "HW")) (Mana.MkHalf (part "w" (motif Mana.MkWhite)))
    Spec.it s "a symbol with no motif is its letters in a pill" $
      Spec.assertEq s (Mana.drawing (Text.pack "W/Q")) (Mana.MkPill (Text.pack "WQ"))

  Spec.describe s "spoken" $ do
    Spec.it s "names each part" $
      Spec.assertEq s (fmap (Mana.spoken . Text.pack) ["T", "3", "W/U", "2/G", "B/P", "R/W/P", "H", "P", "HR", "TK"]) (fmap Text.pack ["tap", "3 generic", "white or blue", "2 generic or green", "Phyrexian black", "Phyrexian red or white", "Phyrexian", "pawprint", "half red", "ticket"])

  Spec.describe s "rich" $ do
    let page = foldMap Html.toText . Mana.rich . Text.pack
    Spec.it s "draws a hybrid symbol's two halves from the sprite" $ do
      let drawn = page "{W/U}"
      Spec.assertBool s (all ((`Text.isInfixOf` drawn) . Text.pack) ["class=\"bg-w\"", "class=\"bg-u\"", "href=\"#ms-w\"", "href=\"#ms-u\"", "aria-label=\"white or blue\""]) (Text.unpack drawn)
    Spec.it s "CR 107.7 draws a loyalty cost by its direction" $ do
      Spec.assertBool s (Text.pack "<span class=\"loyalty up\" title=\"[+2]\" role=\"img\" aria-label=\"loyalty plus 2\">+2</span>: Draw a card." `Text.isPrefixOf` page "[+2]: Draw a card.") "gain"
      Spec.assertBool s (Text.pack "<span class=\"loyalty down\" title=\"[\x2212X]\" role=\"img\" aria-label=\"loyalty minus X\">\x2212X</span>" `Text.isPrefixOf` page "[\x2212X]: Destroy it.") "loss"
      Spec.assertBool s (Text.pack "<span class=\"loyalty level\"" `Text.isPrefixOf` page "[0]: Scry 1.") "zero"
    Spec.it s "leaves other brackets and braces as text" $
      Spec.assertEq s (page "[X] and [+] and {} and { G}") (Text.pack "[X] and [+] and {} and { G}")

    Spec.it s "CR 107.11 and 107.12 the log draws the planar die's symbols" $ do
      Spec.assertBool s (Text.pack "href=\"#ms-chaos\"" `Text.isInfixOf` page (Text.unpack (Render.planarFace PlanarDieFace.Chaos))) "chaos"
      Spec.assertBool s (Text.pack "href=\"#ms-planeswalker\"" `Text.isInfixOf` page (Text.unpack (Render.planarFace PlanarDieFace.Planeswalker))) "planeswalker"
