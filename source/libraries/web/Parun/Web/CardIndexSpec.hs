module Parun.Web.CardIndexSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.Card as Codec.Card
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Web.CardIndex as CardIndex

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.CardIndex" $ do
  Spec.describe s "search" $ do
    Spec.it s "matches a name, any case" $ do
      Spec.assertEq s (names (CardIndex.search (typed "bolt") pool)) ["Lightning Bolt"]

    Spec.it s "matches rules text, any case" $ do
      Spec.assertEq s (names (CardIndex.search (typed "DEALS 3") pool)) ["Lightning Bolt"]

    Spec.it s "orders name prefixes, then names, then rules text" $ do
      Spec.assertEq s (names (CardIndex.search (typed "elf") pool)) ["Elf Replica", "Wood Elf", "Elvish Champion"]

    Spec.it s "keeps cards within the chosen colours, colourless ones included" $ do
      let query = CardIndex.everything {CardIndex.wanted = Set.singleton Color.Green}
      Spec.assertEq s (names (CardIndex.search query pool)) ["Elf Replica", "Elvish Champion", "Llanowar Elves", "Wood Elf"]

    Spec.it s "keeps cards of any chosen type" $ do
      let query = CardIndex.everything {CardIndex.kinds = Set.fromList [CardType.Instant, CardType.Artifact]}
      Spec.assertEq s (names (CardIndex.search query pool)) ["Elf Replica", "Gruul Signet", "Lightning Bolt"]

    Spec.it s "keeps cards of the chosen mana value, the last bucket counting 7 and up" $ do
      let at n = names (CardIndex.search CardIndex.everything {CardIndex.valued = Just n} pool)
      Spec.assertEq s (at 1, at 7) (["Lightning Bolt", "Llanowar Elves", "Wood Elf"], ["Big One", "Bigger One"])

    Spec.it s "caps the results and counts every match" $ do
      let many = Seq.fromList [entry ("Card " <> show n) [] 1 [] "" | n <- [1 .. 61 :: Int]]
          (count, shown) = CardIndex.search CardIndex.everything many
      Spec.assertEq s (count, Seq.length shown) (61, CardIndex.cap)

    Spec.it s "finds a split card by either half's name" $ do
      let split = (entry "Fire // Ice" [Color.Red, Color.Blue] 2 [CardType.Instant] "") {CardIndex.faceNames = [Text.pack "Fire", Text.pack "Ice"]}
      Spec.assertEq s (names (CardIndex.search (typed "ice") (Seq.singleton split))) ["Fire // Ice"]

    Spec.it s "matches nothing for text no card holds" $ do
      Spec.assertEq s (CardIndex.search (typed "zzzz") pool) (0, Seq.empty)

  Spec.describe s "entryOf" $ do
    Spec.it s "indexes a traditional card" $ do
      Spec.assertEq s (fmap (CardName.unwrap . CardIndex.name) (CardIndex.entryOf (card False "Instant"))) (Just (Text.pack "Probe"))

    Spec.it s "names a transforming card by its front face, as a deck list does" $ do
      let json = "{\"faces\":[{\"name\":\"Front\",\"typeLine\":{\"types\":[\"Creature\"]}},{\"name\":\"Back\",\"typeLine\":{\"types\":[\"Creature\"]}}],\"layout\":{\"type\":\"Transforming\"}}"
      Spec.assertEq s (fmap (CardName.unwrap . CardIndex.name) (CardIndex.entryOf (decoded json))) (Just (Text.pack "Front"))

    Spec.it s "leaves out a synthetic card" $ do
      Spec.assertEq s (fmap CardIndex.name (CardIndex.entryOf (card True "Instant"))) Nothing

    Spec.it s "CR 108.2a leaves out a nontraditional card" $ do
      Spec.assertEq s (fmap CardIndex.name (CardIndex.entryOf (card False "Plane"))) Nothing

    Spec.it s "CR 315.3 leaves out a conspiracy card" $ do
      Spec.assertEq s (fmap CardIndex.name (CardIndex.entryOf (card False "Conspiracy"))) Nothing
  where
    names :: (Int, Seq.Seq CardIndex.Entry) -> [String]
    names = fmap (Text.unpack . CardName.unwrap . CardIndex.name) . Foldable.toList . snd
    typed :: String -> CardIndex.Query
    typed t = CardIndex.everything {CardIndex.text = Text.pack t}
    pool :: Seq.Seq CardIndex.Entry
    pool =
      Seq.fromList
        [ entry "Lightning Bolt" [Color.Red] 1 [CardType.Instant] "Lightning Bolt deals 3 damage to any target.",
          entry "Llanowar Elves" [Color.Green] 1 [CardType.Creature] "{T}: Add {G}.",
          entry "Elvish Champion" [Color.Green] 3 [CardType.Creature] "Other Elf creatures get +1/+1.",
          entry "Wood Elf" [Color.Green] 1 [CardType.Creature] "",
          entry "Elf Replica" [] 3 [CardType.Artifact, CardType.Creature] "",
          entry "Gruul Signet" [Color.Red, Color.Green] 2 [CardType.Artifact] "",
          entry "Big One" [Color.Red] 7 [CardType.Creature] "",
          entry "Bigger One" [Color.Red] 9 [CardType.Creature] ""
        ]

-- | An entry as the index would build it, for the tests.
entry :: String -> [Color.Color] -> Integer -> [CardType.CardType] -> String -> CardIndex.Entry
entry name colours value kinds rules =
  CardIndex.MkEntry
    { CardIndex.name = CardName.MkCardName (Text.pack name),
      CardIndex.faceNames = [Text.pack name],
      CardIndex.cost = Nothing,
      CardIndex.value = value,
      CardIndex.colours = Set.fromList colours,
      CardIndex.types = Set.fromList kinds,
      CardIndex.typeLine = Text.unwords (fmap (Text.pack . show) kinds),
      CardIndex.rules = [Text.pack rules | not (null rules)],
      CardIndex.folded = Text.toCaseFold (Text.unwords [Text.pack name, Text.pack rules])
    }

-- | A one-face card named Probe, of the one type given, synthetic or not.
card :: Bool -> String -> Card.Card
card synthetic kind =
  let json = "{\"faces\":[{\"name\":\"Probe\",\"typeLine\":{\"types\":[\"" <> kind <> "\"]}}],\"synthetic\":" <> (if synthetic then "true" else "false") <> "}"
   in decoded json

-- | A card from its JSON.
decoded :: String -> Card.Card
decoded json = Maybe.fromMaybe (error ("undecodable card: " <> json)) $ do
  value <- either (const Nothing) Just (Common.parse (Text.pack json))
  either (const Nothing) Just (Codec.decode Codec.Card.codec value)
