-- | CR 123.6a: the words of a name as name stickers count them, and CR
-- 123.6d-e's letter and vowel reads. Pure text, no game state.
module Parun.Engine.NameWords where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Types.CardName as CardName

-- | CR 123.6a: a blank line, a run of underscores, is not a word.
isBlank :: Text.Text -> Bool
isBlank token = not (Text.null token) && Text.all (== '_') token

-- | CR 123.6a: each maximal run of non-space characters, blanks left out;
-- "_____-o-saurus" is one hyphenated word.
wordsOf :: CardName.CardName -> [Text.Text]
wordsOf = filter (not . isBlank) . Text.words . CardName.unwrap

-- | CR 123.6a: how many words the name has.
wordCount :: CardName.CardName -> Natural
wordCount = Natural.length . wordsOf

-- | CR 123.6b-c: @word@ after the first @k@ words, or at the end of a name
-- with fewer. A blank is no word and stays (CR 123.6a); which side of it the
-- word lands on changes no word's position and nothing reads it.
insertAfter :: Natural -> Text.Text -> CardName.CardName -> CardName.CardName
insertAfter k word name =
  let go :: Natural -> [Text.Text] -> [Text.Text]
      go n tokens = case (n, tokens) of
        (0, _) -> word : tokens
        (_, []) -> [word]
        (_, token : rest) -> token : go (if isBlank token then n else n - 1) rest
   in CardName.MkCardName (Text.unwords (go k (Text.words (CardName.unwrap name))))

-- | CR 123.6d: how many of the text's characters are one of these letters,
-- either case.
letterCount :: Text.Text -> Text.Text -> Natural
letterCount letters text =
  let wanted = Set.fromList (Text.unpack (Text.toLower letters))
   in Natural.length (filter (\c -> Set.member c wanted) (Text.unpack (Text.toLower text)))

-- | CR 123.6d: the letters a "choose a letter" offers, A through Z.
alphabet :: [Char]
alphabet = ['A' .. 'Z']

-- | CR 123.6d: a letter's 1-based position in the alphabet, either case.
letterPosition :: Char -> Maybe Natural
letterPosition c = fmap fst (List.find ((== Char.toUpper c) . snd) (zip [1 ..] alphabet))

-- | CR 123.6d: whether the text's first letter, spaces and punctuation
-- ignored (_____ _____ Rocketship's ruling), is the one at this position.
beginsWithLetterAt :: Natural -> Text.Text -> Bool
beginsWithLetterAt n text = (Text.find Char.isLetter text >>= letterPosition) == Just n

-- | CR 123.6d: how many letters the text holds, either case; a space,
-- hyphen or apostrophe is none.
letterTotal :: Text.Text -> Natural
letterTotal = Natural.length . filter Char.isLetter . Text.unpack

-- | CR 123.6e: how many different vowels, A, E, I, O, U and Y, either case.
uniqueVowels :: Text.Text -> Natural
uniqueVowels text = Natural.length (Set.filter (\c -> elem c "aeiouy") (Set.fromList (Text.unpack (Text.toLower text))))
