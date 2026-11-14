-- The one comparison both sides of the Oracle check go through: a card's
-- stamped text and what Parun.Oracle renders for it. Everything this absorbs
-- goes unchecked, so it absorbs only what the DSL does not record: reminder
-- text, the order of lines and of a line's comma-separated pieces, letter
-- case, and how the card names itself. 'withoutFlavorWords' absorbs, on the
-- stamped side alone, one more thing the DSL does not record.
module Parun.Oracle.Normalise where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Types.AbilityWord as AbilityWord
import qualified Parun.Types.CardName as CardName

-- | Oracle text as a multiset of lowercase pieces: reminder text dropped (CR
-- 207.2a), self-reference unified ('selfReferences'), and each line split at
-- commas and semicolons, so "Flying, vigilance" and "Flying\nVigilance" agree.
-- A semicolon separates keywords too, as older Oracle text writes a keyword
-- line ("Haste; horsemanship", Lu Bu, Master-at-Arms).
normalise :: CardName.CardName -> Text.Text -> [Text.Text]
normalise name =
  List.sort
    . filter (not . Text.null)
    . fmap Text.strip
    . concatMap (Text.splitOn (Text.pack ", "))
    . concatMap (Text.splitOn (Text.pack "; "))
    . Text.lines
    . selfReferences name
    . Text.toLower
    . withoutReminders

-- | Lowercase text with every way a card names itself written "~": its name,
-- the part of a legendary name before the comma or, for a one-word name
-- before " the " or " of ", that word (CR 201.5c: "Arcanis" for Arcanis the
-- Omnipotent, "Kiora" for Kiora of Fire and Ashes), and "this creature" and
-- its siblings, which Oracle text uses
-- for the same thing on newer cards (CR 700.7). A named character's "he",
-- "she" or "him" is written "it", the pronoun the renderer gives the source
-- (Edgar, Ancient Bloodlord's "He gains menace"; Scarlet Spider, Kaine's
-- "on him").
selfReferences :: CardName.CardName -> Text.Text -> Text.Text
selfReferences name text =
  let full = Text.toLower (CardName.unwrap name)
      short = Text.takeWhile (/= ',') full
      epithet separator =
        let (before, after) = Text.breakOn (Text.pack separator) full
         in if Text.null after || Text.any (== ' ') before then Text.empty else before
      nouns = fmap (\noun -> Text.pack ("this " <> noun)) ["creature", "spell", "artifact", "enchantment", "land", "permanent", "card", "planeswalker", "aura", "equipment", "vehicle"]
      named = List.foldl' (\t needle -> replaceWord needle (Text.pack "~") t) text ([full, short, epithet " the ", epithet " of "] <> nouns)
   in List.foldl' (\t pronoun -> replaceWord pronoun (Text.pack "it") t) named (fmap Text.pack ["he", "she", "him"])

-- | Every occurrence of the needle standing as whole words replaced, so a card
-- named Shock leaves "shockwave" alone.
replaceWord :: Text.Text -> Text.Text -> Text.Text -> Text.Text
replaceWord needle replacement haystack =
  let wordy = maybe False Char.isAlphaNum
      final = fmap snd . Text.unsnoc
      go previous rest = case Text.breakOn needle rest of
        (prefix, match)
          | Text.null match -> prefix
          | otherwise ->
              let after = Text.drop (Text.length needle) match
                  before = Monad.mplus (final prefix) previous
                  bounded = not (wordy before) && not (wordy (fmap fst (Text.uncons after)))
                  kept = if bounded then replacement else needle
               in prefix <> kept <> go (final needle) after
   in if Text.null needle then haystack else go Nothing haystack

-- | The text with every parenthesised span removed, nested ones included.
withoutReminders :: Text.Text -> Text.Text
withoutReminders = Text.pack . go (0 :: Int) . Text.unpack
  where
    go depth string = case string of
      [] -> []
      '(' : rest -> go (depth + 1) rest
      ')' : rest | depth > 0 -> go (depth - 1) rest
      c : rest -> if depth > 0 && c /= '\n' then go depth rest else c : go depth rest

-- | Oracle text with the flavor word heading each line or mode dropped (CR
-- 207.2d): what precedes a line's first " \8212 ", past a mode's bullet (The
-- Vision), when that is a phrase (it opens with a letter and holds no sentence
-- or reminder punctuation) that is neither an ability word (CR 207.2c), which
-- the renderer prints, nor a chapter symbol's numeral (CR 715.2). Only the stamped side goes through this, so a heading the
-- renderer prints that is not an ability word fails the check until it is
-- exempted here.
withoutFlavorWords :: Text.Text -> Text.Text
withoutFlavorWords = Text.intercalate (Text.pack "\n") . fmap unheaded . Text.splitOn (Text.pack "\n")
  where
    unheaded line = case Text.stripPrefix bullet line of
      Just mode -> bullet <> unbulleted mode
      Nothing -> unbulleted line
    unbulleted line = case Text.breakOn dash line of
      (heading, rest) | not (Text.null rest), flavorWord heading -> Text.drop (Text.length dash) rest
      _ -> line
    bullet = Text.pack "\8226 "
    dash = Text.pack " \8212 "
    flavorWord heading =
      maybe False (Char.isAlpha . fst) (Text.uncons heading)
        && not (Text.any (`elem` ".,:;(\"{") heading)
        && not (Text.all (`elem` "IVX") heading)
        && Set.notMember (key heading) abilityWords
    key = Text.toLower . Text.filter Char.isAlphaNum
    abilityWords = Set.fromList (fmap (key . Text.pack . show) [minBound .. maxBound :: AbilityWord.AbilityWord])
