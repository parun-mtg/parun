-- | CR 107.4: mana symbols as printed text, "{2}{W/U}" -- what a card's mana
-- cost is ingested from, and what CR 903.4 reads off its rules text.
module Parun.Engine.ManaSymbolText where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Text.Read as Read

-- | A mana symbol's letter as the type of mana it stands for (CR 107.4).
manaType :: Char -> Maybe ManaType.ManaType
manaType c = case c of
  'W' -> Just (ManaType.Colored Color.White)
  'U' -> Just (ManaType.Colored Color.Blue)
  'B' -> Just (ManaType.Colored Color.Black)
  'R' -> Just (ManaType.Colored Color.Red)
  'G' -> Just (ManaType.Colored Color.Green)
  'C' -> Just ManaType.Colorless
  _ -> Nothing

-- | The mana symbol inside one pair of braces, "W/U" for {W/U} (CR 107.4), or
-- Nothing for a symbol that is not a mana symbol ({T}, CR 107.5).
symbol :: Text.Text -> Maybe ManaSymbol.ManaSymbol
symbol s = case Text.unpack s of
  "X" -> Just ManaSymbol.Variable
  "S" -> Just ManaSymbol.Snow
  [c] | Just m <- manaType c -> Just (ManaSymbol.OfType m)
  [c, '/', 'P'] | Just (ManaType.Colored x) <- manaType c -> Just (ManaSymbol.Phyrexian x)
  [a, '/', b, '/', 'P'] | Just (ManaType.Colored l) <- manaType a, Just (ManaType.Colored r) <- manaType b -> Just (ManaSymbol.HybridPhyrexian HybridPhyrexian.MkHybridPhyrexian {HybridPhyrexian.left = l, HybridPhyrexian.right = r})
  ['2', '/', c] | Just m <- manaType c -> Just (ManaSymbol.MonocoloredHybrid m)
  [a, '/', b] | Just l <- manaType a, Just r <- manaType b -> Just (ManaSymbol.Hybrid Hybrid.MkHybrid {Hybrid.left = l, Hybrid.right = r})
  digits | not (null digits), all Char.isDigit digits -> fmap ManaSymbol.Generic (Read.readMaybe digits)
  _ -> Nothing

-- | CR 903.4 / 903.4c: the mana symbols in rules text outside its reminder
-- text, which is parenthesized (CR 207.2a).
rulesTextSymbols :: Text.Text -> [ManaSymbol.ManaSymbol]
rulesTextSymbols = go . Text.unpack
  where
    go s = case s of
      [] -> []
      '(' : rest -> go (drop 1 (dropWhile (/= ')') rest))
      '{' : rest ->
        let (inside, after) = break (== '}') rest
         in foldMap pure (symbol (Text.pack inside)) <> go (drop 1 after)
      _ : rest -> go rest
