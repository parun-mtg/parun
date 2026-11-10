-- | CR 107.4: mana symbols as printed text, "{2}{W/U}" -- what a card's mana
-- cost is ingested from, and what CR 903.4 reads off its rules text -- and the
-- colours each symbol is (CR 202.2).
module Parun.Engine.ManaSymbolText where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
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

-- CR 107.4a / 107.4e / 107.4f: how many of these symbols are of this colour, each counted once
-- whatever else it is -- Drought's coloured symbols and Baron Helmut Zemo's
-- black mana symbols.
symbolsOfColor :: Color.Color -> [ManaSymbol.ManaSymbol] -> Int
symbolsOfColor color = length . filter (elem color . symbolColors)

-- CR 202.2b: only a coloured mana symbol carries a colour; colourless is not a
-- colour (CR 105.2c). A list, since a hybrid is all of its colours (CR 107.4e).
--
-- Here rather than beside Parun.Engine.Quantity's symbolValue because
-- Parun.Engine.Commander reads it, and Parun.Engine.Quantity reads that.
symbolColors :: ManaSymbol.ManaSymbol -> [Color.Color]
symbolColors manaSymbol = case manaSymbol of
  ManaSymbol.OfType (ManaType.Colored c) -> [c]
  ManaSymbol.OfType ManaType.Colorless -> []
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) -> Maybe.mapMaybe colorOfManaType [a, b]
  -- CR 107.4b/107.4e: a monocolored hybrid's other half is generic, so the named
  -- half is the whole contribution.
  ManaSymbol.MonocoloredHybrid t -> Maybe.maybeToList (colorOfManaType t)
  -- CR 107.4f / 202.2d: Phyrexian symbols are coloured mana symbols. Total `[c]`
  -- since Phyrexian carries a Color -- there is no colourless Phyrexian symbol.
  ManaSymbol.Phyrexian c -> [c]
  -- CR 107.4f: "a hybrid Phyrexian mana symbol is BOTH of its component
  -- colors", which CR 202.2d makes the object. Tamiyo, Compleated Sage is green
  -- and blue whichever of her {G/U/P}'s three ways paid for her.
  ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian l r) -> [l, r]
  -- CR 107.4h: snow is neither a colour nor a type of mana.
  ManaSymbol.Snow -> []
  ManaSymbol.Generic _ -> []
  -- CR 107.3 / 202.2b: {X} is a placeholder, not a coloured mana symbol, so it
  -- contributes nothing to CR 700.5's devotion whatever was announced for it.
  ManaSymbol.Variable -> []

-- CR 105.2c: colourless is not a colour, so a colourless hybrid half adds none.
colorOfManaType :: ManaType.ManaType -> Maybe Color.Color
colorOfManaType t = case t of
  ManaType.Colored c -> Just c
  ManaType.Colorless -> Nothing
