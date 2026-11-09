-- | ChaCha20 (RFC 8439 section 2.3), the generator chance and the random bot
-- draw from: a cryptographically secure one, so no draw seen predicts another.
-- Saved games record outcomes, never a key, so nothing outside a running game
-- depends on which generator this is.
module Parun.Play.Generator where

import qualified Control.Monad.Trans.State.Strict as State
import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Word as Word
import Numeric.Natural (Natural)
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Extra.Word32 as Word32
import qualified Parun.Extra.Word64 as Word64
import qualified Parun.Types.Generator as Generator
import qualified Parun.Types.Key as Key

-- | A generator at the start of the key's keystream.
fromKey :: Key.Key -> Generator.Generator
fromKey key = Generator.MkGenerator {Generator.key = key, Generator.block = 0, Generator.unused = []}

-- | RFC 8439 section 2.3's block function: sixteen words of keystream from a
-- key, a block counter and a nonce of three words.
block :: Key.Key -> Word.Word32 -> (Word.Word32, Word.Word32, Word.Word32) -> [Word.Word32]
block key counter (n0, n1, n2) =
  let initial = Seq.fromList [0x61707865, 0x3320646e, 0x79622d32, 0x6b206574] <> Key.unwrap key <> Seq.fromList [counter, n0, n1, n2]
      mixed = Foldable.foldl' (\s _ -> doubleRound s) initial (replicate 10 ())
   in Foldable.toList (Seq.zipWith (+) mixed initial)

-- | RFC 8439 section 2.3's inner block: a column round, then a diagonal one.
doubleRound :: Seq.Seq Word.Word32 -> Seq.Seq Word.Word32
doubleRound state =
  Foldable.foldl'
    quarterRound
    state
    [(0, 4, 8, 12), (1, 5, 9, 13), (2, 6, 10, 14), (3, 7, 11, 15), (0, 5, 10, 15), (1, 6, 11, 12), (2, 7, 8, 13), (3, 4, 9, 14)]

-- | RFC 8439 section 2.2's quarter round on the state's words at these indices.
quarterRound :: Seq.Seq Word.Word32 -> (Int, Int, Int, Int) -> Seq.Seq Word.Word32
quarterRound state (i, j, k, l) = Maybe.fromMaybe state $ do
  a0 <- Seq.lookup i state
  b0 <- Seq.lookup j state
  c0 <- Seq.lookup k state
  d0 <- Seq.lookup l state
  let a1 = a0 + b0
      d1 = Bits.rotateL (Bits.xor d0 a1) 16
      c1 = c0 + d1
      b1 = Bits.rotateL (Bits.xor b0 c1) 12
      a2 = a1 + b1
      d2 = Bits.rotateL (Bits.xor d1 a2) 8
      c2 = c1 + d2
      b2 = Bits.rotateL (Bits.xor b1 c2) 7
  pure (Seq.update i a2 (Seq.update j b2 (Seq.update k c2 (Seq.update l d2 state))))

-- | A key of its own for each index, made from this one: the first half of
-- its block 0 under a nonce holding the index and a last word of 1, where
-- every block 'next' draws has a last word of 0.
derive :: Key.Key -> Word.Word64 -> Key.Key
derive key index =
  let (low, high) = Word64.toWord32s index
   in Key.UnsafeKey (Seq.fromList (take 8 (block key 0 (low, high, 1))))

-- | The next 64 bits: the keystream's next eight bytes, little-endian. Block
-- @n@ is the RFC's under counter @n@'s low word and a nonce of its high word
-- and two zeros.
next :: (Monad m) => State.StateT Generator.Generator m Word.Word64
next = do
  generator <- State.get
  case Generator.unused generator of
    low : high : rest -> do
      State.put generator {Generator.unused = rest}
      pure (Word32.toWord64 low .|. Bits.shiftL (Word32.toWord64 high) 32)
    _ -> do
      let (low, high) = Word64.toWord32s (Generator.block generator)
      State.put
        generator
          { Generator.block = Generator.block generator + 1,
            Generator.unused = block (Generator.key generator) low (high, 0, 0)
          }
      next

-- | Uniform from least to greatest, both included; least when they cross. A
-- range wider than 2^64 is drawn from its first 2^64 values.
between :: (Monad m) => Natural -> Natural -> State.StateT Generator.Generator m Natural
between least greatest
  | greatest <= least = pure least
  | otherwise =
      fmap ((+ least) . Word64.toNatural) $
        maybe next below (Natural.toWord64 (greatest - least + 1))

-- | Uniform below a size of at least one. A draw under 2^64 mod the size would
-- favour the low remainders, so it is drawn again.
below :: (Monad m) => Word.Word64 -> State.StateT Generator.Generator m Word.Word64
below size = do
  x <- next
  if x < negate size `mod` size then below size else pure (x `mod` size)

-- | One of the options, each equally likely.
pick :: (Monad m) => NonEmpty.NonEmpty a -> State.StateT Generator.Generator m a
pick options = do
  i <- between 0 (Natural.length options - 1)
  pure (Maybe.fromMaybe (NonEmpty.head options) (Maybe.listToMaybe (List.genericDrop i (NonEmpty.toList options))))

-- | Every order equally likely: Fisher and Yates, swapping each position from
-- the last down with one at or below it.
shuffle :: (Monad m) => [a] -> State.StateT Generator.Generator m [a]
shuffle xs = fmap Foldable.toList (go (Seq.length items - 1) items)
  where
    items = Seq.fromList xs
    go i acc
      | i < 1 = pure acc
      | otherwise = do
          j <- fmap Natural.toIntSaturating (between 0 (Int.toNaturalSaturating i))
          go (i - 1) (swap i j acc)
    swap i j acc = Maybe.fromMaybe acc $ do
      a <- Seq.lookup i acc
      b <- Seq.lookup j acc
      pure (Seq.update i b (Seq.update j a acc))
