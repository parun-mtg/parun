module Parun.Play.GeneratorSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Parun.Extra.Word64 as Word64
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Key as Key
import qualified Parun.Spec as Spec
import qualified Parun.Types.Generator as Generator.Type
import qualified Parun.Types.Key as Key.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Play.Generator" $ do
  Spec.describe s "quarterRound" $ do
    Spec.it s "matches RFC 8439 section 2.1.1" $ do
      Spec.assertEq s (Foldable.toList (Generator.quarterRound (Seq.fromList [0x11111111, 0x01020304, 0x9b8d6f43, 0x01234567]) (0, 1, 2, 3))) [0xea2a92f4, 0xcb1cf8ce, 0x4581472e, 0x5881c4bb]

    Spec.it s "matches RFC 8439 section 2.2.1, touching only its four words" $ do
      let sample = Seq.fromList [0x879531e0, 0xc5ecf37d, 0x516461b1, 0xc9a62f8a, 0x44c20ef3, 0x3390af7f, 0xd9fc690b, 0x2a5f714c, 0x53372767, 0xb00a5631, 0x974c541a, 0x359e9963, 0x5c971061, 0x3d631689, 0x2098d9d6, 0x91dbd320] :: Seq.Seq Word.Word32
      Spec.assertEq s (Foldable.toList (Generator.quarterRound sample (2, 7, 8, 13))) [0x879531e0, 0xc5ecf37d, 0xbdb886dc, 0xc9a62f8a, 0x44c20ef3, 0x3390af7f, 0xd9fc690b, 0xcfacafd2, 0xe46bea80, 0xb00a5631, 0x974c541a, 0x359e9963, 0x5c971061, 0xccc07c79, 0x2098d9d6, 0x91dbd320]

  Spec.describe s "block" $ do
    Spec.it s "matches RFC 8439 section 2.3.2" $ do
      key <- maybe (Spec.assertFailure s "not a key") pure (Key.fromHex (Text.pack "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
      Spec.assertEq s (Generator.block key 1 (0x09000000, 0x4a000000, 0)) [0xe4e7f110, 0x15593bd1, 0x1fdd0f50, 0xc47120a3, 0xc7f4d1c7, 0x0368c033, 0x9aaa2204, 0x4e6cd4c3, 0x466482d2, 0x09aa9f07, 0x05d7c214, 0xa2028bd9, 0xd19c12b5, 0xb94e16de, 0xe883d0cb, 0x4e3c50a2]

    Spec.it s "matches RFC 8439 appendix A.1's test vector 1" $ do
      Spec.assertEq s (Generator.block (keyed 0) 0 (0, 0, 0)) [0xade0b876, 0x903df1a0, 0xe56a5d40, 0x28bd8653, 0xb819d2bd, 0x1aed8da0, 0xccef36a8, 0xc70d778b, 0x7c5941da, 0x8d485751, 0x3fe02477, 0x374ad8b8, 0xf4b8436a, 0x1ca11815, 0x69b687c3, 0x8665eeb2]

    Spec.it s "matches RFC 8439 appendix A.1's test vector 3" $ do
      key <- maybe (Spec.assertFailure s "not a key") pure (Key.fromHex (Text.pack (replicate 62 '0' <> "01")))
      Spec.assertEq s (Generator.block key 1 (0, 0, 0)) [0x2452eb3a, 0x9249f8ec, 0x8d829d9b, 0xddd4ceb1, 0xe8252083, 0x60818b01, 0xf38422b8, 0x5aaa49c9, 0xbb00ca8e, 0xda3ba7b4, 0xc4b592d1, 0xfdf2732f, 0x4436274e, 0x2561b3c8, 0xebdd4aa6, 0xa0136c00]

    Spec.it s "matches RFC 8439 appendix A.1's test vector 4" $ do
      key <- maybe (Spec.assertFailure s "not a key") pure (Key.fromHex (Text.pack ("00ff" <> replicate 60 '0')))
      Spec.assertEq s (Generator.block key 2 (0, 0, 0)) [0xfb4dd572, 0x4bc42ef1, 0xdf922636, 0x327f1394, 0xa78dea8f, 0x5e269039, 0xa1bebbc1, 0xcaf09aae, 0xa25ab213, 0x48a6b46c, 0x1b9d9bcb, 0x092c5be6, 0x546ca624, 0x1bec45d5, 0x87f47473, 0x96f0992e]

  Spec.describe s "next" $ do
    Spec.it s "draws the keystream eight bytes at a time, block after block (RFC 8439 appendix A.1's test vectors 1 and 2)" $ do
      Spec.assertEq s (from (keyed 0) (Monad.replicateM 9 Generator.next)) [0x903df1a0ade0b876, 0x28bd8653e56a5d40, 0x1aed8da0b819d2bd, 0xc70d778bccef36a8, 0x8d4857517c5941da, 0x374ad8b83fe02477, 0x1ca11815f4b8436a, 0x8665eeb269b687c3, 0x7a385155bee7079f]

  Spec.describe s "derive" $ do
    Spec.it s "gives each index a key of its own, none the key itself" $ do
      let derived = fmap (Generator.derive (keyed 0)) [0 .. 99]
      Spec.assertEq s (Set.size (Set.fromList (fmap Key.toHex (keyed 0 : derived)))) 101

    Spec.it s "depends on the key" $ do
      Spec.assertBool s (Generator.derive (keyed 0) 0 /= Generator.derive (keyed 1) 0) "differs"

    Spec.it s "is no part of the keystream the key itself draws" $ do
      let drawn = Seq.fromList (take 8 (Generator.block (keyed 0) 0 (0, 0, 0)))
      Spec.assertBool s (Key.Type.unwrap (Generator.derive (keyed 0) 0) /= drawn) "differs from the key's first block"

  Spec.describe s "between" $ do
    Spec.it s "draws again rather than favour a low remainder" $ do
      -- (2^64 - n) mod n for n = 2^63 + 1, below which a draw is rejected.
      let size = 9223372036854775809 :: Word.Word64
          rejected = negate size `mod` size
          key = Maybe.fromMaybe (keyed 0) (List.find (\k -> from k Generator.next < rejected) (fmap keyed [0 .. 63]))
          second = from key (Generator.next >> Generator.next)
      Spec.assertBool s (from key Generator.next < rejected) "the first draw is rejected"
      Spec.assertEq s (from key (Generator.between 0 9223372036854775808)) (Word64.toNatural (mod second size))

    Spec.it s "stays within its bounds" $ do
      let drawn = from (keyed 0) (Monad.replicateM 200 (Generator.between 3 5))
      Spec.assertEq s (Set.toList (Set.fromList drawn)) [3, 4, 5]

    Spec.it s "answers its one value when the bounds meet" $ do
      Spec.assertEq s (from (keyed 0) (Generator.between 7 7)) 7

  Spec.describe s "pick" $ do
    Spec.it s "reaches every option" $ do
      let picked = from (keyed 0) (Monad.replicateM 100 (Generator.pick ('a' NonEmpty.:| "bc")))
      Spec.assertEq s (Set.toList (Set.fromList picked)) "abc"

  Spec.describe s "shuffle" $ do
    Spec.it s "is a permutation" $ do
      let shuffled = from (keyed 0) (Generator.shuffle [1 .. 20 :: Int])
      Spec.assertEq s (List.sort shuffled) [1 .. 20]
      Spec.assertBool s (shuffled /= [1 .. 20]) "moved"

    Spec.it s "reaches every order of three about equally often" $ do
      let orders = from (keyed 0) (Monad.replicateM 6000 (Generator.shuffle "abc"))
          counts = Map.fromListWith (+) (fmap (\o -> (o, 1 :: Int)) orders)
      Spec.assertEq s (Map.size counts) 6
      Spec.assertBool s (all (\n -> n > 900 && n < 1100) counts) ("even: " <> show counts)

    Spec.it s "depends on the key" $ do
      Spec.assertBool s (from (keyed 0) (Generator.shuffle [1 .. 20 :: Int]) /= from (keyed 1) (Generator.shuffle [1 .. 20])) "differs"

-- | The key whose first word is this and every other word zero.
keyed :: Word.Word32 -> Key.Type.Key
keyed n = Key.Type.UnsafeKey (Seq.fromList (n : replicate 7 0))

from :: Key.Type.Key -> State.State Generator.Type.Generator a -> a
from key action = State.evalState action (Generator.fromKey key)
