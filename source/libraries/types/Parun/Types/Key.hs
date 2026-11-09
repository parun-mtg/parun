module Parun.Types.Key where

import qualified Data.Sequence as Seq
import qualified Data.Word as Word

-- | A ChaCha20 key (RFC 8439 section 2.3): eight words, each four bytes read
-- little-endian. Parun.Play.Key makes one, so it always has eight.
newtype Key = UnsafeKey
  { unwrap :: Seq.Seq Word.Word32
  }
  deriving (Eq)

-- | Redacted: a key predicts every draw of the game it seeds.
instance Show Key where
  show _ = "UnsafeKey <redacted>"
