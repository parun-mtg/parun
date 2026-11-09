module Parun.Types.Generator where

import qualified Data.Word as Word
import qualified Parun.Types.Key as Key

-- | ChaCha20's state (RFC 8439 section 2.3), which chance and the random bot
-- draw from.
data Generator = MkGenerator
  { key :: Key.Key,
    -- | The number of the next block of keystream to make.
    block :: Word.Word64,
    -- | The current block's words not drawn yet, the next first.
    unused :: [Word.Word32]
  }
  deriving (Eq)

-- | Redacted but for the block: the key and unused words predict every draw.
instance Show Generator where
  show g = "MkGenerator {block = " <> show (block g) <> ", ..}"
