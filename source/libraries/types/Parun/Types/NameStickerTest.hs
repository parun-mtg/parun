module Parun.Types.NameStickerTest where

import qualified Parun.Types.LetterCount as LetterCount

-- | Which name stickers Parun.Types.Quantity's NameStickers counts.
data NameStickerTest
  = -- | CR 123.6: every name sticker.
    AnyNameSticker
  | -- | CR 123.6d: a name sticker whose letters number thus.
    WithLetterCount LetterCount.LetterCount
  deriving (Eq, Ord, Show)
