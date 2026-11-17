module Parun.Codec.NameStickerTest where

import qualified Parun.Codec.LetterCount as LetterCount
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.NameStickerTest as NameStickerTest

codec :: Codec.Codec NameStickerTest.NameStickerTest
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "AnyNameSticker" NameStickerTest.AnyNameSticker,
      Arm.payload "WithLetterCount" LetterCount.codec NameStickerTest.WithLetterCount (\x -> case x of NameStickerTest.WithLetterCount y -> Just y; _ -> Nothing)
    ]

tagOf :: NameStickerTest.NameStickerTest -> String
tagOf x = case x of
  NameStickerTest.AnyNameSticker {} -> "AnyNameSticker"
  NameStickerTest.WithLetterCount {} -> "WithLetterCount"
