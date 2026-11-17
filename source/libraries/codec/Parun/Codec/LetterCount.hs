{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.LetterCount where

import qualified Parun.Codec.Comparison as Comparison
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.LetterCount as LetterCount

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec LetterCount.LetterCount
codec = Fields.object $ do
  comparison <- Fields.required "comparison" Comparison.codec LetterCount.comparison
  letters <- Fields.required "letters" Common.natural LetterCount.letters
  pure
    LetterCount.MkLetterCount
      { LetterCount.comparison = comparison,
        LetterCount.letters = letters
      }
