{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TappedToPay where

import qualified Parun.Codec.KeywordFamily as KeywordFamily
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.TappedToPay as TappedToPay

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec TappedToPay.TappedToPay
codec = Fields.object $ do
  permanent <- Fields.required "permanent" ObjectId.codec TappedToPay.permanent
  keyword <- Fields.required "keyword" KeywordFamily.codec TappedToPay.keyword
  pure
    TappedToPay.MkTappedToPay
      { TappedToPay.permanent = permanent,
        TappedToPay.keyword = keyword
      }
