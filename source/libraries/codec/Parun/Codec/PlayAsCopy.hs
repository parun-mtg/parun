{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayAsCopy where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayAsCopy as PlayAsCopy

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec PlayAsCopy.PlayAsCopy
codec = Fields.object $ do
  cards <- Fields.required "cards" (Filter.codec Keyword.codec) PlayAsCopy.cards
  original <- Fields.required "original" (Filter.codec Keyword.codec) PlayAsCopy.original
  pure
    PlayAsCopy.MkPlayAsCopy
      { PlayAsCopy.cards = cards,
        PlayAsCopy.original = original
      }
