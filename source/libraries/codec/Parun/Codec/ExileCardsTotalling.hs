{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ExileCardsTotalling where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CardTally as CardTally
import qualified Parun.Codec.Filter as Filter
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ExileCardsTotalling as ExileCardsTotalling

-- | A bare object keyed by the record's field names. The keyword codec is a
-- PARAMETER; see Parun.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ExileCardsTotalling.ExileCardsTotalling keyword)
codec keywordCodec = Fields.object $ do
  threshold <- Fields.required "threshold" Common.natural ExileCardsTotalling.threshold
  tally <- Fields.required "tally" CardTally.codec ExileCardsTotalling.tally
  whichCards <- Fields.required "whichCards" (Filter.codec keywordCodec) ExileCardsTotalling.whichCards
  pure ExileCardsTotalling.MkExileCardsTotalling {ExileCardsTotalling.threshold = threshold, ExileCardsTotalling.tally = tally, ExileCardsTotalling.whichCards = whichCards}
