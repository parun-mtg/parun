{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.LegalityTable where

import qualified Parun.Codec.Legality as Legality
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.LegalityTable as LegalityTable

-- | A bare object keyed by the record's field names, data/legalities.json.
codec :: Codec.Codec LegalityTable.LegalityTable
codec = Fields.object $ do
  date <- Fields.required "date" Common.text LegalityTable.date
  cards <- Fields.required "cards" (Common.textMap id Right (Common.textMap id Right Legality.codec)) LegalityTable.cards
  pure LegalityTable.MkLegalityTable {LegalityTable.date = date, LegalityTable.cards = cards}
