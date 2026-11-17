{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ExileCardsTotalling where

import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CardTally as CardTally
import qualified Parun.Codec.Comparison as Comparison
import qualified Parun.Codec.CostAmount as CostAmount
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Comparison as Comparison.Type
import qualified Parun.Types.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Types.Zone as Zone.Type

-- | A bare object keyed by the record's field names. The keyword codec is a
-- PARAMETER; see Parun.Codec.Filter's header. @comparison@, @fewest@ and
-- @zone@ default to Baron Helmut Zemo's AtLeast, 0 and Graveyard.
--
-- Unprinted: a @zone@ other than Graveyard or Battlefield. It is refused, and
-- Parun.Codec.ExileCardsTotallingSpec's "rejects a zone" proves the refusal.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ExileCardsTotalling.ExileCardsTotalling keyword)
codec keywordCodec = Fields.objectWith zoneIsPrinted $ do
  threshold <- Fields.required "threshold" CostAmount.codec ExileCardsTotalling.threshold
  comparison <- Fields.defaulted "comparison" Comparison.Type.AtLeast Comparison.codec ExileCardsTotalling.comparison
  fewest <- Fields.defaulted "fewest" 0 Common.natural ExileCardsTotalling.fewest
  tally <- Fields.required "tally" CardTally.codec ExileCardsTotalling.tally
  whichCards <- Fields.required "whichCards" (Filter.codec keywordCodec) ExileCardsTotalling.whichCards
  zone <- Fields.defaulted "zone" Zone.Type.Graveyard Zone.codec ExileCardsTotalling.zone
  pure
    ExileCardsTotalling.MkExileCardsTotalling
      { ExileCardsTotalling.threshold = threshold,
        ExileCardsTotalling.comparison = comparison,
        ExileCardsTotalling.fewest = fewest,
        ExileCardsTotalling.tally = tally,
        ExileCardsTotalling.whichCards = whichCards,
        ExileCardsTotalling.zone = zone
      }

zoneIsPrinted :: ExileCardsTotalling.ExileCardsTotalling keyword -> Either Text.Text (ExileCardsTotalling.ExileCardsTotalling keyword)
zoneIsPrinted exile
  | ExileCardsTotalling.zone exile `elem` [Zone.Type.Graveyard, Zone.Type.Battlefield] = Right exile
  | otherwise = Left (Text.pack "ExileCardsTotalling: a zone other than Graveyard or Battlefield is unprinted and not supported")
