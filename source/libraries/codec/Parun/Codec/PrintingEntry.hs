{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PrintingEntry where

import qualified Data.Set as Set
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PrintingEntry as PrintingEntry

-- | An object of the card and, for an Attraction alone, its lights.
codec :: Codec.Codec PrintingEntry.PrintingEntry
codec = Fields.object $ do
  card <- Fields.required "card" Common.text PrintingEntry.card
  lights <- Fields.defaulted "lights" Set.empty (Common.set Common.natural) PrintingEntry.lights
  pure PrintingEntry.MkPrintingEntry {PrintingEntry.card = card, PrintingEntry.lights = lights}
