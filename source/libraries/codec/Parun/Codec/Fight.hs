{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Fight where

import qualified Parun.Codec.Hop as Hop
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Fight as Fight
import qualified Parun.Types.Hop as Hop

-- Both keys required: CR 701.14a's fight has no one-sided form, so a card that
-- named only one slot would be a fight with nothing.
codec :: Codec.Codec Fight.Fight
codec = Fields.object $ do
  first <- Fields.required "first" SlotName.codec Fight.first
  second <- Fields.required "second" SlotName.codec Fight.second
  hop <- Fields.defaulted "hop" Hop.Itself Hop.codec Fight.hop
  excessTally <- Fields.defaulted "excessTally" Nothing (Common.maybe SlotName.codec) Fight.excessTally
  pure
    Fight.MkFight
      { Fight.first = first,
        Fight.second = second,
        Fight.hop = hop,
        Fight.excessTally = excessTally
      }
