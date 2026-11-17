{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Fight where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.Hop as Hop
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Fight as Fight
import qualified Parun.Types.Hop as Hop

-- Both slot keys required: CR 701.14a's fight has no one-sided form, so a card
-- that named only one slot would be a fight with nothing.
codec :: Codec.Codec Fight.Fight
codec = Fields.objectWith pairedAlone $ do
  first <- Fields.required "first" SlotName.codec Fight.first
  second <- Fields.required "second" SlotName.codec Fight.second
  hop <- Fields.defaulted "hop" Hop.Itself Hop.codec Fight.hop
  excessTally <- Fields.defaulted "excessTally" Nothing (Common.maybe SlotName.codec) Fight.excessTally
  paired <- Fields.defaulted "paired" False Common.boolean Fight.paired
  pure
    Fight.MkFight
      { Fight.first = first,
        Fight.second = second,
        Fight.hop = hop,
        Fight.excessTally = excessTally,
        Fight.paired = paired
      }

-- Unprinted: a paired fight tallying excess damage, whose second creature is
-- several. Refused, and Parun.Codec.FightSpec's "rejects a paired tally"
-- proves it.
pairedAlone :: Fight.Fight -> Either Text.Text Fight.Fight
pairedAlone fight
  | Fight.paired fight && Maybe.isJust (Fight.excessTally fight) = Left (Text.pack "Fight: paired beside excessTally is unprinted and not supported")
  | otherwise = Right fight
