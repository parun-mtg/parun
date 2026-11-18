{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.RollExtreme where

import qualified Parun.Codec.RollEnd as RollEnd
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.RollExtreme as RollExtreme

-- | Both keys required: neither end is the usual one, and a slot nothing names
-- binds nothing a card reads.
codec :: Codec.Codec RollExtreme.RollExtreme
codec = Fields.object $ do
  end <- Fields.required "end" RollEnd.codec RollExtreme.end
  slot <- Fields.required "slot" SlotName.codec RollExtreme.slot
  pure RollExtreme.MkRollExtreme {RollExtreme.end = end, RollExtreme.slot = slot}
