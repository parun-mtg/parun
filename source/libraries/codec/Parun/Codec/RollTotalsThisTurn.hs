{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.RollTotalsThisTurn where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.RollTotalsThisTurn as RollTotalsThisTurn

-- | A bare object keyed by the record's field names, the quantity codec a
-- parameter for Parun.Codec.AgainstSlot's reason.
codec :: (Typeable.Typeable quantity) => Codec.Codec quantity -> Codec.Codec (RollTotalsThisTurn.RollTotalsThisTurn quantity)
codec quantityCodec = Fields.object $ do
  roller <- Fields.required "roller" PlayerRef.codec RollTotalsThisTurn.roller
  total <- Fields.required "total" quantityCodec RollTotalsThisTurn.total
  pure RollTotalsThisTurn.MkRollTotalsThisTurn {RollTotalsThisTurn.roller = roller, RollTotalsThisTurn.total = total}
