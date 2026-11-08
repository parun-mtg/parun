{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CommanderColors where

import qualified Parun.Codec.Color as Color
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CommanderColors as CommanderColors

-- | A bare object keyed by the record's field names; the tag that picks it is
-- Parun.Codec.Quantity's "CommanderColors".
codec :: Codec.Codec CommanderColors.CommanderColors
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec CommanderColors.player
  colors <- Fields.required "colors" (Common.set Color.codec) CommanderColors.colors
  pure CommanderColors.MkCommanderColors {CommanderColors.player = player, CommanderColors.colors = colors}
