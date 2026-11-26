{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChooseNumber where

import qualified Parun.Codec.CardSelection as CardSelection
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CardSelection as CardSelection.Type
import qualified Parun.Types.ChooseNumber as ChooseNumber
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | The bound is ELIDED when absent, as Draw's slot is, the chooser when it
-- is CR 608.2c's default, you, and the selection when it is a choice.
codec :: Codec.Codec ChooseNumber.ChooseNumber
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec ChooseNumber.slot
  upTo <- Fields.defaulted "upTo" Nothing (Common.maybe Quantity.codec) ChooseNumber.upTo
  chooser <- Fields.defaulted "chooser" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec ChooseNumber.chooser
  selection <- Fields.defaulted "selection" CardSelection.Type.ByChoice CardSelection.codec ChooseNumber.selection
  pure
    ChooseNumber.MkChooseNumber
      { ChooseNumber.slot = slot,
        ChooseNumber.upTo = upTo,
        ChooseNumber.chooser = chooser,
        ChooseNumber.selection = selection
      }
