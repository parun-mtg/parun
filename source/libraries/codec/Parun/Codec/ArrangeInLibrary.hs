{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ArrangeInLibrary where

import qualified Parun.Codec.CardSelection as CardSelection
import qualified Parun.Codec.LibraryPosition as LibraryPosition
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ArrangeInLibrary as ArrangeInLibrary
import qualified Parun.Types.CardSelection as CardSelection.Type
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | @chooser@ defaults to "you", the reading of every printed "put them back in
-- any order" that names nobody (CR 608.2c), and @selection@ to the chooser's
-- order.
codec :: Codec.Codec ArrangeInLibrary.ArrangeInLibrary
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec ArrangeInLibrary.ref
  chooser <- Fields.defaulted "chooser" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec ArrangeInLibrary.chooser
  end <- Fields.defaulted "end" Nothing (Common.maybe LibraryPosition.codec) ArrangeInLibrary.end
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) ArrangeInLibrary.slot
  selection <- Fields.defaulted "selection" CardSelection.Type.ByChoice CardSelection.codec ArrangeInLibrary.selection
  pure
    ArrangeInLibrary.MkArrangeInLibrary
      { ArrangeInLibrary.ref = ref,
        ArrangeInLibrary.chooser = chooser,
        ArrangeInLibrary.end = end,
        ArrangeInLibrary.slot = slot,
        ArrangeInLibrary.selection = selection
      }
