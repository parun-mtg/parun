{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChooseGroup where

import qualified Parun.Codec.Grouping as Grouping
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChooseGroup as ChooseGroup
import qualified Parun.Types.PlayerRef as PlayerRef.Type
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec ChooseGroup.ChooseGroup
codec = Fields.object $ do
  among <- Fields.required "among" Grouping.codec ChooseGroup.among
  slot <- Fields.required "slot" SlotName.codec ChooseGroup.slot
  about <- Fields.defaulted "about" Nothing (Common.maybe ObjectRef.codec) ChooseGroup.about
  chooser <- Fields.defaulted "chooser" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec ChooseGroup.chooser
  pure
    ChooseGroup.MkChooseGroup
      { ChooseGroup.among = among,
        ChooseGroup.slot = slot,
        ChooseGroup.about = about,
        ChooseGroup.chooser = chooser
      }
