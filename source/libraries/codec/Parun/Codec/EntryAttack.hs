module Parun.Codec.EntryAttack where

import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.EntryAttack as EntryAttack

-- | CR 508.4's readings: chosen as it enters, specified by the effect as an
-- object's subject or as a player, or CR 702.116a's choice narrowed to one
-- player's subjects.
codec :: Codec.Codec EntryAttack.EntryAttack
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Chosen" EntryAttack.Chosen,
      Arm.payload "SameAs" SlotName.codec EntryAttack.SameAs (\x -> case x of EntryAttack.SameAs y -> Just y; _ -> Nothing),
      Arm.payload "UnderPlayer" SlotName.codec EntryAttack.UnderPlayer (\x -> case x of EntryAttack.UnderPlayer y -> Just y; _ -> Nothing),
      Arm.payload "ThatPlayer" SlotName.codec EntryAttack.ThatPlayer (\x -> case x of EntryAttack.ThatPlayer y -> Just y; _ -> Nothing)
    ]

tagOf :: EntryAttack.EntryAttack -> String
tagOf x = case x of
  EntryAttack.Chosen {} -> "Chosen"
  EntryAttack.SameAs {} -> "SameAs"
  EntryAttack.UnderPlayer {} -> "UnderPlayer"
  EntryAttack.ThatPlayer {} -> "ThatPlayer"
