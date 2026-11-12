module Parun.Codec.DraftNote where

import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.Color as Color
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.DraftNote as DraftNote

codec :: Codec.Codec DraftNote.DraftNote
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Number" Common.natural DraftNote.Number (\x -> case x of DraftNote.Number y -> Just y; _ -> Nothing),
      Arm.payload "Color" Color.codec DraftNote.Color (\x -> case x of DraftNote.Color y -> Just y; _ -> Nothing),
      Arm.payload "Name" CardName.codec DraftNote.Name (\x -> case x of DraftNote.Name y -> Just y; _ -> Nothing),
      Arm.payload "CreatureType" Subtype.codec DraftNote.CreatureType (\x -> case x of DraftNote.CreatureType y -> Just y; _ -> Nothing),
      Arm.payload "Player" PlayerId.codec DraftNote.Player (\x -> case x of DraftNote.Player y -> Just y; _ -> Nothing)
    ]

tagOf :: DraftNote.DraftNote -> String
tagOf x = case x of
  DraftNote.Number {} -> "Number"
  DraftNote.Color {} -> "Color"
  DraftNote.Name {} -> "Name"
  DraftNote.CreatureType {} -> "CreatureType"
  DraftNote.Player {} -> "Player"
