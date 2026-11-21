module Parun.Codec.ControllerRelation where

import qualified Data.Text as Text
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.Json.Value as Value
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonSchema.Schema as Schema
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.PlayerRelation as PlayerRelation.Type

codec :: Codec.Codec ControllerRelation.ControllerRelation
codec =
  Arm.tagged
    tagOf
    ( PlayerRelation.arms ControllerRelation.Related
        <> [ Arm.nullary "EnchantedPlayers" ControllerRelation.EnchantedPlayers,
             Arm.payload "InSlot" SlotName.codec ControllerRelation.InSlot (\x -> case x of ControllerRelation.InSlot y -> Just y; _ -> Nothing),
             Arm.payload "Among" (Common.set PlayerId.codec) ControllerRelation.Among (\x -> case x of ControllerRelation.Among y -> Just y; _ -> Nothing)
           ]
    )

tagOf :: ControllerRelation.ControllerRelation -> String
tagOf x = case x of
  ControllerRelation.Related relation -> PlayerRelation.tag relation
  ControllerRelation.EnchantedPlayers {} -> "EnchantedPlayers"
  ControllerRelation.InSlot {} -> "InSlot"
  ControllerRelation.Among {} -> "Among"

-- | 'codec' with its nullary arms written bare, Arm.enum's wire: Filter.HeldBy's
-- relation, which every card spelled as a bare PlayerRelation before the wrapper.
-- Reads the tagged object too.
bare :: Codec.Codec ControllerRelation.ControllerRelation
bare =
  let nullaryTags = fmap PlayerRelation.tag [minBound .. maxBound :: PlayerRelation.Type.PlayerRelation] <> ["EnchantedPlayers"]
   in codec
        { Codec.encode = \x -> case x of
            ControllerRelation.Related relation -> Codec.encode PlayerRelation.codec relation
            ControllerRelation.EnchantedPlayers -> Value.string "EnchantedPlayers"
            ControllerRelation.InSlot {} -> Codec.encode codec x
            ControllerRelation.Among {} -> Codec.encode codec x,
          Codec.decode = \value -> case value of
            Value.String _ -> Codec.decode codec . Common.nullary . Text.unpack =<< Common.asText value
            _ -> Codec.decode codec value,
          Codec.schema = fmap (\tagged -> Schema.oneOf [Schema.oneOf (fmap (Schema.constant . Text.pack) nullaryTags), tagged]) (Codec.schema codec)
        }
