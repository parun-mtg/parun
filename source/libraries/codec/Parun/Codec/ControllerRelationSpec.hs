module Parun.Codec.ControllerRelationSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.ControllerRelation as ControllerRelation
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.SourcePlayer as SourcePlayer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ControllerRelation" $ do
  Spec.it s "You" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Related PlayerRelation.You)
      " {\"type\":\"You\"} "
  Spec.it s "AnyPlayer" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Related PlayerRelation.AnyPlayer)
      " {\"type\":\"AnyPlayer\"} "
  Spec.it s "Opponent" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Related PlayerRelation.Opponent)
      " {\"type\":\"Opponent\"} "
  Spec.it s "NamedBySource Enchanted" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.NamedBySource SourcePlayer.Enchanted)
      " {\"type\":\"NamedBySource\",\"value\":{\"type\":\"Enchanted\"}} "
  Spec.it s "NamedBySource Chosen" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.NamedBySource SourcePlayer.Chosen)
      " {\"type\":\"NamedBySource\",\"value\":{\"type\":\"Chosen\"}} "
  -- CR 601.2c: Plagiarize's "target player".
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.InSlot (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"InSlot\",\"value\":\"target\"} "
  Spec.it s "Among" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Among (Set.fromList [PlayerId.MkPlayerId 1, PlayerId.MkPlayerId 2]))
      " {\"type\":\"Among\",\"value\":[1,2]} "
  -- Exhaustive over the relation, whose arms the codec derives; see
  -- Parun.Codec.PlayerScopeSpec's twin.
  Spec.it s "speaks every relation under the relation's own tag" $
    mapM_
      (\relation -> Common.assertCodec s ControllerRelation.codec (ControllerRelation.Related relation) (" {\"type\":\"" <> show relation <> "\"} "))
      [minBound .. maxBound :: PlayerRelation.PlayerRelation]
  Spec.it s "has a schema" $
    Common.assertHasSchema s ControllerRelation.codec
  -- Filter.HeldBy's wire: a nullary arm bare, a payload arm tagged.
  Spec.it s "bare Opponent" $
    Common.assertCodec s ControllerRelation.bare (ControllerRelation.Related PlayerRelation.Opponent) " \"Opponent\" "
  Spec.it s "bare NamedBySource Enchanted" $
    Common.assertCodec s ControllerRelation.bare (ControllerRelation.NamedBySource SourcePlayer.Enchanted) " {\"type\":\"NamedBySource\",\"value\":{\"type\":\"Enchanted\"}} "
  Spec.it s "bare NamedBySource Chosen" $
    Common.assertCodec s ControllerRelation.bare (ControllerRelation.NamedBySource SourcePlayer.Chosen) " {\"type\":\"NamedBySource\",\"value\":{\"type\":\"Chosen\"}} "
  Spec.it s "bare InSlot" $
    Common.assertCodec s ControllerRelation.bare (ControllerRelation.InSlot (SlotName.MkSlotName (Text.pack "target"))) " {\"type\":\"InSlot\",\"value\":\"target\"} "
  Spec.it s "bare reads a tagged nullary arm" $
    Common.assertFromJson s (Codec.decode ControllerRelation.bare) " {\"type\":\"You\"} " (ControllerRelation.Related PlayerRelation.You)
  Spec.it s "bare has a schema" $
    Common.assertHasSchema s ControllerRelation.bare
