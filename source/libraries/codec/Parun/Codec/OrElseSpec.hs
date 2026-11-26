module Parun.Codec.OrElseSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.OrElse as OrElse
import qualified Parun.Json.Value as Value
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.OrElse as OrElse
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

toJson :: OrElse.OrElse -> Value.Value
toJson = Codec.encode OrElse.codec

fromJson :: Value.Value -> Either Text.Text OrElse.OrElse
fromJson = Codec.decode OrElse.codec

-- One constructor, so five cases: the unmarked chooser, which is elided, a group
-- of three, a card that names somebody else, CR 701.55a's villainous flag,
-- elided the same way, and a random group drawing two, whose selection and count
-- are elided likewise when unmarked.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OrElse" $ do
  Spec.it s "a branch announced by the resolving controller writes only its siblings" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (Set.singleton (ClauseIndex.MkClauseIndex 1)) (PlayerRef.Relative PlayerRelation.You) False CardSelection.ByChoice 1)
      " {\"siblings\":[1]} "
  Spec.it s "a member of a group of three writes both its siblings" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (Set.fromList [ClauseIndex.MkClauseIndex 0, ClauseIndex.MkClauseIndex 2]) (PlayerRef.Relative PlayerRelation.You) False CardSelection.ByChoice 1)
      " {\"siblings\":[0,2]} "
  Spec.it s "and one the whole table announces writes the chooser" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (Set.singleton (ClauseIndex.MkClauseIndex 0)) (PlayerRef.Relative PlayerRelation.AnyPlayer) False CardSelection.ByChoice 1)
      " {\"siblings\":[0],\"chooser\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"}} "
  Spec.it s "CR 701.55a a villainous choice writes the flag" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (Set.singleton (ClauseIndex.MkClauseIndex 1)) (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) True CardSelection.ByChoice 1)
      " {\"siblings\":[1],\"chooser\":{\"type\":\"InSlot\",\"value\":\"target\"},\"villainous\":true} "
  Spec.it s "a group drawing two branches at random writes its selection and count" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (Set.fromList [ClauseIndex.MkClauseIndex 1, ClauseIndex.MkClauseIndex 2]) (PlayerRef.Relative PlayerRelation.You) False CardSelection.AtRandom 2)
      " {\"siblings\":[1,2],\"selection\":\"AtRandom\",\"count\":2} "
