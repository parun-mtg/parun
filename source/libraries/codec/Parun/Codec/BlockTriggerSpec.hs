module Parun.Codec.BlockTriggerSpec where

import qualified Parun.Codec.BlockTrigger as BlockTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.BlockCount as BlockCount
import qualified Parun.Types.BlockRole as BlockRole
import qualified Parun.Types.BlockTrigger as BlockTrigger
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BlockTrigger" $ do
  -- Pride Guardian's "whenever this creature blocks".
  Spec.it s "the defaults write no key" $
    Common.assertCodec
      s
      BlockTrigger.codec
      (BlockTrigger.MkBlockTrigger BlockRole.Blocker Filter.IsSource BlockCount.Once Batching.EachOccurrence Nothing)
      " {\"role\":\"Blocker\"} "
  -- Venom's "whenever enchanted creature becomes blocked by a creature".
  Spec.it s "a creature and a count write their keys" $
    Common.assertCodec
      s
      BlockTrigger.codec
      (BlockTrigger.MkBlockTrigger BlockRole.Attacker (Filter.HasAttached Filter.IsSource) (BlockCount.EachCreature (Filter.And [])) Batching.EachOccurrence Nothing)
      " {\"role\":\"Attacker\",\"creature\":{\"type\":\"HasAttached\",\"value\":{\"type\":\"IsSource\"}},\"count\":{\"type\":\"EachCreature\",\"value\":{\"type\":\"And\",\"value\":[]}}} "
  -- Neyith of the Dire Hunt's "whenever one or more creatures you control ...
  -- become blocked".
  Spec.it s "a batch writes its key" $
    Common.assertCodec
      s
      BlockTrigger.codec
      (BlockTrigger.MkBlockTrigger BlockRole.Attacker (Filter.HasCardType CardType.Creature) BlockCount.Once Batching.OneOrMore Nothing)
      " {\"role\":\"Attacker\",\"creature\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"batching\":\"OneOrMore\"} "
  -- Burning Sun Cavalry's "blocks while you control a Dinosaur".
  Spec.it s "a condition writes its key" $
    Common.assertCodec
      s
      BlockTrigger.codec
      (BlockTrigger.MkBlockTrigger BlockRole.Blocker Filter.IsSource BlockCount.Once Batching.EachOccurrence (Just (Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)))))
      " {\"role\":\"Blocker\",\"while\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":0},\"comparison\":\"Exactly\",\"threshold\":{\"type\":\"Literal\",\"value\":0}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockTrigger.codec
