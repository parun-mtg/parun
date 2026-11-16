module Parun.Codec.SacrificeTriggerSpec where

import qualified Parun.Codec.SacrificeTrigger as SacrificeTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerActsOn as PlayerActsOn
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SacrificeTrigger as SacrificeTrigger

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SacrificeTrigger" $ do
  -- Mayhem Devil's "whenever a player sacrifices a permanent".
  Spec.it s "EachOccurrence writes no batching key" $
    Common.assertCodec
      s
      SacrificeTrigger.codec
      (SacrificeTrigger.MkSacrificeTrigger (PlayerActsOn.MkPlayerActsOn PlayerRelation.AnyPlayer (Filter.And [])) Batching.EachOccurrence)
      " {\"player\":\"AnyPlayer\",\"filter\":{\"type\":\"And\",\"value\":[]}} "
  -- Camellia, the Seedmiser's "whenever you sacrifice one or more Foods".
  Spec.it s "OneOrMore writes its key" $
    Common.assertCodec
      s
      SacrificeTrigger.codec
      (SacrificeTrigger.MkSacrificeTrigger (PlayerActsOn.MkPlayerActsOn PlayerRelation.You (Filter.And [])) Batching.OneOrMore)
      " {\"player\":\"You\",\"filter\":{\"type\":\"And\",\"value\":[]},\"batching\":\"OneOrMore\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SacrificeTrigger.codec
