module Parun.Codec.SuperlativeSpec where

import qualified Parun.Codec.Superlative as Superlative
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerMeasure as PlayerMeasure
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Superlative as Superlative

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Superlative" $ do
  -- Damping Engine's strict lead on permanents.
  Spec.it s "PermanentsControlled, strict" $
    Common.assertCodec
      s
      Superlative.codec
      (Superlative.MkSuperlative {Superlative.measure = PlayerMeasure.PermanentsControlled, Superlative.comparison = Comparison.GreaterThan, Superlative.among = PlayerRelation.AnyPlayer})
      " {\"measure\":{\"type\":\"PermanentsControlled\"},\"comparison\":\"GreaterThan\"} "
  -- Raiders' Allegiance's "the most point counters or tied for the most".
  Spec.it s "CountersOf, ties included" $
    Common.assertCodec
      s
      Superlative.codec
      (Superlative.MkSuperlative {Superlative.measure = PlayerMeasure.CountersOf PlayerCounterKind.Point, Superlative.comparison = Comparison.AtLeast, Superlative.among = PlayerRelation.AnyPlayer})
      " {\"measure\":{\"type\":\"CountersOf\",\"value\":\"Point\"},\"comparison\":\"AtLeast\"} "
  -- Ghazb\225n Ogre's "more life than each other player".
  Spec.it s "LifeTotal, strict" $
    Common.assertCodec
      s
      Superlative.codec
      (Superlative.MkSuperlative {Superlative.measure = PlayerMeasure.LifeTotal, Superlative.comparison = Comparison.GreaterThan, Superlative.among = PlayerRelation.AnyPlayer})
      " {\"measure\":{\"type\":\"LifeTotal\"},\"comparison\":\"GreaterThan\"} "
  -- The Mighty Will Fall's "the highest life total among your opponents".
  Spec.it s "LifeTotal among opponents" $
    Common.assertCodec
      s
      Superlative.codec
      (Superlative.MkSuperlative {Superlative.measure = PlayerMeasure.LifeTotal, Superlative.comparison = Comparison.AtLeast, Superlative.among = PlayerRelation.Opponent})
      " {\"measure\":{\"type\":\"LifeTotal\"},\"comparison\":\"AtLeast\",\"among\":\"Opponent\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Superlative.codec
