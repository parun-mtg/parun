module Parun.Benchmark.HarnessSpec where

import qualified Data.Text as Text
import qualified Parun.Benchmark.Config as Config
import qualified Parun.Benchmark.Harness as Harness
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Benchmark.Harness" $ do
  Spec.describe s "selected" $ do
    let benchmark = Harness.whnf "legal actions 40" id ()
        config ps = Config.initial {Config.patterns = fmap Text.pack ps}
    Spec.it s "keeps every name when no pattern is given" $
      Spec.assertBool s (Harness.selected (config []) benchmark) "expected a match"
    Spec.it s "keeps a name holding any pattern" $
      Spec.assertBool s (Harness.selected (config ["payable", "actions 4"]) benchmark) "expected a match"
    Spec.it s "drops a name lacking every pattern" $
      Spec.assertBool s (not $ Harness.selected (config ["payable", "goldfish"]) benchmark) "expected no match"

  Spec.describe s "fitTimes" $ do
    Spec.it s "fits exactly linear timings with no residual" $
      Spec.assertEq s (Harness.fitTimes 100 200) (100, 0)
    Spec.it s "sums the squared residuals of noisy timings" $
      -- fit = (100 + 2 * 250) / 5 = 120; (100 - 120)^2 + (250 - 240)^2 = 500
      Spec.assertEq s (Harness.fitTimes 100 250) (120, 500)

  Spec.describe s "converged" $ do
    let at t1 t2 = (Harness.MkMeasurement t1 Nothing, Harness.MkMeasurement t2 Nothing)
    Spec.it s "accepts a long enough run within 5%" $
      Spec.assertBool s (uncurry Harness.converged (at 10000000 20000000)) "expected convergence"
    Spec.it s "refuses a run too short to time" $
      Spec.assertBool s (not . uncurry Harness.converged $ at 1000 2000) "expected no convergence"
    Spec.it s "refuses a spread over 5%" $
      Spec.assertBool s (not . uncurry Harness.converged $ at 10000000 30000000) "expected no convergence"

  Spec.describe s "estimate" $ do
    Spec.it s "divides the fit by the iterations, in picoseconds" $
      Spec.assertEq
        s
        (Harness.estimate (Text.pack "x") 4 (Harness.MkMeasurement 100 (Just 40)) (Harness.MkMeasurement 250 (Just 80)))
        Harness.MkResult
          { Harness.label = Text.pack "x",
            Harness.meanPicoseconds = 30000,
            Harness.stdevPicoseconds = 5500,
            Harness.allocatedBytes = Just 10
          }
    Spec.it s "reports no allocations without RTS statistics" $
      Spec.assertEq
        s
        (Harness.allocatedBytes $ Harness.estimate (Text.pack "x") 1 (Harness.MkMeasurement 1 Nothing) (Harness.MkMeasurement 2 Nothing))
        Nothing

  Spec.describe s "squareRoot" $ do
    Spec.it s "floors" $
      Spec.assertEq s (fmap Harness.squareRoot [0, 1, 2, 3, 4, 15, 16, 17, 500]) [0, 1, 1, 1, 2, 3, 4, 4, 22]

  Spec.describe s "renderScaled" $ do
    Spec.it s "picks the largest unit reached" $
      Spec.assertEq s (Harness.renderScaled Harness.picosecondUnits 12345678000) "12.345 ms"
    Spec.it s "pads the fraction" $
      Spec.assertEq s (Harness.renderScaled Harness.byteUnits 1005) "1.005 KB"
    Spec.it s "renders zero in the smallest unit" $
      Spec.assertEq s (Harness.renderScaled Harness.picosecondUnits 0) "0 ps"

  Spec.describe s "resultCodec" $ do
    Spec.it s "writes allocations when known" $
      Common.assertJsonCodec
        s
        (Codec.encode Harness.resultCodec)
        (Codec.decode Harness.resultCodec)
        (Harness.MkResult (Text.pack "goldfish 2p") 3 1 (Just 7))
        "{\"name\":\"goldfish 2p\",\"meanPicoseconds\":3,\"stdevPicoseconds\":1,\"allocatedBytes\":7}"
    Spec.it s "omits allocations when unknown" $
      Common.assertJsonCodec
        s
        (Codec.encode Harness.resultCodec)
        (Codec.decode Harness.resultCodec)
        (Harness.MkResult (Text.pack "goldfish 2p") 3 1 Nothing)
        "{\"name\":\"goldfish 2p\",\"meanPicoseconds\":3,\"stdevPicoseconds\":1}"
