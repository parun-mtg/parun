module Parun.Play.FitSpec where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Play.Fit as Fit
import qualified Parun.Spec as Spec
import qualified Parun.Types.Features as Features
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Position as Position
import qualified Parun.Types.Weights as Weights

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Play.Fit" $ do
  Spec.describe s "rowsOf" $ do
    Spec.it s "reads back the rows parun match --positions writes, each with its game and winner" $ do
      let position = Position.MkPosition {Position.turn = 3, Position.features = Map.fromList [(one, Features.MkFeatures 20 7 0 0 1 1), (two, Features.MkFeatures 17 6 3 2 2 3)]}
      Spec.assertEq s (Fit.rowsOf (Text.unlines [Fit.header, Fit.row (Text.pack "abc") two position])) (Right [(Text.pack "abc", two, position)])

    Spec.it s "refuses a file without the header, and a line that is not a row" $ do
      Spec.assertBool s (Either.isLeft (Fit.rowsOf (Text.pack "abc\t1\t1\n"))) "no header"
      Spec.assertBool s (Either.isLeft (Fit.rowsOf (Text.unlines [Fit.header, Text.pack "abc\t1\t3"]))) "a seat no game has, and no features"
      Spec.assertBool s (Either.isLeft (Fit.rowsOf (Text.unlines [Fit.header, Text.pack "abc\t1\t1\t20\t7"]))) "too few features"
      Spec.assertBool s (Either.isLeft (Fit.rowsOf (Text.unlines [Fit.header, Text.intercalate (Text.pack "\t") (Text.pack "abc" : replicate 15 (Text.pack "1"))]))) "too many features"

  Spec.describe s "newton" $ do
    Spec.it s "finds the maximum-likelihood coefficient: a feature of 1 winning three games in four is worth log 3" $ do
      let samples :: [Fit.Sample]
          samples = replicate 3 (1, True, [1]) <> [(1, False, [1])] <> replicate 3 (1, False, [-1]) <> [(1, True, [-1])]
      Spec.assertBool s (all (\beta -> abs (beta - log 3) < 1e-4) (Fit.newton 0 samples)) (show (Fit.newton 0 samples))

    Spec.it s "counts a sample as often as its weight" $ do
      let samples :: [Fit.Sample]
          samples = [(3, True, [1]), (1, False, [1]), (3, False, [-1]), (1, True, [-1])]
      Spec.assertBool s (all (\beta -> abs (beta - log 3) < 1e-4) (Fit.newton 0 samples)) (show (Fit.newton 0 samples))

    Spec.it s "holds the coefficients to Weights.neutral's shape, every one alike, under a large ridge" $ do
      -- The first feature wins three games in four, the second one in two.
      let samples :: [Fit.Sample]
          samples = [(3, True, [1, 0]), (1, False, [1, 0]), (3, False, [-1, 0]), (1, True, [-1, 0]), (1, True, [0, 1]), (1, False, [0, 1]), (1, True, [0, -1]), (1, False, [0, -1])]
      case (Fit.newton 0 samples, Fit.newton 1e4 samples) of
        ([first, second], [first', second']) -> do
          Spec.assertBool s (abs (first - log 3) < 1e-4 && abs second < 1e-4) ("unpenalized: " <> show (first, second))
          Spec.assertBool s (abs (first' - second') < 1e-3 && first' > 0.1) ("penalized: " <> show (first', second'))
        other -> Spec.assertFailure s (show other)

  Spec.describe s "samplesOf" $ do
    Spec.it s "weighs each game 1 in all, merging a position it holds twice, from each seat's side" $ do
      let at life = Position.MkPosition {Position.turn = 1, Position.features = Map.fromList [(one, Features.MkFeatures life 0 0 0 0 0), (two, Features.MkFeatures 20 0 0 0 0 0)]}
          rows = [(Text.pack "a", one, at 18), (Text.pack "a", one, at 18), (Text.pack "b", two, at 15)]
      Spec.assertEq s (List.sort [(w, won) | (w, won, _) <- Fit.samplesOf Fit.plain rows]) [(1, False), (1, False), (1, True), (1, True)]

  Spec.describe s "weighted" $ do
    Spec.it s "scores a unit of the fit's sum half a win, as a cut-off playout's expected result rises where a win is even odds" $ do
      let weights = Fit.weighted ([log 3, 0, 0, 0, 0, 0] <> [0, 0, 0, 0, 0, -1])
      Spec.assertEqWith s "ours" (Weights.ours weights) (Features.MkFeatures 549 0 0 0 0 0)
      Spec.assertEqWith s "theirs" (Weights.theirs weights) (Features.MkFeatures 0 0 0 0 0 (-500))
      Spec.assertEqWith s "read back" (Fit.weighted (Fit.unweighted weights)) weights

  Spec.describe s "clock" $ do
    Spec.it s "counts the attacks a player's power needs to take the opponent's life to 0, at most 20" $ do
      Spec.assertEqWith s "7 life at 3 power" (Fit.clock (Features.MkFeatures 20 0 3 3 0 1) (Features.MkFeatures 7 0 0 0 0 0)) 3
      Spec.assertEqWith s "no power" (Fit.clock (Features.MkFeatures 20 0 0 0 0 0) (Features.MkFeatures 7 0 0 0 0 0)) 20

  Spec.describe s "fit" $ do
    Spec.it s "weights the feature that tells the winner, beating Weights.neutral on held-out games" $ do
      -- Three cards more in hand win four games in five; three permanents
      -- more lose as often, which a weight of 1 on each cannot tell apart.
      let game k =
            let (mine, ahead) = if even k then (Features.MkFeatures 20 5 0 0 1 1, if mod k 10 == 0 then two else one) else (Features.MkFeatures 20 2 0 0 1 4, if mod k 10 == 1 then one else two)
                position = Position.MkPosition {Position.turn = 1, Position.features = Map.fromList [(one, mine), (two, Features.MkFeatures 20 2 0 0 1 1)]}
             in [(Text.pack ("game" <> show k), ahead, position) | _ <- [1 .. 3 :: Integer]]
      case Fit.fit (concatMap game [1 .. 40 :: Integer]) of
        Left problem -> Spec.assertFailure s (Text.unpack problem)
        Right fitted -> do
          Spec.assertLtWith s "log loss" (Fit.fittedLoss fitted) (Fit.neutralLoss fitted)
          Spec.assertGtWith s "a card in hand" (Features.hand (Weights.ours (Fit.weights fitted))) 0

    Spec.it s "scores Weights.neutral better at its best scale than as it stands, a unit worth 1 of a win's 1000" $ do
      -- Five life more win four games in five.
      let game k =
            let position = Position.MkPosition {Position.turn = 1, Position.features = Map.fromList [(one, Features.MkFeatures 20 2 0 0 1 1), (two, Features.MkFeatures 15 2 0 0 1 1)]}
             in [(Text.pack ("game" <> show k), if mod k 5 == 0 then two else one, position)]
      case Fit.fit (concatMap game [1 .. 40 :: Integer]) of
        Left problem -> Spec.assertFailure s (Text.unpack problem)
        Right fitted -> Spec.assertLtWith s "log loss" (Fit.neutralLoss fitted) (Fit.standingLoss fitted)

    Spec.it s "refuses positions too few to hold a game out" $ do
      let position = Position.MkPosition {Position.turn = 1, Position.features = Map.fromList [(one, Features.MkFeatures 20 7 0 0 0 0), (two, Features.MkFeatures 20 7 0 0 0 0)]}
      Spec.assertBool s (Either.isLeft (Fit.fit [(Text.pack "only", one, position)])) "one game"
  where
    one = PlayerId.MkPlayerId 0
    two = PlayerId.MkPlayerId 1
