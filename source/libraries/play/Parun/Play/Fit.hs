-- | `parun fit`: the search bot's evaluation weights (Parun.Types.Weights)
-- fitted to how games end, by a logistic regression of each position's
-- winner on both players' features (Search.featureFields), the positions
-- those `parun match --positions` writes.
module Parun.Play.Fit where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TextIO
import qualified Numeric
import Numeric.Natural (Natural)
import qualified Parun.Codec.Weights as Weights.Codec
import qualified Parun.CommandLine as CommandLine
import qualified Parun.CommandLine.Completion.Command as Completion.Command
import qualified Parun.CommandLine.Completion.Value as Value
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Search as Search
import qualified Parun.Types.Features as Features
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Position as Position
import qualified Parun.Types.Weights as Weights
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: [String] -> IO ()
main arguments = do
  (flags, files) <- CommandLine.configured usage (helped . fst) (fromArguments arguments)
  contents <- traverse TextIO.readFile files
  case traverse rowsOf contents of
    Left problem -> TextIO.hPutStrLn IO.stderr problem >> Exit.exitFailure
    Right parsed -> case fit (concat parsed) of
      Left problem -> TextIO.hPutStrLn IO.stderr problem >> Exit.exitFailure
      Right fitted -> do
        mapM_ TextIO.putStrLn (report fitted)
        Monad.forM_ [path | Out path <- flags] $ \path -> do
          ByteString.writeFile path (Encoding.encodeUtf8 (Common.render (Codec.encode Weights.Codec.codec (weights fitted)) <> Text.pack "\n"))
          IO.hPutStrLn IO.stderr ("Saved " <> path <> ".")

-- | The flags on a command line and the positions files it names; throws
-- at an unknown option, or at no file unless the usage is asked for.
fromArguments :: (Catch.MonadThrow m) => [String] -> m ([Flag], [FilePath])
fromArguments arguments = do
  (flags, files) <- CommandLine.options options arguments
  Monad.unless (helped flags) $ CommandLine.positionals ["POSITIONS"] Nothing files
  pure (flags, files)

-- | Whether the last help flag asks for the usage.
helped :: [Flag] -> Bool
helped flags = List.foldl' (\_ flag -> flag) False [shown | Help shown <- flags]

-- | One flag on the command line.
data Flag
  = Help Bool
  | Out FilePath
  deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
  [ GetOpt.Option "h" ["help"] (GetOpt.NoArg $ Help True) "print this usage and exit",
    GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) "",
    GetOpt.Option [] ["out"] (GetOpt.ReqArg Out "FILE") "write the fitted weights to this file (Parun.Codec.Weights)"
  ]

usage :: String
usage = GetOpt.usageInfo "usage: parun fit [OPTION...] POSITIONS..." options

-- | `parun fit` as tab completion sees it.
command :: Completion.Command.Command
command = (Completion.Command.new "fit" options) {Completion.Command.values = [("out", Value.MkFiles)], Completion.Command.positionals = [Value.MkFiles]}

-- | The first line of a positions file: the game, the turn, the winning
-- seat, then each seat's features.
header :: Text.Text
header = Text.intercalate (Text.pack "\t") ([Text.pack "game", Text.pack "turn", Text.pack "winner"] <> [name <> Text.pack (show n) | n <- seats, (name, _) <- Search.featureFields])

-- | A position of the game `key` that `winner` won, as a line of a positions
-- file.
row :: Text.Text -> PlayerId.PlayerId -> Position.Position -> Text.Text
row key winner position =
  Text.intercalate (Text.pack "\t") $
    [key, Text.pack (show (Position.turn position)), Text.pack (show (seatOf winner))]
      <> [Text.pack (show (field (Map.findWithDefault absent (Command.idOf n) (Position.features position)))) | n <- seats, (_, field) <- Search.featureFields]
  where
    absent = Features.MkFeatures 0 0 0 0 0 0

-- | The positions of a file 'header' heads, each with its game and winner;
-- Left naming the first line that is not a 'row'.
rowsOf :: Text.Text -> Either Text.Text [(Text.Text, PlayerId.PlayerId, Position.Position)]
rowsOf contents = case Text.lines contents of
  first : rest | first == header -> traverse (\line -> maybe (Left (Text.pack "not a position: " <> line)) Right (parsed line)) (filter (not . Text.null) rest)
  _ -> Left (Text.pack "not a positions file: its first line is not the header parun match --positions writes")
  where
    parsed line = case Text.splitOn (Text.pack "\t") line of
      key : turn : winner : counts -> do
        turnNumber <- Read.readMaybe (Text.unpack turn)
        seat <- Monad.mfilter (`elem` seats) (Read.readMaybe (Text.unpack winner))
        numbers <- traverse (Read.readMaybe . Text.unpack) counts
        Monad.guard (length numbers == width * length seats)
        features <- Monad.zipWithM (\n chunk -> (,) (Command.idOf n) <$> featuresFrom chunk) seats (chunked numbers)
        pure (key, Command.idOf seat, Position.MkPosition {Position.turn = turnNumber, Position.features = Map.fromList features})
      _ -> Nothing
    width = length Search.featureFields
    chunked numbers = [take width (drop (width * k) numbers) | k <- [0 .. length seats - 1]]

-- | The features of a 'row's counts, in Search.featureFields's order.
featuresFrom :: [Integer] -> Maybe Features.Features
featuresFrom counts = case counts of
  [life, hand, power, toughness, lands, permanents] -> Just (Features.MkFeatures life hand power toughness lands permanents)
  _ -> Nothing

-- | The seats a positions file has.
seats :: [Natural]
seats = [1, 2]

-- | A player's seat, as Command.idOf numbers them.
seatOf :: PlayerId.PlayerId -> Natural
seatOf pid = PlayerId.unwrap pid + 1

-- | What a fit found: how many positions and games it read, the log loss of
-- each evaluation on games held out of its fit ('crossLoss'), the ridge
-- chosen and the weights fitted to every game.
data Fitted = MkFitted
  { positions :: Natural,
    games :: Natural,
    -- | Weights.neutral's as it stands, read as 'weighted' writes weights.
    standingLoss :: Double,
    -- | Weights.neutral's, at the scale fitting the training games best.
    neutralLoss :: Double,
    fittedLoss :: Double,
    -- | The fit's with each player's damage clock ('clocked') added.
    clockedLoss :: Double,
    -- | The ridge cross-validation chose among 'ridges' for the weights.
    ridge :: Double,
    weights :: Weights.Weights
  }
  deriving (Eq, Show)

-- | One side of a position: its weight, whether the side won, and its
-- features less its opponent's.
type Sample = (Double, Bool, [Double])

-- | The weights fitted to every game's positions under the ridge
-- cross-validation chooses ('crossValidated'), and each evaluation's log
-- loss on held-out games; Left for positions from fewer than two games.
fit :: [(Text.Text, PlayerId.PlayerId, Position.Position)] -> Either Text.Text Fitted
fit rows = do
  let count = Natural.length (Set.fromList [key | (key, _, _) <- rows])
      summed samples = [(w, won, [sum x]) | (w, won, x) <- samples]
      fitted with kept = newton (crossValidated with kept) (samplesOf with kept)
      chosen = crossValidated plain rows
  Monad.when (count < 2) $ Left (Text.pack "too few games to hold any out: fit needs positions from at least two games")
  pure
    MkFitted
      { positions = Natural.length rows,
        games = count,
        standingLoss = logLoss (unweighted Weights.neutral) (samplesOf plain rows),
        neutralLoss = crossLoss (newton 0 . summed . samplesOf plain) (summed . samplesOf plain) rows,
        fittedLoss = crossLoss (fitted plain) (samplesOf plain) rows,
        clockedLoss = crossLoss (fitted clocked) (samplesOf clocked) rows,
        ridge = chosen,
        weights = weighted (newton chosen (samplesOf plain rows))
      }

-- | The games of every `k`th fifth, by the order of their keys.
fifth :: Integer -> [(Text.Text, PlayerId.PlayerId, Position.Position)] -> Set.Set Text.Text
fifth k rows = Set.fromList [key | (n, key) <- zip [0 :: Integer ..] (Set.toAscList (Set.fromList [key | (key, _, _) <- rows])), mod n 5 == k]

-- | The log loss of each fifth of the games under the coefficients `train`
-- fits to the other four, pooled over the five; each game held out once.
crossLoss :: ([(Text.Text, PlayerId.PlayerId, Position.Position)] -> [Double]) -> ([(Text.Text, PlayerId.PlayerId, Position.Position)] -> [Sample]) -> [(Text.Text, PlayerId.PlayerId, Position.Position)] -> Double
crossLoss train samples rows =
  let held k =
        let within = fifth k rows
            (inside, outside) = List.partition (\(key, _, _) -> Set.member key within) rows
         in if null inside || null outside then (0, 0) else lossTotal (train outside) (samples inside)
      totals = fmap held [0 .. 4]
   in sum (fmap fst totals) / max 1e-12 (sum (fmap snd totals))

-- | Of 'ridges', the one under which the fit to four fifths of the games
-- predicts the other fifth best ('crossLoss'), the first on a tie.
crossValidated :: (Features.Features -> Features.Features -> [Double]) -> [(Text.Text, PlayerId.PlayerId, Position.Position)] -> Double
crossValidated with rows =
  let scored = [(crossLoss (newton candidate . samplesOf with) (samplesOf with) rows, candidate) | candidate <- ridges]
   in maybe 0 snd (Maybe.listToMaybe (List.sortOn fst scored))

-- | The ridges cross-validation chooses among, from none to one holding the
-- fit to Weights.neutral's shape.
ridges :: [Double]
ridges = [0, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 1e4]

-- | The samples of the rows, each game weighted 1 in all however many
-- positions it holds, since a game's positions share one result, and alike
-- samples merged.
samplesOf :: (Features.Features -> Features.Features -> [Double]) -> [(Text.Text, PlayerId.PlayerId, Position.Position)] -> [Sample]
samplesOf with rows =
  let counts = Map.fromListWith (+) [(key, 1 :: Integer) | (key, _, _) <- rows]
      merged = Map.fromListWith (+) [((won, x), 1 / Integer.toDouble (Map.findWithDefault 1 key counts)) | (key, winner, position) <- rows, (won, x) <- examples with winner position]
   in [(w, won, x) | ((won, x), w) <- Map.toList merged]

-- | A position's examples, one from each seat's side: whether the seat won,
-- and its features ('plain' or 'clocked') less its opponent's, the shape
-- Search.evaluate weighs.
examples :: (Features.Features -> Features.Features -> [Double]) -> PlayerId.PlayerId -> Position.Position -> [(Bool, [Double])]
examples with winner position = case Map.toList (Position.features position) of
  [(one, mine), (two, theirs)] -> [(one == winner, with mine theirs <> fmap negate (with theirs mine)), (two == winner, with theirs mine <> fmap negate (with mine theirs))]
  _ -> []

-- | A player's features, in Search.featureFields's order.
plain :: Features.Features -> Features.Features -> [Double]
plain mine _ = [Integer.toDouble (field mine) | (_, field) <- Search.featureFields]

-- | A player's features and their damage clock ('clock'), negated so a
-- faster clock counts up as every other feature does.
clocked :: Features.Features -> Features.Features -> [Double]
clocked mine theirs = plain mine theirs <> [negate (clock mine theirs)]

-- | The turns a player's creatures need to attack their opponent's life to
-- 0, unblocked, at most 'longest'.
clock :: Features.Features -> Features.Features -> Double
clock mine theirs
  | Features.power mine <= 0 = longest
  | otherwise = min longest (Integer.toDouble (max 0 (negate (div (negate (Features.life theirs)) (Features.power mine)))))

-- | The longest damage clock counted.
longest :: Double
longest = 20

-- | The fitted coefficients as integer weights: a cut-off playout's score
-- stands for its expected result, Search.won times 2p − 1 at a win
-- probability p of the logistic of the coefficients' sum, which rises by
-- Search.won / 2 per unit of that sum where p is a half.
weighted :: [Double] -> Weights.Weights
weighted betas =
  let integral = [round (beta * Integer.toDouble Search.won / 2) :: Integer | beta <- betas]
      width = length Search.featureFields
      side = Maybe.fromMaybe (Features.MkFeatures 0 0 0 0 0 0) . featuresFrom
   in Weights.MkWeights {Weights.ours = side (take width integral), Weights.theirs = side (take width (drop width integral))}

-- | The coefficients integer weights stand for, as 'weighted' writes them.
unweighted :: Weights.Weights -> [Double]
unweighted given = [Integer.toDouble (field (side given)) * 2 / Integer.toDouble Search.won | side <- [Weights.ours, Weights.theirs], (_, field) <- Search.featureFields]

-- | The report of a fit, a line each.
report :: Fitted -> [Text.Text]
report fitted =
  [ Text.pack (show (positions fitted) <> " positions from " <> show (games fitted) <> " games, each game held out once of the fit scoring it (five folds)."),
    Text.pack ("Log loss, Weights.neutral as it stands: " <> decimal (standingLoss fitted) <> "."),
    Text.pack ("Held-out log loss, Weights.neutral at its best scale: " <> decimal (neutralLoss fitted) <> "."),
    Text.pack ("Held-out log loss, fitted: " <> decimal (fittedLoss fitted) <> "."),
    Text.pack ("Held-out log loss, fitted with a damage clock: " <> decimal (clockedLoss fitted) <> "."),
    Text.pack ("Fitted to every game at ridge " <> show (ridge fitted) <> ": ours " <> listed (Weights.ours (weights fitted)) <> ", theirs " <> listed (Weights.theirs (weights fitted)) <> ".")
  ]
  where
    decimal x = Numeric.showFFloat (Just 4) x ""
    listed features = List.intercalate ", " [Text.unpack name <> " " <> show (field features) | (name, field) <- Search.featureFields]

-- | The weighted mean negative log-likelihood of the samples' results under
-- the logistic of the coefficients times their features.
logLoss :: [Double] -> [Sample] -> Double
logLoss betas samples = let (loss, weight) = lossTotal betas samples in loss / max 1e-12 weight

-- | The weighted negative log-likelihood of the samples' results, and their
-- weight.
lossTotal :: [Double] -> [Sample] -> (Double, Double)
lossTotal betas samples =
  let one (w, won, x) = let p = logistic (dotted betas x) in w * negate (log (max tiny (if won then p else 1 - p)))
   in (sum (fmap one samples), sum [w | (w, _, _) <- samples])
  where
    tiny = 1e-12 :: Double

logistic :: Double -> Double
logistic z = 1 / (1 + exp (negate z))

dotted :: [Double] -> [Double] -> Double
dotted xs ys = sum (zipWith (*) xs ys)

-- | The coefficients of a weighted logistic regression of the results on the
-- features, with no intercept, by Newton's method from zero, their spread
-- about their mean penalized by `penalty` times the samples' weight, so a
-- large one holds them to Weights.neutral's shape, every coefficient alike,
-- and each by a trace so a feature constant at zero leaves the step solvable.
newton :: Double -> [Sample] -> [Double]
newton penalty samples = go (50 :: Integer) (fmap (const 0) columns)
  where
    weights' = [w | (w, _, _) <- samples]
    results = [if won then 1 else 0 :: Double | (_, won, _) <- samples]
    columns = List.transpose [x | (_, _, x) <- samples]
    total = sum weights'
    width = Natural.toDouble (Natural.length columns)
    -- The penalty's matrix: penalty * total * (I - J / width), plus the trace.
    penalized = [[(if i == j then penalty * total + 1e-9 * total else 0) - penalty * total / width | (j, _) <- indexed] | (i, _) <- indexed]
    indexed = zip [0 :: Integer ..] columns
    go left betas
      | left <= 0 = betas
      | otherwise =
          let predicted = fmap logistic (foldr (zipWith (+)) (fmap (const 0) weights') (zipWith (\beta column -> fmap (* beta) column) betas columns))
              residuals = zipWith3 (\w p y -> w * (p - y)) weights' predicted results
              curvatures = zipWith (\w p -> w * p * (1 - p)) weights' predicted
              gradient = zipWith (+) (fmap (dotted betas) penalized) (fmap (dotted residuals) columns)
              hessian = zipWith (zipWith (+)) penalized [[sum (zipWith3 (\c a b -> c * a * b) curvatures one two) | two <- columns] | one <- columns]
           in case solve hessian gradient of
                Nothing -> betas
                Just step ->
                  let next = zipWith (-) betas step
                   in if maximum (0 : fmap abs step) < 1e-9 then next else go (left - 1) next

-- | The solution of a square linear system, by Gaussian elimination with
-- partial pivoting; Nothing where it is singular.
solve :: [[Double]] -> [Double] -> Maybe [Double]
solve matrix vector = backward <$> forward (zipWith (\r v -> r <> [v]) matrix vector)
  where
    forward rows = case rows of
      [] -> Just []
      _ -> do
        let pivot = List.maximumBy (\a b -> compare (abs (head' a)) (abs (head' b))) rows
            rest = List.delete pivot rows
        Monad.guard (abs (head' pivot) > 1e-12)
        let eliminated = [zipWith (\x y -> x - y * (head' r / head' pivot)) (drop 1 r) (drop 1 pivot) | r <- rest]
        (pivot :) <$> forward eliminated
    head' entries = Maybe.fromMaybe 0 (Maybe.listToMaybe entries)
    backward :: [[Double]] -> [Double]
    backward = Foldable.foldr (\r solved -> maybe solved (\(coefficients, rhs) -> (rhs - dotted (drop 1 coefficients) solved) / head' coefficients : solved) (List.unsnoc r)) []
