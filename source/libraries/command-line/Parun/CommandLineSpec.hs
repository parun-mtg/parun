module Parun.CommandLineSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import Numeric.Natural (Natural)
import qualified Parun.CommandLine as CommandLine
import qualified Parun.Spec as Spec
import qualified System.Console.GetOpt as GetOpt

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine" $ do
  Spec.describe s "options" $ do
    Spec.it s "reads each option, in order, and the positional arguments among them" $
      Spec.assertEq s (parsed ["a", "-x", "1", "b", "--why=2", "--", "-c"]) (Right (["1", "2"], ["a", "b", "-c"]))
    Spec.it s "names an unknown option" $
      Spec.assertEq s (parsed ["--zed"]) (Left "unknown option: --zed")
    Spec.it s "refuses an option missing its argument" $
      Spec.assertEq s (parsed ["--why"]) (Left "option `--why' requires an argument N")

  Spec.describe s "positionals" $ do
    Spec.it s "takes the arguments required, and up to the most allowed" $
      Spec.assertEq s (fmap (checked ["A"] (Just 2)) [["a"], ["a", "b"]]) [Right (), Right ()]
    Spec.it s "names the first argument missing" $
      Spec.assertEq s (checked ["A", "B"] Nothing ["a"]) (Left "missing argument: B")
    Spec.it s "refuses the first extra argument" $
      Spec.assertEq s (checked [] (Just 1) ["a", "b", "c"]) (Left "unexpected argument: b")
    Spec.it s "takes any number of arguments when no most is given" $
      Spec.assertEq s (checked [] Nothing ["a", "b", "c"]) (Right ())
  where
    parsed :: [String] -> Either String ([String], [String])
    parsed = Bifunctor.first shown . CommandLine.options [GetOpt.Option "x" [] (GetOpt.ReqArg id "N") "", GetOpt.Option [] ["why"] (GetOpt.ReqArg id "N") ""]
    checked :: [String] -> Maybe Natural -> [String] -> Either String ()
    checked required most = Bifunctor.first shown . CommandLine.positionals required most
    shown (Exception.SomeException e) = Exception.displayException e
