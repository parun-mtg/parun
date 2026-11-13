module Parun.Test.ContextSpec where

import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Exceptions.ShowHelp as ShowHelp
import qualified Parun.Play.Key as Key
import qualified Parun.Spec as Spec
import qualified Parun.Test.Config as Config
import qualified Parun.Test.Context as Context

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Test.Context" $ do
  Spec.describe s "fromConfig" $ do
    Spec.it s "keys the shuffle with the config's seed" $ do
      let k = Maybe.fromJust . Key.fromHex . Text.pack $ replicate 63 '0' <> "7"
      context <- Context.fromConfig Config.initial {Config.seed = Just k}
      Spec.assertEq s (Context.key context, Context.config context) (k, Config.initial {Config.seed = Just k})
    Spec.it s "asks for the usage instead when the config does" $ do
      result <- Exception.try $ Context.fromConfig Config.initial {Config.help = True}
      Spec.assertEq s (either Just (const Nothing) result) (Just ShowHelp.MkShowHelp)
