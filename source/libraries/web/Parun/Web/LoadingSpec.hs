module Parun.Web.LoadingSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Loading as Loading

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Loading" $ do
  let arrived names loading = Foldable.foldl' (flip (Loading.arrive . Right . Text.pack)) loading names
  Spec.describe s "ready" $ do
    Spec.it s "waits for every slice of the cards" $ do
      Spec.assertEqWith s "three of four" (Loading.ready Loading.MkCards (arrived ["cards-0", "cards-1", "cards-2"] Loading.new)) False
      Spec.assertEqWith s "all four" (Loading.ready Loading.MkCards (arrived ["cards-0", "cards-1", "cards-2", "cards-3"] Loading.new)) True

  Spec.describe s "demand" $ do
    Spec.it s "asks for the chunks not yet arrived" $ do
      Spec.assertEqWith s "scenarios" (snd (Loading.demand Loading.MkScenarios Loading.new)) [Text.pack "scenarios"]
      Spec.assertEqWith s "decks, arrived" (snd (Loading.demand Loading.MkDecks (arrived ["decks"] Loading.new))) []

    Spec.it s "records the need as wanted" $ do
      Spec.assertEq s (Loading.wanted (fst (Loading.demand Loading.MkRules Loading.new))) [Loading.MkRules]

  Spec.describe s "blocked" $ do
    Spec.it s "names a failed chunk the need has, which keeps it unready" $ do
      let failed = Loading.arrive (Left (Text.pack "cards-2")) (arrived ["cards-0", "cards-1", "cards-3"] Loading.new)
      Spec.assertEqWith s "blocked" (Loading.blocked Loading.MkCards failed) (Just (Text.pack "cards-2"))
      Spec.assertEqWith s "unready" (Loading.ready Loading.MkCards failed) False
      Spec.assertEqWith s "another need" (Loading.blocked Loading.MkDecks failed) Nothing
