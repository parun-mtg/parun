module Parun.Web.LoadingSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Html as Html
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

  Spec.describe s "problem" $ do
    Spec.it s "says which need did not load" $ do
      Spec.assertEqWith s "none" (Loading.problem Loading.new) Nothing
      Spec.assertEqWith s "cards" (Loading.problem (Loading.arrive (Left (Text.pack "cards-1")) Loading.new)) (Just (Text.pack "Couldn't load the cards. Reload to try again."))

    Spec.it s "says why one need did not load, not another" $ do
      let both = Loading.arrive (Left (Text.pack "decks")) (Loading.arrive (Left (Text.pack "scenarios")) Loading.new)
      Spec.assertEqWith s "scenarios" (Loading.problemFor Loading.MkScenarios both) (Just (Text.pack "Couldn't load the scenarios. Reload to try again."))
      Spec.assertEqWith s "cards" (Loading.problemFor Loading.MkCards both) Nothing

  Spec.describe s "steps" $ do
    Spec.it s "says how far the cards have come, then sets the table" $ do
      Spec.assertEqWith s "none" (fmap snd (Loading.steps Loading.new)) [Loading.MkDoing (Just 0), Loading.MkWaiting]
      Spec.assertEqWith s "half" (fmap snd (Loading.steps (arrived ["cards-0", "cards-2"] Loading.new))) [Loading.MkDoing (Just 50), Loading.MkWaiting]
      Spec.assertEqWith s "all" (fmap snd (Loading.steps (arrived ["cards-0", "cards-1", "cards-2", "cards-3"] Loading.new))) [Loading.MkDone, Loading.MkDoing Nothing]

  Spec.describe s "view" $ do
    Spec.it s "shows a percentage only where one is known" $ do
      let page ns = Html.toText (Loading.view (arrived ns Loading.new))
      Spec.assertEqWith s "cards" (Text.isInfixOf (Text.pack "<li class=\"now\"><span class=\"dot\" aria-hidden=\"true\"></span><span class=\"label\">Fetching the engine and the cards</span><span class=\"state\">Cards 75%</span></li>") (page ["cards-0", "cards-1", "cards-2"])) True
      Spec.assertEqWith s "table" (Text.isInfixOf (Text.pack "<span class=\"label\">Setting the table</span><span class=\"state\"></span>") (page ["cards-0", "cards-1", "cards-2", "cards-3"])) True
