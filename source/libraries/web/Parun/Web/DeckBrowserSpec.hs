module Parun.Web.DeckBrowserSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.DeckBrowser as DeckBrowser

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.DeckBrowser" $ do
  Spec.describe s "describe" $ do
    Spec.it s "names the set by its code and the deck in words" $ do
      Spec.assertEq s (DeckBrowser.describe $ Text.pack "sets/m12/entangling-webs") (Text.pack "M12", Text.pack "Entangling Webs")

    Spec.it s "describes a saved deck as saved" $ do
      Spec.assertEq s (DeckBrowser.describe saved) (Text.pack "SAVED", Text.pack "My Deck")

  Spec.describe s "names" $ do
    Spec.it s "slugs a name as lowercase words joined by dashes" $ do
      Spec.assertEqWith s "words" (DeckBrowser.slugOf (Text.pack "  My  Deck!")) (Text.pack "my-deck")
      Spec.assertEqWith s "nothing left" (DeckBrowser.slugOf (Text.pack "!!!")) Text.empty

    Spec.it s "refuses an empty name and one a saved deck holds" $ do
      let holding = DeckBrowser.withDecks (Seq.fromList [a, saved]) Map.empty DeckBrowser.new
      Spec.assertBool s (not (null (DeckBrowser.nameProblems (DeckBrowser.decks holding) (Text.pack "My Deck")))) "taken"
      Spec.assertBool s (not (null (DeckBrowser.nameProblems (DeckBrowser.decks holding) (Text.pack "!!!")))) "empty"
      Spec.assertEqWith s "free" (DeckBrowser.nameProblems (DeckBrowser.decks holding) (Text.pack "Other")) []

a :: Text.Text
a = Text.pack "sets/por/demo-a"

saved :: Text.Text
saved = Text.pack "saved/my-deck"
