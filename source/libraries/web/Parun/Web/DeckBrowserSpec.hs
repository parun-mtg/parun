module Parun.Web.DeckBrowserSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.DeckBrowser" $ do
  Spec.describe s "describe" $ do
    Spec.it s "names the set by its code and the deck in words" $ do
      Spec.assertEq s (DeckBrowser.describe Map.empty $ Text.pack "sets/m12/entangling-webs") (Text.pack "M12", Text.pack "Entangling Webs")

    Spec.it s "describes a saved deck as saved" $ do
      Spec.assertEq s (DeckBrowser.describe Map.empty saved) (Text.pack "SAVED", Text.pack "My Deck")

    Spec.it s "names a deck as its header does, not as its file does" $ do
      Spec.assertEq s (DeckBrowser.describe (Map.singleton temper (Text.pack "Kamahl's Temper")) temper) (Text.pack "10E", Text.pack "Kamahl's Temper")

  Spec.describe s "label" $ do
    Spec.it s "names a precon with its set and a saved deck alone" $ do
      Spec.assertEqWith s "precon" (DeckBrowser.label Map.empty a) (Text.pack "Demo A (POR)")
      Spec.assertEqWith s "saved" (DeckBrowser.label Map.empty saved) (Text.pack "My Deck")

    Spec.it s "names a deck sent as its list's header does" $ do
      let sent = (saved, Text.pack "Name: Tiny <b>Bolt</b>\n4 Lightning Bolt\n")
      Spec.assertEqWith s "headed" (DeckBrowser.label (DeckBrowser.titleIn sent) saved) (Text.pack "Tiny <b>Bolt</b>")
      Spec.assertEqWith s "bare" (DeckBrowser.titleIn (saved, Text.pack "4 Lightning Bolt\n")) Map.empty

  Spec.describe s "tile" $ do
    Spec.it s "badges a precon with its set and a saved deck Saved" $ do
      let badge name = Text.isInfixOf (Text.pack ("<span class=\"set-badge\">" <> name <> "</span>")) . Html.toText . DeckBrowser.tile id False DeckBrowser.new
      Spec.assertEqWith s "precon" (badge "POR" a) True
      Spec.assertEqWith s "saved" (badge "Saved" saved) True

    Spec.it s "shows a deck's own name verbatim, markup escaped" $ do
      let named = DeckBrowser.withDecks (Seq.fromList [temper, saved]) Map.empty (Map.fromList [(temper, Text.pack "Kamahl's Temper"), (saved, Text.pack "Tiny <b>Bolt</b>")]) DeckBrowser.new
          shown name = Html.toText (DeckBrowser.tile id False named name)
      Spec.assertEqWith s "precon" (Text.isInfixOf (Text.pack "Kamahl's Temper") (shown temper)) True
      Spec.assertEqWith s "saved, escaped" (Text.isInfixOf (Text.pack "Tiny &lt;b&gt;Bolt&lt;/b&gt;") (shown saved)) True
      Spec.assertEqWith s "saved, no markup" (Text.isInfixOf (Text.pack "<b>") (shown saved)) False

  Spec.describe s "resultsView" $ do
    Spec.it s "counts one deck in the singular and more in the plural" $ do
      let count names =
            let offered = Seq.fromList names
             in Html.toText (DeckBrowser.resultsView id (const False) [] offered (DeckBrowser.withDecks offered Map.empty Map.empty DeckBrowser.new))
      Spec.assertBool s (Text.isInfixOf (Text.pack ">1 deck<") (count [a])) "one"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">2 decks<") (count [a, b])) "two"

  Spec.describe s "names" $ do
    Spec.it s "slugs a name as lowercase words joined by dashes" $ do
      Spec.assertEqWith s "words" (DeckBrowser.slugOf (Text.pack "  My  Deck!")) (Text.pack "my-deck")
      Spec.assertEqWith s "nothing left" (DeckBrowser.slugOf (Text.pack "!!!")) Text.empty

    Spec.it s "refuses an empty name and one a saved deck holds" $ do
      let holding = DeckBrowser.withDecks (Seq.fromList [a, saved]) Map.empty Map.empty DeckBrowser.new
      Spec.assertBool s (not (null (DeckBrowser.nameProblems (DeckBrowser.decks holding) (Text.pack "My Deck")))) "taken"
      Spec.assertBool s (not (null (DeckBrowser.nameProblems (DeckBrowser.decks holding) (Text.pack "!!!")))) "empty"
      Spec.assertEqWith s "free" (DeckBrowser.nameProblems (DeckBrowser.decks holding) (Text.pack "Other")) []

a :: Text.Text
a = Text.pack "sets/por/demo-a"

saved :: Text.Text
saved = Text.pack "saved/my-deck"

b :: Text.Text
b = Text.pack "sets/por/demo-b"

temper :: Text.Text
temper = Text.pack "sets/10e/kamahls-temper"
