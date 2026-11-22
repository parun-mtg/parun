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
      Spec.assertEq s (DeckBrowser.describe Map.empty $ Text.pack "sets/m12/entangling-webs") (Just (Text.pack "M12"), Text.pack "Entangling Webs")

    Spec.it s "gives a saved deck and a format deck no set" $ do
      Spec.assertEqWith s "saved" (DeckBrowser.describe Map.empty saved) (Nothing, Text.pack "My Deck")
      Spec.assertEqWith s "format" (DeckBrowser.describe Map.empty pauper) (Nothing, Text.pack "Rakdos Madness")

    Spec.it s "names a deck as its header does, not as its file does" $ do
      Spec.assertEq s (DeckBrowser.describe (Map.singleton temper (Text.pack "Kamahl's Temper")) temper) (Just (Text.pack "10E"), Text.pack "Kamahl's Temper")

  Spec.describe s "label" $ do
    Spec.it s "names a precon with its set and a saved deck alone" $ do
      Spec.assertEqWith s "precon" (DeckBrowser.label Map.empty a) (Text.pack "Demo A (POR)")
      Spec.assertEqWith s "saved" (DeckBrowser.label Map.empty saved) (Text.pack "My Deck")
      Spec.assertEqWith s "format" (DeckBrowser.label Map.empty pauper) (Text.pack "Rakdos Madness")

    Spec.it s "names a deck sent as its list's header does" $ do
      let sent = (saved, Text.pack "Name: Tiny <b>Bolt</b>\n4 Lightning Bolt\n")
      Spec.assertEqWith s "headed" (DeckBrowser.label (DeckBrowser.titleIn sent) saved) (Text.pack "Tiny <b>Bolt</b>")
      Spec.assertEqWith s "bare" (DeckBrowser.titleIn (saved, Text.pack "4 Lightning Bolt\n")) Map.empty

  Spec.describe s "tile" $ do
    Spec.it s "badges a precon with its set's name, a saved deck Saved, and a format deck nothing" $ do
      let page = Html.toText . DeckBrowser.tile id False dated
          badge name = Text.isInfixOf (Text.pack ("<span class=\"set-badge\">" <> name <> "</span>")) . page
      Spec.assertEqWith s "precon" (badge "Tenth Edition" temper) True
      Spec.assertEqWith s "no code" (badge "10E" temper) False
      Spec.assertEqWith s "undated, by code" (badge "POR" a) True
      Spec.assertEqWith s "saved" (badge "Saved" saved) True
      Spec.assertEqWith s "format" (Text.isInfixOf (Text.pack "set-badge") (page pauper)) False

  Spec.describe s "shown" $ do
    Spec.it s "filters My decks and the precons by format alike" $ do
      let pick = DeckBrowser.update DeckBrowser.MkFormat (Text.pack "pauper") dated
          on which = DeckBrowser.shown (DeckBrowser.decks dated) pick {DeckBrowser.shelf = which}
      Spec.assertEqWith s "precons" (on DeckBrowser.MkPrecons) [pauper]
      Spec.assertEqWith s "my decks" (on DeckBrowser.MkSaved) [mine]
      Spec.assertEqWith s "any" (DeckBrowser.shown (DeckBrowser.decks dated) (DeckBrowser.update DeckBrowser.MkFormat Text.empty pick) {DeckBrowser.shelf = DeckBrowser.MkSaved}) [mine, saved]

    Spec.it s "orders by name, or by when the player last played each first" $ do
      let played = dated {DeckBrowser.played = Map.fromList [(a, 1), (temper, 2)]}
          ordered which = DeckBrowser.shown (DeckBrowser.decks played) (DeckBrowser.update DeckBrowser.MkOrder (Text.pack which) played)
      Spec.assertEqWith s "by name" (ordered "name") [a, temper, pauper]
      Spec.assertEqWith s "by play" (ordered "played") [temper, a, pauper]

    Spec.it s "finds a precon by its set's name" $ do
      Spec.assertEq s (DeckBrowser.shown (DeckBrowser.decks dated) dated {DeckBrowser.search = Text.pack "tenth"}) [temper]

    Spec.it s "keeps only the set chosen among the precons, and ignores it on My decks" $ do
      let tenth = DeckBrowser.update DeckBrowser.MkSet (Text.pack "10E") dated
      Spec.assertEqWith s "precons" (DeckBrowser.shown (DeckBrowser.decks tenth) tenth) [temper]
      Spec.assertEqWith s "my decks" (DeckBrowser.shown (DeckBrowser.decks tenth) tenth {DeckBrowser.shelf = DeckBrowser.MkSaved}) [mine, saved]

  Spec.describe s "filtersView" $ do
    Spec.it s "offers the sets by name, newest first, on the precons alone" $ do
      let page which = Html.toText (DeckBrowser.filtersView id [] dated {DeckBrowser.shelf = which})
      Spec.assertEqWith s "sets" (DeckBrowser.setsOf dated) [Text.pack "10E", Text.pack "POR"]
      Spec.assertEqWith s "named" (Text.isInfixOf (Text.pack "<option value=\"10E\">Tenth Edition</option>") (page DeckBrowser.MkPrecons)) True
      Spec.assertEqWith s "no set menu on my decks" (Text.isInfixOf (Text.pack "deck-set") (page DeckBrowser.MkSaved)) False
      Spec.assertEqWith s "formats on my decks" (Text.isInfixOf (Text.pack "<option value=\"pauper\">Pauper</option>") (page DeckBrowser.MkSaved)) True

    Spec.it s "keeps the format chosen in its menu on a shelf without it" $ do
      let page = Html.toText (DeckBrowser.filtersView id [] (DeckBrowser.update DeckBrowser.MkFormat (Text.pack "modern") dated))
      Spec.assertBool s (Text.isInfixOf (Text.pack "<option value=\"modern\" selected=\"\">Modern</option>") page) "chosen"

  Spec.describe s "releasesOf" $ do
    Spec.it s "reads a set's name and release date by its code" $ do
      Spec.assertEq s (DeckBrowser.releasesOf (Text.pack "{\"10E\": {\"name\": \"Tenth Edition\", \"releaseDate\": \"2007-07-13\"}}")) (Right (Map.singleton (Text.pack "10E") (DeckBrowser.MkRelease (Text.pack "Tenth Edition") (Text.pack "2007-07-13"))))

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

pauper :: Text.Text
pauper = Text.pack "pauper/rakdos-madness"

mine :: Text.Text
mine = Text.pack "saved/burn"

-- | A precon of a set data/decks/sets.json dates, one of a set it does not,
-- a shipped Pauper deck, and two saved decks, one Pauper.
dated :: DeckBrowser.DeckBrowser
dated =
  (DeckBrowser.withDecks (Seq.fromList [pauper, mine, saved, a, temper]) (Map.fromList [(pauper, Text.pack "pauper"), (mine, Text.pack "pauper")]) (Map.singleton temper (Text.pack "Kamahl's Temper")) DeckBrowser.new)
    { DeckBrowser.releases = Map.singleton (Text.pack "10E") (DeckBrowser.MkRelease (Text.pack "Tenth Edition") (Text.pack "2007-07-13"))
    }
