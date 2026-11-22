-- Covers Parun.Play.Meta and Parun.Play.Command.parseMeta: a meta file, a
-- deck's matchups against it over shared seeds, and the report's rates and
-- intervals.
module Parun.MetaSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Word as Word
import Numeric.Natural (Natural)
import qualified Parun.BotSpec as BotSpec
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Match.Config as Config
import qualified Parun.Play.Meta as Meta
import qualified Parun.Play.Search as Search
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Generator as Generator
import qualified Parun.Types.Key as Key
import qualified Parun.Types.MatchOptions as MatchOptions

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Meta" $ do
  Spec.it s "a meta file names each deck with its share, 1 when unsaid" $ do
    Spec.assertEqWith s "meta" (Command.parseMeta (Text.pack "# a meta\n3 decks/red.txt\n\nblue deck.txt\n")) (Right ((3, Text.pack "decks/red.txt") NonEmpty.:| [(1, Text.pack "blue deck.txt")]))
    Spec.assertEqWith s "a share of 0" (Command.parseMeta (Text.pack "red.txt\n0 blue.txt\n")) (Left [Text.pack "line 2: a share of 0"])
    Spec.assertEqWith s "no deck" (Command.parseMeta (Text.pack "# nothing\n")) (Left [Text.pack "no deck"])
    Spec.assertEqWith s "beside the meta" (fmap (Command.relativeTo "metas/standard.txt") ["red.txt", "/decks/blue.txt"]) ["metas/red.txt", "/decks/blue.txt"]

  Spec.it s "reads parun match's flags but --save, a meta file and one or two decks" $ do
    let parsed :: [String] -> Either String ([FilePath], Natural)
        parsed = Bifunctor.bimap (\(Exception.SomeException e) -> Exception.displayException e) (\c -> (Config.files c, MatchOptions.games (Config.options c))) . Meta.fromArguments
    Spec.assertEqWith s "two decks" (parsed ["--games", "4", "meta.txt", "a.txt", "b.txt"]) (Right (["meta.txt", "a.txt", "b.txt"], 4))
    Spec.assertEqWith s "--save" (parsed ["--save", "games", "meta.txt", "a.txt"]) (Left "unknown option: --save")
    Spec.assertEqWith s "no deck" (parsed ["meta.txt"]) (Left "missing argument: DECK")
    Spec.assertEqWith s "a third deck" (parsed ["meta.txt", "a.txt", "b.txt", "c.txt"]) (Left "unexpected argument: c.txt")
    Spec.assertEqWith s "no games" (parsed ["--games", "0", "meta.txt", "a.txt"]) (Left "no games to play")
    Spec.assertEqWith s "no games, the usage asked for" (fmap fst (parsed ["--games", "0", "--help"])) (Right [])

  Spec.it s "a matchup's rate carries its Wilson interval" $ do
    Spec.assertEqWith s "5 of 10" (Meta.percent (Meta.wilson 5 10)) (Text.pack "50.0% (23.7% to 76.3%).")
    Spec.assertEqWith s "0 of 4" (Meta.percent (Meta.wilson 0 4)) (Text.pack "0.0% (0.0% to 49.0%).")

  Spec.it s "reports each version's matchups, the overall rate weighted by share, and the first version's lead" $ do
    let wins :: NonEmpty.NonEmpty String -> NonEmpty.NonEmpty [Bool]
        wins = fmap (fmap (== 'w'))
        played :: NonEmpty.NonEmpty (Natural, Text.Text, NonEmpty.NonEmpty [Bool])
        played =
          (3, Text.pack "red.txt", wins ("wwll" NonEmpty.:| ["wlll"]))
            NonEmpty.:| [(1, Text.pack "blue.txt", wins ("wwww" NonEmpty.:| ["wwwl"]))]
    Spec.assertEqWith
      s
      "report"
      (Meta.report (Text.pack "a.txt" NonEmpty.:| [Text.pack "b.txt"]) played)
      ( fmap
          Text.pack
          [ "Win rates with 95% confidence intervals.",
            "Against red.txt (share 3 of 4):",
            "  a.txt: won 2 of 4 games, 50.0% (15.0% to 85.0%).",
            "  b.txt: won 1 of 4 games, 25.0% (4.6% to 69.9%).",
            "  a.txt minus b.txt, over the same seeds: +25.0 points (-35.2 to +85.2).",
            "Against blue.txt (share 1 of 4):",
            "  a.txt: won 4 of 4 games, 100.0% (51.0% to 100.0%).",
            "  b.txt: won 3 of 4 games, 75.0% (30.1% to 95.4%).",
            "  a.txt minus b.txt, over the same seeds: +25.0 points (-35.2 to +85.2).",
            "Overall, weighted by share:",
            "  a.txt: 62.5% (35.5% to 89.5%).",
            "  b.txt: 37.5% (11.0% to 64.0%).",
            "  a.txt minus b.txt, over the same seeds: +25.0 points (-22.6 to +72.6)."
          ]
      )

  Spec.it s "two versions meet a meta deck over the same seeds, and each meta deck over its own" $ do
    bears <- snd . NonEmpty.head <$> BotSpec.bears s registry
    sevens <- snd . NonEmpty.head <$> BotSpec.sevens s registry
    first <- Meta.matchup (S.fixtureRegistry []) Nothing Map.empty Search.known (key 7) 4 0 bears sevens
    again <- Meta.matchup (S.fixtureRegistry []) Nothing Map.empty Search.known (key 7) 4 0 bears sevens
    other <- Meta.matchup (S.fixtureRegistry []) Nothing Map.empty Search.known (key 7) 4 1 bears sevens
    Spec.assertEqWith s "games played" (length first) 4
    Spec.assertEqWith s "the same games again" (fmap Meta.won first) (fmap Meta.won again)
    Spec.assertEqWith s "no lead over itself" (Meta.report (Text.pack "a" NonEmpty.:| [Text.pack "b"]) ((1, Text.pack "x", fmap Meta.won first NonEmpty.:| [fmap Meta.won again]) NonEmpty.:| []) !! 4) (Text.pack "  a minus b, over the same seeds: +0.0 points (-50.6 to +50.6).")
    Spec.assertBool s (first /= other) "another meta deck, other games"

-- The key 'S.seeded' starts from.
key :: Word.Word32 -> Key.Key
key = Generator.key . S.seeded
