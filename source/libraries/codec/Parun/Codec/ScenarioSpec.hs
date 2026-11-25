module Parun.Codec.ScenarioSpec where

import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.Scenario as Scenario
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackOption as AttackOption.Type
import qualified Parun.Types.Board as Board.Type
import qualified Parun.Types.Entrant as Entrant.Type
import qualified Parun.Types.Format as Format.Type
import qualified Parun.Types.GameSettings as GameSettings.Type
import qualified Parun.Types.GraveyardOrder as GraveyardOrder.Type
import qualified Parun.Types.Label as Label.Type
import qualified Parun.Types.Phase as Phase.Type
import qualified Parun.Types.RuleFamily as RuleFamily.Type
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.Seat as Seat.Type
import qualified Parun.Types.Start as Start.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Scenario" $ do
  Spec.it s "an empty timeline and no final checks are left out" $
    Common.assertCodec
      s
      Scenario.codec
      Scenario.Type.MkScenario
        { Scenario.Type.description = Text.pack "nothing happens",
          Scenario.Type.start =
            Start.Type.FromBoard
              Board.Type.MkBoard
                { Board.Type.seats = Seat.Type.MkSeat (Label.Type.MkLabel (Text.pack "alice")) 20 Map.empty Nothing Nothing False Nothing Nothing GraveyardOrder.Type.Indifferent [] Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty Map.empty Map.empty Map.empty Set.empty Map.empty NonEmpty.:| [],
                  Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
                  Board.Type.stack = Seq.empty,
                  Board.Type.turn = 1,
                  Board.Type.phase = Phase.Type.PrecombatMain,
                  Board.Type.monarch = Nothing,
                  Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers,
                  Board.Type.brawl = False,
                  Board.Type.sharedTeamTurns = False,
                  Board.Type.sharedTeamLife = False,
                  Board.Type.deployCreatures = False,
                  Board.Type.twoHeadedGiant = False,
                  Board.Type.alternatingTeams = False,
                  Board.Type.ante = False
                },
          Scenario.Type.timeline = Seq.empty,
          Scenario.Type.final = Seq.empty
        }
      " {\"description\":\"nothing happens\",\"board\":{\"seats\":[{\"name\":\"alice\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\"}} "
  Spec.it s "a new game starts from each seat's deck" $
    Common.assertCodec
      s
      Scenario.codec
      Scenario.Type.MkScenario
        { Scenario.Type.description = Text.pack "a game",
          Scenario.Type.start = Start.Type.FromDecks GameSettings.Type.plain (Entrant.Type.MkEntrant (Label.Type.MkLabel (Text.pack "alice")) [Text.pack "20 Forest"] GraveyardOrder.Type.Indifferent NonEmpty.:| []) Nothing,
          Scenario.Type.timeline = Seq.empty,
          Scenario.Type.final = Seq.empty
        }
      " {\"description\":\"a game\",\"game\":[{\"name\":\"alice\",\"deck\":[\"20 Forest\"]}]} "
  Spec.it s "CR 100.2: a new game carries the format its decks were built for" $
    Common.assertCodec
      s
      Scenario.codec
      Scenario.Type.MkScenario
        { Scenario.Type.description = Text.pack "a game",
          Scenario.Type.start = Start.Type.FromDecks GameSettings.Type.plain {GameSettings.Type.format = Just (Format.Type.MkFormat RuleFamily.Type.Constructed Nothing Set.empty)} (Entrant.Type.MkEntrant (Label.Type.MkLabel (Text.pack "alice")) [Text.pack "60 Forest"] GraveyardOrder.Type.Indifferent NonEmpty.:| []) Nothing,
          Scenario.Type.timeline = Seq.empty,
          Scenario.Type.final = Seq.empty
        }
      " {\"description\":\"a game\",\"game\":[{\"name\":\"alice\",\"deck\":[\"60 Forest\"]}],\"format\":{\"rules\":\"Constructed\"}} "
  Spec.it s "CR 800.2: a new game carries the options it was played with beside its format" $
    Common.assertCodec
      s
      Scenario.codec
      Scenario.Type.MkScenario
        { Scenario.Type.description = Text.pack "a game",
          Scenario.Type.start = Start.Type.FromDecks GameSettings.Type.plain {GameSettings.Type.ante = True, GameSettings.Type.format = Just (Format.Type.MkFormat RuleFamily.Type.Constructed Nothing Set.empty)} (Entrant.Type.MkEntrant (Label.Type.MkLabel (Text.pack "alice")) [Text.pack "60 Forest"] GraveyardOrder.Type.Indifferent NonEmpty.:| []) Nothing,
          Scenario.Type.timeline = Seq.empty,
          Scenario.Type.final = Seq.empty
        }
      " {\"description\":\"a game\",\"game\":[{\"name\":\"alice\",\"deck\":[\"60 Forest\"]}],\"format\":{\"rules\":\"Constructed\"},\"settings\":{\"brawl\":false,\"attackOption\":\"MultiplePlayers\",\"teams\":{},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{},\"deployCreatures\":false,\"emperors\":{},\"twoHeadedGiant\":false,\"alternatingTeams\":false,\"grandMelee\":false,\"ante\":true,\"singlePlanarDeck\":false,\"format\":null}} "
  Spec.it s "a game's settings naming a format of their own do not decode" $
    Spec.assertBool s (Either.isLeft (Common.parse (Text.pack "{\"description\":\"x\",\"game\":[{\"name\":\"a\",\"deck\":[]}],\"settings\":{\"brawl\":false,\"attackOption\":null,\"teams\":{},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{},\"deployCreatures\":false,\"emperors\":{},\"twoHeadedGiant\":false,\"alternatingTeams\":false,\"grandMelee\":false,\"ante\":true,\"singlePlanarDeck\":false,\"format\":{\"rules\":\"Constructed\"}}}") >>= Codec.decode Scenario.codec)) "settings with a format"
  Spec.it s "CR 103.1: a new game names its starting player where not the first seat" $
    Common.assertCodec
      s
      Scenario.codec
      Scenario.Type.MkScenario
        { Scenario.Type.description = Text.pack "a game",
          Scenario.Type.start = Start.Type.FromDecks GameSettings.Type.plain (Entrant.Type.MkEntrant (Label.Type.MkLabel (Text.pack "alice")) [Text.pack "20 Forest"] GraveyardOrder.Type.Indifferent NonEmpty.:| [Entrant.Type.MkEntrant (Label.Type.MkLabel (Text.pack "bob")) [Text.pack "20 Forest"] GraveyardOrder.Type.Indifferent]) (Just (Label.Type.MkLabel (Text.pack "bob"))),
          Scenario.Type.timeline = Seq.empty,
          Scenario.Type.final = Seq.empty
        }
      " {\"description\":\"a game\",\"game\":[{\"name\":\"alice\",\"deck\":[\"20 Forest\"]},{\"name\":\"bob\",\"deck\":[\"20 Forest\"]}],\"first\":\"bob\"} "
  Spec.it s "a board with a starting player does not decode" $
    Spec.assertBool s (Either.isLeft (Common.parse (Text.pack "{\"description\":\"x\",\"first\":\"a\",\"board\":{\"seats\":[{\"name\":\"a\"}],\"active\":\"a\",\"step\":\"PrecombatMain\"}}") >>= Codec.decode Scenario.codec)) "board and starting player"
  Spec.it s "a board with a format does not decode" $
    Spec.assertBool s (Either.isLeft (Common.parse (Text.pack "{\"description\":\"x\",\"format\":{\"rules\":\"Constructed\"},\"board\":{\"seats\":[{\"name\":\"a\"}],\"active\":\"a\",\"step\":\"PrecombatMain\"}}") >>= Codec.decode Scenario.codec)) "board and format"
  Spec.it s "a scenario with no start, or two, does not decode" $ do
    Spec.assertBool s (Either.isLeft (Common.parse (Text.pack "{\"description\":\"x\"}") >>= Codec.decode Scenario.codec)) "neither"
    Spec.assertBool s (Either.isLeft (Common.parse (Text.pack "{\"description\":\"x\",\"game\":[{\"name\":\"a\",\"deck\":[]}],\"board\":{\"seats\":[{\"name\":\"a\"}],\"active\":\"a\",\"step\":\"PrecombatMain\"}}") >>= Codec.decode Scenario.codec)) "both"
  Spec.it s "has a schema" $
    Common.assertHasSchema s Scenario.codec
