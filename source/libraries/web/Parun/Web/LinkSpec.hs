module Parun.Web.LinkSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.PlayerViewSpec as PlayerViewSpec
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Log as Log

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Link" $ do
  Spec.describe s "encode" $ do
    Spec.it s "reads back every message as it was sent" $ do
      let request = Request.MkRequest Nothing (Question.MkQuestion (PlayerId.MkPlayerId 1) (Text.pack "PlayOrDraw") (R.Array [R.Text (Text.pack "Play"), R.Text (Text.pack "Draw")]) Nothing Nothing) (Just (Text.pack "expected Play or Draw")) 0
          messages =
            [ Link.MkHello (Text.pack "saved/elves") (Text.pack "Format: constructed\n24 Forest\n36 Llanowar Elves\n"),
              Link.MkMatchup [Link.MkPlace (Text.pack "Player 1") (Text.pack "Elves (Decks)") (Just True) False (Set.fromList [Color.Green, Color.White]), Link.MkPlace (Text.pack "You") (Text.pack "Choosing a deck") (Just False) False Set.empty, Link.MkPlace (Text.pack "Player 3") (Text.pack "Random deck") Nothing True Set.empty, Link.MkPlace (Text.pack "Player 4") (Text.pack "Random deck") Nothing False Set.empty],
              Link.MkReady True,
              Link.MkReady False,
              Link.MkStart (PlayerId.MkPlayerId 2) Ending.unseen,
              Link.MkStart (PlayerId.MkPlayerId 1) (Ending.MkSeen (Map.fromList [((PlayerId.MkPlayerId 1, 0), (3, 2))]) (Map.singleton (PlayerId.MkPlayerId 1) (Seq.fromList [Log.MkPregame, Log.MkTurn 3 (PlayerId.MkPlayerId 0), Log.MkEvent Log.MkMinor (Text.pack "Grizzly Bears enters.")])) (Seq.fromList [(1, [(PlayerId.MkPlayerId 0, 20), (PlayerId.MkPlayerId 1, 20)]), (3, [(PlayerId.MkPlayerId 0, 18), (PlayerId.MkPlayerId 1, 20)])])),
              Link.MkAsk request,
              Link.MkDeciding (PlayerId.MkPlayerId 2) PlayerViewSpec.testView 1,
              Link.MkAnswer (R.Object [(Text.pack "action", R.Text (Text.pack "Pass"))]),
              Link.MkEnd (Ending.MkEnding Ending.MkLost [Text.pack "Player 1 wins."] (Text.pack "{}\n") (Seq.fromList [Log.MkTurn 1 (PlayerId.MkPlayerId 0), Log.MkMana (PlayerId.MkPlayerId 0) [Text.pack "Forest", Text.pack "Forest"], Log.MkEvent Log.MkMajor (Text.pack "Player 1 casts Grizzly Bears."), Log.MkEvent Log.MkMinor (Text.pack "Grizzly Bears enters.")]) (PlayerId.MkPlayerId 1) False [(1, [(PlayerId.MkPlayerId 0, 20), (PlayerId.MkPlayerId 1, 20)]), (4, [(PlayerId.MkPlayerId 0, 20), (PlayerId.MkPlayerId 1, -2)])]),
              Link.MkEnd (Ending.MkEnding Ending.MkWon [] Text.empty Seq.empty (PlayerId.MkPlayerId 2) False []),
              Link.MkEnd (Ending.MkEnding (Ending.MkWonBy (PlayerId.MkPlayerId 1)) [] Text.empty Seq.empty (PlayerId.MkPlayerId 0) True []),
              Link.MkEnd (Ending.MkEnding Ending.MkDrawn [] Text.empty Seq.empty (PlayerId.MkPlayerId 1) False []),
              Link.MkEnd (Ending.MkEnding Ending.MkStopped [Text.pack "The game stopped: a player's connection closed."] Text.empty Seq.empty (PlayerId.MkPlayerId 1) False [])
            ]
          build = Text.pack "parun-0123456789abcdef.wasm"
      Spec.assertEq s (fmap (Link.decode build . Link.encode build) messages) (fmap Right messages)

    Spec.it s "puts each message on one line" $ do
      Spec.assertBool s (not (Text.any (== '\n') (Link.encode (Text.pack "b") (Link.MkHello (Text.pack "x") (Text.pack "1 Forest\n1 Island\n"))))) "one line"

  Spec.describe s "decode" $ do
    Spec.it s "refuses a message from another build" $ do
      Spec.assertBool s (either (Text.isInfixOf (Text.pack "another build")) (const False) (Link.decode (Text.pack "new") (Link.encode (Text.pack "old") (Link.MkReady True)))) "refused"

    Spec.it s "refuses a type it does not know" $ do
      Spec.assertEq s (Link.decode (Text.pack "b") (Text.pack "{\"type\":\"shout\",\"build\":\"b\"}")) (Left (Text.pack "not a message: shout"))

  Spec.describe s "saved" $ do
    Spec.it s "reads back a kept game, each bot's level and style by its slot, a reply added as a line" $ do
      let game = Link.MkSaved key (NonEmpty.fromList [(Text.pack "sets/a/one", Text.pack "Format: constructed\n24 Forest\n"), (Text.pack "Bot", Text.pack "20 Island\n"), (Text.pack "Here", Text.pack "20 Swamp\n"), (Text.pack "Yours", Text.pack "20 Plains\n")]) [0] (Map.fromList [(1, Link.MkHandedOver), (2, Link.MkYours)]) (Map.fromList [(0, (Difficulty.MkEasy, Persona.MkAggro)), (3, (Difficulty.MkHard, Persona.MkControl))]) [R.Text (Text.pack "Keep")]
          more = R.Object [(Text.pack "action", R.Text (Text.pack "Pass"))]
          build = Text.pack "b"
      Spec.assertEq s (Link.unsaved build (Link.saved build game <> Text.pack "\n" <> Link.savedReply more)) (Right game {Link.replies = Link.replies game <> [more]})

    Spec.it s "refuses a game another build played" $ do
      let game = Link.MkSaved key (NonEmpty.fromList [(Text.pack "one", Text.pack "20 Forest\n")]) [] Map.empty Map.empty []
      Spec.assertBool s (either (Text.isInfixOf (Text.pack "another build")) (const False) (Link.unsaved (Text.pack "new") (Link.saved (Text.pack "old") game))) "refused"
  where
    key = Key.Type.UnsafeKey (Seq.fromList [1, 2, 3, 4, 5, 6, 7, 0xdeadbeef])
