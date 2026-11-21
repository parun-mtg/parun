{-# LANGUAGE GADTs #-}

-- Covers: Parun.Engine.Limit, the host's object limit (Parun.Types.Limits).
--
-- Intruder Alarm and Presence of Gond on Grizzly Bears are the loop #9339
-- names: each activation's token untaps the Bears, so a player always
-- activating never stops by themselves. The answerer gives up at twenty
-- objects, so a game the limit fails to stop still ends, there rather than
-- at the eight the case sets.
module Parun.LimitSpec where

import qualified Data.Map.Strict as Map
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Limit as Limit
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Asked as Asked
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Object as Object
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient

-- Activates whatever it is offered until the game holds twenty objects.
activating :: Asked.Asked GameState r -> r
activating asked = case Asked.prompt asked of
  Prompt.ChooseAction _ _ actions
    | Map.size (GameState.objects (Asked.game asked)) < 20,
      activation : _ <- [action | action@Action.Activate {} <- actions] ->
        activation
    | otherwise -> Action.Pass
  p -> S.identityAnswer p

spec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Limit" $ do
  Spec.it s "a player at the object limit is offered only Pass, so a loop they would repeat forever stops there" $ do
    alarm <- S.printingOf s registry "Intruder Alarm"
    bears <- S.printingOf s registry "Grizzly Bears"
    gond <- S.printingOf s registry "Presence of Gond"
    let (_, g1) = S.addPermanent alarm S.alice (Setup.emptyGame S.bothPlayers)
        (bearsId, g2) = S.addPermanent bears S.alice g1
        (gondId, g3) = S.addPermanent gond S.alice g2
        enchanted = g3 {GameState.objects = Map.adjust (\o -> o {Object.attachedTo = Just (Recipient.ToCreature bearsId)}) gondId (GameState.objects g3)}
        limited = Limit.under Limits.MkLimits {Limits.objects = 8} enchanted
        (_, after) = Engine.runGameAskedPure activating limited Engine.priorityLoop
    Spec.assertEqWith s "objects" (Map.size (GameState.objects after)) 8
