{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayerView where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Parun.Codec.AttackTarget as AttackTarget
import qualified Parun.Codec.Daytime as Daytime
import qualified Parun.Codec.LoggedEvent as LoggedEvent
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.ObjectView as ObjectView
import qualified Parun.Codec.Phase as Phase
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Result as Result
import qualified Parun.Codec.SeatView as SeatView
import qualified Parun.Codec.Undoing as Undoing
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayerView as PlayerView

-- | A bare object keyed by the record's field names: what a client is sent
-- in place of the game state.
codec :: Codec.Codec PlayerView.PlayerView
codec = Fields.object $ do
  viewer <- Fields.required "viewer" PlayerId.codec PlayerView.viewer
  game <- Fields.required "game" Common.natural PlayerView.game
  turn <- Fields.required "turn" Common.natural PlayerView.turn
  begun <- Fields.defaulted "begun" True Common.boolean PlayerView.begun
  phase <- Fields.required "phase" Phase.codec PlayerView.phase
  remaining <- Fields.defaulted "remaining" Seq.empty (Common.seq Phase.codec) PlayerView.remaining
  extra <- Fields.defaulted "extra" False Common.boolean PlayerView.extra
  active <- Fields.required "active" PlayerId.codec PlayerView.active
  priority <- Fields.required "priority" (Common.maybe PlayerId.codec) PlayerView.priority
  seats <- Fields.required "seats" (Common.list SeatView.codec) PlayerView.seats
  battlefield <- Fields.required "battlefield" (Common.list ObjectView.codec) PlayerView.battlefield
  phasedOut <- Fields.defaulted "phasedOut" [] (Common.list ObjectView.codec) PlayerView.phasedOut
  command <- Fields.defaulted "command" [] (Common.list ObjectView.codec) PlayerView.command
  stack <- Fields.required "stack" (Common.list ObjectView.codec) PlayerView.stack
  attackers <- Fields.defaulted "attackers" Map.empty (Common.naturalMap ObjectId.codec AttackTarget.codec) PlayerView.attackers
  blockers <- Fields.defaulted "blockers" Map.empty (Common.naturalMap ObjectId.codec (Common.set ObjectId.codec)) PlayerView.blockers
  parkedStacks <- Fields.defaulted "parkedStacks" Map.empty (Common.naturalMap Common.natural (Common.list ObjectView.codec)) PlayerView.parkedStacks
  witnessed <- Fields.required "witnessed" (Common.list LoggedEvent.codec) PlayerView.witnessed
  witnessedLastTurn <- Fields.required "witnessedLastTurn" (Common.list LoggedEvent.codec) PlayerView.witnessedLastTurn
  monarch <- Fields.defaulted "monarch" Nothing (Common.maybe PlayerId.codec) PlayerView.monarch
  initiative <- Fields.defaulted "initiative" Nothing (Common.maybe PlayerId.codec) PlayerView.initiative
  daytime <- Fields.defaulted "daytime" Nothing (Common.maybe Daytime.codec) PlayerView.daytime
  result <- Fields.required "result" (Common.maybe Result.codec) PlayerView.result
  undone <- Fields.required "undone" (Common.maybe Undoing.codec) PlayerView.undone
  pure
    PlayerView.MkPlayerView
      { PlayerView.viewer = viewer,
        PlayerView.game = game,
        PlayerView.turn = turn,
        PlayerView.begun = begun,
        PlayerView.phase = phase,
        PlayerView.remaining = remaining,
        PlayerView.extra = extra,
        PlayerView.active = active,
        PlayerView.priority = priority,
        PlayerView.seats = seats,
        PlayerView.battlefield = battlefield,
        PlayerView.phasedOut = phasedOut,
        PlayerView.command = command,
        PlayerView.stack = stack,
        PlayerView.attackers = attackers,
        PlayerView.blockers = blockers,
        PlayerView.parkedStacks = parkedStacks,
        PlayerView.witnessed = witnessed,
        PlayerView.witnessedLastTurn = witnessedLastTurn,
        PlayerView.monarch = monarch,
        PlayerView.initiative = initiative,
        PlayerView.daytime = daytime,
        PlayerView.result = result,
        PlayerView.undone = undone
      }
