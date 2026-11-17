{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EntriesTrigger where

import qualified Parun.Codec.EntryTriggering as EntryTriggering
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.TriggeringMove as TriggeringMove
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.EntriesTrigger as EntriesTrigger
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.TriggeringMove as TriggeringMove.Type

codec :: Codec.Codec EntriesTrigger.EntriesTrigger
codec = Fields.object $ do
  entrants <- Fields.defaulted "entrants" (Filter.Type.And []) (Filter.codec Keyword.codec) EntriesTrigger.entrants
  bearers <- Fields.defaulted "bearers" (Filter.Type.And []) (Filter.codec Keyword.codec) EntriesTrigger.bearers
  move <- Fields.defaulted "move" TriggeringMove.Type.Entering TriggeringMove.codec EntriesTrigger.move
  triggering <- Fields.required "triggering" EntryTriggering.codec EntriesTrigger.triggering
  pure EntriesTrigger.MkEntriesTrigger {EntriesTrigger.entrants = entrants, EntriesTrigger.bearers = bearers, EntriesTrigger.move = move, EntriesTrigger.triggering = triggering}
