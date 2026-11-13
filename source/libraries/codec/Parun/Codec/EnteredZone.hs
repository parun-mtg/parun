{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EnteredZone where

import qualified Data.Set as Set
import qualified Parun.Codec.EntryWindow as EntryWindow
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.EnteredZone as EnteredZone
import qualified Parun.Types.EntryWindow as EntryWindow.Type

-- | A bare object keyed by the record's field names, as
-- Parun.Codec.PutIntoZoneTrigger is. The origin zones are defaulted, "from
-- anywhere" naming none, and the window is defaulted to this turn.
codec :: Codec.Codec EnteredZone.EnteredZone
codec = Fields.object $ do
  to <- Fields.required "to" Zone.codec EnteredZone.to
  from <- Fields.defaulted "from" Set.empty (Common.set Zone.codec) EnteredZone.from
  window <- Fields.defaulted "window" EntryWindow.Type.ThisTurn EntryWindow.codec EnteredZone.window
  pure EnteredZone.MkEnteredZone {EnteredZone.to = to, EnteredZone.from = from, EnteredZone.window = window}
