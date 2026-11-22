{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EnteredZone where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.EntryWindow as EntryWindow
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.EnteredZone as EnteredZone
import qualified Parun.Types.EntryWindow as EntryWindow.Type
import qualified Parun.Types.Zone as Zone.Type

-- | A bare object keyed by the record's field names, as
-- Parun.Codec.PutIntoZoneTrigger is. The origin zones are defaulted, "from
-- anywhere" naming none, the window is defaulted to this turn, and the
-- controller it entered under to any.
--
-- Unprinted: an @under@ beside a zone other than the battlefield or a window
-- other than this turn. It is refused, and Parun.Codec.EnteredZoneSpec's
-- "rejects under" cases prove the refusal.
codec :: Codec.Codec EnteredZone.EnteredZone
codec = Fields.objectWith underIsPrinted $ do
  to <- Fields.required "to" Zone.codec EnteredZone.to
  from <- Fields.defaulted "from" Set.empty (Common.set Zone.codec) EnteredZone.from
  window <- Fields.defaulted "window" EntryWindow.Type.ThisTurn EntryWindow.codec EnteredZone.window
  under <- Fields.defaulted "under" Nothing (Common.maybe PlayerRelation.codec) EnteredZone.under
  pure EnteredZone.MkEnteredZone {EnteredZone.to = to, EnteredZone.from = from, EnteredZone.window = window, EnteredZone.under = under}

underIsPrinted :: EnteredZone.EnteredZone -> Either Text.Text EnteredZone.EnteredZone
underIsPrinted entered
  | null (EnteredZone.under entered) || (EnteredZone.to entered == Zone.Type.Battlefield && EnteredZone.window entered == EntryWindow.Type.ThisTurn) = Right entered
  | otherwise = Left (Text.pack "EnteredZone: under beside a zone other than Battlefield or a window other than ThisTurn is unprinted and not supported")
