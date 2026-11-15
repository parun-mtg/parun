{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EntryHost where

import qualified Parun.Codec.CardSelection as CardSelection
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CardSelection as CardSelection.Type
import qualified Parun.Types.EntryHost as EntryHost
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.Keyword as Keyword.Type
import qualified Parun.Types.PlayerRef as PlayerRef.Type

-- | CR 303.4i's specified host, or CR 303.4f's chosen one narrowed by a filter.
codec :: Codec.Codec EntryHost.EntryHost
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Specified" SlotName.codec EntryHost.Specified (\x -> case x of EntryHost.Specified y -> Just y; _ -> Nothing),
      Arm.payload "Chosen" chosen (\(selection, chooser, filter_) -> EntryHost.Chosen selection chooser filter_) (\x -> case x of EntryHost.Chosen selection chooser filter_ -> Just (selection, chooser, filter_); _ -> Nothing)
    ]

-- Chosen's payload, keyed by field; @selection@ is elided rather than written
-- ByChoice, and @chooser@ where it is the player the Aura enters under.
chosen :: Codec.Codec (CardSelection.Type.CardSelection, Maybe PlayerRef.Type.PlayerRef, Filter.Type.Filter Keyword.Type.Keyword)
chosen = Fields.object $ do
  selection <- Fields.defaulted "selection" CardSelection.Type.ByChoice CardSelection.codec (\(x, _, _) -> x)
  chooser <- Fields.defaulted "chooser" Nothing (Common.maybe PlayerRef.codec) (\(_, x, _) -> x)
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) (\(_, _, x) -> x)
  pure (selection, chooser, filter_)

tagOf :: EntryHost.EntryHost -> String
tagOf x = case x of
  EntryHost.Specified {} -> "Specified"
  EntryHost.Chosen {} -> "Chosen"
