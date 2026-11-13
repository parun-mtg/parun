{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EntryHost where

import qualified Parun.Codec.CardSelection as CardSelection
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CardSelection as CardSelection.Type
import qualified Parun.Types.EntryHost as EntryHost
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.Keyword as Keyword.Type

-- | CR 303.4i's specified host, or CR 303.4f's chosen one narrowed by a filter.
codec :: Codec.Codec EntryHost.EntryHost
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Specified" SlotName.codec EntryHost.Specified (\x -> case x of EntryHost.Specified y -> Just y; _ -> Nothing),
      Arm.payload "Chosen" chosen (uncurry EntryHost.Chosen) (\x -> case x of EntryHost.Chosen selection filter_ -> Just (selection, filter_); _ -> Nothing)
    ]

-- Chosen's payload, keyed by field; @selection@ is elided rather than written
-- ByChoice.
chosen :: Codec.Codec (CardSelection.Type.CardSelection, Filter.Type.Filter Keyword.Type.Keyword)
chosen = Fields.object $ do
  selection <- Fields.defaulted "selection" CardSelection.Type.ByChoice CardSelection.codec fst
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) snd
  pure (selection, filter_)

tagOf :: EntryHost.EntryHost -> String
tagOf x = case x of
  EntryHost.Specified {} -> "Specified"
  EntryHost.Chosen {} -> "Chosen"
