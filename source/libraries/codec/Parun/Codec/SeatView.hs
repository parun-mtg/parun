{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SeatView where

import qualified Data.Map.Strict as Map
import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.DraftNote as DraftNote
import qualified Parun.Codec.Mana as Mana
import qualified Parun.Codec.ObjectView as ObjectView
import qualified Parun.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.SeatView as SeatView

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec SeatView.SeatView
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec SeatView.player
  life <- Fields.required "life" Common.integer SeatView.life
  counters <- Fields.required "counters" (Common.multiset PlayerCounterKind.codec) SeatView.counters
  handSize <- Fields.required "handSize" Common.natural SeatView.handSize
  hand <- Fields.required "hand" (Common.list ObjectView.codec) SeatView.hand
  librarySize <- Fields.required "librarySize" Common.natural SeatView.librarySize
  library <- Fields.required "library" (Common.list ObjectView.codec) SeatView.library
  graveyard <- Fields.required "graveyard" (Common.list ObjectView.codec) SeatView.graveyard
  exile <- Fields.required "exile" (Common.list ObjectView.codec) SeatView.exile
  manaPool <- Fields.required "manaPool" Mana.codec SeatView.manaPool
  draftNotes <- Fields.defaulted "draftNotes" Map.empty (Common.keyedList (Common.keyValue CardName.codec (Common.seq DraftNote.codec))) SeatView.draftNotes
  pure
    SeatView.MkSeatView
      { SeatView.player = player,
        SeatView.life = life,
        SeatView.counters = counters,
        SeatView.handSize = handSize,
        SeatView.hand = hand,
        SeatView.librarySize = librarySize,
        SeatView.library = library,
        SeatView.graveyard = graveyard,
        SeatView.exile = exile,
        SeatView.manaPool = manaPool,
        SeatView.draftNotes = draftNotes
      }
