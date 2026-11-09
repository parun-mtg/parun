{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DraftNoteTally where

import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.NoteFold as NoteFold
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DraftNoteTally as DraftNoteTally

-- | The tag that picks it is Parun.Codec.Quantity's "DraftNotes".
codec :: Codec.Codec DraftNoteTally.DraftNoteTally
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec DraftNoteTally.player
  card <- Fields.required "card" CardName.codec DraftNoteTally.card
  fold <- Fields.required "fold" NoteFold.codec DraftNoteTally.fold
  pure DraftNoteTally.MkDraftNoteTally {DraftNoteTally.player = player, DraftNoteTally.card = card, DraftNoteTally.fold = fold}
