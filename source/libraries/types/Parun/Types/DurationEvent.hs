module Parun.Types.DurationEvent where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.SlotName as SlotName

-- | CR 611.2a: the event a stated "until ..." duration ends at.
data DurationEvent
  = -- | CR 701.27b: the effect's source is turned face down (Vesuvan Shapeshifter).
    SourceTurnedFaceDown
  | -- | CR 601.2i: a player casts a spell this matches (Soul Sculptor).
    SpellCast (Filter.Filter Keyword.Keyword)
  | -- | CR 901.11: a player planeswalks (Agyrem).
    Planeswalked
  | -- | CR 607.2a: an instruction of the effect's source exiled a card (Furious Rise).
    SourceExiledCard
  | -- | CR 611.2a / 400.7: the effect's source leaves the battlefield, which phasing out is not (CR 702.26d; Gaea's Liege).
    SourceLeftBattlefield
  | -- | CR 601.2i / 400.7: the card this slot bound is cast, as that very object (Emrakul, the Exigent Doom).
    BoundCardCast SlotName.SlotName
  | -- | BoundCardCast with its slot baked by Parun.Engine.Expiry.arm; runtime-only, Parun.Types.Filter's IsObject posture.
    CardCast ObjectId.ObjectId
  deriving (Eq, Ord, Show)
