{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChosenPermanents where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.HowMany as HowMany
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.TotalCap as TotalCap
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChosenPermanents as ChosenPermanents
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Zone as Zone

-- | A bare object keyed by the record's field names, the shape every other
-- 'Parun.Types.ObjectRef' payload record takes.
--
-- @chooser@, @count@, @zone@ and @ownZones@ are defaulted, so the printed
-- choice of one permanent addressed to the resolving controller writes none of
-- those keys.
--
-- The zone must be one CR 400.1 shares: a per-player zone's choice is
-- Parun.Types.ChosenCardInGraveyard's or ChosenCardInHand's, which say whose,
-- or an @ownZones@ entry's, which is the chooser's. An @ownZones@ entry must be
-- a hand or graveyard, and its chooser not a relation that can name several
-- seats.
codec :: Codec.Codec ChosenPermanents.ChosenPermanents
codec = Fields.objectWith validZones $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ChosenPermanents.filter
  chooser <- Fields.defaulted "chooser" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec ChosenPermanents.chooser
  count <- Fields.defaulted "count" HowMany.one HowMany.codec ChosenPermanents.count
  total <- Fields.defaulted "total" Nothing (Common.maybe TotalCap.codec) ChosenPermanents.total
  zone <- Fields.defaulted "zone" Zone.Battlefield Zone.codec ChosenPermanents.zone
  ownZones <- Fields.defaulted "ownZones" Set.empty (Common.set Zone.codec) ChosenPermanents.ownZones
  pure
    ChosenPermanents.MkChosenPermanents
      { ChosenPermanents.filter = filter_,
        ChosenPermanents.chooser = chooser,
        ChosenPermanents.count = count,
        ChosenPermanents.total = total,
        ChosenPermanents.zone = zone,
        ChosenPermanents.ownZones = ownZones
      }

validZones :: ChosenPermanents.ChosenPermanents -> Either Text.Text ChosenPermanents.ChosenPermanents
validZones choice
  | not (Zone.shared (ChosenPermanents.zone choice)) = Left (Text.pack ("ChosenPermanents: CR 400.1 gives each player their own " <> show (ChosenPermanents.zone choice)))
  | Just zone <- List.find Zone.shared (Set.toList (ChosenPermanents.ownZones choice)) = Left (Text.pack ("ChosenPermanents: CR 400.1 gives no player their own " <> show zone))
  | Set.member Zone.Library (ChosenPermanents.ownZones choice) = Left (Text.pack "ChosenPermanents: CR 701.23a finds a library card by a search")
  | PlayerRef.Relative relation <- ChosenPermanents.chooser choice,
    relation /= PlayerRelation.You,
    not (Set.null (ChosenPermanents.ownZones choice)) =
      Left (Text.pack "ChosenPermanents: ownZones needs a chooser naming one seat")
  | otherwise = Right choice
