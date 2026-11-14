{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CardAbilities where

import qualified Parun.Codec.AbilityKind as AbilityKind
import qualified Parun.Codec.ActivationRestriction as ActivationRestriction
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.LentCards as LentCards
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CardAbilities as CardAbilities
import qualified Parun.Types.LentCards as LentCards.Type

-- | A bare object keyed by the record's field names; `from` defaults to exile,
-- `triggered` to false, `only` to both kinds and `restrictions` to none.
codec :: Codec.Codec CardAbilities.CardAbilities
codec = Fields.object $ do
  from <- Fields.defaulted "from" LentCards.Type.InExile LentCards.codec CardAbilities.from
  cards <- Fields.required "cards" (Filter.codec Keyword.codec) CardAbilities.cards
  triggered <- Fields.defaulted "triggered" False Common.boolean CardAbilities.triggered
  only <- Fields.defaulted "only" Nothing (Common.maybe AbilityKind.codec) CardAbilities.only
  restrictions <- Fields.defaulted "restrictions" [] (Common.list ActivationRestriction.codec) CardAbilities.restrictions
  pure CardAbilities.MkCardAbilities {CardAbilities.from = from, CardAbilities.cards = cards, CardAbilities.triggered = triggered, CardAbilities.only = only, CardAbilities.restrictions = restrictions}
