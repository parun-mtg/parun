{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AbilityWasActivated where

import qualified Parun.Codec.AbilityKind as AbilityKind
import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.LoyaltyKind as LoyaltyKind
import qualified Parun.Codec.ManaCost as ManaCost
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AbilityFacts as AbilityFacts
import qualified Parun.Types.AbilityWasActivated as AbilityWasActivated

-- | Parun.Codec.SpellWasCopied's shape, the facts' fields flattened beside the
-- event's own. Runtime-only, GameEvent serialising transcripts rather than
-- card data.
codec :: Codec.Codec AbilityWasActivated.AbilityWasActivated
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec AbilityWasActivated.player
  ability <- Fields.defaulted "ability" Nothing (Common.maybe ObjectId.codec) AbilityWasActivated.ability
  source <- Fields.required "source" ObjectId.codec AbilityWasActivated.source
  keyword <- Fields.defaulted "keyword" Nothing (Common.maybe Keyword.codec) (AbilityFacts.keyword . AbilityWasActivated.facts)
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) (AbilityFacts.name . AbilityWasActivated.facts)
  kind <- Fields.required "kind" AbilityKind.codec (AbilityFacts.kind . AbilityWasActivated.facts)
  loyalty <- Fields.required "loyalty" LoyaltyKind.codec (AbilityFacts.loyalty . AbilityWasActivated.facts)
  activationCost <- Fields.defaulted "activationCost" Nothing (Common.maybe ManaCost.codec) AbilityWasActivated.activationCost
  pure
    AbilityWasActivated.MkAbilityWasActivated
      { AbilityWasActivated.player = player,
        AbilityWasActivated.ability = ability,
        AbilityWasActivated.source = source,
        AbilityWasActivated.facts =
          AbilityFacts.MkAbilityFacts
            { AbilityFacts.keyword = keyword,
              AbilityFacts.name = name,
              AbilityFacts.kind = kind,
              AbilityFacts.loyalty = loyalty
            },
        AbilityWasActivated.activationCost = activationCost
      }
