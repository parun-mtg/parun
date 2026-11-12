module Parun.Codec.AbilityWasActivatedSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.AbilityWasActivated as AbilityWasActivated
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityFacts as AbilityFacts
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.AbilityWasActivated as AbilityWasActivated
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LoyaltyKind as LoyaltyKind
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AbilityWasActivated" $ do
  Spec.it s "MkAbilityWasActivated, every key" $
    Common.assertCodec
      s
      AbilityWasActivated.codec
      ( AbilityWasActivated.MkAbilityWasActivated
          { AbilityWasActivated.player = PlayerId.MkPlayerId 1,
            AbilityWasActivated.ability = Just (ObjectId.MkObjectId 2),
            AbilityWasActivated.source = ObjectId.MkObjectId 3,
            AbilityWasActivated.facts = AbilityFacts.MkAbilityFacts (Just Keyword.Boast) (Just (AbilityName.MkAbilityName (Text.pack "boast"))) AbilityKind.NonManaAbility LoyaltyKind.LoyaltyAbility,
            AbilityWasActivated.activationCost = Just (ManaCost.MkManaCost [ManaSymbol.Variable])
          }
      )
      " {\"player\":1,\"ability\":2,\"source\":3,\"keyword\":{\"type\":\"Boast\"},\"name\":\"boast\",\"kind\":\"NonManaAbility\",\"loyalty\":\"LoyaltyAbility\",\"activationCost\":[{\"type\":\"Variable\"}]} "
  -- CR 605.3b: a mana ability puts nothing on the stack, so no ability key; a
  -- cost with no mana, no activationCost key; an unstamped, unnamed ability, no
  -- keyword or name key.
  Spec.it s "MkAbilityWasActivated, no stack object" $
    Common.assertCodec
      s
      AbilityWasActivated.codec
      ( AbilityWasActivated.MkAbilityWasActivated
          { AbilityWasActivated.player = PlayerId.MkPlayerId 1,
            AbilityWasActivated.ability = Nothing,
            AbilityWasActivated.source = ObjectId.MkObjectId 3,
            AbilityWasActivated.facts = AbilityFacts.MkAbilityFacts Nothing Nothing AbilityKind.ManaAbility LoyaltyKind.NonLoyaltyAbility,
            AbilityWasActivated.activationCost = Nothing
          }
      )
      " {\"player\":1,\"source\":3,\"kind\":\"ManaAbility\",\"loyalty\":\"NonLoyaltyAbility\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityWasActivated.codec
