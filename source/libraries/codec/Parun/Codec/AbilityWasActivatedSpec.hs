module Parun.Codec.AbilityWasActivatedSpec where

import qualified Parun.Codec.AbilityWasActivated as AbilityWasActivated
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.AbilityWasActivated as AbilityWasActivated
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
            AbilityWasActivated.loyalty = True,
            AbilityWasActivated.kind = AbilityKind.NonManaAbility,
            AbilityWasActivated.activationCost = Just (ManaCost.MkManaCost [ManaSymbol.Variable])
          }
      )
      " {\"player\":1,\"ability\":2,\"source\":3,\"loyalty\":true,\"kind\":\"NonManaAbility\",\"activationCost\":[{\"type\":\"Variable\"}]} "
  -- CR 605.3b: a mana ability puts nothing on the stack, so no ability key; a
  -- cost with no mana, no activationCost key.
  Spec.it s "MkAbilityWasActivated, no stack object" $
    Common.assertCodec
      s
      AbilityWasActivated.codec
      ( AbilityWasActivated.MkAbilityWasActivated
          { AbilityWasActivated.player = PlayerId.MkPlayerId 1,
            AbilityWasActivated.ability = Nothing,
            AbilityWasActivated.source = ObjectId.MkObjectId 3,
            AbilityWasActivated.loyalty = False,
            AbilityWasActivated.kind = AbilityKind.ManaAbility,
            AbilityWasActivated.activationCost = Nothing
          }
      )
      " {\"player\":1,\"source\":3,\"loyalty\":false,\"kind\":\"ManaAbility\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityWasActivated.codec
