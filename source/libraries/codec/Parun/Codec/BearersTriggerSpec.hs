module Parun.Codec.BearersTriggerSpec where

import qualified Parun.Codec.BearersTrigger as BearersTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.BearersTrigger as BearersTrigger
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.EntryTriggering as EntryTriggering
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.KeywordFamily as KeywordFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BearersTrigger" $ do
  -- Harmonic Prodigy's "a triggered ability of a Shaman or another Wizard".
  Spec.it s "MkBearersTrigger" $
    Common.assertCodec
      s
      BearersTrigger.codec
      ( BearersTrigger.MkBearersTrigger
          { BearersTrigger.bearers = Filter.HasCardType CardType.Creature,
            BearersTrigger.abilities = Nothing,
            BearersTrigger.triggering = EntryTriggering.AnAdditionalTime
          }
      )
      " {\"bearers\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"triggering\":\"AnAdditionalTime\"} "
  -- Nowhere to Run's "ward abilities of those creatures don't trigger".
  Spec.it s "MkBearersTrigger abilities" $
    Common.assertCodec
      s
      BearersTrigger.codec
      ( BearersTrigger.MkBearersTrigger
          { BearersTrigger.bearers = Filter.HasCardType CardType.Creature,
            BearersTrigger.abilities = Just (KeywordDesignator.OfFamily KeywordFamily.Ward),
            BearersTrigger.triggering = EntryTriggering.NoTrigger
          }
      )
      " {\"abilities\":{\"type\":\"OfFamily\",\"value\":\"Ward\"},\"bearers\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"triggering\":\"NoTrigger\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s BearersTrigger.codec
