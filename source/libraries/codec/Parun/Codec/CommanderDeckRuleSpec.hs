module Parun.Codec.CommanderDeckRuleSpec where

import qualified Parun.Codec.CommanderDeckRule as CommanderDeckRule
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CommanderDeckRule" $ do
  Spec.it s "NoMaximumDeckSize" $
    Common.assertCodec s CommanderDeckRule.codec CommanderDeckRule.NoMaximumDeckSize " {\"type\":\"NoMaximumDeckSize\"} "
  -- CR 903.5e: Seluma, Light of Aysen's Angel cards.
  Spec.it s "AnyColorIdentity" $
    Common.assertCodec s CommanderDeckRule.codec (CommanderDeckRule.AnyColorIdentity (Filter.HasSubtype Subtype.Angel)) " {\"type\":\"AnyColorIdentity\",\"value\":{\"type\":\"HasSubtype\",\"value\":\"Angel\"}} "
  -- CR 903.5e: Tolabow, Loch Rascal's instant cards.
  Spec.it s "ChosenExtraColor" $
    Common.assertCodec s CommanderDeckRule.codec (CommanderDeckRule.ChosenExtraColor (Filter.HasCardType CardType.Instant)) " {\"type\":\"ChosenExtraColor\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Instant\"}} "
