module Parun.Codec.RuleFamilySpec where

import qualified Parun.Codec.RuleFamily as RuleFamily
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RuleFamily as RuleFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RuleFamily" $ do
  Spec.it s "Constructed" $
    Common.assertCodec s RuleFamily.codec RuleFamily.Constructed " \"Constructed\" "
  Spec.it s "Limited" $
    Common.assertCodec s RuleFamily.codec RuleFamily.Limited " \"Limited\" "
  Spec.it s "Commander" $
    Common.assertCodec s RuleFamily.codec RuleFamily.Commander " \"Commander\" "
  Spec.it s "Brawl" $
    Common.assertCodec s RuleFamily.codec RuleFamily.Brawl " \"Brawl\" "
  Spec.it s "CommanderDraft" $
    Common.assertCodec s RuleFamily.codec RuleFamily.CommanderDraft " \"CommanderDraft\" "
