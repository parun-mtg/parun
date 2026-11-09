module Parun.Codec.DraftBoosterSpec where

import qualified Parun.Codec.DraftBooster as DraftBooster
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DraftBooster as DraftBooster

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DraftBooster" $ do
  Spec.it s "CommanderLegends" $
    Common.assertCodec s DraftBooster.codec DraftBooster.CommanderLegends " \"CommanderLegends\" "
  Spec.it s "CommanderMasters" $
    Common.assertCodec s DraftBooster.codec DraftBooster.CommanderMasters " \"CommanderMasters\" "
  Spec.it s "BattleForBaldursGate" $
    Common.assertCodec s DraftBooster.codec DraftBooster.BattleForBaldursGate " \"BattleForBaldursGate\" "
