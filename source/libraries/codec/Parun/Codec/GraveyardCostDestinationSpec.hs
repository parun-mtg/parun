module Parun.Codec.GraveyardCostDestinationSpec where

import qualified Parun.Codec.GraveyardCostDestination as GraveyardCostDestination
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.GraveyardCostDestination" $ do
  Spec.it s "LibraryBottom" $
    Common.assertCodec
      s
      GraveyardCostDestination.codec
      GraveyardCostDestination.LibraryBottom
      " \"LibraryBottom\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s GraveyardCostDestination.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s GraveyardCostDestination.codec
