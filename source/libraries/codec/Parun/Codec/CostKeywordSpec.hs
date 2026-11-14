module Parun.Codec.CostKeywordSpec where

import qualified Parun.Codec.CostKeyword as CostKeyword
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CostKeyword as CostKeyword
import qualified Parun.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CostKeyword" $ do
  Spec.it s "Scavenge" $
    Common.assertCodec
      s
      CostKeyword.codec
      CostKeyword.Scavenge
      " \"Scavenge\" "
  Spec.it s "Unearth" $
    Common.assertCodec
      s
      CostKeyword.codec
      CostKeyword.Unearth
      " \"Unearth\" "
  Spec.it s "BeamMeUp restricting no creature" $
    Common.assertCodec
      s
      CostKeyword.codec
      (CostKeyword.BeamMeUp Nothing)
      " \"BeamMeUp\" "
  -- Scotty, Miracle Worker's "Scotty can't be beamed up to cast a spell this way".
  Spec.it s "BeamMeUp restricting its granter" $
    Common.assertCodec
      s
      CostKeyword.codec
      (CostKeyword.BeamMeUp (Just Filter.IsSource))
      " {\"BeamMeUp\":{\"type\":\"IsSource\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CostKeyword.codec
