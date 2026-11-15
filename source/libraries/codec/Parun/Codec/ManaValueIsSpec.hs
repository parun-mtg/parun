module Parun.Codec.ManaValueIsSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ManaValueIs as ManaValueIs
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Label as Label.Type
import qualified Parun.Types.ManaValueIs as ManaValueIs.Type
import qualified Parun.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaValueIs" $ do
  Spec.it s "an object and its mana value" $
    Common.assertCodec s ManaValueIs.codec (ManaValueIs.Type.MkManaValueIs (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "akki"))) 4) " {\"object\":\"$akki\",\"value\":4} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaValueIs.codec
