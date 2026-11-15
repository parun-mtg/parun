module Parun.Codec.StackPlacementSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.PlacementSpec as PlacementSpec
import qualified Parun.Codec.StackPlacement as StackPlacement
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Label as Label.Type
import qualified Parun.Types.Placement as Placement.Type
import qualified Parun.Types.StackPlacement as StackPlacement.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.StackPlacement" $ do
  Spec.it s "an owner beside the placement's own keys" $
    Common.assertCodec
      s
      StackPlacement.codec
      StackPlacement.Type.MkStackPlacement
        { StackPlacement.Type.owner = Label.Type.MkLabel (Text.pack "alice"),
          StackPlacement.Type.placement =
            (PlacementSpec.plain "Divination")
              { Placement.Type.label = Just (Label.Type.MkLabel (Text.pack "divination")),
                Placement.Type.controller = Just (Label.Type.MkLabel (Text.pack "bob"))
              }
        }
      " {\"owner\":\"alice\",\"card\":\"Divination\",\"label\":\"divination\",\"controller\":\"bob\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s StackPlacement.codec
