module Parun.Codec.CommanderColorsSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.CommanderColors as CommanderColors
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CommanderColors as CommanderColors
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CommanderColors" $ do
  Spec.it s "MkCommanderColors" $
    Common.assertCodec
      s
      CommanderColors.codec
      ( CommanderColors.MkCommanderColors
          { CommanderColors.player = PlayerRef.Relative PlayerRelation.You,
            CommanderColors.colors = Set.singleton Color.Red
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"colors\":[\"Red\"]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CommanderColors.codec
