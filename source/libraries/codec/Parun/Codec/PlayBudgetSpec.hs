module Parun.Codec.PlayBudgetSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.PlayBudget as PlayBudget
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayAllowance as PlayAllowance
import qualified Parun.Types.PlayBudget as PlayBudget
import qualified Parun.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayBudget" $ do
  Spec.it s "March of Reckless Joy's two plays over three cards" $
    Common.assertCodec
      s
      PlayBudget.codec
      PlayBudget.MkPlayBudget
        { PlayBudget.remaining = PlayAllowance.Plays 2,
          PlayBudget.covers = Set.fromList (fmap ObjectId.MkObjectId [4, 5, 6]),
          PlayBudget.granted = Timestamp.MkTimestamp 7
        }
      " {\"remaining\":{\"type\":\"Plays\",\"value\":2},\"covers\":[4,5,6],\"granted\":7} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayBudget.codec
