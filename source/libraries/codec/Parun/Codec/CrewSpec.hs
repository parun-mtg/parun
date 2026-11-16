module Parun.Codec.CrewSpec where

import qualified Parun.Codec.Crew as Crew
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Crew as Crew

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Crew" $ do
  -- CR 702.122a: plain crew, so the rider is elided.
  Spec.it s "MkCrew" $
    Common.assertCodec
      s
      Crew.codec
      Crew.MkCrew {Crew.power = 3, Crew.timesEachTurn = Nothing}
      " {\"power\":3} "
  -- CR 602.5b: Luxurious Locomotive's "Crew 1. Activate only once each turn."
  Spec.it s "MkCrew limited each turn" $
    Common.assertCodec
      s
      Crew.codec
      Crew.MkCrew {Crew.power = 1, Crew.timesEachTurn = Just 1}
      " {\"power\":1,\"timesEachTurn\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Crew.codec
