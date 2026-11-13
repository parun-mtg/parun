module Parun.Codec.EnteringTogetherSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Parun.Codec.EnteringTogether as EnteringTogether
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CandidateId as CandidateId
import qualified Parun.Types.EnteringTogether as EnteringTogether
import qualified Parun.Types.FloatingCandidate as FloatingCandidate
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.EnteringTogether" $ do
  -- CR 613.7m: a moved card and a token, in arrival order; only the token waits
  -- for its CR 111.2 entry event.
  Spec.it s "a batch mid-action" $
    Common.assertCodec
      s
      EnteringTogether.codec
      EnteringTogether.MkEnteringTogether
        { EnteringTogether.arrivals = Seq.fromList [ObjectId.MkObjectId 7, ObjectId.MkObjectId 4],
          EnteringTogether.minted = Seq.singleton (ObjectId.MkObjectId 4),
          EnteringTogether.substitutions = Map.empty
        }
      " {\"arrivals\":[7,4],\"minted\":[4]} "
  -- CR 614.1a: a token row's controller chose to copy object 9 instead.
  Spec.it s "a batch a token replacement answered for" $
    Common.assertCodec
      s
      EnteringTogether.codec
      EnteringTogether.empty
        { EnteringTogether.substitutions =
            Map.singleton
              (CandidateId.OfFloating FloatingCandidate.MkFloatingCandidate {FloatingCandidate.source = ObjectId.MkObjectId 3, FloatingCandidate.timestamp = Timestamp.MkTimestamp 8})
              (OptionalDecision.Exercises, Just (ObjectId.MkObjectId 9))
        }
      " {\"substitutions\":[{\"by\":{\"type\":\"OfFloating\",\"value\":{\"source\":3,\"timestamp\":8}},\"decision\":\"Exercises\",\"copied\":9}]} "
  Spec.it s "an empty batch" $
    Common.assertCodec s EnteringTogether.codec EnteringTogether.empty " {} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s EnteringTogether.codec
