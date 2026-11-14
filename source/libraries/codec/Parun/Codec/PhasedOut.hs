{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PhasedOut where

import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.ReturnEnding as ReturnEnding
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PhasedOut as PhasedOut
import qualified Parun.Types.PlayerId as PlayerId.Type
import qualified Parun.Types.ReturnEnding as ReturnEnding.Type

-- | Four arms: CR 702.26a's schedule, CR 702.26g's attached permanent, CR
-- 702.26n's rescheduled row, and CR 610.4's "until". The tag is the schedule
-- and the payload is the player, so neither is recoverable from the other.
codec :: Codec.Codec PhasedOut.PhasedOut
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Directly" PlayerId.codec PhasedOut.Directly (\x -> case x of PhasedOut.Directly y -> Just y; _ -> Nothing),
      Arm.payload "Indirectly" PlayerId.codec PhasedOut.Indirectly (\x -> case x of PhasedOut.Indirectly y -> Just y; _ -> Nothing),
      Arm.payload "Orphaned" PlayerId.codec PhasedOut.Orphaned (\x -> case x of PhasedOut.Orphaned y -> Just y; _ -> Nothing),
      Arm.payload "Until" untilCodec (uncurry PhasedOut.Until) (\x -> case x of PhasedOut.Until y z -> Just (y, z); _ -> Nothing)
    ]

-- | Until's two fields as an object; a spent ending is an absent key.
untilCodec :: Codec.Codec (PlayerId.Type.PlayerId, Maybe ReturnEnding.Type.ReturnEnding)
untilCodec = Fields.object $ do
  under <- Fields.required "under" PlayerId.codec fst
  ending <- Fields.defaulted "ending" Nothing (Common.maybe ReturnEnding.codec) snd
  pure (under, ending)

tagOf :: PhasedOut.PhasedOut -> String
tagOf x = case x of
  PhasedOut.Directly {} -> "Directly"
  PhasedOut.Indirectly {} -> "Indirectly"
  PhasedOut.Orphaned {} -> "Orphaned"
  PhasedOut.Until {} -> "Until"
