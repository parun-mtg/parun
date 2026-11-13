module Parun.Codec.WatchedAbility where

import qualified Parun.Codec.ChapterMoment as ChapterMoment
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.WatchedAbility as WatchedAbility

codec :: Codec.Codec WatchedAbility.WatchedAbility
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "SagaFinalChapter" ChapterMoment.codec WatchedAbility.SagaFinalChapter (\x -> case x of WatchedAbility.SagaFinalChapter y -> Just y; _ -> Nothing),
      Arm.nullary "CausedByOwnAttack" WatchedAbility.CausedByOwnAttack
    ]

tagOf :: WatchedAbility.WatchedAbility -> String
tagOf x = case x of
  WatchedAbility.SagaFinalChapter {} -> "SagaFinalChapter"
  WatchedAbility.CausedByOwnAttack -> "CausedByOwnAttack"
