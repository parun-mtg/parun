module Parun.Codec.UntapRewrite where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.UntapRewrite as UntapRewrite

-- | The effect codec is a PARAMETER, Parun.Codec.DamageR's reason.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (UntapRewrite.UntapRewrite effect)
codec effectCodec =
  Arm.tagged
    tagOf
    [ Arm.nullary "RemoveStunCounter" UntapRewrite.RemoveStunCounter,
      Arm.payload "RemoveCounterToUntap" (CounterKind.codec Keyword.codec) UntapRewrite.RemoveCounterToUntap (\x -> case x of UntapRewrite.RemoveCounterToUntap y -> Just y; _ -> Nothing),
      Arm.payload "RunEffects" (Common.seq effectCodec) UntapRewrite.RunEffects (\x -> case x of UntapRewrite.RunEffects y -> Just y; _ -> Nothing)
    ]

tagOf :: UntapRewrite.UntapRewrite effect -> String
tagOf x = case x of
  UntapRewrite.RemoveStunCounter -> "RemoveStunCounter"
  UntapRewrite.RemoveCounterToUntap {} -> "RemoveCounterToUntap"
  UntapRewrite.RunEffects {} -> "RunEffects"
