module Parun.Codec.UntapRewriteSpec where

import qualified Data.Sequence as Seq
import qualified Parun.Codec.Card as Card
import qualified Parun.Codec.Effect as Effect
import qualified Parun.Codec.GrantedAbility as GrantedAbility
import qualified Parun.Codec.UntapRewrite as UntapRewrite
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Effect as Effect.Type
import qualified Parun.Types.UntapRewrite as UntapRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.UntapRewrite" $ do
  -- CR 122.1d's replacement. Minted from a permanent's stun counters and never
  -- authored on a card, so this codec is the only place its wire form is pinned.
  Spec.it s "RemoveStunCounter" $
    Common.assertCodec
      s
      (UntapRewrite.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
      UntapRewrite.RemoveStunCounter
      " {\"type\":\"RemoveStunCounter\"} "
  -- Bewitching Leechcraft's "remove a +1/+1 counter from it instead. If you do,
  -- untap it."
  Spec.it s "RemoveCounterToUntap" $
    Common.assertCodec
      s
      (UntapRewrite.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
      (UntapRewrite.RemoveCounterToUntap CounterKind.PlusOnePlusOne)
      " {\"type\":\"RemoveCounterToUntap\",\"value\":{\"type\":\"PlusOnePlusOne\"}} "
  -- Edge of Malacol's "put two +1/+1 counters on it instead".
  Spec.it s "RunEffects" $
    Common.assertCodec
      s
      (UntapRewrite.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
      (UntapRewrite.RunEffects (Seq.singleton (Effect.Type.Proliferate Nothing)))
      " {\"type\":\"RunEffects\",\"value\":[{\"type\":\"Proliferate\"}]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s (UntapRewrite.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
