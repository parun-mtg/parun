module Parun.Codec.UntapRSpec where

import qualified Parun.Codec.Card as Card
import qualified Parun.Codec.Effect as Effect
import qualified Parun.Codec.GrantedAbility as GrantedAbility
import qualified Parun.Codec.UntapR as UntapR
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.UntapR as UntapR
import qualified Parun.Types.UntapRewrite as UntapRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.UntapR" $ do
  -- CR 122.1d: every untap.
  Spec.it s "MkUntapR (any untap)" $
    Common.assertCodec
      s
      (UntapR.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
      (UntapR.MkUntapR {UntapR.matching = Nothing, UntapR.during = Nothing, UntapR.rewrite = UntapRewrite.RemoveStunCounter})
      " {\"rewrite\":{\"type\":\"RemoveStunCounter\"}} "
  -- CR 502.3, Bewitching Leechcraft's "during your untap step".
  Spec.it s "MkUntapR (during the untap step)" $
    Common.assertCodec
      s
      (UntapR.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
      (UntapR.MkUntapR {UntapR.matching = Nothing, UntapR.during = Just (Phase.Beginning BeginningStep.Untap), UntapR.rewrite = UntapRewrite.RemoveCounterToUntap CounterKind.PlusOnePlusOne})
      " {\"during\":{\"type\":\"Beginning\",\"value\":\"Untap\"},\"rewrite\":{\"type\":\"RemoveCounterToUntap\",\"value\":{\"type\":\"PlusOnePlusOne\"}}} "
  -- CR 614.1a, Edge of Malacol's printed "a creature you control".
  Spec.it s "MkUntapR (a printed subject)" $
    Common.assertCodec
      s
      (UntapR.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
      (UntapR.MkUntapR {UntapR.matching = Just Filter.IsSource, UntapR.during = Nothing, UntapR.rewrite = UntapRewrite.RemoveStunCounter})
      " {\"matching\":{\"type\":\"IsSource\"},\"rewrite\":{\"type\":\"RemoveStunCounter\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (UntapR.codec (Effect.codec Card.codec (GrantedAbility.codec Card.codec)))
