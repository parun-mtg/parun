module Parun.Codec.PlayerDrawsNthCardSpec where

import qualified Parun.Codec.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.StepBegins as StepBegins
import qualified Parun.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayerDrawsNthCard" $ do
  -- Underworld Dreams' shape read from the other side: WHICH draw of the turn
  -- fires the ability, not how many cards are drawn.
  Spec.it s "MkPlayerDrawsNthCard, both keys" $
    Common.assertCodec
      s
      PlayerDrawsNthCard.codec
      ( PlayerDrawsNthCard.MkPlayerDrawsNthCard
          { PlayerDrawsNthCard.player = PlayerRelation.You,
            PlayerDrawsNthCard.nth = Just 2,
            PlayerDrawsNthCard.within = Nothing
          }
      )
      " {\"player\":\"You\",\"nth\":2} "
  -- Teferi, Hero of Dominaria's emblem: every draw, so no key.
  Spec.it s "no ordinal writes no key" $
    Common.assertCodec
      s
      PlayerDrawsNthCard.codec
      (PlayerDrawsNthCard.MkPlayerDrawsNthCard PlayerRelation.You Nothing Nothing)
      " {\"player\":\"You\"} "
  -- Wiretapping: the first draw of each of your draw steps.
  Spec.it s "a step writes its StepBegins" $
    Common.assertCodec
      s
      PlayerDrawsNthCard.codec
      ( PlayerDrawsNthCard.MkPlayerDrawsNthCard
          { PlayerDrawsNthCard.player = PlayerRelation.You,
            PlayerDrawsNthCard.nth = Just 1,
            PlayerDrawsNthCard.within =
              Just
                ( StepBegins.MkStepBegins
                    { StepBegins.ordinal = Nothing,
                      StepBegins.phase = Phase.Beginning BeginningStep.DrawStep,
                      StepBegins.scope = TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.You)
                    }
                )
          }
      )
      " {\"player\":\"You\",\"nth\":1,\"within\":{\"phase\":{\"type\":\"Beginning\",\"value\":\"DrawStep\"},\"scope\":{\"type\":\"PlayersTurn\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerDrawsNthCard.codec
