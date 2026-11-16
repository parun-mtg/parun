module Parun.Codec.BecomesTapStateSpec where

import qualified Parun.Codec.BecomesTapState as BecomesTapState
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.BecomesTapState as BecomesTapState
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.DuringPhase as DuringPhase
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PhaseSelector as PhaseSelector
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BecomesTapState" $ do
  Spec.it s "MkBecomesTapState, both keys" $
    Common.assertCodec
      s
      BecomesTapState.codec
      ( BecomesTapState.MkBecomesTapState
          { BecomesTapState.status = TapState.Untapped,
            BecomesTapState.actor = ActingPermanent.Matching (Filter.HeldBy Holder.Controller PlayerRelation.You),
            BecomesTapState.batching = Batching.EachOccurrence,
            BecomesTapState.paying = Nothing,
            BecomesTapState.while = Nothing
          }
      )
      " {\"actor\":{\"type\":\"Matching\",\"value\":{\"type\":\"HeldBy\",\"value\":{\"holder\":\"Controller\",\"relation\":\"You\"}}},\"status\":\"Untapped\"} "
  -- Deeproot Pilgrimage's "one or more", the batching key written.
  Spec.it s "MkBecomesTapState, a batch" $
    Common.assertCodec
      s
      BecomesTapState.codec
      (BecomesTapState.MkBecomesTapState TapState.Tapped (ActingPermanent.Matching (Filter.Not Filter.IsToken)) Batching.OneOrMore Nothing Nothing)
      " {\"actor\":{\"type\":\"Matching\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"IsToken\"}}},\"status\":\"Tapped\",\"batching\":\"OneOrMore\"} "
  -- Agent Maria Hill's "to pay a teamwork cost", the paying key written.
  Spec.it s "MkBecomesTapState, paying a keyword's cost" $
    Common.assertCodec
      s
      BecomesTapState.codec
      (BecomesTapState.MkBecomesTapState TapState.Tapped ActingPermanent.Self Batching.EachOccurrence (Just KeywordFamily.Teamwork) Nothing)
      " {\"actor\":{\"type\":\"Self\"},\"status\":\"Tapped\",\"paying\":\"Teamwork\"} "
  -- The Ninth Doctor's "during your untap step", the while key written.
  Spec.it s "MkBecomesTapState, while a condition holds" $
    Common.assertCodec
      s
      BecomesTapState.codec
      (BecomesTapState.MkBecomesTapState TapState.Untapped ActingPermanent.Self Batching.EachOccurrence Nothing (Just (Condition.During (DuringPhase.MkDuringPhase (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)) (TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.You))))))
      " {\"actor\":{\"type\":\"Self\"},\"status\":\"Untapped\",\"while\":{\"type\":\"During\",\"value\":{\"window\":{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":\"Untap\"}},\"scope\":{\"type\":\"PlayersTurn\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecomesTapState.codec
