module Parun.Types.Settle where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.Game as Game
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Throw as Throw

-- | What the roads below Parun.Engine.Resolve reach of the resolver: the
-- zone-change funnel, CR 614's loop and every instruction under them. A
-- PARAMETER rather than an import because Resolve sits above them in the module
-- graph, Parun.Types.ManaAbilityPerformer's reason; that record carries one for
-- the payment path.
--
-- Built by Parun.Engine.Resolve.Effect.settleWith around CR 730.1a's subgame
-- runner. A road with no resolver to reach passes `none`.
data Settle = MkSettle
  { -- | CR 614.11a: run what a replacement queued in an event's place
    -- (Resolve.runReplacementEffects).
    queued :: Game.Game (),
    -- | CR 614.1c with CR 616.1g's Voice of All example: run the as-enters
    -- effects the entries so far queued (Resolve.runEntryEffects), so a member
    -- of a batch acts before the next member's entry applies.
    entryEffects :: Game.Game (),
    -- | CR 706.1: throw and record a roll settled below Resolve -- Pokey, the
    -- Scallywagg's instead of a coin flip (CR 614.6), the planar die (CR 901.9)
    -- -- answering the kept results. Nothing where no thrower reaches, which
    -- keeps Pokey's substitution out of the offer
    -- (Parun.Engine.Event.flipOneCoin).
    throwRecorded :: Maybe (Throw.Throw -> Game.Game [Natural.Natural]),
    -- | CR 705.1: the window Goblin Bookie's reflip is activated in, over the
    -- flipper's coin just flipped; answers whether it is flipped again.
    reflipWindow :: PlayerId.PlayerId -> CoinFace.CoinFace -> Game.Game Bool
  }

-- | A road that reaches no resolver: nothing queued runs, no roll is thrown and
-- no reflip window opens.
none :: Settle
none =
  MkSettle
    { queued = pure (),
      entryEffects = pure (),
      throwRecorded = Nothing,
      reflipWindow = \_ _ -> pure False
    }
