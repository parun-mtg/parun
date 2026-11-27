module Parun.Types.TapCause where

-- | Why Parun.Engine.Event's tap funnel turns a permanent sideways (CR 701.26a),
-- as far as a prohibition asks (Parun.Types.Prohibition's TapExceptAttacking).
data TapCause
  = -- | Its declaration as an attacker taps it (CR 508.1f).
    DeclaredAsAttacker
  | -- | Anything else: a cost, an effect, regeneration (CR 701.19a).
    OtherTap
  deriving (Bounded, Enum, Eq, Ord, Show)
