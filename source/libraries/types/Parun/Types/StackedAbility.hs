module Parun.Types.StackedAbility where

import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Card as Card
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.TriggeredAbility as TriggeredAbility

-- | CR 113.7a: which ability an ability on the stack is, as every seat may
-- read it (CR 400.2).
data StackedAbility
  = -- | CR 602.2a: an activated ability.
    Activated (ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card))
  | -- | CR 603.3: a triggered ability.
    Triggered (TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card))
  deriving (Eq, Show)
