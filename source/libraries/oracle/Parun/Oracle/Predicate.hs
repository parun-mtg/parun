-- One instruction as the parts of a sentence (CR 608.2c), which
-- Parun.Oracle.Effect strings together and its arm modules build.
module Parun.Oracle.Predicate where

import qualified Data.Text as Text
import qualified Parun.Oracle.Target as Target
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

-- | One instruction as a sentence's parts: who does it (none for an
-- imperative, "Destroy target creature"), the verb phrase agreeing with them,
-- the sentences that must follow it ("It can't be regenerated."), the target
-- slots the verb phrase names, the mana it adds, and whether its subject is a
-- "you" that a sentence of its own leaves out ("Draw a card.") and a sentence
-- joined by "and" keeps ("You draw a card and gain 2 life.").
data Predicate = MkPredicate
  { subject :: Maybe Target.Reference,
    verb :: Text.Text,
    after :: [Text.Text],
    mentions :: [SlotName.SlotName],
    mana :: Maybe Mana,
    implied :: Bool
  }
  deriving (Eq, Show)

-- | Mana an instruction adds: symbols, which a following addition joins
-- ("Add {C}{C}."), or a description, which stands alone ("Add one mana of any
-- color.").
data Mana
  = Symbols Text.Text
  | Described
  deriving (Eq, Show)

-- | The predicates with these slots named in them too.
naming :: [SlotName.SlotName] -> [Predicate] -> [Predicate]
naming slots = fmap (\p -> p {mentions = mentions p <> slots})

-- | A player's instruction that Oracle text gives "you" as an imperative
-- when it stands as a sentence of its own ("Draw two cards.").
imperative :: Target.Context -> PlayerRef.PlayerRef -> Text.Text -> Text.Text -> [Predicate]
imperative context ref second third = do
  (who, person) <- Target.player context ref
  case person of
    Target.Second -> [MkPredicate (Just who) second [] [] Nothing True]
    Target.Third -> [MkPredicate (Just who) third [] [] Nothing False]
    Target.ThirdPlural -> [MkPredicate (Just who) second [] [] Nothing False]

-- | A player's instruction that always names the player: "You gain 4 life."
declarative :: Target.Context -> PlayerRef.PlayerRef -> Text.Text -> Text.Text -> [Predicate]
declarative context ref second third = do
  (who, person) <- Target.player context ref
  case person of
    Target.Second -> [MkPredicate (Just who) second [] [] Nothing False]
    Target.Third -> [MkPredicate (Just who) third [] [] Nothing False]
    Target.ThirdPlural -> [MkPredicate (Just who) second [] [] Nothing False]
