module Parun.Types.ExiledPile where

import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.CardName as CardName

-- | CR 607.2a: which of the cards in exile linked to a source a reader means,
-- as Parun.Engine.Binding.isLinkedExile answers it.
data ExiledPile
  = -- | CR 607.2a / 607.4: every card an ability the source has of its own
    -- exiled (Steward of the Harvest, Sutured Ghoul, Patchwork Crawler).
    OwnAbilities
  | -- | CR 607.2a: every card the named ability of the source exiled (Synthetic
    -- Split Reliquary).
    OfAbility AbilityName.AbilityName
  | -- | CR 607.2d: the one card of OwnAbilities' pile last chosen for the source
    -- (Koh, the Face Stealer).
    LastChosen
  | -- | CR 607.2b / 614.14: every card the source's as-enters exile took
    -- (Mimeoplasm, Revered One).
    AsItEntered
  | -- | CR 702.167c: the cards exiled to craft the source (Locus of
    -- Enlightenment).
    CraftMaterials
  | -- | CR 607.2n: every card the reading player exiled before the game with
    -- cards of that name (Arcane Savant), whatever the source.
    WithCardsNamed CardName.CardName
  deriving (Eq, Ord, Show)
