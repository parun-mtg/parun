# Web Deck Builder (slice 2: open a deck) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Open a saved deck in the builder to edit and save back under its
name, or a precon as a copy saved under a new name, losing nothing the
editor doesn't show.

**Architecture:** A pure `Builder.open` reads a list's text into the model
(main deck as counts, everything else verbatim in `suffixes` and `rest`),
reusing `Parun.DeckList`'s pure line readers (`formatNamed`,
`sectionNamed`, `entryOf`), so `web` gains a `parun:registry` dependency.
The lobby offers Edit (saved) or Edit a copy (precon) under the shown
deck; the shell reads the file and opens the builder on it.

**Tech Stack:** Haskell, `Parun.Spec`, the wasm `browser` library.

**Spec:** `docs/superpowers/specs/2026-11-20-web-deck-builder-design.md`
(slice 2, and "The deck model keeps what the editor doesn't show").

## Global Constraints

- CLAUDE.md applies whole, as in slice 1's plan.
- `listText (open text)` keeps every non-main line and every main-deck
  card's printing and annotations; only comments and blank lines inside
  the main deck may drop, and main-deck lines are re-sorted by name.
- An opened saved deck saves over itself; an opened precon never does.

## Review Focus

1. A commander precon (`Commander` section before `Deck`) round-trips with
   its commander kept (Task 1 test).
2. A list with no header and no section opens as a casual main deck
   (Task 1 test).
3. A card listed on two lines opens as their sum (Task 1 test).
4. Opening a precon pre-fills its name; saving refuses a clash rather
   than overwriting another saved deck (Task 1 test, `saveTarget`).
5. Adding then removing a card leaves nothing to save, so Back does not
   ask (Task 1 test; folds in slice 1's deferred minor).

---

### Task 1: `Builder.open` and a baseline for changes

**Files:** `source/libraries/web/Parun/Web/Builder.hs`,
`BuilderSpec.hs`, `parun.cabal` (`parun:registry` in `library web`).

**Interfaces:**
- `open :: Maybe Text -> Text -> Text -> Builder -> Builder`: the saved
  deck's name if it is one, the name to offer for saving, the list's text.
  Sets `format`, `deck`, `suffixes`, `rest`, `savedAs`, `saveName`, and the
  baseline; keeps `index`, `formats`.
- The `dirty :: Bool` field becomes `baseline :: (Map CardName Natural, Text)`
  (the deck and format last saved or opened) and
  `dirty :: Builder -> Bool` compares against it. `saved` resets it.
- Tests: "opens a commander precon and writes its commander back";
  "opens a list with no header as a casual main deck"; "sums a card
  listed twice"; "keeps a card's printing and annotations"; "an opened
  saved deck saves over itself"; "an opened precon offers its name and
  refuses a clash"; "adding then removing a card leaves nothing to save".

### Task 2: The lobby's Edit and the shell

**Files:** `Lobby.hs`, `LobbySpec.hs`, `Shell.hs`, `style.css`.

- `Lobby.MkEdit Text`: under the shown deck, "Edit" for a saved deck and
  "Edit a copy" for any other. Test: each label, and its event.
- Shell: `Next` gains `MkToBuilderOn Text`; `builder` takes the deck to
  open, if any, reads its file, and starts from `Builder.open`. The name
  field shows `saveName` (a `value` attribute).
- `script/build-web.sh` clean; a Safari run: edit a saved deck and save
  over it; edit a copy of a commander precon and save it under its name;
  the copy plays.

### Task 3: Verify and open the PR

Mutations: `open` dropping `rest`; `open` replacing rather than summing a
repeated card; `dirty` ignoring the baseline; the precon path setting
`savedAs`. Self-review per CLAUDE.md; rebase on `origin/main` once #9158
has merged; PR related to #8729.
