# Web Deck Builder (slice 3: card pools) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build from a set's cards, or from a pasted limited pool whose
counts cap each card, checked under Limited against that pool.

**Architecture:** `Builder` gains a `pool`; `CardIndex.Query` gains the
names a search keeps (a parameter, not a second search); `Entry` gains
whether a card is a basic land, which every pool admits (CR 100.2b). A
pasted pool is kept across a save as the list's Sideboard, the cards of
the pool the deck leaves out (CR 100.4b), and `open` reads a Limited
list's main deck and Sideboard back into a pool. The shell lists the sets
that have a whole card list (`sets/<code>/all`), reads one when chosen,
and passes the pool to the live check.

**Spec:** `docs/superpowers/specs/2026-11-20-web-deck-builder-design.md`
(slice 3).

## Global Constraints

- CLAUDE.md applies whole.
- A set's list holds one of each card, so a set pool scopes and never
  caps; only a pasted pool caps.
- Pool names compare case-folded, as `Legality.copiesAtMost` compares
  names loosely; a basic land is never capped or excluded.

## Review Focus

1. A pasted pool with a card the index lacks (typo): it shows nowhere in
   search and its count is kept in the Sideboard (Task 2 test).
2. Removing a pooled card returns it to the Sideboard (Task 2 test).
3. Switching back to Every card drops the caps and the Sideboard the pool
   wrote (Task 2 test).
4. A pasted pool replaces a Sideboard an opened list kept (Task 2 test).
5. The live check passes the pool, so a card beyond its pool count is
   flagged (Safari).

---

### Task 1: The index's basic flag and pool filter

`Entry.basic :: Bool` (CR 205.4c: the Basic supertype on a land);
`Query.among :: Maybe (Set Text)` (case-folded names a search keeps, a
basic land always kept). Tests: "keeps only the pool's cards, and basic
lands"; "marks a basic land, a snow one too".

### Task 2: The builder's pool

`data Pool = MkEveryCard | MkSetCards Text (Set Text) | MkCardPool (Map CardName Natural)`;
events `MkPoolChoice` (the select's value), `MkPoolText`, `MkUsePool`,
`MkClosePool`; `pasting :: Maybe Text`. `setPool :: Text -> [CardName] -> Builder -> Builder`
for the shell. `MkUsePool` parses with `Lobby.entries`, sets Limited, and
drops a kept Sideboard. `MkAdd` refuses a capped card at its count.
`listText` writes the pool's unused cards as the Sideboard; `open` reads a
Limited list's Sideboard back into the pool. Tests per Review Focus 1-4,
plus "caps a pooled card at its count", "a basic land is never capped",
"round-trips a limited deck's pool".

### Task 3: View and shell

A "Card pool" select (Every card, each set, Paste a pool…), the paste
panel, "n left" on a pooled result and a disabled add at none. Shell:
`poolFiles` from the deck root's `all` lists, `MkPoolChoice "set:<code>"`
reads one, the check passes `Just pool` for a pasted pool. Safari run.
Mutations; PR related to #8729.
