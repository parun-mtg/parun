# Web client deck builder

The deck-building slice of #8729, step two, after the paste-in import
(`2026-11-20-web-deck-import-design.md`). Typing names and counts into a box
is the only way to make a deck today; this replaces it with an editor that
searches the card pool and builds the list for you.

## Intent

One editor over a **card pool** and a **deck**. Every way of building is the
same editor started differently:

| Start | Deck | Pool |
|-------|------|------|
| From scratch | empty | every card |
| Edit a saved deck | that deck | every card |
| Tweak a precon | a copy of the precon | every card |
| Limited | empty | a fixed pool, each card capped at its count |

The owner wants all four. They land in three PRs on one editor, in order:

1. **Build from scratch**: the editor, search, add and remove, the live
   check, the summary, save.
2. **Open a deck**: a saved deck (saved back under its name) or a precon
   (saved as a new deck).
3. **Pool scoping**: the pool as one set's cards (its `all.txt`), each card
   capped, checked with `Legality.check`'s `CardPool`. Sealed and draft
   build on it later.

The editor's model carries the pool from slice 1, so slices 2 and 3 add
entry points, not a second editor.

## Goals

- **Search** the pool by name and rules text as you type, narrowed by
  colour, card type and mana value. Results show each card's name, cost and
  type line; tapping one shows its rules text (`Web.Card`'s sheet, as in
  game).
- **Add and remove** with one tap per copy, from the results and from the
  deck list. Basic lands are one tap away whatever the search says.
- **A format**, chosen from `data/formats` and written as the list's header,
  or Casual, the default, which writes none. Most precons are under 60 cards
  and name no format, and the lobby pairs decks only under one format, so a
  constructed default would leave a new deck no bot opponents (a call made
  while building; the doc first said constructed).
- **A live check.** The deck is parsed and checked (`DeckList`,
  `Legality.check`) after every change. The editor shows the count against
  the format's size, and marks every card over its copy limit or outside the
  format's pool. The check never blocks an edit.
- **A summary**: the curve and colours, reusing `Lobby.preview`'s curve, and
  counts of creatures, other spells and lands.
- **Save** as text through the existing `parunSave` hook, so the deck lands
  in the lobby's Saved group as an imported one does. Saving with problems
  is allowed: a deck still being built is a deck you may want to keep.
- **A deck with problems is flagged in the lobby** (owner's choice). The
  lobby checks each saved deck once the registry is built, off the critical
  path, and shows a "Not legal" badge on any that fail; the badge may appear
  a moment after the list. Picking a flagged deck is allowed, and Start
  refuses it with the check's reasons listed (today `Command.settle`'s
  refusal is a bare notice). Rejected: keeping unfinished decks in a
  builder-only drafts list (a second place decks live), and saving only
  legal decks (loses half-built work).

## Non-goals

- Card images. The client draws cards as text and mana symbols. Art is a
  separate decision.
- Sideboard, commander and other sections in slices 1 to 3: main deck only.
  The text format keeps them, so a later slice can add them.
- Sealed packs and drafting (#8729's limited piece). Slice 3 provides the
  pool they will fill.
- Recommending cards, or building a deck automatically.
- Sharing decks across browsers.

## Anti-goals

- A second deck store, or a second card representation in JS. The builder
  writes the same saved text the import writes, and reads cards through the
  engine's registry.
- A builder that refuses an illegal deck. It shows the problems and still
  lets you edit and save.

## Approaches

- **A. A Haskell screen in `Parun.Web` (chosen).** A pure model and view
  beside `Lobby`, tested in its own spec. Search runs in the wasm worker over
  the cards the registry loads, and save uses the import's hook.
- **B. A JS builder in `page.js` over a JSON card index.** A second UI stack
  and a second copy of the card data. The client talks to the engine, not
  around it.
- **C. A prebuilt static search index.** Only worth it if searching in the
  worker is slow. Measure first (below).

The owner agreed to A and the three-PR split.

## Design

- **A new screen**, `Parun.Web.Builder`: a `Builder` model, an `Event`, a
  pure `update` and a `view`, with `Screen.MkBuilder` beside the others. The
  model holds the pool (every card, or a capped map in slice 3), the deck as
  counts by card name, the format, the search text and filters, the card
  being read, the last check's problems, and the name to save under.
- **The search index.** A slim record per card, built once from the parsed
  cards: name, colours (`Lobby.cardColours`), mana value, the card types,
  each face's type line and Oracle text, case-folded for matching. Matching
  and filtering are pure functions over the index, in the spec. Synthetic
  cards (`"synthetic": true`) are left out. So is anything else no deck may
  hold (CR 100.2a), which the builder settles against the card data.
- **Building the index** is a `Parun.Web.Loading` step. It runs after the
  registry, and only once the builder is opened (the lobby shows
  "Preparing the cards…" until then), so play never waits on it. Parsing
  every card file in wasm has never been timed. The first PR measures it
  and reports the number. If it is too slow to bear, store a precomputed
  index in the card bundle (approach C), and say so in that PR.
- **Results** are capped (say 60) and ordered with name matches first, then
  by name, with a "showing 60 of N" line, so a one-letter query never
  renders thousands of rows.
- **The live check** runs `Command.check`'s path on the rendered list after
  each edit. It runs in the shell, since it needs the registry, which keeps
  `update` pure: the shell hands the problems back through an event. Copy
  limits come from the check, not a second copy of the rules in the view.
- **The deck model keeps what the editor doesn't show** (my call). It holds
  the main deck's counts by name, which the editor changes, and the rest of
  the parsed list (sideboard, commander, other sections, `(SET) NUMBER`
  printings, annotations) untouched. Saving writes it all back through
  `DeckList.render`, so opening a deck in slice 2 and saving it loses
  nothing. The editor notes when such parts are present. Commander precons
  open like any other.
- **Saving** writes that text via `parunSave`. The first save's name is
  refused as the import refuses it (`Lobby.nameProblems`). After that the
  editor stays open and remembers the name, and later saves overwrite that
  deck, which also updates the lobby's lists and preview (my call). Unsaved
  changes mean edits since the last save.
- **Entry**: a "Build a deck" button in the lobby's deck tools, next to
  "Import a deck". Leaving with unsaved changes asks first.

## Left to the builder

- Layout: results and deck side by side on wide screens, stacked on narrow
  ones, as the lobby does today.
- The exact filter controls (colour pips as toggles is the obvious fit,
  using `Mana.symbol`).
- Keyboard flow: Enter adds the top result, a natural extra.
- The result cap and ordering details.
- Whether the format picker shows every format or only those with a known
  size.
