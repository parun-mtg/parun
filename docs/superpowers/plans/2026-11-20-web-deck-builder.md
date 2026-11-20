# Web Deck Builder (slice 1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A "Build a deck" screen in the web client: search every card,
add and remove copies, see the deck's legality and curve live, and save it
as a lobby deck; saved decks with problems show "Not legal" in the lobby.

**Architecture:** A pure index of cards (`Parun.Web.CardIndex`) and a pure
screen (`Parun.Web.Builder`), both tested in their specs; the shell
(`Parun.Browser.Shell`, wasm-only) builds the index after the registry,
runs the legality check after each edit, and saves through the import's
`Browser.saveDeck`. Typed violations come from a new `Legality.assess`.

**Tech Stack:** Haskell (GHC wasm backend for `browser`), the project's
`Parun.Web.Element`/`Html` views and `Parun.Spec` tests.

**Spec:** `docs/superpowers/specs/2026-11-20-web-deck-builder-design.md`

## Global Constraints

- CLAUDE.md applies whole: worktree rules, `script/with-build-lock.sh` on
  every `cabal`, `hooky fix` before each commit, terse commit messages with
  the attribution trailer, no `git stash`.
- `docs/adding-a-module.md` before adding `Parun.Web.CardIndex`,
  `Parun.Web.Builder` and their specs.
- `docs/style-guide.md`: `Mk` constructors, one-line haddocks.
- Boot libraries only; prefer `Seq`/`Set`/`Map` over lists for new
  collections.
- Results cap: 60, with "Showing 60 of N" when more match.
- Saved text: `Format: <name>` header, then main-deck lines `N Name`, then
  the untouched rest of the list.
- The live check never blocks an edit; saving with problems is allowed.
- Run the web tests as
  `script/with-build-lock.sh cabal run -v0 parun -- test -p Parun.Web -p Parun.Legality`,
  redirected to a file. Wasm build: `script/build-web.sh`.

## Review Focus

1. Removing the last copy of a card drops it from the list, never leaves a
   `0 Name` line (Task 3 test).
2. A search matching nothing, or the empty search with no filters, shows a
   sensible line, not an empty or 14k-row list (Task 1 and Task 4 tests).
3. Split and double-faced cards are found by either face's name and text,
   and saved under the name `DeckList.parse` accepts (Task 1 test; the
   save round-trip in Task 3).
4. Saving again after a save overwrites the same deck; a fresh name that
   clashes with another saved deck is refused (Task 3 test).
5. A deck whose format header names a format the check cannot load shows
   that problem instead of crashing or showing nothing (Task 2 test).

---

### Task 1: The card index

**Files:**
- Create: `source/libraries/web/Parun/Web/CardIndex.hs`
- Create: `source/libraries/web/Parun/Web/CardIndexSpec.hs` (and register
  it as `docs/adding-a-module.md` says)

**Interfaces:**
- Consumes: `Lobby.cardColours`, `Quantity.manaValueOf`, `Card`, `Face`.
- Produces:
  - `data Entry = MkEntry { name :: CardName, faceNames :: [Text], cost :: Maybe ManaCost, value :: Integer, colours :: Set Color, types :: Set CardType, typeLine :: Text, rules :: [Text], folded :: Text }`
    (`folded`: every face's name, type line and Oracle text, case-folded).
  - `entryOf :: Card.Card -> Maybe Entry`: `Nothing` for a synthetic card
    and a nontraditional one (CR 108.2a). Grep the CR for which card types
    are nontraditional; do not take a list from memory or from this plan.
  - `data Query = MkQuery { text :: Text, wanted :: Set Color, kinds :: Set CardType, valued :: Maybe Integer }`,
    `everything :: Query` (all empty).
  - `search :: Query -> Seq Entry -> (Int, Seq Entry)`: the count matching
    and the first `cap` in order.
  - `cap :: Int` (60).

- [ ] **Step 1: Write the failing tests** in `CardIndexSpec`, building
  entries with a local helper (not real card files):
  - "matches name and rules text, any case": `"bolt"` finds Lightning
    Bolt by name; `"deals 3"` finds it by text.
  - "orders name prefixes, then names, then text": for `"elf"`,
    `Elf Replica` before `Llanowar Elves` before a card whose text alone
    says "Elf".
  - "keeps cards within the chosen colours, colourless ones included":
    wanted `{Green}` keeps a green card and an artifact, drops a red-green
    one.
  - "keeps cards of any chosen type" and "keeps cards of the chosen mana
    value, the last bucket counting 7 and up".
  - "caps the results and counts every match": 61 matches give
    `(61, 60 entries)`.
  - "finds a split card by either half's name".
  - "leaves out a synthetic card" and a nontraditional one, through
    `entryOf` on a minimal `Card`.
- [ ] **Step 2:** Run them; expect compile failure or FAIL.
- [ ] **Step 3:** Implement the module. Order: prefix match on `name`,
  then infix on any face name, then infix on `folded`; by name within each.
- [ ] **Step 4:** Run; expect PASS.
- [ ] **Step 5:** Commit ("Add a searchable card index for the deck builder").

### Task 2: Typed legality results

**Files:**
- Modify: `source/libraries/registry/Parun/Legality.hs` (`report`)
- Test: `source/libraries/test/Parun/LegalitySpec.hs`

**Interfaces:**
- Produces:
  - `assess :: Maybe Text -> Maybe CardPool -> DeckList.Listed -> IO (Maybe Text, [Text], [Violation.Violation])`:
    the format checked, the list's own problems and a format that can't be
    loaded (as lines), and the violations. `report` becomes
    `assess` with the violations explained. It is the same function with a
    richer result, not a sibling; name `report` in the PR as the function
    extended.
  - `namedCard :: Violation.Violation -> Maybe CardName.CardName`: the card
    a violation is about, `Nothing` for deck-wide ones. An exhaustive case
    with no `_` arm.
  - `minimumFor :: Text -> IO (Maybe Natural)`: a shipped format's minimum
    deck size (`shipped`, `RuleFamily.Engine.minimumDeckSize`).

- [ ] **Step 1: Write the failing tests:** "assess returns a constructed
  deck's five Shocks as TooManyCopies"; "namedCard names the card of
  TooManyCopies and nothing for TooFewCards"; "assess reports a header
  naming no shipped format as a line, with no violations";
  "minimumFor constructed is 60".
- [ ] **Step 2:** Run; expect FAIL.
- [ ] **Step 3:** Implement. Keep `report`'s output unchanged, which the
  existing `report` tests confirm.
- [ ] **Step 4:** Run `-p Parun.Legality`; expect PASS.
- [ ] **Step 5:** Commit.

### Task 3: The builder's model

**Files:**
- Create: `source/libraries/web/Parun/Web/Builder.hs`
- Create: `source/libraries/web/Parun/Web/BuilderSpec.hs`

**Interfaces:**
- Consumes: Task 1's `Entry`, `Query`, `search`; `Lobby.slugOf`,
  `Lobby.savedPrefix`.
- Produces:
  - `data Builder = MkBuilder { index :: Maybe (Seq Entry), query :: Query, deck :: Map CardName Natural, suffixes :: Map CardName Text, rest :: [Text], format :: Text, formats :: [Text], minimum :: Maybe Natural, problems :: [Text], flagged :: Set CardName, reading :: Maybe CardName, saveName :: Text, savedAs :: Maybe Text, saveProblems :: [Text], dirty :: Bool, leaving :: Bool }`.
    `suffixes` (a main-deck card's `(SET) NUMBER` and annotations) and
    `rest` (every line after the main deck, verbatim) are empty in this
    slice; slice 2 fills them when it opens a list.
  - `new :: [Text] -> Builder`: the shipped format names, with
    `"constructed"` chosen.
  - `data Event = MkSearch | MkToggleColour Color | MkToggleKind CardType | MkValue Text | MkAdd CardName | MkRemove CardName | MkRead CardName | MkCloseCard | MkFormat | MkSaveName | MkSave | MkBack | MkStay | MkLeave`.
  - `update :: Event -> Text -> Builder -> Builder` (as `Lobby.update`: the
    typed value arrives alongside). `MkSave`, `MkLeave` and `MkBack` with
    `dirty` false are the shell's; `update` leaves the model unchanged
    for them except as below.
  - `listText :: Builder -> Text`: the saved text (Global Constraints),
    main lines sorted by name, each with its suffix.
  - `checked :: [Text] -> Set CardName -> Builder -> Builder` and
    `saved :: Text -> Builder -> Builder` (sets `savedAs`, clears `dirty`
    and `saveProblems`).
  - `saveTarget :: Seq Text -> Builder -> Either [Text] Text`: the saved
    deck's name (`saved/<slug>`): `savedAs` when set; otherwise the typed
    name, refused as `Lobby.nameProblems` refuses it against the decks
    given.

- [ ] **Step 1: Write the failing tests:**
  - "adds and removes a copy at a time, dropping a card at none".
  - "an edit marks the deck changed and a save clears it".
  - "writes the format header, then the main deck by name".
  - "writes back the rest of a list untouched" (a builder with `rest` and
    `suffixes` set by hand).
  - "listText parses back to the same deck": through the existing
    `Lobby.entries`, for a deck holding a split card.
  - "a first save takes the typed name; a clash is refused; later saves
    keep the saved name".
  - "Back with changes asks first; Stay closes the question".
  - "toggling a colour twice clears it".
- [ ] **Step 2:** Run; expect FAIL.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** Run; expect PASS.
- [ ] **Step 5:** Commit.

### Task 4: The builder's view

**Files:**
- Modify: `source/libraries/web/Parun/Web/Builder.hs` (`view`)
- Modify: `source/libraries/web/Parun/Web/Lobby.hs`: split `preview`'s
  curve into `curve :: [Line] -> Html msg`, and reuse it.
- Modify: `source/web/style.css`
- Test: `BuilderSpec`, using the `Diff`/`Html` helpers `LobbySpec` uses to
  read rendered text.

**Interfaces:**
- Produces: `view :: Builder -> Html Event`, `linesOf :: Builder -> [Lobby.Line]`
  (the deck as the lobby's preview lines, from the index).

The view is a keyed `main` (as the lobby is, so typed fields survive):
- a top bar with Back, the format `select` (`MkFormat`), the name field,
  and Save, showing "Saved" or the save problems;
- the search field, colour toggles (`Mana.symbol`), type toggles and a
  mana-value `select`;
- a basic-lands row: Plains, Island, Swamp, Mountain, Forest, each a `+`;
- results: name (`MkRead`), cost, type line, `+`; "Showing 60 of N"
  beyond the cap; "No card matches." at none; "Preparing the cards…"
  while `index` is `Nothing`;
- the deck: count against `minimum` ("43 / 60"), the curve, creatures,
  spells and lands with `-` and `+` per line, flagged lines marked, and
  `problems` listed;
- the card being read: name, type line, rules lines, and Close;
- the leave question when `leaving`.

- [ ] **Step 1: Write the failing tests:** "shows the count against the
  format's minimum"; "marks a flagged card"; "says how many matched beyond
  the cap"; "says no card matches"; "says the cards are being prepared";
  "offers the five basic lands whatever the search".
- [ ] **Step 2:** Run; expect FAIL.
- [ ] **Step 3:** Implement the view and the CSS (two columns on wide
  screens, stacked on narrow ones, as the lobby).
- [ ] **Step 4:** Run `-p Parun.Web`; expect PASS, the lobby tests
  included after the curve split.
- [ ] **Step 5:** Commit.

### Task 5: The lobby's flag and the way in

**Files:**
- Modify: `source/libraries/web/Parun/Web/Lobby.hs`
- Test: `LobbySpec`

**Interfaces:**
- Produces: `Lobby.illegal :: Map Text [Text]` (a saved deck's problems,
  by name), `Lobby.refusal :: Maybe [Text]` (why Start was refused, shown
  until the next pick), `Lobby.MkOpenBuilder`, and
  `Lobby.startable :: Lobby -> Either [Text] Text` (your deck, or its
  problems).

- [ ] **Step 1: Write the failing tests:** "badges a saved deck with
  problems Not legal"; "refuses to start a flagged deck, listing why";
  "a new pick clears the refusal"; "offers Build a deck beside Import a
  deck".
- [ ] **Step 2:** Run; expect FAIL.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** Run; expect PASS.
- [ ] **Step 5:** Commit.

### Task 6: The shell

**Files:**
- Modify: `source/libraries/web/Parun/Web/Screen.hs` (`MkBuilder Builder.Event`)
- Modify: `source/libraries/browser/Parun/Browser/Shell.hs`

No unit tests (the module builds only for wasm); verified in Task 7.

- [ ] **Step 1:** `Loaded` gains `index :: Maybe (Seq Entry)` and
  `illegal :: Map Text [Text]` plus whether each is wanted; the new fields
  go in `start`'s `MkLoaded` construction. `work` gains two steps after
  the registry: build the index (`entryOf` over every parsed card) once
  the builder has asked for it, and check unchecked saved decks, a batch
  at a time, with `Legality.assess`. Time the index build and print the
  number of cards and seconds to the console (an FFI `console.log`
  beside `saveJs`), and report it in the PR.
- [ ] **Step 2:** `Next` gains `MkToBuilder`; the lobby's
  `MkOpenBuilder` leads there; Back and Leave lead to the lobby.
  `refreshed` copies `illegal` into the lobby. `MkStart` goes through
  `Lobby.startable`. Opening the builder lists the shipped formats (the
  files under `Legality`'s formats root) for `Builder.new`.
- [ ] **Step 3:** The builder loop: after every edit that changes `deck`
  or `format`, check `listText` (parse with `Command.loadText`'s parse,
  then `Legality.assess`) and apply `Builder.checked` with the lines and
  each violation's `namedCard`. A format change also sets `minimum` from
  `Legality.minimumFor`. On Save, `saveTarget` against the deck
  files, then `Browser.saveDeck` (confirm in `engine.js` that it
  overwrites a file already mounted), the `deckFiles`, `formats` and
  `unread` updates `importDeck` makes, a fresh entry in `illegal`, and
  `Builder.saved`.
- [ ] **Step 4:** `script/build-web.sh`; expect a clean `-Werror` build.
- [ ] **Step 5:** Commit.

### Task 7: Verify and open the PR

- [ ] **Step 1:** Run `-p Parun.Web -p Parun.Legality`; all pass.
- [ ] **Step 2:** Safari Technology Preview over WebDriver
  (`script/walk-replays.sh` shows the driving pattern): open the builder;
  search "bolt"; add 4 of one card and 20 Mountains (the count shows
  "24 / 60" and a problem); save as "Burn"; see it in the lobby with
  "Not legal"; Start is refused with the reason; reopen the builder, build
  60 legal cards, save as another name, start a game to turn 1; reload and
  both decks are still there.
- [ ] **Step 3:** Mutations, each named in the PR with the assertion it
  reddened: `MkRemove` keeping a zero count; `search` dropping the cap;
  `namedCard` returning `Nothing` for `TooManyCopies`; `startable`
  ignoring `illegal`; `saveTarget` ignoring `savedAs`.
- [ ] **Step 4:** Self-review per CLAUDE.md (CR citations, touched
  comments, `MkLobby`/`MkLoaded` construction sites, `report`'s callers).
- [ ] **Step 5:** Commit the spec and this plan, push, and open the PR
  (related to #8729, not closing it), with the body CLAUDE.md lists and the
  index-build timing.
