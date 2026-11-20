# Web Deck Import Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Paste a deck list into the web client's lobby, check it, save it in the browser, and play it.

**Architecture:** The lobby (`Parun.Web.Lobby`, pure) gains an import panel and saved-deck actions. The shell (`Parun.Browser.Shell`) checks the pasted text through a text half of `Command.load` and `Command.settle`, then asks the worker to store it. The worker (`engine.js`) writes saved decks into the in-memory `/data` mount under `decks/saved/`, and the page (`page.js`) keeps them in `localStorage` and hands them back to the worker on every load, before the deck list is read.

**Tech Stack:** Haskell (GHC wasm backend, JS FFI), plain JS in a Web Worker, `browser_wasi_shim`.

**Spec:** `docs/superpowers/specs/2026-11-20-web-deck-import-design.md`

## Global Constraints

- Every `cabal` call goes through `script/with-build-lock.sh`; tests run as `script/with-build-lock.sh cabal run -v0 parun -- test -p PATTERN`.
- `-Werror` with pedantic warnings (from `cabal.project.local`, already seeded in this worktree).
- Constructors take a `Mk` prefix; a constructor's haddock is one line.
- Stage, then `hooky fix`, then `git add` again, until it exits 0.
- Saved decks' names beneath the deck root are `saved/<slug>`; the slug is the name lowercased, each run of non-alphanumerics one `-`, trimmed of `-`.
- `localStorage` key per saved deck: `parun-deck:<slug>`.
- An empty name (empty slug) or a slug a saved deck already holds is refused as a problem; no replace, no confirm.
- No format picker: the deck's own header names its format.

## Review Focus

- A pasted list with a card the pool lacks: every problem listed, nothing saved (Task 1, Task 2 tests).
- A list whose header names a format it breaks (e.g. 40 cards under a 60-card format): the legality problems shown (Task 1 test).
- A name that slugs to nothing (`"!!!"`) or collides with a saved deck after slugging (`"My Deck"` vs `"my-deck"`): refused (Task 2 tests).
- Deleting the deck currently chosen as yours or a bot's: the slot empties, not a dangling name (Task 2 test).
- A reload: saved decks are back in the lobby before it lists decks, so a saved deck is never missing on first paint (Task 4 manual check).

---

### Task 1: Check a deck list given as text

**Files:**
- Modify: `source/libraries/play/Parun/Play/Command.hs` (`load`, new `loadText`, new `check`)
- Test: `source/libraries/test/Parun/PlaySpec.hs`

**Interfaces:**
- Produces: `Command.loadText :: Registry.Registry Identity.Identity -> FilePath -> Text.Text -> Either [Text.Text] DeckList.Listed` (the `FilePath` labels problems, as `load`'s path does); `load` becomes `readFile` + UTF-8 decode + `loadText`.
- Produces: `Command.check :: Registry.Registry Identity.Identity -> FilePath -> Text.Text -> IO [Text.Text]`: `loadText`'s problems, else `settle Nothing [(label, listed)]`'s, else `[]`.

- [ ] **Step 1: Write the failing tests** in `PlaySpec`, using the spec's `registry`:
  - `"a pasted deck list naming a card the pool lacks is refused, naming it"`: `check registry "pasted" "60 Forest\n1 No Such Card"` is non-empty and some problem contains `"No Such Card"`.
  - `"a pasted deck list breaking its header's format is refused"`: a list with the header line some `data/decks` file uses for a 60-card constructed format (copy the header from a real deck there) and `40 Forest`; `check` is non-empty.
  - `"a sound pasted deck list is accepted"`: `"60 Forest"` with no header gives `[]`.
- [ ] **Step 2: Run** `script/with-build-lock.sh cabal run -v0 parun -- test -p 'pasted deck list'`. Expected: build fails, `check` not in scope.
- [ ] **Step 3: Implement `loadText` and `check`;** rewrite `load` over `loadText`, keeping its UTF-8 error.
- [ ] **Step 4: Run the same command.** Expected: 3 pass. Also run `-p Play` to see `load` unbroken.
- [ ] **Step 5: Mutate** (`script/mutate.sh`): make `check` skip `settle`; the format test must fail. Record it for the PR.
- [ ] **Step 6: Commit** "Check a deck list given as text".

### Task 2: The lobby's import panel and saved-deck actions

**Files:**
- Modify: `source/libraries/web/Parun/Web/Lobby.hs`
- Test: `source/libraries/web/Parun/Web/LobbySpec.hs`

**Interfaces:**
- Produces, in `Lobby`:
  - `data Import = MkImport { importName :: Text.Text, importText :: Text.Text, importProblems :: [Text.Text], checking :: Bool }`, and field `importing :: Maybe Import` on `Lobby` (`Nothing` in `new`).
  - field `exporting :: Maybe (Text.Text, Text.Text)` on `Lobby` (deck name and its text; `Nothing` in `new`).
  - `Event` gains `MkOpenImport`, `MkCloseImport`, `MkImportName` (value typed), `MkImportText` (value typed), `MkImport`, `MkDelete Text.Text`, `MkExport Text.Text`, `MkCloseExport`.
  - `slugOf :: Text.Text -> Text.Text`.
  - `savedPrefix :: Text.Text` = `"saved/"`; `isSaved :: Text.Text -> Bool`.
  - `nameProblems :: Lobby -> Text.Text -> [Text.Text]`: `["Give the deck a name."]` when the slug is empty, `["A saved deck is already named that."]` when `savedPrefix <> slug` is in `decks`.
  - `imported :: Text.Text -> Lobby -> Lobby`: closes the panel and makes the named deck yours (as `MkPick` would).
  - `deleted :: Text.Text -> Lobby -> Lobby`: drops the deck from `decks`, `formats`, `palette`, `previews`, and empties `mine` / any `theirs` slot holding it.
- `update` handles `MkOpenImport` (a blank `Import`), `MkCloseImport`, `MkImportName`/`MkImportText` (store the typed value), `MkCloseExport`; `MkImport`, `MkDelete`, `MkExport` are left to the shell (like `MkStart`) and are no-ops in `update`.
- `view`: an "Import a deck" button by Random opens the panel; the panel holds a name `input` (`input` event `MkImportName`), a `textarea` (`input` event `MkImportText`, placeholder showing a two-line example list), Import and Cancel buttons, Import disabled while `checking`, and each of `importProblems` as a `p` in a `role="alert"` list. A saved deck's tile carries Export and Delete buttons. When `exporting` is set, a read-only `textarea` with the text and a Close button.

- [ ] **Step 1: Write the failing tests** in `LobbySpec`:
  - `slugOf "My  Deck!"` == `"my-deck"`; `slugOf "!!!"` == `""`.
  - `nameProblems` on a lobby whose decks hold `"saved/my-deck"`: `"My Deck"` refused, `"Other"` accepted (`[]`), `"!!!"` refused.
  - `update (MkImportText) "4 Forest" (update MkOpenImport Text.empty lobby)` has `importText` `"4 Forest"`.
  - `imported "saved/x"` on a lobby holding it: `importing` is `Nothing`, `mine` is `Just "saved/x"`.
  - `deleted "saved/x"` where `mine` and `theirs` hold it: both empty, `decks` lacks it.
  - The view with `importProblems = ["bad line"]` renders the text `"bad line"` (follow the existing view tests' way of finding text, `Html` helpers in `HtmlSpec`).
  - `describe "saved/my-deck"` == `("SAVED", "My Deck")` (pins the group the shell relies on).
- [ ] **Step 2: Run** `script/with-build-lock.sh cabal run -v0 parun -- test -p Parun.Web.Lobby`. Expected: build fails on the new names.
- [ ] **Step 3: Implement** the interfaces above. Add CSS for the panel and buttons to `source/web/style.css`, following the lobby's existing classes.
- [ ] **Step 4: Run the same command.** Expected: all pass.
- [ ] **Step 5: Mutate** `deleted` to leave `theirs` alone; the deletion test must fail.
- [ ] **Step 6: Commit** "Give the lobby an import panel and saved-deck actions".

### Task 3: The shell checks, saves, deletes and exports

**Files:**
- Modify: `source/libraries/browser/Parun/Browser.hs` (FFI imports)
- Modify: `source/libraries/browser/Parun/Browser/Shell.hs` (`lobby`)

**Interfaces:**
- Consumes: Task 1's `Command.check`; Task 2's events, `slugOf`, `savedPrefix`, `nameProblems`, `imported`, `deleted`, `Lobby.withDecks`.
- Produces, in `Browser`: `saveDeck :: Text.Text -> Text.Text -> IO ()` (slug, text) over `foreign import javascript unsafe "parunSave($1, $2)"`, and `forgetDeck :: Text.Text -> IO ()` over `"parunForget($1)"`. Task 4 defines both JS functions.
- In `Shell.lobby`:
  - `MkImport` with the value of `importName`: `nameProblems` non-empty sets them as `importProblems`. Else set `checking`, redraw, `untilBuilt shell [MkCards, MkRules] registry`, then `Command.check reg "pasted" text`. Problems go to `importProblems` (`checking` off). None: `Browser.saveDeck slug text`; add `savedPrefix <> slug` to `Loaded`'s `deckFiles` (path `<deck root>/saved/<slug>.txt`), its `DeckList.formatOf text` to `formats`, and the pair to `unread` (colours fill in as other decks' do); then `Lobby.withDecks` with the new keys and formats, and `Lobby.imported`.
  - `MkDelete name` (only when `isSaved name`): `Browser.forgetDeck` its slug, drop it from `Loaded`'s `deckFiles`, `formats`, `palette`, and `Lobby.deleted`.
  - `MkExport name`: read the deck's file, set `exporting`.
- No new spec: the parts the shell composes are each tested; Task 4's browser check drives this path.

- [ ] **Step 1: Implement** the two FFI imports and the three shell arms.
- [ ] **Step 2: Build** `script/with-build-lock.sh cabal build -v0 all` (native; the shell's module is in a sublibrary the native build compiles, check `docs/adding-a-module.md` if not) and `script/build-web.sh` for wasm. Expected: both clean under `-Werror`.
- [ ] **Step 3: Commit** "Check, save and delete pasted decks in the lobby".

### Task 4: The worker mounts saved decks, the page keeps them

**Files:**
- Modify: `source/web/engine.js`
- Modify: `source/web/page.js`

**Interfaces:**
- Consumes: Task 3's calls `parunSave(slug, text)` and `parunForget(slug)`.
- Message shapes: page→worker `{ saved: { [slug]: text } }`, sent once right after `new Worker`; worker→page `{ save: { slug, text } }` and `{ forget: slug }`.

- [ ] **Step 1: page.js.** After creating the worker, post `{ saved }` built from every `localStorage` key starting `parun-deck:`. In `engine.onmessage`, handle `data.save` (`setItem`) and `data.forget` (`removeItem`) before the existing branches.
- [ ] **Step 2: engine.js.** In `onmessage`, take `data.saved` (resolve a `savedDecks` promise) before handing other messages to `dispatch`. In `fetchAll`, after merging the `decks` chunk and before pushing its arrival, `await savedDecks` and `merge` a tree `decks → saved → <slug>.txt` of `File`s (encode with `TextEncoder`). `parunSave` merges one such file and posts `{ save }`; `parunForget` deletes `decks/saved/<slug>.txt` from the mount and posts `{ forget }`. Saved files are not `readonly`.
- [ ] **Step 3: Browser check.** `script/build-web.sh`, then `script/serve-web.sh`, in Safari Technology Preview (`script/walk-replays.sh` shows how agents drive it). Confirm in order:
  1. Import `"Test"` with `60 Forest`: it shows under SAVED, chosen as yours.
  2. Import a list with `1 No Such Card`: a problem naming it, nothing saved.
  3. Import `"test"` again: refused as taken.
  4. Export shows the text; Start plays a game with it.
  5. Reload: "Test" is still under SAVED.
  6. Delete: gone, and still gone after a reload.
- [ ] **Step 4: Commit** "Keep pasted decks in the browser across loads".

### Task 5: Finish the unit

- [ ] **Step 1:** Self-review the branch per `CLAUDE.md` (comments touched, sibling grep: `Lobby.new`'s positional construction gains two fields, so grep `MkLobby` and `Lobby.new` in specs).
- [ ] **Step 2:** Run `-p Parun.Web -p Play`.
- [ ] **Step 3:** Draft PR, body per `CLAUDE.md`, "related to #8729" (the issue stays open: matches, tournaments, limited and the card-search builder remain), mutations from Tasks 1 and 2 named, rules core casing on effect identity: no.
