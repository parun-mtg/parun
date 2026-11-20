# Web Client Home Screen Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The web client opens on a Haskell-drawn home screen as soon as the wasm compiles, loads its data in chunks behind the menus, and moves between home, lobby, game, ending and replays without reloading; a game can be left and resumed.

**Architecture:** One Haskell loop (`Parun.Browser.Shell`) owns the page, the current screen and the loaded data. While no tap waits it does one batch of loading work, then yields (`Browser.pause`), today's deck-colour pattern generalised. A chunk the worker mounts arrives as an input the loop sees, like a tap. A running game pauses by staying inside its `exchange` wait: Home runs the home loop from inside that wait, and Resume returns to the pending question. What can be pure (the loading schedule, every view) lives in the `web` sublibrary and is spec-tested natively; the `browser` sublibrary (wasm-only) interprets it.

**Tech Stack:** Haskell (GHC wasm backend, JSFFI), `source/web/*.js` (Web Worker, WASI shim), `script/build-web.sh`, `Parun.Spec`.

**Spec:** issue #9075 (`gh issue view 9075`). Read it whole before starting a task.

## Global Constraints

- Every screen, the home screen included, is drawn by Haskell through the patch protocol. No plain-HTML or JS-drawn screens; `index.html`'s splash stays only until the first patch.
- No page reload between screens. In-app navigation replaces query parameters.
- URL parameters kept, read once at boot, then cleared from the address bar: `mine`, `theirs`, `seed` (Same shuffle) and `scenario`, `at`, `branch` (replay deep links, used by `script/walk-replays.sh`). `trace` and the trace panel are removed. `level` stays as today (it comes from `localStorage`, not the URL).
- One paused game at most. Starting a new game while one is paused asks for confirmation, then concedes the paused game.
- A paused game lives only in the running worker; a reload loses it.
- Resume covers a game (a hotseat game or a replay's play-on), never a replay's viewer.
- An action whose data hasn't arrived shows its screen and waits there, and the loader moves what that screen needs to the front of its queue. Nothing errors for want of data.
- Chunk order: `decks`, then `cards-0` … `cards-3`, then `rules`. `scenarios` only when Replays is first opened or a replay deep link names one, otherwise never.
- CLAUDE.md applies: every `cabal` call under `script/with-build-lock.sh`, new modules staged with `parun.cabal` so `hooky fix` regenerates `exposed-modules`, specs wired into `Parun.Test`.
- The browser sublibrary builds only for wasm: `nix develop .#wasm --command script/build-web.sh` is its compile check.

## Review Focus

1. A tap during the one unbatched step (building the registry) is answered after it, never lost. `await`'s order (a waiting tap before any work) is the guard; Task 4's Safari check taps during the build.
2. A chunk that fails to download leaves the screen that waits on it saying so, not spinning forever. Task 2 adds `MkFailed`, Task 3 pins `blocked`, and Task 4's Safari check removes a chunk from `dist-web`.
3. A stale Same shuffle link naming a deck that no longer exists opens the lobby, as today. Task 7 pins that a deep link with an unknown deck falls back.
4. Home tapped while bots are acting does nothing. The control is drawn disabled unless the seat is being asked (Task 6), so no tap is half-taken.
5. After a paused game is conceded to start another, the new game starts with fresh seat state: log, lives and level marks don't carry over. Task 6 pins `newSeat` per game.

---

### Task 1: Remove the trace

**Files:**
- Modify: `source/libraries/browser/Parun/Browser.hs` (drop `tracing`, `trace`, the timing in `display`; keep `milliseconds` only if still used)
- Modify: `source/libraries/browser/Parun/Browser/Hotseat.hs` (`traced`, `answeredAt` and its `Clock` use)
- Modify: `source/libraries/browser/Parun/Browser/Replay.hs` (`reach`'s timing)
- Modify: `source/web/page.js` (`trace`, `traced`, the `trace` message branch), `source/web/engine.js` (`tracing`, `parun_trace`, the stderr routing), `source/web/index.html` (the Trace `details`)

**Interfaces:**
- Produces: `Browser.Page` without `tracing`, and `Hotseat.Seat` without `answeredAt`; `newSeat :: Web.Question.Level -> Seat` (pure now).

- [ ] **Step 1:** Remove each site listed. The load timings at the page's foot (`timings`, `phase`) stay.
- [ ] **Step 2:** `grep -rn -i trace source/web source/libraries/browser` shows no trace hits (`Web.Question`'s unrelated words aside).
- [ ] **Step 3:** `nix develop .#wasm --command script/build-web.sh` builds, and `script/with-build-lock.sh cabal run -v0 parun -- test -p Parun.Web` passes.
- [ ] **Step 4:** Commit: "Remove the web client's trace panel".

### Task 2: Ship the data in chunks, and compile while they download

**Files:**
- Modify: `script/build-web.sh` (pack and content-address `decks`, `cards-0`…`cards-3`, `rules`, `scenarios` in place of `data.tar.gz`/`scenarios.tar.gz`)
- Modify: `source/web/engine.js`
- Modify: `source/libraries/registry/Parun/Registry.hs` (`bundlePath` → `bundlePaths`)
- Modify: `source/libraries/browser/Parun/Browser.hs`
- Test: `source/libraries/test/Parun/RegistrySpec.hs` (if it reads `bundlePath`)

**Interfaces:**
- Produces, in `Parun.Browser`:
  - `data Arrival = MkArrived Text.Text | MkFailed Text.Text` (the chunk name: `"decks"`, `"cards-0"`…`"cards-3"`, `"rules"`, `"scenarios"`)
  - `data Input msg = MkTap msg Text.Text | MkArrival Arrival`
  - `nextInput :: Page msg -> IO (Input msg)`: an arrival first if one is queued, else a tap, else waits for either.
  - `next :: Page msg -> IO (msg, Text.Text)`, unchanged in type: taps only, leaving arrivals queued.
  - `want :: Text.Text -> IO ()`: asks the worker to fetch that chunk next (JSFFI `parunWant($1)`).
- Produces, in `Parun.Registry`: `bundlePaths :: IO [FilePath]`, the four `cards-K.bundle` paths in order. Their concatenation through `loadBundle` and `List.sortOn fst` equals today's single bundle.
- Chunk contents: `decks` = `data/decks`, `data/formats`. `cards-K` = `cards-K.bundle`, the sorted card list cut into four runs of equal count. `rules` = `legalities.json`, `printings.json`, `sticker-sheets`. `scenarios` = as today.

- [ ] **Step 1:** `build-web.sh`: build the four bundles and five archives, `address` each into `engine.js`. `ls -l dist-web` shows them and no `data.tar`.
- [ ] **Step 2:** `engine.js`: start `WebAssembly.compileStreaming(fetch("./parun.wasm"))` at once. Mount an empty `/data` and instantiate and `start()` as soon as it compiles, without awaiting any data. Fetch the chunks one at a time in the order above. As each untars, merge its tree into the mounted root directory's `contents`, then queue the name for Haskell (`globalThis.parunArrivals`) and call `parunWake?.()`. `parunWant(name)` moves a not-yet-fetched name to the front, and adds `scenarios` to the queue. A failed fetch queues `!name`. Deep-link query keys go into the WASI env as today, minus `trace`.
- [ ] **Step 3:** `Browser`: a JSFFI `parunTakeArrivals()` (returns and clears the queue, newline-joined) read by `nextInput`, decoded to `Arrival` (a leading `!` means `MkFailed`).
- [ ] **Step 4:** `Registry.bundlePaths`. Update `Hotseat` and `Replay` to concatenate the four bundles, so both still work until Task 7 replaces their entry points.
- [ ] **Step 5:** Build. Serve with `script/serve-web.sh`, open it in Safari Technology Preview over WebDriver as `script/walk-replays.sh` does, and check that today's lobby still loads and a game starts.
- [ ] **Step 6:** Commit: "Ship the web client's data in chunks, compiling the engine meanwhile".

### Task 3: The loading schedule, pure

**Files:**
- Create: `source/libraries/web/Parun/Web/Loading.hs`
- Test: `source/libraries/web/Parun/Web/LoadingSpec.hs` (wired into `Parun.Test`)

**Interfaces:**
- Produces:
  - `data Need = MkDecks | MkCards | MkRules | MkScenarios` (`Eq, Ord, Show, Enum, Bounded`): what a screen waits on.
  - `data Loading = MkLoading { arrived :: Set.Set Text.Text, failed :: Set.Set Text.Text, wanted :: [Need] }`, with `new :: Loading`.
  - `chunksOf :: Need -> [Text.Text]` (`MkCards` gives the four `cards-K`).
  - `arrive :: Either Text.Text Text.Text -> Loading -> Loading` (Left a failed chunk's name, Right an arrived one's).
  - `ready :: Need -> Loading -> Bool`: every chunk of the need arrived.
  - `blocked :: Need -> Loading -> Maybe Text.Text`: the first failed chunk the need has.
  - `demand :: Need -> Loading -> (Loading, [Text.Text])`: records the need as wanted, and returns the chunk names to `want` that haven't arrived.

- [ ] **Step 1: Write the failing tests**, in `LoadingSpec`:
  - `ready MkCards` is False after `cards-0`…`cards-2` and True after all four.
  - `demand MkScenarios new` returns `["scenarios"]`. `demand MkDecks` after `decks` arrived returns `[]`.
  - `blocked MkCards` is `Just "cards-2"` after `cards-2` failed, and `ready` stays False.
- [ ] **Step 2:** `script/with-build-lock.sh cabal run -v0 parun -- test -p Parun.Web.Loading` fails to compile.
- [ ] **Step 3:** Implement `Parun.Web.Loading`. Stage it and the spec with `parun.cabal`.
- [ ] **Step 4:** The same command passes.
- [ ] **Step 5:** Commit: "Add the web client's loading schedule".

### Task 4: The home screen view, and the shell loop that shows it with no data

**Files:**
- Create: `source/libraries/web/Parun/Web/Home.hs`, `source/libraries/web/Parun/Web/HomeSpec.hs`
- Create: `source/libraries/web/Parun/Web/Screen.hs`: the page's one event type.
- Create: `source/libraries/browser/Parun/Browser/Shell.hs`
- Modify: `source/web/Exports.hs` (`start = Parun.Browser.Shell.start`)

**Interfaces:**
- Produces, in `Parun.Web.Home`:
  - `data Home = MkHome { cards :: Maybe Natural, paused :: Bool, confirming :: Maybe Choice, problem :: Maybe Text.Text }`
  - `data Choice = MkPlayBots | MkReplays` (`Eq, Show`)
  - `data Event = MkChoose Choice | MkResume | MkConfirm | MkCancel` (`Eq, Show`)
  - `data Outcome = MkStay Home | MkGo Choice | MkBack` (MkBack = resume the paused game)
  - `update :: Event -> Home -> Outcome`: choosing while `paused` sets `confirming`, and `MkConfirm` then gives `MkGo` of it. `MkCancel` clears it.
  - `view :: Home -> Html.Html Event`: the hero (logo, "Parun", tagline with the card count once known), a primary **Play against bots** button, a secondary **Replays** button, a **Resume game** button above them only while `paused`, the confirmation ("Starting a new game concedes the one in progress." Concede and start / Cancel) while `confirming`, and `problem` as an alert.
- Produces, in `Parun.Web.Screen`: `data Event = MkHome Home.Event | MkLobby Lobby.Event | MkQuestion Web.Question.Event | MkReplay Web.Replay.Event | MkEnding Ending.Event`. `Ending.Event` arrives in Task 6. Until then, leave that constructor out.
- Produces, in `Parun.Browser.Shell`:
  - `data Shell = MkShell { page :: Browser.Page Screen.Event, loaded :: IORef.IORef Loaded }`
  - `data Loaded`: the `Loading`, plus what's been built from arrived chunks, each `Maybe` until built: the deck names and `byName` paths, the formats, the parsed card list (`[(FilePath, Either Text Card)]`), the palette, the work still to do (deck paths whose colours are unread), and the `Registry.Registry Identity.Identity`.
  - `data Woke = MkTapped Screen.Event Text.Text | MkChanged`
  - `await :: Shell -> IO Woke`. In this order: a waiting tap is returned first. Then a queued arrival is folded into `Loading` and gives `MkChanged`. Then one batch of work runs (deck list and formats once `decks` is ready; the card list once `MkCards` is ready; eight decks' colours; the registry once cards and `rules` are ready), followed by `Browser.pause`, and gives `MkChanged`. Otherwise it waits on `Browser.nextInput`.
  - `need :: Shell -> Loading.Need -> IO ()` runs `Loading.demand` and `Browser.want` on each returned name.
  - `start :: IO ()`: mounts the page, shows the home screen at once, and runs `home`. Errors are shown as `Hotseat.start` shows them today.
  - `home :: Shell -> Bool -> IO Home.Outcome`: the home loop, `paused` given. It redraws on `MkChanged` (card count), and returns `MkGo`/`MkBack`.
- Batch work's order follows the needs a screen has demanded first, then the schedule order.

- [ ] **Step 1: Write the failing tests**, in `HomeSpec`:
  - `view` with `paused = False` has no "Resume game". With `paused = True`, it has it.
  - `update (MkChoose MkPlayBots)` on an unpaused home gives `MkGo MkPlayBots`. On a paused home it gives `MkStay` with `confirming = Just MkPlayBots`, and `MkConfirm` then gives `MkGo MkPlayBots`.
  - The tagline reads "… 14082 cards loaded." for `cards = Just 14082`, and leaves the count out for `Nothing`.
- [ ] **Step 2:** `… test -p Parun.Web.Home` fails, then implement `Home` and `Screen` and pass it.
- [ ] **Step 3:** Implement `Shell` (`await`, `need`, `start`, `home`), with `MkPlayBots` and `MkReplays` falling through to the existing `Hotseat`/`Replay` flows until Tasks 5 and 7. Point `Exports.start` at it.
- [ ] **Step 4:** Build, serve, and check in Safari Technology Preview with the network throttled (the Develop menu's network conditions):
  - The home screen appears before the `cards-*` chunks finish, and the card count fills in.
  - A tap made while the registry builds is answered once it's done.
  - With `cards-2-*.tar.gz` deleted from `dist-web`, the home screen shows `problem` ("Couldn't load the cards. Reload to try again."), as does any screen waiting on a blocked need (`Loading.blocked`).
- [ ] **Step 5:** Commit: "Open the web client on a home screen drawn before its data".

### Task 5: The lobby inside the shell, filling in as decks and cards arrive

**Files:**
- Modify: `source/libraries/web/Parun/Web/Lobby.hs`, `LobbySpec.hs`
- Modify: `source/libraries/browser/Parun/Browser/Hotseat.hs` (the `choose` loop moves to `Shell`, and `play` takes the chosen decks and the built registry)
- Modify: `source/libraries/browser/Parun/Browser/Shell.hs`

**Interfaces:**
- Produces, in `Lobby`: a `reading :: Bool` field (decks not read yet: the picker says "Reading the decks…" in place of "No deck matches."), `MkHome` in `Event` (a Home button in the hero), and `withDecks :: Natural -> Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Lobby -> Lobby` (adds the decks once read, keeping choices and search). `new` gives a lobby with `reading = True` and no decks.
- Produces, in `Hotseat`: `game :: Browser.Page Screen.Event -> Registry.Registry Identity.Identity -> Web.Question.Level -> NonEmpty.NonEmpty FilePath -> Maybe Key.Key -> IO Ending.Ending` (decks in seat order, the player's first; the seed if given, else fresh), which the shell displays. Task 6 changes its result to `Played`.
- `Shell.lobby :: Shell -> IO ()`: demands `MkDecks`, then `MkCards`. It redraws on `MkChanged`, folding in decks and palette as they arrive. On Start it demands `MkRules`, shows `Hotseat.starting` until the registry is built, then runs the game. On `MkHome` it returns.

- [ ] **Step 1: Write the failing tests**, in `LobbySpec`:
  - `view new` contains "Reading the decks" and no "No deck matches".
  - `withDecks` on a lobby where `mine` was picked keeps `mine` and `search`.
  - `update MkHome` leaves the lobby unchanged (the shell acts on it).
- [ ] **Step 2:** Run `-p Parun.Web.Lobby`: fail, implement, pass.
- [ ] **Step 3:** Move the lobby loop into `Shell.lobby` on `await`, and drop `Hotseat`'s own bundle reading and formats pass.
- [ ] **Step 4:** Build and check in Safari: Play against bots right after the home screen appears shows "Reading the decks…", then decks, then pips. A game starts and plays.
- [ ] **Step 5:** Commit: "Run the lobby in the shell, filling in as the data arrives".

### Task 6: Leave and resume a game; the ending screen in-app

**Files:**
- Modify: `source/libraries/web/Parun/Web/Question.hs` (a Home button in `barControls`), `WebQuestionSpec` in `source/libraries/test/Parun/WebQuestionSpec.hs`
- Modify: `source/libraries/web/Parun/Web/Ending.hs`, `EndingSpec.hs`
- Modify: `source/libraries/browser/Parun/Browser/Hotseat.hs`, `Shell.hs`, `source/libraries/web/Parun/Web/Screen.hs`

**Interfaces:**
- Produces, in `Web.Question`: `MkHome` in `Event`. The turn bar draws a Home button (`Icon.MkHome` if there is one, else a text button), disabled unless `asked`.
- Produces, in `Ending`: `data Event = MkRematch | MkSameShuffle | MkNewGame` (`MkNewGame` goes home). Rematch and New game become buttons. Same shuffle becomes a button that replays in-app, plus a link beside it, "Share this shuffle", that keeps today's `rematch … (Just seed)` href. Save as a scenario stays a download link.
- Produces, in `Hotseat`:
  - `data Leave = MkResume | MkAbandon`
  - `exchange` gains a parameter `IO Leave`, run when the seat taps `MkHome` while asked. `MkResume` redraws the pending question. `MkAbandon` answers as `MkConcede` does (setting `conceding`).
  - `game` returns `Hotseat.Played = MkFinished Ending.Ending | MkConceded`. `MkConceded` means abandoned for a new game, so the shell skips the ending screen.
- `Shell`: the game's `IO Leave` runs `home shell True`. `MkBack` gives `MkResume`. `MkGo choice` records the choice in the shell and gives `MkAbandon`, and once `game` returns the shell runs that choice. Each game gets a fresh `newSeat`. The ending screen's events: Rematch replays the same decks with a fresh key, Same shuffle replays with the same seed, New game goes home.

- [ ] **Step 1: Write the failing tests:**
  - `WebQuestionSpec`: an asked request's view has an enabled Home button, and a watched board's (`watching`) has a disabled one.
  - `EndingSpec`: `view` has a Rematch button (not an `<a href="?mine=`), and keeps a "Share this shuffle" link whose href holds `seed=`.
- [ ] **Step 2:** Run `-p Parun.Web.Ending -p Parun.WebQuestion`: fail, implement, pass.
- [ ] **Step 3:** Wire `Leave` through `exchange`, `game` and `Shell`.
- [ ] **Step 4:** Check in Safari:
  - Home mid-game, then Resume, comes back to the same question.
  - Home, then Play against bots, then Concede and start, opens the lobby, and the new game's log starts empty.
  - Concede from the settings menu ends on the ending screen, and New game returns home.
  - Rematch and Same shuffle start without a page load (the foot's load timings don't repeat).
- [ ] **Step 5:** Commit: "Leave and resume a game from the home screen".

### Task 7: Replays in the shell, and the boot-time deep links

**Files:**
- Modify: `source/libraries/web/Parun/Web/Replay.hs`, `ReplaySpec.hs`
- Modify: `source/libraries/browser/Parun/Browser/Replay.hs`, `Shell.hs`
- Modify: `source/web/page.js` (clear the query after handing it to the worker)

**Interfaces:**
- Produces, in `Web.Replay`: `MkOpen Text.Text` (a scenario picked), `MkBranch` (play on from the position shown), `MkBack` (viewer → picker), and `MkHome` (picker → home) in `Event`. The picker's entries, Play on from here, and the ended screen's links become buttons on these events. The picker says "Fetching the scenarios…" while `MkScenarios` isn't ready.
- `Browser.Replay`: `choose`, `replay` and `playOn` take the `Shell` (or its page and a ready registry) instead of mounting their own page or reading the bundle. `playOn` passes the shell's `IO Leave` to `exchange`, so a play-on is resumable too.
- `Shell.start` reads the deep-link env vars once: `parun_mine` with `parun_theirs` (and `parun_seed`) go straight to that game, demanding what it needs and showing `starting` meanwhile, or to the lobby with the decks unpicked if a name is unknown. `parun_scenario` goes to that replay (or to the picker for the empty name), with `parun_at` and `parun_branch` as today.
- `page.js`: after building the worker URL, `history.replaceState(null, "", location.pathname)`.

- [ ] **Step 1: Write the failing tests**, in `ReplaySpec`:
  - The picker for a ready list has a button on `MkOpen "a"` and no `href="?scenario=`.
  - The picker with scenarios not ready says "Fetching the scenarios".
  - The viewer's controls have `MkBranch` and no `&branch` href.
- [ ] **Step 2:** Run `-p Parun.Web.Replay`: fail, implement, pass.
- [ ] **Step 3:** Move the replay flows into the shell, then the deep links, then `page.js`.
- [ ] **Step 4:** Check in Safari:
  - Home, then Replays, shows the picker. A scenario opens, Play on from here plays, and Home with Resume returns to the play-on.
  - `script/walk-replays.sh` over one theme produces its screenshots.
  - A `?mine=…&theirs=…&seed=…` link opens its game, and the address bar is then bare.
  - A link naming an unknown deck opens the lobby.
- [ ] **Step 5:** Run `script/with-build-lock.sh cabal run -v0 parun -- test -p Parun.Web -p Parun.WebQuestion` and confirm it's green.
- [ ] **Step 6:** Commit: "Run replays in the shell and keep deep links as boot-time only".
