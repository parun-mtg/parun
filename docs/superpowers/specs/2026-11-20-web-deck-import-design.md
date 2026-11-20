# Web client deck import

The deck-building slice of #8729, step one: paste a deck list into the web
client, check it, save it in the browser, and play it from the lobby. A later
card-search builder writes the same saved text, so this is its foundation.

## Goals

- A "Paste a deck" panel in the lobby: a name, a text box, and an Import
  button. The text is the `data/decks` format (`Parun.DeckList`).
- On Import the list is parsed (`DeckList.parse`, `DeckList.sound`) and
  checked under the format its header names (`Legality.check`, through
  `Command.settle`'s path). Every problem shows inline and nothing is saved.
- A clean list is saved in `localStorage` and appears at once in the lobby
  under a "Saved" group, previewed, coloured and format-matched like any deck.
- A saved deck can be deleted and exported (its text shown, selected, to
  copy).
- Saved decks survive a reload, and a rematch names one like any other deck.

## Non-goals

- Searching the card pool or editing a deck card by card (the next slice).
- Choosing a format apart from the header's; a header line picks one.
- Sharing a saved deck across browsers: a "Same shuffle" link naming one
  opens only where that deck is saved.
- Matches, sideboarding, limited (other #8729 slices).

## Approaches

- **A. Paste and play, nothing saved.** The text rides the button's event,
  as the deck filter's does. No JS change, but the deck dies on reload and a
  rematch cannot name it.
- **B. Paste, save in `localStorage`, mount saved decks as files (chosen).**
  `engine.js` mounts each saved deck at `/data/decks/saved/<slug>.txt`
  before the engine starts, so the lobby, previews, `Command.load` and
  rematch links need no new deck source.
- **C. Save to IndexedDB from the worker behind a Haskell-side store.** More
  general, but a second deck source duplicating what the mount gives.

The owner picked B: saving is what the builder will build on, and the file
mount keeps everything downstream unchanged.

## Design

- **Lobby (`Parun.Web.Lobby`).** New events: open/close the import panel,
  Import (the text box's value arrives with it, as `MkSearch`'s does), the
  name typed, Delete and Export on a saved deck. The model holds the panel's
  name and text and the last import's problems. Pure, tested in
  `LobbySpec`.
- **Checking (`Parun.Browser.Hotseat`).** Import builds the registry on first
  use (it is built after the lobby shows today), parses and checks the text,
  and on success saves it and adds it to the lobby's decks, formats and
  palette in place. Parsing text rather than a path means splitting
  `Command.load` into a text half and a file half.
- **Saving (Haskell to page).** A second FFI hook beside `parunApply`,
  `parunSave(name, text)` and `parunDelete(name)`, posted from the worker to
  the page, which writes `localStorage` key `parun-deck:<slug>`.
- **Loading (page to worker).** The page posts its saved decks to the worker
  first; `engine.js` waits for that message and mounts them as files under
  `decks/saved/` in the unpacked tree. A deck saved during a session is also
  written into the in-memory mount so the running engine can load it.
- **Names.** The slug is the name lowercased, non-alphanumerics to `-`;
  `Lobby.describe` already titles it back. An empty name, or one whose slug a
  saved deck already holds, is refused as a problem like any other; delete
  the old deck first. (My call: no confirm dialog to build or test.)

## Left to the builder

- Panel placement and styling within the lobby.
- Whether the registry build on first Import shows a "Checking…" state.
- The exact export affordance (a read-only text box, or a copy button).
