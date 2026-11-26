// The web client's page: applies the patches the engine sends and reports
// each event's path back (Parun.Web.Wire). The engine runs in a Web Worker
// (engine.js), so the page stays responsive while it works. The view's root
// is #app's only child, and paths count from it.
const root = document.getElementById("app");
const events = ["click", "input", "change"];
const engineUrl = new URL("./engine.js", import.meta.url);
// The query names the screen to open (Parun.Web.Route), which the engine
// reads once as its query's "query".
if (location.search) engineUrl.searchParams.set("query", location.search);
// A host's link (Parun.Web.Link) carries its code as its fragment, which the
// engine reads once as its query's "offer", and names the join screen for
// its game's stage as its query (Parun.Browser.linkHost).
const offered = location.hash.slice(1);
if (offered) engineUrl.searchParams.set("offer", offered);
// The auto-pass level last chosen (Parun.Web.Question.Level) starts the game.
const level = localStorage.getItem("parun-level");
if (level !== null) engineUrl.searchParams.set("level", level);
const engine = new Worker(engineUrl, { type: "module" });
// The decks saved in this browser go to the engine first (engine.js), each
// kept as parun-deck:<slug>, with the game in progress kept as parun-game
// (Parun.Web.Link.saved), unless a link joins another browser's game.
const deckKey = "parun-deck:";
const gameKey = "parun-game";
engine.postMessage({ saved: Object.fromEntries(Object.keys(localStorage).filter((key) => key.startsWith(deckKey)).map((key) => [key.slice(deckKey.length), localStorage.getItem(key)])), game: offered ? "" : (localStorage.getItem(gameKey) ?? "") });
// Safari runs WebAssembly far slower outside a secure context (#8419), and
// plain http from a network address is not one.
if (!isSecureContext) document.getElementById("insecure").hidden = false;
// Once fetched, the engine is kept for later visits (sw.js).
if ("serviceWorker" in navigator && isSecureContext) navigator.serviceWorker.register("./sw.js").catch((problem) => console.warn(problem));

// The engine's load phases go to the console, and the loading screen marks
// each step as it is heard of (engine.js). The engine compiles while the
// cards download, so the two are one step, done once both are, saying how
// many of the cards' chunks have arrived; the table is set once the engine
// is compiled. A failure is said as it is, and stops the ring.
const cardsFetched = new Set();
let compiled = false;
// How many chunks the cards come in, as engine.js says before fetching any.
let cardChunks = Infinity;
function step(name, state, said = "") {
  const item = document.querySelector(`.splash [data-step="${name}"]`);
  if (!item) return;
  item.className = state;
  item.querySelector(".state").textContent = said;
}
function note(line) {
  console.log(line);
  const name = line.split(" ")[0];
  if (name === "compiled" || name.startsWith("cards-")) {
    if (name === "compiled") compiled = true;
    else cardsFetched.add(name);
    const cardsDone = cardsFetched.size === cardChunks;
    if (cardsDone && compiled) step("fetch", "done", "Done");
    else step("fetch", "now", cardsDone ? "Cards done" : `Cards ${Math.round((100 * cardsFetched.size) / cardChunks)}%`);
    if (compiled) step("table", "now");
  } else if (!["decks", "rules", "scenarios", "started"].includes(name)) {
    const phase = document.getElementById("phase");
    if (phase) phase.textContent = line;
    document.querySelector(".splash .loading-ring")?.classList.add("stopped");
  }
}

// The settings menu: theme (the system's unless one is picked), card size
// and motion, each kept and applied as an attribute of the page's root. It
// opens from the game's turn bar (data-settings), else from its own button.
const settingsButton = document.getElementById("settings-button");
const settings = document.getElementById("settings");
const dark = matchMedia("(prefers-color-scheme: dark)");
const systemTheme = () => (dark.matches ? "dark" : "light");
const chosen = { theme: localStorage.getItem("parun-theme") ?? "system", density: document.documentElement.dataset.density, motion: document.documentElement.dataset.motion, notify: localStorage.getItem("parun-notify") === "on" && "Notification" in window && Notification.permission === "granted" ? "on" : "off" };
for (const [name, value] of Object.entries(chosen)) {
  const input = settings.querySelector(`input[name="${name}"][value="${value}"]`);
  if (input) input.checked = true;
}
settings.addEventListener("change", async ({ target }) => {
  // The focus follows the choice a click makes, as the arrow keys move it,
  // so the ring never marks a choice but the one made (Safari leaves the
  // focus where it was on a click).
  target.focus();
  if (target.name === "notify") {
    // Turning notifications on asks the browser's permission, once.
    const allowed = target.value === "on" && "Notification" in window && (Notification.permission === "granted" || (await Notification.requestPermission()) === "granted");
    localStorage.setItem("parun-notify", allowed ? "on" : "off");
    settings.querySelector(`input[name="notify"][value="${allowed ? "on" : "off"}"]`).checked = true;
  } else if (target.name === "theme") {
    if (target.value === "system") localStorage.removeItem("parun-theme");
    else localStorage.setItem("parun-theme", target.value);
    document.documentElement.dataset.theme = target.value === "system" ? systemTheme() : target.value;
  } else {
    localStorage.setItem(`parun-${target.name}`, target.value);
    document.documentElement.dataset[target.name] = target.value;
  }
});
dark.addEventListener("change", () => {
  if (localStorage.getItem("parun-theme") === null) document.documentElement.dataset.theme = systemTheme();
});
// The button the menu opens from: the turn bar's during a game.
function gear() {
  return root.querySelector("[data-settings]") ?? settingsButton;
}
// Ties the menu to the button it opens from, which a patch may have
// replaced, and offers Concede during a game: enabled where the view gives
// the turn bar's button a "concede" event, while the seat has priority.
function syncSettings() {
  const from = gear();
  for (const button of new Set([settingsButton, from])) button.setAttribute("aria-expanded", String(!settings.hidden && button === from));
  if (from === settingsButton) settings.removeAttribute("style");
  else {
    const box = from.getBoundingClientRect();
    settings.style.position = "fixed";
    settings.style.top = `${box.bottom + 6}px`;
    settings.style.right = `${document.documentElement.clientWidth - box.right}px`;
    settings.style.bottom = "auto";
  }
  const conceding = root.querySelector("[data-concede]");
  const playing = root.querySelector("main.game:not(.replay)") !== null;
  leave.hidden = !playing;
  leaveButton.disabled = root.querySelector("[data-home]") === null;
  concede.hidden = !playing;
  concedeButton.disabled = conceding === null;
  document.getElementById("concede-why").hidden = conceding !== null;
  if (settings.hidden || conceding === null) askConcede(false);
}
function openSettings(open) {
  settings.hidden = !open;
  syncSettings();
  if (open) settings.querySelector("input:checked")?.focus();
}
settingsButton.addEventListener("click", () => openSettings(settings.hidden));
document.addEventListener("click", (event) => {
  if (!settings.hidden && !event.target.closest(".chrome, [data-settings]")) openSettings(false);
  for (const open of root.querySelectorAll("details.auto-pass[open], details.log[open]")) if (!open.contains(event.target)) open.open = false;
});
// The whole log opens below the turn bar (style.css's .log-panel).
root.addEventListener(
  "toggle",
  (event) => {
    if (!event.target.matches?.("details.log") || !event.target.open) return;
    const bar = root.querySelector(".turn-bar");
    if (bar) root.style.setProperty("--log-top", `${Math.max(0, bar.getBoundingClientRect().bottom)}px`);
    else root.style.removeProperty("--log-top");
  },
  true,
);
document.addEventListener("keydown", (event) => {
  if (event.key === "Escape" && !settings.hidden) {
    event.preventDefault();
    openSettings(false);
    gear().focus();
  }
});
// Leaving for the home screen goes through the turn bar's menu button,
// which carries the game's "home" event while the seat is asked.
const leave = settings.querySelector(".leave");
const leaveButton = document.getElementById("leave-button");
leaveButton.addEventListener("click", () => {
  const leaving = root.querySelector("[data-home]");
  if (leaving === null) return;
  openSettings(false);
  document.body.classList.add("busy");
  engine.postMessage({ type: "home", path: pathOf(leaving), value: "" });
});
// Conceding asks first, in the menu: the button gives way to the question
// and its own Concede and Cancel.
const concede = settings.querySelector(".concede");
const concedeButton = document.getElementById("concede-button");
const concedeConfirm = document.getElementById("concede-confirm");
function askConcede(open) {
  concedeConfirm.hidden = !open;
  concedeButton.hidden = open;
}
concedeButton.addEventListener("click", () => {
  if (root.querySelector("[data-concede]") === null) return;
  askConcede(true);
  document.getElementById("concede-no").focus();
});
document.getElementById("concede-no").addEventListener("click", () => {
  askConcede(false);
  concedeButton.focus();
});
document.getElementById("concede-yes").addEventListener("click", () => {
  const conceding = root.querySelector("[data-concede]");
  openSettings(false);
  if (conceding === null) return;
  document.body.classList.add("busy");
  engine.postMessage({ type: "concede", path: pathOf(conceding), value: "" });
});

const svgNamespace = "http://www.w3.org/2000/svg";

// The elements built by the patch being applied that ask, as they arrive,
// for the focus (data-focus) or to be scrolled into view (data-reveal).
let arrived = [];

// A node and its children; an svg and everything in it in SVG's namespace.
function build(node, svg = false) {
  if ("text" in node) return document.createTextNode(node.text);
  const inSvg = svg || node.tag === "svg";
  const element = inSvg ? document.createElementNS(svgNamespace, node.tag) : document.createElement(node.tag);
  for (const [name, value] of node.props) element.setAttribute(name, value);
  if (element.hasAttribute("data-focus") || element.hasAttribute("data-reveal")) arrived.push(element);
  for (const child of node.children) element.append(build(child, inSvg));
  return element;
}

// Whether a new child of this element belongs in SVG's namespace.
function inSvg(parent) {
  return parent?.namespaceURI === svgNamespace;
}

function at(path) {
  let node = root.firstChild;
  for (const index of path) node = node.childNodes[index];
  return node;
}

// Each seat's life as the page shows it, by seat.
function lives() {
  return new Map(Array.from(root.querySelectorAll("[data-seat-life]"), (box) => [box.dataset.seatLife, Number(box.textContent)]));
}

// A life total that changed floats its difference up from the total. A
// screen with no totals (the hand-off) takes away those still floating.
function floatChanges(before) {
  if (root.querySelector("[data-seat-life]") === null) for (const delta of document.querySelectorAll(".float-delta")) delta.remove();
  for (const box of root.querySelectorAll("[data-seat-life]")) {
    const was = before.get(box.dataset.seatLife);
    const now = Number(box.textContent);
    if (was === undefined || was === now) continue;
    const rect = box.getBoundingClientRect();
    const delta = document.createElement("div");
    delta.className = `float-delta ${now > was ? "gain" : "loss"}`;
    delta.textContent = `${now > was ? "+" : "\u2212"}${Math.abs(now - was)}`;
    delta.setAttribute("aria-hidden", "true");
    delta.style.left = `${rect.left + rect.width / 2}px`;
    delta.style.top = `${rect.top}px`;
    document.body.append(delta);
    delta.addEventListener("animationend", () => delta.remove());
    setTimeout(() => delta.remove(), 2000);
  }
}

// While the page is out of sight, a decision waiting for the player marks
// the tab's title, and notifies where the player turned that on.
const title = document.title;
let told = false;
function announce() {
  const prompt = root.querySelector(".prompt");
  const waiting = prompt !== null && prompt.querySelector(".waiting") === null && prompt.querySelector("button") !== null;
  if (!document.hidden || !waiting || told) return;
  told = true;
  document.title = `\u25CF Your turn \u00B7 ${title}`;
  if (localStorage.getItem("parun-notify") === "on" && "Notification" in window && Notification.permission === "granted") {
    const heading = prompt.querySelector("h2")?.textContent ?? "It's your decision.";
    new Notification("Parun", { body: heading, icon: "./icon.png", tag: "parun-turn" });
  }
}
document.addEventListener("visibilitychange", () => {
  if (!document.hidden) {
    told = false;
    document.title = title;
  }
});

function apply(json) {
  document.body.classList.remove("busy");
  const hadSheet = root.querySelector(".sheet-backdrop:not(.peek) .sheet") !== null;
  const before = lives();
  const docked = dockHeight();
  const focused = document.activeElement;
  arrived = [];
  unkey();
  for (const patch of JSON.parse(json)) {
    const node = at(patch.path);
    switch (patch.op) {
      case "replace":
        node.replaceWith(build(patch.node, inSvg(node.parentNode)));
        break;
      case "set":
        node.setAttribute(patch.name, patch.value);
        break;
      case "remove":
        node.removeAttribute(patch.name);
        break;
      case "append":
        node.append(...patch.nodes.map((child) => build(child, inSvg(node))));
        break;
      case "truncate":
        while (node.childNodes.length > patch.count) node.lastChild.remove();
        break;
      case "arrange": {
        const current = Array.from(node.childNodes);
        node.replaceChildren(...patch.sources.map((source) => ("kept" in source ? current[source.kept] : build(source.node, inSvg(node)))));
        break;
      }
      default:
        throw new Error(`unknown patch ${patch.op}`);
    }
  }
  holdDock(docked);
  syncSheet();
  // A preview the view no longer shows previews again when its card is next
  // focused.
  if (root.querySelector(".sheet-backdrop.peek") === null) peeking = null;
  placePeek();
  syncSettings();
  floatChanges(before);
  announce();
  rekey();
  playOn();
  tickClocks();
  for (const element of arrived) {
    if (!element.isConnected) continue;
    if (element.hasAttribute("data-reveal")) element.scrollIntoView({ block: "nearest", behavior: "smooth" });
    if (element.hasAttribute("data-focus")) element.focus({ preventScroll: element.hasAttribute("data-reveal") });
  }
  arrived = [];
  // A sheet that just opened takes the focus, on its first action; closed,
  // it gives the focus back to the card that had it.
  const sheet = openSheet();
  if (sheet && !hadSheet) {
    returnTo = focused?.matches?.(".card, .stack-item") ? focused : null;
    sheet.querySelector(".sheet-actions button, button.close")?.focus({ preventScroll: true });
  } else if (!sheet && hadSheet) {
    if (returnTo?.isConnected) returnTo.focus({ preventScroll: true });
    returnTo = null;
  }
}

// A running clock (Parun.Web.Clock) counts down to its data-ends, in
// milliseconds since the epoch, between the views the engine sends; a part
// second counts whole, as the engine shows it.
function tickClocks() {
  for (const clock of root.querySelectorAll(".clock[data-ends]")) {
    const left = Math.ceil(Math.max(0, Number(clock.dataset.ends) - Date.now()) / 1000);
    const time = clock.querySelector(".clock-time");
    const shown = `${Math.floor(left / 60)}:${String(left % 60).padStart(2, "0")}`;
    if (time !== null && time.textContent !== shown) time.textContent = shown;
    clock.classList.toggle("low", left < 60);
  }
}
setInterval(tickClocks, 250);

// A replay playing by itself (Parun.Web.Replay's "playing") presses its
// next step a second after each position shows, holding while a card is
// open over the table; the engine stops it at the end.
const playInterval = 1000;
let playTimer = null;
function playOn() {
  clearTimeout(playTimer);
  playTimer = null;
  if (root.querySelector("main.replay.playing") === null) return;
  playTimer = setTimeout(() => {
    playTimer = null;
    if (openSheet()) return;
    root.querySelector('main.replay.playing .step-buttons button[aria-label="Next position"]:not(:disabled)')?.click();
  }, playInterval);
}

// While the game goes on without the seat (Parun.Web.Question.watching), the
// dock keeps the height it had, so on a phone the hand and board above it hold
// still through a pass of many steps; the next question sizes it afresh.
function dock() {
  return root.querySelector("main.game > .side");
}

function dockHeight() {
  return dock()?.getBoundingClientRect().height ?? 0;
}

function holdDock(height) {
  const side = dock();
  if (!side) return;
  if (side.querySelector(".prompt > .waiting")) side.style.minHeight = `${height}px`;
  else side.style.removeProperty("min-height");
}

// The child indices from the view's root to the node, dotted; null outside it.
function pathOf(target) {
  const path = [];
  for (let node = target; node !== root.firstChild; node = node.parentNode) {
    if (node === root || node === null) return null;
    path.unshift(Array.prototype.indexOf.call(node.parentNode.childNodes, node));
  }
  return path.join(".");
}

for (const type of events) {
  root.addEventListener(type, (event) => {
    const path = pathOf(event.target);
    if (path === null) return;
    // The turn bar's settings button opens the menu, the page's own.
    if (type === "click" && event.target.closest("[data-settings]")) {
      openSettings(settings.hidden);
      return;
    }
    // A Copy button copies its box (Parun.Web.Element's copyable); the
    // engine has nothing to do with it.
    const copying = type === "click" ? event.target.closest("[data-copy]") : null;
    if (copying) {
      copy(copying);
      return;
    }
    // An auto-pass level tapped is kept for the next game, and closes its
    // menu.
    const chosen = type === "click" ? event.target.closest("[data-level]") : null;
    if (chosen) {
      localStorage.setItem("parun-level", chosen.dataset.level);
      chosen.closest("details").open = false;
    }
    // A tap shows the page busy until the engine next patches it.
    if (type === "click" && event.target.closest("button")) {
      document.body.classList.add("busy");
    }
    engine.postMessage({ type, path, value: String(event.target.value ?? "") });
    // A typed line is sent once; the box is ready for the next. A filter
    // (an "input" handler) keeps what is typed.
    if (type === "change" && event.target.dataset.clear !== undefined) event.target.value = "";
  });
}

// Enter in a box naming a button (data-enter) presses it, as a form's
// submit button would.
root.addEventListener("keydown", (event) => {
  if (event.key !== "Enter" || event.isComposing || !event.target.matches?.("[data-enter]")) return;
  const button = document.getElementById(event.target.dataset.enter);
  if (button === null || button.disabled) return;
  event.preventDefault();
  button.click();
});

// A read-only box (a link or code to send) is selected whole when focused
// or tapped, so one copy takes all of it; selecting on the click too keeps
// Safari, which clears a selection made on focus, from dropping it.
for (const type of ["focusin", "click"]) {
  root.addEventListener(type, (event) => {
    if (event.target.matches?.("input[readonly], textarea[readonly]")) event.target.select();
  });
}

// Copies the box a Copy button names, and says so on the button for a
// moment; where the clipboard is refused, the box is left selected.
async function copy(button) {
  const box = document.getElementById(button.dataset.copy);
  if (box === null) return;
  box.select();
  try {
    await navigator.clipboard.writeText(box.value);
  } catch {
    if (!document.execCommand("copy")) return;
  }
  button.dataset.copied = "";
  setTimeout(() => delete button.dataset.copied, 1500);
}

// A pointer resting on a card (a mouse, not a finger) previews its sheet
// almost at once, and leaving the card ends the preview (Parun.Web.Card's
// "peek" and "unpeek"). Where the side column has room for the card tapped
// (style.css's inspector), a card the keyboard focuses previews too, until
// the focus leaves the cards.
const hovering = matchMedia("(hover: hover) and (pointer: fine)");
const inspecting = matchMedia("(min-width: 900px)");
let resting = null;
let peeking = null;
// Where the pointer last was on the card previewed (placePeek); null for a
// card focused.
let pointer = null;
function peek(card, at) {
  if (!card.isConnected || root.querySelector(".sheet-backdrop:not(.peek)")) return;
  const path = pathOf(card);
  if (path === null) return;
  peeking = card;
  pointer = at;
  engine.postMessage({ type: "peek", path, value: "" });
}
function unpeek(card) {
  peeking = null;
  const path = pathOf(card);
  if (path !== null) engine.postMessage({ type: "unpeek", path, value: "" });
}
// Ends whatever preview shows, through its card or, where a patch replaced
// that, any card; whether one showed.
function endPeek() {
  if (root.querySelector(".sheet-backdrop.peek") === null) {
    peeking = null;
    return false;
  }
  const from = peeking?.isConnected ? peeking : cards()[0];
  if (from) unpeek(from);
  else peeking = null;
  return true;
}
root.addEventListener("pointerover", (event) => {
  const card = event.target.closest?.(".card, .stack-item");
  if (!hovering.matches || event.pointerType === "touch" || !card || card === resting) return;
  resting = card;
  const at = { x: event.clientX, y: event.clientY };
  // A moment, so a pointer sweeping across the hand previews less of it.
  setTimeout(() => {
    if (resting === card) peek(card, at);
  }, 60);
});
root.addEventListener("pointermove", (event) => {
  if (peeking === null || pointer === null || !peeking.contains(event.target)) return;
  pointer = { x: event.clientX, y: event.clientY };
  placePeek();
});
root.addEventListener("pointerout", (event) => {
  const card = event.target.closest?.(".card, .stack-item");
  if (!card || card.contains(event.relatedTarget)) return;
  if (resting === card) resting = null;
  // The card previewed, or the one a patch put in its place.
  if (pointer !== null && (peeking === card || (peeking !== null && !peeking.isConnected))) unpeek(card);
});
// A press elsewhere, or a pointer the browser takes over (a long press, a
// drag), ends a preview; a tap on the card opens its sheet in its place.
root.addEventListener("pointerdown", (event) => {
  if (pointer !== null && !peeking?.contains(event.target)) endPeek();
});
root.addEventListener("pointercancel", () => {
  if (pointer !== null) endPeek();
});
// The pointer's preview sat beside where the pointer was before the window
// changed size.
window.addEventListener("resize", () => {
  if (pointer !== null) endPeek();
});
root.addEventListener("focusin", (event) => {
  const card = event.target.closest?.(".card, .stack-item");
  if (inspecting.matches && card && card !== peeking && !card.closest(".sheet-backdrop") && card.matches(":focus-visible")) peek(card, null);
});
root.addEventListener("focusout", (event) => {
  const card = event.target.closest?.(".card, .stack-item");
  if (card && peeking === card && pointer === null && !event.relatedTarget?.closest?.(".card, .stack-item")) unpeek(card);
});

// A long press on a card (a finger can't rest on one) or a right click
// opens its sheet without what a tap does (Parun.Web.Card's "read", on a
// tile marked data-read), so a card a prompt picks is read before it is
// picked; the click ending the press is dropped, wherever the sheet put it.
const holdTime = 450;
const holdSlop = 10;
// The press under way: where it began, and its timer.
let hold = null;
let dropClick = false;
function readCard(card) {
  const path = card.isConnected ? pathOf(card) : null;
  if (path === null) return;
  endPeek();
  engine.postMessage({ type: "read", path, value: "" });
}
function letGo() {
  if (hold === null) return;
  clearTimeout(hold.timer);
  hold = null;
}
root.addEventListener("pointerdown", (event) => {
  letGo();
  const card = event.target.closest?.("[data-read]");
  if (event.pointerType === "mouse" || !card || card.closest(".sheet-backdrop")) return;
  const timer = setTimeout(() => {
    hold = null;
    dropClick = true;
    readCard(card);
  }, holdTime);
  hold = { id: event.pointerId, x: event.clientX, y: event.clientY, timer };
});
root.addEventListener("pointermove", (event) => {
  if (hold !== null && event.pointerId === hold.id && Math.hypot(event.clientX - hold.x, event.clientY - hold.y) > holdSlop) letGo();
});
for (const type of ["pointerup", "pointercancel"]) {
  root.addEventListener(type, (event) => {
    if (hold !== null && event.pointerId === hold.id) letGo();
    // A browser that sends no click after a long press drops none later.
    if (dropClick) setTimeout(() => (dropClick = false), 400);
  });
}
root.addEventListener(
  "click",
  (event) => {
    if (!dropClick) return;
    dropClick = false;
    event.preventDefault();
    event.stopImmediatePropagation();
  },
  true,
);
// A long press some browsers end in a context menu, and a right click.
root.addEventListener("contextmenu", (event) => {
  const card = event.target.closest?.("[data-read]");
  if (!card || card.closest(".sheet-backdrop")) return;
  event.preventDefault();
  // The press's timer read it already.
  if (dropClick) return;
  if (hold !== null) dropClick = true;
  letGo();
  readCard(card);
});

// The open card sheet where it is a dialog over the table, as on a phone,
// rather than the side column's inspector; only then does it take the
// keys from the prompt.
function modalSheet() {
  return inspecting.matches ? null : openSheet();
}
function syncSheet() {
  const backdrop = root.querySelector(".sheet-backdrop:not(.peek)");
  if (backdrop === null) return;
  if (inspecting.matches) backdrop.removeAttribute("aria-modal");
  else backdrop.setAttribute("aria-modal", "true");
}
// A focused card's preview, which only the inspector's width allows, ends
// when the window narrows.
inspecting.addEventListener("change", () => {
  syncSheet();
  if (inspecting.matches || peeking === null || pointer !== null) return;
  endPeek();
});

// A preview sits beside the pointer, as a tooltip would, or beside the card
// focused: to its right where it fits, else to its left, level with it as
// far as the window allows.
function placePeek() {
  const sheet = root.querySelector(".sheet-backdrop.peek .sheet");
  if (!sheet || !peeking?.isConnected) return;
  const at = pointer === null ? peeking.getBoundingClientRect() : { left: pointer.x, right: pointer.x, top: pointer.y, width: 0, height: 0 };
  const box = sheet.getBoundingClientRect();
  const width = document.documentElement.clientWidth;
  const height = document.documentElement.clientHeight;
  const gap = 12;
  const off = pointer === null ? gap : 2 * gap;
  let left = at.right + off;
  if (left + box.width > width - gap) left = at.left - off - box.width;
  if (left < gap) left = Math.max(gap, Math.min(width - gap - box.width, at.left + at.width / 2 - box.width / 2));
  const level = pointer === null ? at.top + at.height / 2 - box.height / 2 : at.top - 2 * gap;
  const top = Math.max(gap, Math.min(height - gap - box.height, level));
  sheet.style.position = "fixed";
  sheet.style.margin = "0";
  sheet.style.left = `${left}px`;
  sheet.style.top = `${top}px`;
}

// The keyboard, which the shortcuts list (#keys, opened by ?) spells out:
// Enter or Space presses the prompt's main button, a digit the button
// numbered for it, a letter the button the view gave that key
// (aria-keyshortcuts: A attacks with all, E ends the turn), the arrows step
// through the cards, L opens or closes the log, and Escape closes the card
// sheet. None fires while typing in a box or with a menu open.
const shortcuts = document.getElementById("keys");
document.getElementById("keys-button").addEventListener("click", () => {
  openSettings(false);
  shortcuts.showModal();
});
// A click outside the list closes it, as Escape does.
shortcuts.addEventListener("click", (event) => {
  const box = shortcuts.getBoundingClientRect();
  if (event.clientX < box.left || event.clientX > box.right || event.clientY < box.top || event.clientY > box.bottom) shortcuts.close();
});
// The buttons' keys show while Alt is held, and pressing one then works as
// it does alone.
function showKeys(on) {
  document.documentElement.classList.toggle("show-keys", on);
}
document.addEventListener("keydown", (event) => showKeys(event.altKey && !event.metaKey && !event.ctrlKey));
document.addEventListener("keyup", (event) => showKeys(event.altKey && !event.metaKey && !event.ctrlKey));
window.addEventListener("blur", () => showKeys(false));
// The key a press means, as though Alt were not held (Alt+A types "\u00e5"
// on a Mac).
function pressed(event) {
  if (!event.altKey) return event.key;
  if (event.code.startsWith("Key")) return event.code.slice(3).toLowerCase();
  if (event.code.startsWith("Digit")) return event.code.slice(5);
  return event.key;
}
// Marks a key page.js gave, so unkey takes back only those.
const given = "data-key-given";
let returnTo = null;

function openSheet() {
  return root.querySelector(".sheet-backdrop:not(.peek) .sheet");
}

function mainButton() {
  return root.querySelector(".prompt button.primary:not(:disabled), .handoff button.primary, .lobby-start button.start:not(.ready, :disabled)");
}

// Takes back the keys rekey gave, leaving the page as the engine built it,
// which its patches are paths into.
function unkey() {
  for (const button of root.querySelectorAll(`[${given}]`)) {
    button.removeAttribute(given);
    button.removeAttribute("aria-keyshortcuts");
  }
}

// Gives the main button Enter, then numbers 1 to 9 and 0 the buttons a
// card sheet over the table offers, or else the prompt's: in the order
// shown, but the main button, any with a key of the view's, the damage
// rows' amounts and cards' tiles. A prompt's numbers follow its own
// buttons, so the same kind of prompt numbers alike; a meaning that recurs
// has its letter.
function rekey() {
  const give = (button, key) => {
    button.setAttribute("aria-keyshortcuts", key);
    button.setAttribute(given, "");
  };
  const sheet = modalSheet();
  const main = sheet ? null : mainButton();
  if (main && !main.hasAttribute("aria-keyshortcuts")) give(main, "Enter");
  const candidates = sheet ? sheet.querySelectorAll(".sheet-actions button") : root.querySelectorAll(".prompt button");
  const numbered = Array.from(candidates).filter((button) => button !== main && !button.disabled && !button.hasAttribute("aria-keyshortcuts") && !button.closest(".segmented") && !button.classList.contains("card"));
  // Buttons that are numbers themselves (choosing X) each take their own digit.
  if (numbered.length > 0 && numbered.every((button) => /^\d+$/.test(button.textContent))) {
    for (const button of numbered) if (/^\d$/.test(button.textContent)) give(button, button.textContent);
  } else numbered.slice(0, 10).forEach((button, i) => give(button, String((i + 1) % 10)));
}

// The cards on the screen, the stack's included, in the order shown.
function cards() {
  return Array.from(root.querySelectorAll(".card, .stack-item")).filter((card) => card.getClientRects().length > 0 && !card.closest(".sheet-backdrop"));
}

// The card nearest `from` in a direction, nearer across counting double;
// from anything but a card, the first card in hand, else the first shown.
function nearest(from, dx, dy) {
  const all = cards();
  if (!all.includes(from)) return all.find((card) => card.closest(".hand")) ?? all[0] ?? null;
  const centre = (box) => [box.left + box.width / 2, box.top + box.height / 2];
  const [fx, fy] = centre(from.getBoundingClientRect());
  let best = null;
  let bestScore = Infinity;
  for (const card of all) {
    const [cx, cy] = centre(card.getBoundingClientRect());
    const along = (cx - fx) * dx + (cy - fy) * dy;
    const across = Math.abs(dx !== 0 ? cy - fy : cx - fx);
    if (along < 1) continue;
    const score = along + 2 * across;
    if (score < bestScore) {
      best = card;
      bestScore = score;
    }
  }
  return best;
}

const arrows = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] };

document.addEventListener("keydown", (event) => {
  if (event.defaultPrevented || event.metaKey || event.ctrlKey) return;
  if (!settings.hidden || shortcuts.open) return;
  const key = pressed(event);
  const sheet = modalSheet();
  if (key === "Escape") {
    const levels = root.querySelector("details.auto-pass[open], details.log[open]");
    if (endPeek()) return;
    if (levels) {
      levels.open = false;
      levels.querySelector("summary").focus();
    } else root.querySelector(".sheet button.close")?.click();
    return;
  }
  // Tab stays within an open card sheet over the table, as within any dialog.
  if (key === "Tab" && sheet) {
    const stops = Array.from(sheet.querySelectorAll("button"));
    const first = stops[0];
    const last = stops[stops.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    } else if (!sheet.contains(document.activeElement)) {
      event.preventDefault();
      first.focus();
    }
    return;
  }
  const target = event.target;
  const typing = target instanceof Element && (target.matches("input, textarea, select") || target.isContentEditable);
  if (typing) return;
  if (key === "Enter" || key === " ") {
    // A focused button or summary takes its own Enter.
    // (Safari has no HTMLSummaryElement.)
    const onButton = target instanceof Element && target.matches("button, summary");
    const main = onButton || sheet ? null : mainButton();
    if (main) {
      event.preventDefault();
      main.click();
    }
    return;
  }
  if (key === "?") {
    event.preventDefault();
    shortcuts.showModal();
    return;
  }
  if (sheet === null && key in arrows) {
    const next = nearest(target, ...arrows[key]);
    if (next) {
      event.preventDefault();
      next.focus();
    }
    return;
  }
  if (sheet === null && key.toUpperCase() === "L") {
    const log = root.querySelector("details.log");
    if (log) log.open = !log.open;
    return;
  }
  if (key.length !== 1) return;
  const letter = key.toUpperCase();
  const scope = sheet ?? root.querySelector(".prompt");
  const button = scope && Array.from(scope.querySelectorAll("button[aria-keyshortcuts]")).find((b) => !b.disabled && b.getAttribute("aria-keyshortcuts") === letter);
  if (button) {
    event.preventDefault();
    button.click();
  }
});

engine.onmessage = ({ data }) => {
  if ("patches" in data) apply(data.patches);
  else if ("phase" in data) note(data.phase);
  else if ("cardChunks" in data) cardChunks = data.cardChunks;
  else if ("save" in data) localStorage.setItem(deckKey + data.save.slug, data.save.text);
  else if ("forget" in data) localStorage.removeItem(deckKey + data.forget);
  else if ("link" in data) linkStep(data.link);
  else if ("game" in data) keepGame(data.game);
  else if ("locate" in data) locate(data.locate);
  else if ("download" in data) download(data.download);
};
// A file the engine hands over (Parun.Browser.download), saved to the
// player's device under its name.
function download({ name, text }) {
  const url = URL.createObjectURL(new Blob([text], { type: "application/json" }));
  const link = document.createElement("a");
  link.href = url;
  link.download = name;
  link.click();
  setTimeout(() => URL.revokeObjectURL(url), 0);
}
// The engine names each screen it shows by a query (Parun.Web.Route), which
// the address bar shows; back and forward load the page afresh at the query
// they reach, since the engine opens a screen only from its start. A move
// within the page that keeps the query (a fragment's) is left alone. The
// join screen's queries (Parun.Web.Route's MkJoin) keep the fragment carrying
// the host's code, so a reload there makes a fresh reply to it; every other
// screen's drops it.
const joining = new Set(["?join", "?rejoin"]);
let located = location.search;
function locate({ search, push }) {
  located = search;
  const url = location.pathname + search + (joining.has(search) ? location.hash : "");
  if (url === location.pathname + location.search + location.hash) return;
  if (push) history.pushState(null, "", url);
  else history.replaceState(null, "", url);
}
addEventListener("popstate", () => {
  if (location.search !== located) location.reload();
});
// A host's link pasted over the address changes only its fragment, so the
// page loads afresh to read its code.
addEventListener("hashchange", () => {
  if (location.hash.length > 1) location.reload();
});
// The game in progress: its first line once it starts, each reply's line
// added as it is made, and all of it forgotten once it is over. A reply
// with no game kept to add to is dropped, and a game that cannot be kept
// whole (storage full, say) is forgotten, since a reply missing would
// replay another game. The clock running (Parun.Web.Link.savedRunning) is
// held until the next reply, and added last, stamped, as the page hides or
// unloads (stampRunning), for a reload to charge its seat the time it ran.
let running = null;
function keepGame({ start, reply, retract, running: clock }) {
  if (clock !== undefined) {
    running = clock;
    return;
  }
  running = null;
  try {
    const kept = localStorage.getItem(gameKey);
    if (start !== undefined) localStorage.setItem(gameKey, start);
    else if (reply !== undefined) {
      if (kept !== null) localStorage.setItem(gameKey, `${unstamped(kept)}\n${reply}`);
    } else if (retract !== undefined) {
      // The last reply's line taken back (Parun.Browser.retractReply): the
      // game's own line, first, is never a reply's.
      const replies = kept === null ? "" : unstamped(kept);
      const last = replies.lastIndexOf("\n");
      if (last !== -1) localStorage.setItem(gameKey, replies.slice(0, last));
    } else localStorage.removeItem(gameKey);
  } catch (problem) {
    console.warn(problem);
    try {
      localStorage.removeItem(gameKey);
    } catch (again) {
      console.warn(again);
    }
  }
}
// The game kept without a running clock's line, which is only ever last.
function unstamped(kept) {
  const last = kept.lastIndexOf("\n");
  return kept.startsWith('{"running"', last + 1) ? kept.slice(0, last) : kept;
}
function stampRunning() {
  if (running === null) return;
  try {
    const kept = localStorage.getItem(gameKey);
    const line = JSON.parse(running);
    line.running.left = Date.now();
    if (kept !== null) localStorage.setItem(gameKey, `${unstamped(kept)}\n${JSON.stringify(line)}`);
  } catch (problem) {
    console.warn(problem);
  }
}
addEventListener("pagehide", stampRunning);
addEventListener("visibilitychange", () => {
  if (document.visibilityState === "hidden") stampRunning();
});
engine.onerror = (error) => note(`engine failed: ${error.message}`);

// Seats played from another browser (Parun.Web.Link): one peer connection
// and one reliable, ordered data channel each, by the engine's number for
// it (Parun.Browser.linkHost). Two codes, swapped by hand, connect them: the
// host's offer, as a link, and the guest's reply. Each is a session
// description, packed (`pack`) once ICE gathering is done, so one code goes
// each way. Only the STUN server sees either page.
const links = new Map();
const iceServers = [{ urls: "stun:stun.l.google.com:19302" }];
// A line goes in pieces small enough for every browser's channel, each
// marked with whether more of the line follows.
const piece = 16000;
// An open channel sends an empty "~" piece every `beat` ms, and one that
// hears nothing for `silence` ms is dropped. A browser can leave a dead
// connection `disconnected` with its channel open, its page never told; the
// other page's channel then falls silent, so both pages see the drop.
const beat = 5000;
const silence = 30000;
function linked(seat, kind, text = "") {
  engine.postMessage({ linked: { seat, kind, text } });
}
// What a code carries, in the order it carries it: the format's version,
// the DTLS role and fingerprint hash, the ICE credentials six bits a
// character, the fingerprint, and each candidate of component 1. The rest of
// the description is the same on every page and rebuilt (`described`).
const format = 1;
const alphabet = "0123456789abcdefghjkmnpqrstvwxyz";
const iceChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const roles = ["actpass", "active", "passive"];
const hashes = [
  ["sha-256", 32],
  ["sha-1", 20],
  ["sha-384", 48],
  ["sha-512", 64],
];
const kinds = ["host", "srflx", "prflx", "relay"];
const transports = ["udp", "tcp active", "tcp passive", "tcp so"];
const uuidHost = /^([0-9a-f]{8})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{12})\.local$/;
// An IPv6 address's eight groups, or null for any other text.
function groupsOf(address) {
  if (!/^[0-9a-f:]+$/i.test(address)) return null;
  const halves = address.split("::");
  if (halves.length > 2) return null;
  const [head, tail] = halves.map((half) => (half === "" ? [] : half.split(":")));
  const gap = 8 - head.length - (tail?.length ?? 0);
  if (tail === undefined ? gap !== 0 : gap < 1) return null;
  const groups = [...head, ...Array(tail === undefined ? 0 : gap).fill("0"), ...(tail ?? [])];
  return groups.every((group) => group.length >= 1 && group.length <= 4) ? groups.map((group) => parseInt(group, 16)) : null;
}
// A session description as a code: Crockford's base32 of a bit string, so
// a code is letters and digits only, read in either case.
function pack(description) {
  const lines = description.sdp.split(/\r?\n/);
  const value = (name) => lines.find((line) => line.startsWith(`a=${name}:`))?.slice(name.length + 3);
  const [hashName, print = ""] = (value("fingerprint") ?? "").split(" ");
  const role = roles.indexOf(value("setup"));
  const hash = hashes.findIndex(([name]) => name === hashName);
  const fingerprint = print.split(":").map((pair) => parseInt(pair, 16));
  const credentials = [value("ice-ufrag") ?? "", value("ice-pwd") ?? ""];
  const unpackable = value("mid") !== "0" || (value("sctp-port") ?? "5000") !== "5000" || role < 0 || (role === 0) !== (description.type === "offer") || hash < 0 || fingerprint.length !== hashes[hash][1] || fingerprint.some(Number.isNaN) || credentials.some((text) => text.length === 0 || text.length > 255 || [...text].some((c) => !iceChars.includes(c)));
  if (unpackable) throw new Error(`a session description no code carries: ${description.sdp}`);
  const digits = [];
  let held = 0;
  let width = 0;
  const put = (number, bits) => {
    for (let at = bits - 1; at >= 0; at -= 1) {
      held = held * 2 + (Math.floor(number / 2 ** at) % 2);
      width += 1;
      if (width === 5) {
        digits.push(alphabet[held]);
        held = 0;
        width = 0;
      }
    }
  };
  put(format, 4);
  put(role, 2);
  put(hash, 2);
  for (const text of credentials) put(text.length, 8);
  for (const text of credentials) for (const c of text) put(iceChars.indexOf(c), 6);
  for (const byte of fingerprint) put(byte, 8);
  for (const line of lines) {
    const [, component, protocol, , address, port, , kindName, ...rest] = line.startsWith("a=candidate:") ? line.split(" ") : [];
    const tcpType = rest[rest.indexOf("tcptype") + 1];
    const transport = transports.indexOf(protocol?.toLowerCase() === "udp" ? "udp" : `tcp ${tcpType}`);
    const kind = kinds.indexOf(kindName);
    if (component !== "1" || transport < 0 || kind < 0) continue;
    const groups = groupsOf(address);
    const uuid = address.match(uuidHost);
    const name = new TextEncoder().encode(address);
    const form = /^(25[0-5]|2[0-4]\d|1?\d?\d)(\.(25[0-5]|2[0-4]\d|1?\d?\d)){3}$/.test(address) ? 0 : groups ? 1 : uuid ? 2 : name.length <= 255 ? 3 : -1;
    if (form < 0) continue;
    put(kind, 2);
    put(transport, 2);
    put(form, 2);
    if (form === 0) for (const part of address.split(".")) put(Number(part), 8);
    else if (form === 1) for (const group of groups) put(group, 16);
    else if (form === 2) for (const pair of uuid.slice(1).join("").match(/../g)) put(parseInt(pair, 16), 8);
    else {
      put(name.length, 8);
      for (const byte of name) put(byte, 8);
    }
    put(Number(port), 16);
  }
  if (width > 0) put(0, 5 - width);
  return digits.join("");
}
// A code's session description; whatever is not a letter or digit is
// skipped, as a chat app's mangling.
function unpack(code) {
  const digits = [...code.toLowerCase().replace(/[^0-9a-z]/g, "").replaceAll("o", "0").replace(/[il]/g, "1")].map((c) => alphabet.indexOf(c));
  if (digits.includes(-1)) throw new Error(`not a code: ${code}`);
  let at = 0;
  const take = (bits) => {
    let number = 0;
    for (let end = at + bits; at < end; at += 1) {
      if (at >= digits.length * 5) throw new Error(`a code cut short: ${code}`);
      number = number * 2 + (Math.floor(digits[Math.floor(at / 5)] / 2 ** (4 - (at % 5))) % 2);
    }
    return number;
  };
  if (take(4) !== format) throw new Error(`a code from another build: ${code}`);
  const role = roles[take(2)];
  const [hashName, hashBytes] = hashes[take(2)];
  if (role === undefined) throw new Error(`not a code: ${code}`);
  const lengths = [take(8), take(8)];
  const [ufrag, pwd] = lengths.map((length) => Array.from({ length }, () => iceChars[take(6)]).join(""));
  const fingerprint = Array.from({ length: hashBytes }, () => take(8).toString(16).toUpperCase().padStart(2, "0")).join(":");
  const candidates = [];
  // A candidate takes more than 8 bits, and the padding fewer.
  while (digits.length * 5 - at >= 8) {
    const kind = kinds[take(2)];
    const [protocol, tcpType] = transports[take(2)].split(" ");
    const form = take(2);
    const bytes = (length) => Array.from({ length }, () => take(8));
    const address = [() => bytes(4).join("."), () => Array.from({ length: 8 }, () => take(16).toString(16)).join(":"), () => `${bytes(16).map((byte) => byte.toString(16).padStart(2, "0")).join("").replace(/^(.{8})(.{4})(.{4})(.{4})/, "$1-$2-$3-$4-")}.local`, () => new TextDecoder().decode(new Uint8Array(bytes(take(8))))][form]();
    const port = take(16);
    // A candidate's priority, not carried, ranks host over reflexive over
    // relayed, then UDP over TCP, then the order sent (RFC 8445 5.1.2).
    const preference = { host: 126, prflx: 110, srflx: 100, relay: 0 }[kind] * 2 ** 24 + ((protocol === "udp" ? 32768 : 0) + 32767 - candidates.length) * 256 + 255;
    candidates.push(`a=candidate:${candidates.length + 1} 1 ${protocol} ${preference} ${address} ${port} typ ${kind}${kind === "host" ? "" : " raddr 0.0.0.0 rport 0"}${tcpType ? ` tcptype ${tcpType}` : ""}`);
  }
  return { type: role === "actpass" ? "offer" : "answer", sdp: described({ ufrag, pwd, hashName, fingerprint, role, candidates }) };
}
// The session description every page makes for its one data channel, with
// what a code carries put in.
function described({ ufrag, pwd, hashName, fingerprint, role, candidates }) {
  return [
    "v=0",
    "o=- 1 1 IN IP4 0.0.0.0",
    "s=-",
    "t=0 0",
    "a=group:BUNDLE 0",
    "m=application 9 UDP/DTLS/SCTP webrtc-datachannel",
    "c=IN IP4 0.0.0.0",
    `a=ice-ufrag:${ufrag}`,
    `a=ice-pwd:${pwd}`,
    "a=ice-options:trickle",
    `a=fingerprint:${hashName} ${fingerprint}`,
    `a=setup:${role}`,
    "a=mid:0",
    "a=sctp-port:5000",
    ...candidates,
    "a=end-of-candidates",
    "",
  ].join("\r\n");
}
function gathered(connection) {
  return new Promise((resolve) => {
    const done = () => connection.iceGatheringState === "complete" && resolve();
    connection.addEventListener("icegatheringstatechange", done);
    done();
  });
}
// A link's channel says only while it is the seat's current one; a dropped
// or silent connection closes it, and only a new link, its codes swapped by
// hand again (Parun.Web.Rejoin), reconnects.
function listen(seat, link, channel) {
  const current = () => links.get(seat) === link;
  let partial = "";
  let heard = Date.now();
  let begun = false;
  link.channel = channel;
  // A joining page's channel can be open as it arrives and say so again.
  const begin = () => {
    if (begun) return;
    begun = true;
    heard = Date.now();
    const beating = setInterval(() => {
      if (!current() || channel.readyState !== "open") clearInterval(beating);
      else if (Date.now() - heard > silence) drop(seat, link);
      else channel.send("~");
    }, beat);
    linked(seat, "open");
  };
  channel.onopen = () => current() && begin();
  channel.onmessage = ({ data }) => {
    heard = Date.now();
    if (data[0] === "~") return;
    partial += data.slice(1);
    if (data[0] === "." && current()) {
      linked(seat, "message", partial);
      partial = "";
    }
  };
  channel.onclose = () => current() && drop(seat, link);
  if (channel.readyState === "open") begin();
}
function drop(seat, link) {
  links.delete(seat);
  link.channel?.close();
  link.connection.close();
  linked(seat, "closed");
}
function opened(seat) {
  const old = links.get(seat);
  if (old) {
    links.delete(seat);
    old.channel?.close();
    old.connection.close();
  }
  const link = { connection: new RTCPeerConnection({ iceServers }), channel: null };
  links.set(seat, link);
  link.connection.addEventListener("connectionstatechange", () => {
    if (["failed", "closed"].includes(link.connection.connectionState) && links.get(seat) === link) drop(seat, link);
  });
  return link;
}
async function linkStep(step) {
  const { op, seat } = step;
  try {
    if (op === "host") {
      const link = opened(seat);
      listen(seat, link, link.connection.createDataChannel("parun", { ordered: true }));
      await link.connection.setLocalDescription(await link.connection.createOffer());
      await gathered(link.connection);
      const url = new URL(location.pathname, location.origin);
      url.search = step.query;
      url.hash = pack(link.connection.localDescription);
      if (links.get(seat) === link) linked(seat, "offer", url.href);
    } else if (op === "accept") {
      await links.get(seat)?.connection.setRemoteDescription(unpack(step.code));
    } else if (op === "join") {
      const link = opened(seat);
      link.connection.addEventListener("datachannel", ({ channel }) => listen(seat, link, channel));
      await link.connection.setRemoteDescription(unpack(step.code));
      await link.connection.setLocalDescription(await link.connection.createAnswer());
      await gathered(link.connection);
      if (links.get(seat) === link) linked(seat, "reply", pack(link.connection.localDescription));
    } else if (op === "send") {
      const channel = links.get(seat)?.channel;
      if (channel?.readyState === "open") for (let at = 0; at < step.text.length || at === 0; at += piece) channel.send((at + piece >= step.text.length ? "." : "+") + step.text.slice(at, at + piece));
    } else if (op === "close") {
      const link = links.get(seat);
      if (link) {
        links.delete(seat);
        link.channel?.close();
        link.connection.close();
      }
    }
  } catch (problem) {
    // A code that does not read, say, ends the link.
    console.warn(problem);
    const link = links.get(seat);
    if (link) drop(seat, link);
    else linked(seat, "closed");
  }
}
// Leaving the page ends every link at once, so the other side stops.
addEventListener("pagehide", () => {
  for (const link of links.values()) {
    link.channel?.close();
    link.connection.close();
  }
});
