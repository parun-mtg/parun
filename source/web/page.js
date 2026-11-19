// The web client's page: applies the patches the engine sends and reports
// each event's path back (Parun.Web.Wire). The engine runs in a Web Worker
// (engine.js), so the page stays responsive while it works. The view's root
// is #app's only child, and paths count from it.
const root = document.getElementById("app");
const events = ["click", "input", "change"];
const engineUrl = new URL("./engine.js", import.meta.url);
engineUrl.search = location.search;
// The auto-pass level last chosen (Parun.Web.Question.Level) starts the game.
const level = localStorage.getItem("parun-level");
if (level !== null) engineUrl.searchParams.set("level", level);
const engine = new Worker(engineUrl, { type: "module" });
// Safari runs WebAssembly far slower outside a secure context (#8419), and
// plain http from a network address is not one.
if (!isSecureContext) document.getElementById("insecure").hidden = false;
// Once fetched, the engine is kept for later visits (sw.js).
if ("serviceWorker" in navigator && isSecureContext) navigator.serviceWorker.register("./sw.js").catch((problem) => console.warn(problem));
// With ?trace, the engine's step timings and the page's own go to a panel.
const trace = new URLSearchParams(location.search).has("trace") ? document.getElementById("trace") : null;
if (trace) {
  trace.closest("details").hidden = false;
  document.body.classList.add("tracing");
}
function traced(line) {
  if (trace) trace.textContent += `${Math.round(performance.now())} ${line}\n`;
}

// The engine's load phases and the last tap's response time show at the foot
// of the page, and the latest phase on the loading screen.
const timings = document.getElementById("timings");
function note(line) {
  console.log(line);
  timings.textContent += (timings.textContent ? "; " : "") + line;
  const phase = document.getElementById("phase");
  const what = line.replace(/ in \d+ ms$/, "");
  if (phase) phase.textContent = `${what[0].toUpperCase()}${what.slice(1)}\u2026`;
}

// The settings menu: theme (the system's unless one is picked), card size
// and motion, each kept and applied as an attribute of the page's root.
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
function openSettings(open) {
  settings.hidden = !open;
  settingsButton.setAttribute("aria-expanded", String(open));
  if (open) settings.querySelector("input:checked")?.focus();
}
settingsButton.addEventListener("click", () => openSettings(settings.hidden));
document.addEventListener("click", (event) => {
  if (!settings.hidden && !event.target.closest(".chrome")) openSettings(false);
});
document.addEventListener("keydown", (event) => {
  if (event.key === "Escape" && !settings.hidden) {
    event.preventDefault();
    openSettings(false);
    settingsButton.focus();
  }
});
let tapped = null;

const svgNamespace = "http://www.w3.org/2000/svg";

// A node and its children; an svg and everything in it in SVG's namespace.
function build(node, svg = false) {
  if ("text" in node) return document.createTextNode(node.text);
  const inSvg = svg || node.tag === "svg";
  const element = inSvg ? document.createElementNS(svgNamespace, node.tag) : document.createElement(node.tag);
  for (const [name, value] of node.props) element.setAttribute(name, value);
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

// A life total that changed floats its difference up from the total.
function floatChanges(before) {
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
  unkey();
  if (tapped !== null) {
    document.getElementById("response").textContent = `last response ${Math.round(performance.now() - tapped)} ms`;
    tapped = null;
  }
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
  floatChanges(before);
  announce();
  rekey();
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

// While the game goes on without the seat (Parun.Web.Question.watching), the
// dock under the hand keeps the height it had, so the hand and board above it
// hold still through a pass of many steps; the next question sizes it afresh.
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
    // An auto-pass level tapped is kept for the next game.
    const chosen = type === "click" ? event.target.closest("[data-level]") : null;
    if (chosen) localStorage.setItem("parun-level", chosen.dataset.level);
    // A button that asks first (data-confirm) sends nothing unless confirmed.
    const asking = type === "click" ? event.target.closest("[data-confirm]") : null;
    if (asking && !confirm(asking.dataset.confirm)) return;
    // A tap shows the page busy until the engine next patches it.
    if (type === "click" && event.target.closest("button")) {
      document.body.classList.add("busy");
      tapped = performance.now();
      traced("page: tap");
    }
    engine.postMessage({ type, path, value: String(event.target.value ?? "") });
    // A typed line is sent once; the box is ready for the next. A filter
    // (an "input" handler) keeps what is typed.
    if (type === "change" && event.target.dataset.clear !== undefined) event.target.value = "";
  });
}

// A pointer resting on a card (a mouse, not a finger) previews its sheet
// after a moment, and leaving the card ends the preview (Parun.Web.Card's
// "peek" and "unpeek").
const hovering = matchMedia("(hover: hover) and (pointer: fine)");
let resting = null;
let peeking = null;
root.addEventListener("pointerover", (event) => {
  const card = event.target.closest?.(".card, .stack-item");
  if (!hovering.matches || !card || card === resting) return;
  resting = card;
  setTimeout(() => {
    if (resting !== card || !card.isConnected || root.querySelector(".sheet-backdrop:not(.peek)")) return;
    const path = pathOf(card);
    if (path === null) return;
    peeking = card;
    engine.postMessage({ type: "peek", path, value: "" });
  }, 450);
});
root.addEventListener("pointerout", (event) => {
  const card = event.target.closest?.(".card, .stack-item");
  if (!card || card.contains(event.relatedTarget)) return;
  if (resting === card) resting = null;
  if (peeking === card) {
    peeking = null;
    const path = pathOf(card);
    if (path !== null) engine.postMessage({ type: "unpeek", path, value: "" });
  }
});

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
// Marks a key page.js gave, so unkey takes back only those.
const given = "data-key-given";
let returnTo = null;

function openSheet() {
  return root.querySelector(".sheet-backdrop:not(.peek) .sheet");
}

function mainButton() {
  return root.querySelector(".prompt button.primary:not(:disabled), .handoff button.primary");
}

// Takes back the keys rekey gave, leaving the page as the engine built it,
// which its patches are paths into.
function unkey() {
  for (const button of root.querySelectorAll(`[${given}]`)) {
    button.removeAttribute(given);
    button.removeAttribute("aria-keyshortcuts");
  }
}

// Gives the main button Enter, then numbers 1 to 9 and 0 the buttons an
// open card sheet offers, or else the prompt's: in the order shown, but the
// main button, any with a key of the view's, the news's card links, the
// damage rows' amounts and the auto-pass foot. A prompt's numbers follow its own buttons, so the same
// kind of prompt numbers alike; a meaning that recurs has its letter.
function rekey() {
  const give = (button, key) => {
    button.setAttribute("aria-keyshortcuts", key);
    button.setAttribute(given, "");
  };
  const sheet = openSheet();
  const main = sheet ? null : mainButton();
  if (main && !main.hasAttribute("aria-keyshortcuts")) give(main, "Enter");
  const candidates = sheet ? sheet.querySelectorAll(".sheet-actions button") : root.querySelectorAll(".prompt button");
  const numbered = Array.from(candidates).filter((button) => button !== main && !button.disabled && !button.hasAttribute("aria-keyshortcuts") && !button.closest(".levels, .news, .segmented"));
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
  if (event.defaultPrevented || event.metaKey || event.ctrlKey || event.altKey) return;
  if (!settings.hidden || shortcuts.open) return;
  const sheet = openSheet();
  if (event.key === "Escape") {
    root.querySelector(".sheet button.close")?.click();
    return;
  }
  // Tab stays within an open card sheet, as within any dialog.
  if (event.key === "Tab" && sheet) {
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
  if (event.key === "Enter" || event.key === " ") {
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
  if (event.key === "?") {
    event.preventDefault();
    shortcuts.showModal();
    return;
  }
  if (sheet === null && event.key in arrows) {
    const next = nearest(target, ...arrows[event.key]);
    if (next) {
      event.preventDefault();
      next.focus();
    }
    return;
  }
  if (sheet === null && event.key.toUpperCase() === "L") {
    const log = root.querySelector("details.log");
    if (log) log.open = !log.open;
    return;
  }
  if (event.key.length !== 1) return;
  const key = event.key.toUpperCase();
  const scope = sheet ?? root.querySelector(".prompt");
  const button = scope && Array.from(scope.querySelectorAll("button[aria-keyshortcuts]")).find((b) => !b.disabled && b.getAttribute("aria-keyshortcuts") === key);
  if (button) {
    event.preventDefault();
    button.click();
  }
});

engine.onmessage = ({ data }) => {
  if ("patches" in data) {
    const began = performance.now();
    apply(data.patches);
    traced(`page: applied ${data.patches.length} bytes in ${Math.round(performance.now() - began)} ms`);
  } else if ("phase" in data) note(data.phase);
  else if ("trace" in data) traced(data.trace);
};
engine.onerror = (error) => note(`engine failed: ${error.message}`);
