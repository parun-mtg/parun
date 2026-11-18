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

// Light or dark: the system's until the button picks one, which is kept.
const themeButton = document.getElementById("theme");
const dark = matchMedia("(prefers-color-scheme: dark)");
function showTheme() {
  const isDark = document.documentElement.dataset.theme === "dark";
  themeButton.textContent = isDark ? "\u2600\uFE0E" : "\u263E\uFE0E";
  themeButton.title = isDark ? "Light mode" : "Dark mode";
}
themeButton.addEventListener("click", () => {
  const next = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
  document.documentElement.dataset.theme = next;
  localStorage.setItem("parun-theme", next);
  showTheme();
});
dark.addEventListener("change", () => {
  if (localStorage.getItem("parun-theme") === null) document.documentElement.dataset.theme = dark.matches ? "dark" : "light";
  showTheme();
});
showTheme();
let tapped = null;

function build(node) {
  if ("text" in node) return document.createTextNode(node.text);
  const element = document.createElement(node.tag);
  for (const [name, value] of node.props) element.setAttribute(name, value);
  for (const child of node.children) element.append(build(child));
  return element;
}

function at(path) {
  let node = root.firstChild;
  for (const index of path) node = node.childNodes[index];
  return node;
}

function apply(json) {
  document.body.classList.remove("busy");
  const hadSheet = root.querySelector(".sheet") !== null;
  if (tapped !== null) {
    document.getElementById("response").textContent = `last response ${Math.round(performance.now() - tapped)} ms`;
    tapped = null;
  }
  for (const patch of JSON.parse(json)) {
    const node = at(patch.path);
    switch (patch.op) {
      case "replace":
        node.replaceWith(build(patch.node));
        break;
      case "set":
        node.setAttribute(patch.name, patch.value);
        break;
      case "remove":
        node.removeAttribute(patch.name);
        break;
      case "append":
        node.append(...patch.nodes.map(build));
        break;
      case "truncate":
        while (node.childNodes.length > patch.count) node.lastChild.remove();
        break;
      case "arrange": {
        const current = Array.from(node.childNodes);
        node.replaceChildren(...patch.sources.map((source) => ("kept" in source ? current[source.kept] : build(source.node))));
        break;
      }
      default:
        throw new Error(`unknown patch ${patch.op}`);
    }
  }
  // A sheet that just opened takes the focus, on its first action.
  const sheet = root.querySelector(".sheet");
  if (sheet && !hadSheet) sheet.querySelector(".sheet-actions button, button.close")?.focus({ preventScroll: true });
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

// The keyboard: Escape closes the card sheet, and Enter or Space outside a
// text box presses the prompt's main button.
document.addEventListener("keydown", (event) => {
  if (event.defaultPrevented || event.metaKey || event.ctrlKey || event.altKey) return;
  if (event.key === "Escape") {
    root.querySelector(".sheet button.close")?.click();
    return;
  }
  const typing = event.target instanceof HTMLInputElement || event.target instanceof HTMLTextAreaElement;
  const onButton = event.target instanceof HTMLButtonElement || event.target instanceof HTMLSummaryElement;
  if ((event.key === "Enter" || event.key === " ") && !typing && !onButton && !root.querySelector(".sheet")) {
    const main = root.querySelector(".prompt button.primary:not(:disabled)");
    if (main) {
      event.preventDefault();
      main.click();
    }
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
