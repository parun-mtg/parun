// The web client's page: applies the patches the engine sends and reports
// each event's path back (Parun.Web.Wire). The engine runs in a Web Worker
// (engine.js), so the page stays responsive while it works. The view's root
// is #app's only child, and paths count from it.
const root = document.getElementById("app");
const events = ["click", "input", "change"];
const engine = new Worker(new URL("./engine.js", import.meta.url), { type: "module" });

// The engine's load phases and the last tap's response time show at the foot
// of the page.
const timings = document.getElementById("timings");
function note(line) {
  console.log(line);
  timings.textContent += (timings.textContent ? "; " : "") + line;
}
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
    // A tap shows the page busy until the engine next patches it.
    if (type === "click" && event.target.closest("button")) {
      document.body.classList.add("busy");
      tapped = performance.now();
    }
    engine.postMessage({ type, path, value: String(event.target.value ?? "") });
    // A typed line is sent once; the box is ready for the next.
    if (type === "change") event.target.value = "";
  });
}

engine.onmessage = ({ data }) => {
  if ("patches" in data) apply(data.patches);
  else if ("phase" in data) note(data.phase);
};
engine.onerror = (error) => note(`engine failed: ${error.message}`);
