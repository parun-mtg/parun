// Loads the web client's wasm module and runs its app in #app: applies the
// patches Haskell sends and reports each event's path back (Parun.Web.Wire).
// The view's root is #app's only child, and paths count from it.
import { ConsoleStdout, File, OpenFile, WASI } from "./browser_wasi_shim/index.js";
import jsffi from "./ghc_wasm_jsffi.js";

const root = document.getElementById("app");
const events = ["click", "input", "change"];

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

globalThis.parunApply = (json) => {
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
};

// The child indices from the view's root to the node, dotted; null outside it.
function pathOf(target) {
  const path = [];
  for (let node = target; node !== root.firstChild; node = node.parentNode) {
    if (node === root || node === null) return null;
    path.unshift(Array.prototype.indexOf.call(node.parentNode.childNodes, node));
  }
  return path.join(".");
}

globalThis.parunListen = (dispatch) => {
  for (const type of events) {
    root.addEventListener(type, (event) => {
      const path = pathOf(event.target);
      if (path !== null) dispatch(type, path);
    });
  }
};

const wasi = new WASI([], [], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(line)),
  ConsoleStdout.lineBuffered((line) => console.warn(line)),
], { debug: false });
const exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("parun.wasm"), {
  ghc_wasm_jsffi: jsffi(exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(exports, instance.exports);
wasi.initialize(instance);
await instance.exports.start();
