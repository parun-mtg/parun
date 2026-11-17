// Loads the web client's wasm module and runs its app in #app: applies the
// patches Haskell sends and reports each event's path back (Parun.Web.Wire).
// The view's root is #app's only child, and paths count from it. Parun's data
// directory is data.tar.gz, unpacked into memory and mounted at /data.
import { ConsoleStdout, Directory, File, OpenDirectory, OpenFile, PreopenDirectory, WASI, wasi as defs } from "./browser_wasi_shim/index.js";
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

let started = false;
let tapped = null;
globalThis.parunApply = (json) => {
  document.body.classList.remove("busy");
  if (tapped !== null) {
    document.getElementById("response").textContent = `last response ${Math.round(performance.now() - tapped)} ms`;
    tapped = null;
  }
  if (!started) {
    started = true;
    phase("started");
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
      if (path === null) return;
      const value = String(event.target.value ?? "");
      // A tap shows the page busy until Haskell next patches it, painted
      // before the engine runs: the dispatch waits a frame.
      if (type === "click" && event.target.closest("button")) {
        document.body.classList.add("busy");
        tapped = performance.now();
      }
      requestAnimationFrame(() => setTimeout(() => dispatch(type, path, value), 0));
      // A typed line is sent once; the box is ready for the next.
      if (type === "change") event.target.value = "";
    });
  }
};

// A ustar archive's files as nested directories: a Map of name to File or Map.
function untar(bytes) {
  const tree = new Map();
  const text = (start, length) => new TextDecoder().decode(bytes.subarray(start, start + length)).replace(/\0.*$/s, "");
  for (let at = 0; at + 512 <= bytes.length && bytes[at] !== 0; ) {
    const prefix = text(at + 345, 155);
    const path = (prefix ? `${prefix}/` : "") + text(at, 100);
    const size = parseInt(text(at + 124, 12), 8) || 0;
    const type = text(at + 156, 1);
    const names = path.split("/").filter((name) => name !== "");
    let parent = tree;
    for (const name of names.slice(0, -1)) {
      if (!parent.has(name)) parent.set(name, new Map());
      parent = parent.get(name);
    }
    const last = names[names.length - 1];
    if (type === "5") {
      if (!parent.has(last)) parent.set(last, new Map());
    } else if (type === "0" || type === "") {
      parent.set(last, new File(bytes.slice(at + 512, at + 512 + size), { readonly: true }));
    }
    at += 512 + Math.ceil(size / 512) * 512;
  }
  return tree;
}

// The shim copies a directory's whole listing to read each entry, quadratic
// over the cards directory; the mounted data never changes, so each opened
// directory copies it once.
const readdirSingle = OpenDirectory.prototype.fd_readdir_single;
OpenDirectory.prototype.fd_readdir_single = function (cookie) {
  if (cookie < 2n) return readdirSingle.call(this, cookie);
  this.listing ??= Array.from(this.dir.contents.entries());
  if (cookie >= BigInt(this.listing.length) + 2n) return { ret: 0, dirent: null };
  const [name, entry] = this.listing[Number(cookie - 2n)];
  return { ret: 0, dirent: new defs.Dirent(cookie + 1n, entry.ino, name, entry.stat().filetype) };
};

function inodes(tree) {
  return new Map(Array.from(tree, ([name, node]) => [name, node instanceof Map ? new Directory(inodes(node)) : node]));
}

// Each phase's time goes to the console; with ?profile, so does the time
// spent in each WASI import.
let mark = performance.now();
// The phases and the last tap's response time also show at the foot of the
// page, readable with Web Inspector closed (which slows Safari's wasm).
const timings = document.getElementById("timings");
function phase(name) {
  const now = performance.now();
  const line = `${name} in ${Math.round(now - mark)} ms`;
  console.log(line);
  timings.textContent += (timings.textContent ? "; " : "") + line;
  mark = now;
}

const archive = await fetch("data.tar.gz");
const data = new Uint8Array(await new Response(archive.body.pipeThrough(new DecompressionStream("gzip"))).arrayBuffer());
phase(`fetched ${data.length} bytes`);
const mounted = new PreopenDirectory("/data", inodes(untar(data)));
phase("unpacked");

const wasi = new WASI([], ["parun_datadir=/data"], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(line)),
  ConsoleStdout.lineBuffered((line) => console.warn(line)),
  mounted,
], { debug: false });
const calls = new Map();
if (new URLSearchParams(location.search).has("profile")) {
  for (const [name, call] of Object.entries(wasi.wasiImport)) {
    wasi.wasiImport[name] = (...args) => {
      const start = performance.now();
      try {
        return call(...args);
      } finally {
        const tally = calls.get(name) ?? { count: 0, ms: 0 };
        calls.set(name, { count: tally.count + 1, ms: tally.ms + performance.now() - start });
      }
    };
  }
}
const exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("parun.wasm"), {
  ghc_wasm_jsffi: jsffi(exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(exports, instance.exports);
phase("compiled");
wasi.initialize(instance);
// The game runs inside start, waiting on the page between answers.
instance.exports.start();
await new Promise((resolve) => setTimeout(resolve, 0));
for (const [name, tally] of [...calls].sort((a, b) => b[1].ms - a[1].ms)) {
  console.log(`${name}: ${tally.count} calls, ${Math.round(tally.ms)} ms`);
}
