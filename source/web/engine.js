// The web client's engine, in a Web Worker: loads the wasm module and runs the
// game, sending each view's patches to the page (page.js) and taking the
// page's events back. Parun's data directory is data.tar.gz, unpacked into
// memory and mounted at /data.
import { ConsoleStdout, Directory, File, OpenDirectory, OpenFile, PreopenDirectory, WASI, wasi as defs } from "./browser_wasi_shim/index.js";
import jsffi from "./ghc_wasm_jsffi.js";

// What Haskell calls (Parun.Browser): patches go to the page, and the page's
// events come to the function the engine listens with.
let dispatch = null;
globalThis.parunApply = (json) => {
  if (!started) {
    started = true;
    phase("started");
  }
  postMessage({ patches: json });
};
globalThis.parunListen = (listener) => {
  dispatch = listener;
};
onmessage = ({ data }) => dispatch?.(data.type, data.path, data.value);

// Each load phase's time goes to the page.
let started = false;
let mark = performance.now();
function phase(name) {
  const now = performance.now();
  postMessage({ phase: `${name} in ${Math.round(now - mark)} ms` });
  mark = now;
}

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

const archive = await fetch("./data.tar.gz");
const data = new Uint8Array(await new Response(archive.body.pipeThrough(new DecompressionStream("gzip"))).arrayBuffer());
phase(`fetched ${data.length} bytes`);
const mounted = new PreopenDirectory("/data", inodes(untar(data)));
phase("unpacked");

// ?trace, passed on from the page, has Haskell time each step on standard
// error (Parun.Browser.trace), which goes to the page's trace.
const tracing = new URLSearchParams(location.search).has("trace");
// A rematch's decks (?mine=…&theirs=…) skip the lobby (Parun.Web.Ending).
const query = new URLSearchParams(location.search);
// The auto-pass level the page kept (page.js) goes in as parun_level.
const passed = ["mine", "theirs", "level"].filter((key) => query.has(key)).map((key) => `parun_${key}=${query.get(key)}`);
const wasi = new WASI([], ["parun_datadir=/data", ...(tracing ? ["parun_trace=1"] : []), ...passed], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(line)),
  ConsoleStdout.lineBuffered((line) => (tracing ? postMessage({ trace: line }) : console.warn(line))),
  mounted,
], { debug: false });
const exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("./parun.wasm"), {
  ghc_wasm_jsffi: jsffi(exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(exports, instance.exports);
phase("compiled");
wasi.initialize(instance);
// The game runs inside start, waiting on the page between answers.
// An exception Haskell does not catch rejects start's promise; the page says so.
instance.exports.start().catch((error) => postMessage({ phase: `engine failed: ${error.message}` }));
