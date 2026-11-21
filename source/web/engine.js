// The web client's engine, in a Web Worker: loads the wasm module and runs the
// game, sending each view's patches to the page (page.js) and taking the
// page's events back. Parun's data directory is mounted at /data, empty at
// first and filled in memory chunk by chunk as each downloads.
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
// The page's first message holds the decks it saved (page.js), mounted
// before the deck list is read; every other is an event.
let savedDecks;
const savedArrived = new Promise((resolve) => {
  savedDecks = resolve;
});
onmessage = ({ data }) => {
  if ("saved" in data) savedDecks(data.saved);
  else if ("linked" in data) {
    linked.push(JSON.stringify(data.linked));
    globalThis.parunWake?.();
  } else dispatch?.(data.type, data.path, data.value);
};

// The data channels to other browsers live on the page (page.js), since a
// worker has no RTCPeerConnection: Haskell asks for each step through
// parunLink, and what each channel says (Parun.Browser.Linked) waits here,
// a line of JSON each, until Haskell takes it.
let linked = [];
globalThis.parunTakeLinks = () => {
  const taken = linked.join("\n");
  linked = [];
  return taken;
};
globalThis.parunLink = (op) => postMessage({ link: op });

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
// over a large directory, so each opened directory copies it once. A chunk
// merged in later reaches a directory opened afresh; only the root stays
// open, and fetchAll resets its copy.
const readdirSingle = OpenDirectory.prototype.fd_readdir_single;
OpenDirectory.prototype.fd_readdir_single = function (cookie) {
  if (cookie < 2n) return readdirSingle.call(this, cookie);
  this.listing ??= Array.from(this.dir.contents.entries());
  if (cookie >= BigInt(this.listing.length) + 2n) return { ret: 0, dirent: null };
  const [name, entry] = this.listing[Number(cookie - 2n)];
  return { ret: 0, dirent: new defs.Dirent(cookie + 1n, entry.ino, name, entry.stat().filetype) };
};

// A tree from untar merged into a mounted directory: a directory both hold
// is merged, anything else set.
function merge(dir, tree) {
  for (const [name, node] of tree) {
    if (node instanceof Map) {
      let sub = dir.contents.get(name);
      if (!(sub instanceof Directory)) {
        sub = new Directory(new Map());
        sub.parent = dir;
        dir.contents.set(name, sub);
      }
      merge(sub, node);
    } else dir.contents.set(name, node);
  }
}

async function gunzip(url) {
  const archive = await fetch(url);
  if (!archive.ok) throw new Error(`${url}: ${archive.status}`);
  return new Uint8Array(await new Response(archive.body.pipeThrough(new DecompressionStream("gzip"))).arrayBuffer());
}

const query = new URLSearchParams(location.search);
// The engine compiles while the data downloads, and starts on an empty /data.
const engineFile = "./parun.wasm";
const compiling = WebAssembly.compileStreaming(fetch(engineFile));
const mounted = new PreopenDirectory("/data", new Map());

// Saved decks (Parun.Browser.saveDeck) live at decks/saved/<slug>.txt,
// written and deleted here and kept by the page.
function savedTree(decks) {
  const files = new Map(Object.entries(decks).map(([slug, text]) => [`${slug}.txt`, new File(new TextEncoder().encode(text))]));
  return new Map([["decks", new Map([["saved", files]])]]);
}
globalThis.parunSave = (slug, text) => {
  merge(mounted.dir, savedTree({ [slug]: text }));
  postMessage({ save: { slug, text } });
};
globalThis.parunForget = (slug) => {
  mounted.dir.contents.get("decks")?.contents.get("saved")?.contents.delete(`${slug}.txt`);
  postMessage({ forget: slug });
};

// The data's chunks (script/build-web.sh), fetched one at a time in this
// order, each merged into /data and then named to Haskell
// (Parun.Browser.nextInput), a failed one with a leading "!". The scenarios
// are fetched only when Haskell asks for them (parunWant), as a replay does.
const urls = {
  decks: "./decks.tar.gz",
  "cards-0": "./cards-0.tar.gz",
  "cards-1": "./cards-1.tar.gz",
  "cards-2": "./cards-2.tar.gz",
  "cards-3": "./cards-3.tar.gz",
  rules: "./rules.tar.gz",
  scenarios: "./scenarios.tar.gz",
};
const queue = ["decks", "cards-0", "cards-1", "cards-2", "cards-3", "rules"];
const asked = new Set(queue);
let arrivals = [];
globalThis.parunTakeArrivals = () => {
  const taken = arrivals.join("\n");
  arrivals = [];
  return taken;
};
// A chunk Haskell needs now goes to the front of the queue.
let fetching = false;
globalThis.parunWant = (name) => {
  if (!(name in urls)) return;
  if (asked.has(name)) {
    const at = queue.indexOf(name);
    if (at > 0) queue.unshift(...queue.splice(at, 1));
  } else {
    asked.add(name);
    queue.unshift(name);
  }
  if (!fetching) fetchAll();
};
async function fetchAll() {
  fetching = true;
  while (queue.length > 0) {
    const name = queue.shift();
    try {
      merge(mounted.dir, untar(await gunzip(urls[name])));
      if (name === "decks") merge(mounted.dir, savedTree(await savedArrived));
      // The root's listing, if read, is read afresh.
      mounted.listing = undefined;
      phase(name);
      arrivals.push(name);
    } catch (problem) {
      console.warn(problem);
      arrivals.push(`!${name}`);
    }
    globalThis.parunWake?.();
  }
  fetching = false;
}
fetchAll();

// A replay names its scenario, the empty name for the list of them, and may
// name a position (&at=…) and play on from it (&branch).
// The auto-pass level the page kept (page.js) goes in as parun_level, and a
// host's code from a link to join its game as parun_offer. The engine's
// file, which script/build-web.sh names by its hash, is the build another
// page must share to play with this one (Parun.Web.Link).
const passed = ["level", "scenario", "at", "branch", "offer"].filter((key) => query.has(key)).map((key) => `parun_${key}=${query.get(key)}`);
const wasi = new WASI([], ["parun_datadir=/data", `parun_build=${engineFile.slice(2)}`, ...passed], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(line)),
  ConsoleStdout.lineBuffered((line) => console.warn(line)),
  mounted,
], { debug: false });
const exports = {};
const instance = await WebAssembly.instantiate(await compiling, {
  ghc_wasm_jsffi: jsffi(exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(exports, instance.exports);
phase("compiled");
wasi.initialize(instance);
// The game runs inside start, waiting on the page between answers.
// An exception Haskell does not catch rejects start's promise; the page says so.
instance.exports.start().catch((error) => postMessage({ phase: `engine failed: ${error.message}` }));
