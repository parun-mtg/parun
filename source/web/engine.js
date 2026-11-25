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
// before the deck list is read, and the game in progress it kept, read
// before the engine starts; every other is an event.
let savedDecks;
const savedArrived = new Promise((resolve) => {
  savedDecks = resolve;
});
onmessage = ({ data }) => {
  if ("saved" in data) savedDecks(data);
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

// The game in progress is kept by the page (Parun.Browser.keepGame) to pick
// up after a reload, as parunKeptGame.
globalThis.parunKeepGame = (op) => postMessage({ game: op });

// The screen shown, as its query (Parun.Browser.locate), goes to the page's
// address bar, a new entry of its history if pushed.
globalThis.parunLocate = (search, push) => postMessage({ locate: { search, push } });

// The games this browser finished (Parun.Browser.keepPlayed), each a
// summary line and its scenario file by its key, in IndexedDB rather than
// localStorage: a scenario runs to tens of kilobytes, and the saved decks
// share localStorage's few megabytes. All are read once, before the engine
// starts, and held here.
const playedStore = "played";
const playedDb = new Promise((resolve, reject) => {
  const opening = indexedDB.open("parun", 1);
  opening.onupgradeneeded = () => opening.result.createObjectStore(playedStore);
  opening.onsuccess = () => resolve(opening.result);
  opening.onerror = () => reject(opening.error);
});
const playedRead = playedDb
  .then(
    (db) =>
      new Promise((resolve, reject) => {
        const kept = new Map();
        const walk = db.transaction(playedStore).objectStore(playedStore).openCursor();
        walk.onsuccess = () => {
          const at = walk.result;
          if (!at) resolve(kept);
          else {
            kept.set(at.key, at.value);
            at.continue();
          }
        };
        walk.onerror = () => reject(walk.error);
      }),
  )
  .catch((problem) => {
    console.warn(problem);
    return new Map();
  });
function changePlayed(change) {
  playedDb
    .then((db) => {
      const writing = db.transaction(playedStore, "readwrite");
      writing.onerror = () => console.warn(writing.error);
      change(writing.objectStore(playedStore));
    })
    .catch((problem) => console.warn(problem));
}
let played = new Map();
// Each game's key and summary, a tab between them, a line each.
globalThis.parunPlayed = () => Array.from(played, ([key, game]) => `${key}\t${game.summary}`).join("\n");
globalThis.parunPlayedScenario = (key) => played.get(key)?.scenario ?? "";
globalThis.parunKeepPlayed = (key, summary, scenario) => {
  played.set(key, { summary, scenario });
  changePlayed((store) => store.put({ summary, scenario }, key));
};
globalThis.parunForgetPlayed = (key) => {
  played.delete(key);
  changePlayed((store) => store.delete(key));
};
// Every game, those another tab kept since this one loaded too.
globalThis.parunClearPlayed = () => {
  played.clear();
  changePlayed((store) => store.clear());
};

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
// The page's loading screen counts the cards' chunks as they arrive.
postMessage({ cardChunks: Object.keys(urls).filter((name) => name.startsWith("cards-")).length });
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
      if (name === "decks") merge(mounted.dir, savedTree((await savedArrived).saved));
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

// The page's own query, naming its screen (Parun.Web.Route), goes in as
// parun_query, the auto-pass level the page kept (page.js) as parun_level,
// and a host's code from a link to join its game as parun_offer. The engine's
// file, which script/build-web.sh names by its hash, is the build another
// page must share to play with this one (Parun.Web.Link).
const passed = ["level", "query", "offer"].filter((key) => query.has(key)).map((key) => `parun_${key}=${query.get(key)}`);
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
globalThis.parunKeptGame = (await savedArrived).game ?? "";
played = await playedRead;
// The game runs inside start, waiting on the page between answers.
// An exception Haskell does not catch rejects start's promise; the page says so.
instance.exports.start().catch((error) => postMessage({ phase: `engine failed: ${error.message}` }));
