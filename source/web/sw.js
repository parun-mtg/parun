// The web client's service worker: keeps the content-addressed files
// (script/build-web.sh names each by a hash of its contents) once fetched,
// so a later visit loads the engine without downloading it again, and asks
// the network first for the page itself, so a new build is picked up.
const store = "parun";
const addressed = /-[0-9a-f]{16}(\.[a-z]+)$/;

self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", (event) => event.waitUntil(self.clients.claim()));

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (event.request.method !== "GET" || url.origin !== location.origin) return;
  event.respondWith(addressed.test(url.pathname) ? kept(event.request, url) : fresh(event.request));
});

// A content-addressed file never changes, so a kept copy is always right;
// keeping a new one drops the older builds' copies of the same file.
async function kept(request, url) {
  const cache = await caches.open(store);
  const hit = await cache.match(request);
  if (hit) return hit;
  const response = await fetch(request);
  if (response.ok) {
    const stem = url.pathname.replace(addressed, "$1");
    for (const old of await cache.keys()) {
      const path = new URL(old.url).pathname;
      if (path !== url.pathname && path.replace(addressed, "$1") === stem) await cache.delete(old);
    }
    await cache.put(request, response.clone());
  }
  return response;
}

// Anything else from the network, a kept copy only when offline.
async function fresh(request) {
  const cache = await caches.open(store);
  try {
    const response = await fetch(request);
    if (response.ok) await cache.put(request, response.clone());
    return response;
  } catch (problem) {
    const hit = await cache.match(request);
    if (hit) return hit;
    throw problem;
  }
}
