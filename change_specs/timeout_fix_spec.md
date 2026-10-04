# Web Request Timeout Fix Specification

Status: implemented (194 tests green). TDD: tests first, mainline code to
green. Implementation notes beyond the plan:

- `waitForImage` (route-side bounded poll) recovers from resolver-ask
  failures by treating them as "give up for now": the route answers
  `503` + `Retry-After`, and the client's bounded retry loop takes over.
- The client's Brickset probe uses `fetch(url, { redirect: 'manual' })`:
  an `opaqueredirect` response type unambiguously means the endpoint
  answered `302` (resolved), so the image is then displayed by setting
  `img.src` directly — which follows the redirect without CORS limits.
- The LDraw zip bytes are read from the zip stream
  (`stream.readAllBytes()`); the previous `getImageFromZip` read the
  entry name from the filesystem instead, so the `/part_images` route
  could never serve a real image. Fixed with the lazy-image work.
- `getImageFromZip`/`isZipAvailable`/`canRetryDownload` joined the
  `LDrawImageFetcherTrait` seam so the resolver is testable.
- Negative-marker expiry is a read-time TTL, not a background sweep: the
  original claim that DiskCache entries "expire with the existing 22.5h
  staleness threshold" was wrong (that threshold lives inside
  `CachedDownloader`'s page-cache flow, which the resolver bypasses). The
  resolver now treats a negative marker older than `image-negative-ttl` as
  absent, so an unresolvable part is retried at most once per TTL window —
  no scheduled re-check needed, since the retry is on demand and its page
  fetch is disk-cached.
- Regression fix (found in production use): `TaxonomyData.findBasePart`'s
  synthesized "(modified)" identity copied the base part's
  `imageUrl`/`imageWidth`/`imageHeight`, so printed parts (e.g.
  `98138pr0035`) rendered the base unprinted part's taxonomy image inline
  and never fell through to the lazy chain. The old eager path masked this
  by overwriting `imageUrl` for every Modified result; the lazy renderer's
  taxonomy fast path exposed it. The synthesis now clears the image fields,
  so Modified identities resolve their own image via LDraw
  (`part_images/<colorId>/<full partNumber>.png`) then Brickset (element
  query first, part-number fallback) — exactly the old resolution order.
- Regression fix (found in production use): the old render attached
  resolved images with no explicit dimensions, so they were bounded by the
  shared `max-width: ${maxTaxonomyWidth}px` rule (default 100px). The lazy
  `<img>` was rendered with no size bound, so images loaded by the client
  rendered at natural size and warped the table columns. The lazy element
  now carries the same `style="max-width: ${maxTaxonomyWidth}px"` — the
  bound is baked in at render time and survives the JS `src` assignment.

## Goals

1. Web requests (set number and upload) always receive a response from the
   application, never from Akka HTTP's generic `RequestTimeout` page.
2. Every deadline is ordered: route deadlines exceed the backend work they
   cover, and `akka.http.server.request-timeout` exceeds the route deadlines.
3. No request path ever blocks an actor thread: the LDraw zip download (up to
   5 minutes of `Await` + retry sleeps) moves off the `PartsProcessor`
   dispatcher entirely.
4. Images resolve lazily: the page answers as soon as matching and
   categorization are done; images arrive afterward (LDraw first, Brickset
   for what LDraw lacks), with pending vs known-unavailable as distinct
   states.

## Problem (verified)

Deadline inversion on the web paths:

| Layer | Set-request path | Upload path |
|---|---|---|
| Akka HTTP request-timeout (idle; default, unset in `application.conf`) | **40s** | 40s |
| Outer ask deadline | 30s + 30s sequential = **60s** (`HttpServer.scala:161`, `:209`) | **10s** (`:94`) |
| Backend: `LookupParts` ask | 5s (`PartsProcessor.scala:36`) | 5s |
| Backend: `GetData` ask for Modified parts | 30s (`PartsProcessor.scala:101`) | 30s |
| Backend: LDraw zip download, per color, **blocking on the actor thread** | up to **5 min** + retry sleeps (`LDrawImageFetcher.scala:22,42,72` via `PartsProcessor.scala:137,197`) | same |
| Backend: Brickset UX bound / background ask | 5s / 90s (`PartsProcessor.scala:143,142`) | same |

- Set requests: the chain can need 65s+ while the server stops listening at
  40s — that produces Akka HTTP's `RequestTimeout` (503, "not able to produce
  a timely response").
- Uploads: the route dies at 10s while the backend runs on for minutes; the
  orphaned work is not cancelled, so repeated failures compound the load.
- One blocking `ensureDownloaded` call stalls every concurrent
  `ProcessParts`, because it runs synchronously on the actor's dispatcher
  thread.

Findings that shaped the design (both verified in code):

- `GetData` replies are **by reference, O(1)** — single in-process
  `ActorSystem`, no remoting, so the repeated snapshot asks are cheap. Not a
  performance problem; only minor cross-snapshot consistency if a rebrickable
  reload lands mid-request. No change planned for it.
- `RebrickableHolder` handlers are O(1) (`data.copy`, derived maps are
  `lazy val`), so `GetData` never queues behind meaningful work.
- `PartsProcessor` is **web-only**: batch mode (`processInventories`) calls
  `TaxonomyHolder.LookupParts` directly and writes CSVs without an image
  column (`TaxonomySortMain.scala:107-131`). Removing its eager image path
  costs batch mode nothing.
- `RebrickableSchedulerActor` already prefetches every color's LDraw zip in
  the background after each daily fetch (`RebrickableSchedulerActor.scala:62-67`),
  so lazy `/part_images` hits usually find a warm zip.

## Target budget

```
akka.http.server.request-timeout   = 60s   # > any route deadline; app always answers first
lego-taxonomy.web.route-request    = 30s   # total per request, shared deadline
lego-taxonomy.web.data-ask         = 10s   # GetData step
lego-taxonomy.web.process-ask      = 20s   # ProcessParts step (10 + 20 = 30)
lego-taxonomy.web.image-wait       = 10s   # how long an image request waits on in-flight work
lego-taxonomy.web.image-retry-after = 2s   # Retry-After sent with 503
lego-taxonomy.web.image-negative-ttl = 24h # Brickset negative-marker expiry
```

## Requirements

- R1: the application answers every web request within 30s (shared
  `GetData` → `ProcessParts` deadline), and `akka.http.server.request-timeout`
  (60s) strictly exceeds it.
- R2: a blown budget renders the application's own message, never Akka
  HTTP's generic page.
- R3: no request path blocks an actor thread; `ensureDownloaded` runs on a
  dedicated blocking dispatcher.
- R4: image resolution is lazy — LDraw via `part_images`, Brickset via a
  redirect endpoint — and "pending" is distinguishable from
  "known unavailable" both in protocol and over HTTP.
- R5: negative results (a part Brickset cannot resolve) are durably cached
  so page loads do not refetch them; a negative marker expires after
  `image-negative-ttl` (default 24h), checked at read time, so a part
  Brickset adds later is re-resolved on the first request after the TTL
  lapses — at most one resolve attempt per part per TTL window.
- R6: single-flight: N concurrent requests for the same color zip or the
  same part's Brickset resolve cause exactly one download.
- R7: parts with a taxonomy image (parsed from brickarchitect HTML,
  `TaxonomyParser.scala:132-137`) keep rendering exactly as today.

## Design

### 1. Single source of truth for budgets

New `Timeouts.scala` reading `application.conf` (which gains
`akka.http.server.request-timeout = 60s` and the `lego-taxonomy.web.*`
keys above). Budgets stop being literals scattered across actors.

### 2. `HttpServer.scala` — route deadlines and error surface

- One shared 30s deadline across the sequential `GetData` → `ProcessParts`
  chain (replaces 30s + 30s on set requests and 10s on uploads).
- `onSuccess` → `onComplete` with explicit failure mapping: a blown budget
  renders our own "still processing" message.
- `withRequestTimeout(60s)` on the POST route as a backstop alongside the
  config value.

### 3. `PartsProcessor.scala` — delete the eager image path

- Drops the `downloader` / `ldrawImageFetcher` params, the now-unused
  `rebrickableDataActor` (its only use was color name → color ID for
  images), `findPartImageUrl`, the 30s data ask, the 90s Brickset ask, and
  the blocking zip download call.
- The backend chain collapses from 5s + 30s + up-to-5-min + 5s to **just the
  5s lookup ask**, so 20s is comfortable headroom on both request paths.
- Batch mode is untouched (it never used `PartsProcessor`).

### 4. New `ImageResolver` actor — three-state image protocol

Owns per-color and per-part image state, single-flight in each:

| Query | States and answers |
|---|---|
| `GetLdrawImage(colorId, partNumber)` | zip warm + entry present → `Ready(bytes)`; zip warm + entry absent → `Unavailable` (definitive 404); zip cold → start one background `ensureDownloaded(colorId)` → `Pending`; download exhausted retries → `Unavailable` |
| `GetBricksetImage(partNumber)` | resolved URL → `Resolved(url)` (302); nothing found → `Unavailable` (404); not yet resolved → `Pending` (503 + `Retry-After`) |

- **Pending is a wait, not an error:** the route holds the image request up
  to `image-wait` (10s) for in-flight work. Image requests are not the page
  request, so waiting is invisible to the user.
- **Negative caching is durable for Brickset, with read-time expiry:**
  positive `brickset-image/<pn>` and negative `brickset-image-missing/<pn>`
  entries in `DiskCache`. The resolver compares the entry's `insertedAt`
  against `image-negative-ttl` (default 24h): a lapsed negative marker is
  treated as absent and the part is re-resolved on the next request. The
  re-fetch is cheap — the Brickset page itself goes back through
  `CachedDownloader`'s 22.5h cache. Positive markers are permanent: Brickset
  CDN image URLs are stable, so resolved parts are not re-fetched.
- **LDraw has no cross-day negative state:** every request re-checks the
  part against the current zip, zips carry a 22.5h freshness window, the
  3am scheduler re-downloads every color's zip, and per-zip retry counters
  reset daily — so new LDraw data appears within a day without any
  resolver-side expiry logic.
- **LDraw negative state reuses what exists:**
  `RebrickableBinaryCache.canRetry` / `recordRetry` already track per-zip
  retry exhaustion; no new durable state is needed for zips.
- **Nothing blocks the resolver:** `ensureDownloaded` (`Await.result(...,
  5.minutes)` plus `Thread.sleep` backoff) runs inside `Future { ... }` on a
  dedicated blocking dispatcher.

### 5. `HttpServer.scala` — image routes and rendering

- `GET /part_images/<colorId>/<partNumber>.png` → ask the resolver with the
  10s wait budget; `200` bytes, `404` (definitive), or `503` +
  `Retry-After: 2`.
- `GET /part_images/brickset/<partNumber>` → `302` to the discovered CDN
  URL, `404`, or `503` + `Retry-After: 2`.
- `partsSorterHtml` keeps the taxonomy image fast path untouched (R7): rows
  whose `legoPart.imageUrl` is set render exactly as today with width/height.
  Only rows **without** a taxonomy image get the lazy treatment, via
  `data-image-ldraw` / `data-image-brickset` attributes — precisely the rows
  that previously needed LDraw/Brickset enrichment.
- `start(...)` takes the resolver plus the `CachedDownloader` / `DiskCache`
  refs, all already in scope in `runWebMode` (`TaxonomySortMain.scala:37-38`).

### 6. `parts-sorter.js` — fetch-based loader with bounded retry

`<img onerror>` cannot read a status code, so the retry loop moves into JS
(~15 lines): `fetch` the LDraw URL → `200` sets `img.src` from the blob;
`503` schedules a retry honoring `Retry-After`; `404` falls through to the
Brickset URL with the same rules; `404` there hides the cell. Attempts
capped (≈5 / ≈30s per image). Safe with the column-reorder code, which only
moves `th`s and never rebuilds images.

### 7. Batch mode

No change: batch never used `PartsProcessor` or image resolution; output
CSVs keep their existing columns.

## Set-request path, before → after

| | Before | After |
|---|---|---|
| Worst-case route work | 30 + 30 = 60s, plus up to 5 min of blocking image work inside | ≤ 30s shared deadline |
| Who answers first | Akka HTTP at 40s, generic 503 | the app at ≤30s, friendly 503 |
| Backend work per request | lookup + rebrickable data ask + zip download + Brickset | lookup only |
| Images | downloaded inline (blocking the processor actor) | lazy `part_images`, Brickset via background resolve, pending vs unavailable distinguished |

## Accepted trade-offs

- A part whose color zip is not yet prefetched, or that LDraw lacks and
  Brickset cannot resolve, renders with an empty image cell instead of
  holding up the page.
- An image still pending after the 10s wait + client retry cap appears only
  on the next page load (no unbounded polling).

## Test plan (TDD ordering)

1. `TimeoutsSpec` (new): invariant
   `server > route ≥ dataAsk + processAsk` — regression guard against the
   inversion that caused this.
2. `HttpServerSpec`:
   - set request against a silent processor → our 503 inside the budget
     (small overridden timeouts in test config);
   - the existing "no set found" recovery still works;
   - `404` vs `503` vs `302` distinctions on both image routes;
   - `data-image-*` attributes present only when no taxonomy image exists.
3. `ImageResolverSpec` (new): `Pending` → `Resolved` and `Pending` →
   `Unavailable` transitions; single-flight (N concurrent queries → one
   download); negative marker survives a resolver restart; a blocking
   `ensureDownloaded` stub still lets a second query be served (off-thread
   proof).
4. `PartsProcessorSpec`: updated constructor; asserts `ProcessParts` never
   messages a downloader.

## Out of scope

- Rebrickable fetch pipeline and `RebrickableHolder` internals (snapshot
  asks stay by-reference; no change).
- Ordering semantics, CSV formats, taxonomy fetch/scheduler (covered by the
  previous change).
- No attempt to cancel orphaned page-request work at the route layer; the
  lazy-image design removes the long-running work from that path instead.
