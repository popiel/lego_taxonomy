# Taxonomy Part Service Specification

Status: approved. Implements in two stages (A then B), TDD: tests first, mainline code to green.

## Goals

1. The sorter accesses taxonomy parts on a per-part basis through a service API
   (batched transport: one message per upload).
2. As much of the part-miss logic as possible is folded into the service:
   exact/alt-number lookup, element-ID→design-ID resolution, base-part
   fallback, and fuzzy name inference.
3. The fetcher publishes the unaugmented bulk taxonomy as soon as the category
   phase completes; augmentation streams per part afterward.
4. Problems with augmentation never make category data inaccessible.
5. Batch mode resolves parts through the same service.
6. What stays in the sorter (request-scoped): upload-local prefix matching and
   image resolution (LDraw/Brickset).

## Requirements

- R1: The taxonomy is only unavailable/replaced when the bulk category phase
  fails; the previous snapshot keeps serving (bulk is atomic).
- R2: An augmentation failure never removes or invalidates taxonomy data; the
  worst case is that some parts lack alt numbers.
- R3: One upload's lookups resolve against one merged view (a batched lookup
  message triggers at most one overlay merge).
- R4: `AugmentPart` is O(1); it records into a pending overlay merged lazily
  on the next read.
- R5: During augmentation lag, an alt-number miss degrades to a Modified or
   Guessed match (leading-digit strip), not the unmatched tail.

## Stage A — bulk publish + incremental augmentation

### TaxonomyHolder (`TaxonomyHolder.scala`)

- New command `AugmentPart(partNumber: String, altNumbers: Set[String])`:
  applies O(1) into a pending overlay (`Map[String, Set[String]]`); the overlay
  is merged into the stored `TaxonomyData` once on the next read
  (`GetTaxonomy`/`LookupParts`). Augmentations for unknown parts or when no
  taxonomy is stored are logged and ignored.
- `SetTaxonomy` unchanged in effect (writes `categories.csv`/`parts.csv`,
  replaces the stored taxonomy) and clears the overlay.
- `GetTaxonomy` retained; replies with the merged view.

### TaxonomyFetcher (`TaxonomyFetcher.scala`)

- New command `RegisterHolder(ref: ActorRef[TaxonomyHolder.Command])`; the
  fetcher holds it as behavior state (accepted in every phase).
- Single-response protocol: the only responses to `GetTaxonomy` are
  `AugmentationComplete` (signal) and `Failed` — exactly one response per ask
  (Akka's ask adapter is one-shot, so no intermediate response is sent; the
  `TaxonomyFetched` response is removed). Bulk publish is observable via the
  registered holder receiving `SetTaxonomy` and the fetcher's own logging.
- In `collecting`, when the category phase drains (`pendingFetches == 0`,
  TaxonomyFetcher.scala:77-90):
  1. registered holder `! SetTaxonomy(bulkData)` — the unaugmented taxonomy
     becomes readable immediately;
  2. log the bulk publish;
  3. enter `enhanceParts` with the sliding window of 20 as today.
- In `enhanceParts` on a part-page success: parse alt numbers, send
  `AugmentPart(partNumber, altNumbers)` to the registered holder; drop the
  O(n) `allParts` rewrite (TaxonomyFetcher.scala:118-124). The final
  `AugmentationComplete` no longer carries data.
- In `enhanceParts` on a failure (`CachedDownloader.Failed`/`AskFailure`,
  TaxonomyFetcher.scala:161-169): log warn, decrement `pendingFetches`, refill
  the window, skip the part. **Never reply `Failed` from the augmentation
  phase.**
- Completion (`newPendingWithNext == 0 && remaining.isEmpty`): reply
  `AugmentationComplete` (signal, no data) to `replyTo`.
- `collecting` failures still reply `Failed` (bulk did not complete; the holder
  keeps serving the previous taxonomy) — R1/R2.
- Test seam: `apply(downloader: ActorRef[CachedDownloader.Command], cache:
  ActorRef[DiskCache.Command])` overload so the state machine can be driven
  with a stub downloader serving fixture HTML; the production `apply()`
  spawns its own children as today.

### TaxonomyScheduler (`TaxonomyScheduler.scala`)

- The ask completes on `AugmentationComplete` or `Failed` → log + schedule the
  next 3am fetch (the fetcher publishes `SetTaxonomy` itself, so the scheduler
  no longer forwards it — this removes the SetTaxonomy-vs-AugmentPart ordering
  race). If the fetcher is busy mid-cycle the ask times out (30 min) → log +
  schedule next, preserving the daily chain as today.

## Stage B — per-part lookup service

### TaxonomyHolder (part service)

- Constructor: `TaxonomyHolder(rebrickableRef: ActorRef[RebrickableHolder.Command])`.
- New messages:

  ```scala
  case class LookupPartRequest(partNumber: String, elementId: Option[String], name: String)
  sealed trait LookupVia
  case object Exact extends LookupVia
  case object AltNumber extends LookupVia
  case object Modified extends LookupVia
  case object Guessed extends LookupVia
  case object Miss extends LookupVia
  case class LookupResult(request: LookupPartRequest, legoPart: Option[LegoPart], via: LookupVia, categoriesGuessed: Boolean = false)
  case class LookupParts(requests: List[LookupPartRequest], replyTo: ActorRef[List[LookupResult]])
  ```

- On `LookupParts`: one nested ask to rebrickable `GetData` (short timeout,
  recover → empty `Data`, log warn; resolution continues without design IDs),
  then per request the cascade (response preserves request order):
  1. `findPart(partNumber)` → `Exact` (hit key == part number) or `AltNumber`;
  2. `elementId` → `designId` (`Try(toLong).flatMap(data.elementIdToDesignId)`,
     Rebrickable.scala:72-73), then `findBasePart(partNumber).orElse(
     findBasePart(designId))` → `Modified` (synthesized "... (modified)" part,
     TaxonomyData.scala:54-64);
  3. if no part yet (or the found part has empty categories): `searchByName(name)`
     — tokenized subset match → `Guessed` with the matched part's full
     categories, name "... (guessed)"; else common category prefix of the
     top-5 hits → `Guessed` with the prefix; else `Miss` (wordIndex lives
     inside the holder; the sorter never sees bulk taxonomy data).

### PartsProcessor (`PartsProcessor.scala`)

- Constructor unchanged.
- Flow:
  1. one `LookupParts` ask per upload (timeout ≈ 5s);
  2. all `Exact`/`AltNumber` → build MatchedParts, sort, reply (fast path,
     PartsProcessor.scala:46-48 preserved);
  3. else one `GetData` ask to rebrickable (image logic only now,
     PartsProcessor.scala:74) → image resolution for `Modified` results only
     (existing LDraw→Brickset logic, partNumber override, imageUrl attach,
     PartsProcessor.scala:139-155). `Guessed` results keep their synthesized
     identity (partNumber "", no image) and `Miss` results have no lego part
     to enrich — exactly as today's fuzzy/unmatched output.
  4. upload-local prefix matching for `Miss` results using sibling results
     (existing logic, PartsProcessor.scala:265-285): longest sibling input
     name that the miss's name starts with → inherit its categories,
     "(guessed)";
  5. sort + reply; degrade paths unchanged (`ProcessedParts(Nil)` if the
     lookup ask fails, PartsProcessor.scala:53-55).
- `inferCategoriesByName`/the lookup half of `processSinglePart` move into the
  holder; `createGuessedLegoPart`-style synthesis for upload-local matching
  stays in the sorter.

### TaxonomySortMain wiring (`TaxonomySortMain.scala`)

- `runWebMode`: spawn `rebrickableData` before `taxonomyDataHolder =
  TaxonomyHolder(rebrickableData)`; then `system !
  TaxonomyFetcher.RegisterHolder(taxonomyDataHolder)` before the scheduler
  fires. Scheduler wiring otherwise unchanged.
- `runBatchMode`: spawn `rebrickableData` + `taxonomyHolder`; `system !
  RegisterHolder(taxonomyHolder)`; probe: `AugmentationComplete` → ask the
  holder for data (Stage A: one `GetTaxonomy`; Stage B: `LookupParts` per
  file) and process; `Failed` → error + terminate. The probe no longer writes
  CSVs and no longer consumes taxonomy data from responses (CSVs are written
  by the holder at bulk publish).
- `processInventories(holder, files)` (made public for testing): per `.csv`
  file → read colored parts → one `LookupParts` ask → MatchedPart per result
  (carrying `categoriesGuessed`) → sort → write `-sorted.csv` (header/format
  unchanged, TaxonomySortMain.scala:122-130).

## Behavior changes to expect

- Batch output gains Modified/Guessed matches; golden file
  `Brickset-inventory-21321-1-sorted.csv` may need regeneration.
- During augmentation lag, web/batch lookups return Modified/Guessed instead
  of Miss for most alt-number variants (R5).
- `HttpServerSpec` is unaffected (its fake PartsProcessor stands in above the
  service).

## Test plan (TDD ordering)

Stage A (tests before implementation):
1. `TaxonomyHolderSpec` (new): SetTaxonomy→AugmentPart merge-on-read via
   GetTaxonomy; overlay cleared by SetTaxonomy; AugmentPart ignored with no
   taxonomy / unknown part.
2. `TaxonomyFetcherSpec` (new, uses the test seam + stub downloader serving
   `root.html`, `category-1.html`, `category-2.html`, `part-3069.html`):
   - bulk completion → holder receives `SetTaxonomy` (taxonomy readable
     pre-augmentation; parts unaugmented) before part-page fetches are
     served;
   - part-page success → `AugmentPart` to holder with parsed alt numbers;
   - part-page failure → part skipped, `AugmentationComplete` still arrives,
     taxonomy intact (R2);
   - category-phase failure → `Failed`, holder never touched (R1);
   - unregistered holder → no crash, `AugmentationComplete` still arrives.
3. `TaxonomySchedulerSpec` (new, fetcher+holder probes): `TaxonomyFetched` →
   holder receives nothing; `AugmentationComplete` → handled (schedules
   next); `Failed` → handled.

Stage B (tests before implementation):
4. `TaxonomyHolderSpec` additions (stub rebrickable holder): Exact; AltNumber
   (via AugmentPart overlay); Modified (base strip + "(modified)"; elementId
   → designId path); Guessed (subset match; common prefix); Miss; found-part
   with empty categories falls through to fuzzy; rebrickable ask failure →
   exact/modified still resolve.
5. `PartsProcessorSpec` updates: existing tests keep passing through the
   service; new: Modified match for a patterned input number; upload-local
   prefix match for a Miss with a sibling name prefix.
6. Batch-mode: `processInventories` via a holder serving fixture taxonomy;
   golden `-sorted.csv` regeneration if needed.

## Out of scope

- Ordering semantics, HttpServer routes/UI, JS, Rebrickable fetch pipeline,
  image resolution logic, CSV formats — all unchanged.
- No per-part storage: storage stays a generational snapshot + overlay; the
  per-part shape is the message API only.

## Download-stack protocol (follow-up, implemented)

The ask-based download stack was converted to bi-directional tells with URL
correlation, eliminating timeouts everywhere except the base downloader —
the stacked-equal ask-timeout race, blind re-issues, and dead letters are
removed by construction.

- `Downloader` owns the **only timeout**: a per-request `requestDeadline`
  (default 60s) racing the HTTP response; a hung request replies `Failed`,
  and the late HTTP completion is dropped upstream as a stray.
- `DownloadQueue`: one long-lived `messageAdapter` for base-downloader
  replies, correlated by URL (`outstanding: Map[url, Fetch]`). Duplicate
  `Fetch`es for an in-flight URL share that download's response; stray or
  duplicate replies are logged and ignored. The 429 politeness pause
  remains — nothing upstream races it. The ask-timeout re-enqueue
  (duplicate downloads + mailbox pileup) is deleted.
- `CachedDownloader`: long-lived adapters for queue and disk-cache replies;
  state maps `pending` (URL → waiting replyTos), `foreground` (cache-miss
  fetches), `refreshing` (URL → cached value + If-Modified-Since). Every
  caller is answered exactly once; strays are logged.
- `TaxonomyFetcher`: one long-lived adapter; `AskFailure` plumbing deleted;
  late replies after an aborted cycle arrive in `idle` and are dropped.
- `TaxonomyScheduler`: long-lived adapter instead of a 30-minute ask, plus
  a `cycleTimeout` watchdog (default 35 min) that re-issues the fetch
  request when a cycle never completes — a busy fetcher can no longer
  break the daily chain.
- **Hybrid** at `PartsProcessor.finishModifiedPart`: the Brickset image
  fetch remains an ask (non-actor caller) with a 90s deadline — strictly
  above the base downloader's 60s, so no race is possible — plus a 5s
  user-facing bound (`Future.firstCompletedOf`); when the UX bound wins,
  the download continues in the background and is cached.
- User-facing route deadlines (`HttpServer`) remain: uploads still get a
  bounded answer even if the download stack parks.
- Invariant: exactly one download timeout (base `requestDeadline`);
  everything above it is tell-correlated and never times out.
- Constraint honored: Akka allows only one `messageAdapter` per message
  type per actor — requesting a new one silently discards the previous
  adapter, so adapters must never be used to encapsulate per-request data
  via closure binding. Each actor creates its adapter(s) exactly once in
  `setup` with stateless mapping functions; per-request correlation data
  (caller replyTos, cached values, If-Modified-Since state) lives in actor
  state keyed by the URL carried in every response. Per-request closure
  data that genuinely needs capture uses a one-shot ask instead (the
  holder's `GetData` lookup, the Brickset hybrid).
