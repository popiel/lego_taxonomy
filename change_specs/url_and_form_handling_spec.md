# URL and Form Handling Specification

Status: implemented (HttpServerSpec + RenderingGoldenSpec green; full suite
results recorded in the commit that lands this).

## Goals

1. Submitting a set number puts the set number in the URL query params, so
   the results page is bookmarkable and shareable, and visiting that URL
   immediately looks up the set.
2. The set-number form input is pre-filled with the entered set number for
   visual continuity, including the unknown-set error page.
3. After a file upload, the chosen file name is shown next to the file
   input for continuity.

## Requirements

- R1: POST with a non-empty `setNumber` answers `303 See Other` to
  `/parts-sorter?setNumber=<trimmed, url-encoded>` and performs no lookup
  work.
- R2: GET `/parts-sorter?setNumber=X` runs the same lookup and render
  pipeline as the old POST path (same budgets) and renders the form input
  with `value="X"` (escaped).
- R3: An unknown set via GET renders the same "No set found" page as
  before, with the form still pre-filled.
- R4: An empty or whitespace-only `setNumber` param renders today's
  default page with no lookup and no pre-fill.
- R5: File uploads are unchanged except the response renders a read-only
  "Last uploaded: <name>" line next to the file input. Browsers forbid
  pre-filling `<input type="file">`, so a visible label is the maximum
  possible continuity. No URL param for uploads (a URL cannot auto-load a
  local file).
- R6: A POST carrying both `setNumber` and a file still prefers
  `setNumber`, preserving the previous precedence.
- R7: The GET route is covered by the same `withRequestTimeout` budget as
  the POST route.

## Characterization tests (written before implementation)

- `RenderingGoldenSpec` already pins the exact rendering pipeline markup
  (image cells per lookup via, exact strings). It continues to pin it on
  the new GET path.
- The existing upload rendering tests pin the upload response page; the
  new "Last uploaded" line is additive.
- The POST direct-render behavior is intentionally replaced by the
  redirect (this is the new behavior, not a preservation gap).

## Design

- `Routes.all`: the set-number lookup pipeline moved into a
  `processSetRequest(setNumber)` helper used by the GET route; the POST
  set branch is only `redirect(Uri("/parts-sorter").withQuery(
  Uri.Query("setNumber" -> setNumber.trim)), StatusCodes.SeeOther)`.
  Classic Post-Redirect-Get: works without JavaScript, refresh and back
  re-run a safe GET, no duplicate-POST warning.
- `partsSorterHtml` gained defaulted params `prefillSetNumber` and
  `uploadedFileName`; the set input renders `value="..."` when present,
  and the file input block renders the "Last uploaded" line when present.
  Failure pages carry the pre-fill too.
- `parts-sorter.js`: no change; the browser follows the 303 and the
  Enter/file-change submit flows keep working.

## Test plan (TDD ordering)

1. `HttpServerSpec` (red first):
   - POST set -> 303 + `Location: /parts-sorter?setNumber=21321-1`
   - GET `?setNumber=21321-1` -> 200 results, input `value="21321-1"`
   - GET `?setNumber=999-1` -> "No set found" page, input still pre-filled
   - GET `?setNumber=` -> default page, no value attribute
   - POST upload -> "Last uploaded: test.csv", set input not pre-filled
2. `RenderingGoldenSpec` (red first): POST leg becomes the 303 + Location
   assertion; the exact-markup golden switches to
   `Get("/parts-sorter?setNumber=21321-1")` and pins the pre-filled input,
   so the golden path is the bookmarkable GET path.

## Manual visual smoke (required before done)

- Start web mode (`sbt "runMain com.wolfskeep.TaxonomySortMain -web"`),
  open `http://localhost:37080/parts-sorter?setNumber=21321-1` directly:
  results render, form pre-filled, URL carries the set number.
- Reload the URL, use back/forward: no duplicate-POST warnings.
- Submit a set from the form: the URL updates in the nav bar.
- Submit an unknown set: error page, form keeps the number for editing.
- Upload a CSV: "Last uploaded: <name>" appears next to the file input,
  URL stays `/parts-sorter`.
- Both suites green: `sbt test`, `npm test`.

## Out of scope

- Server-side upload storage with `?upload=<id>` bookmarkable URLs
  (rejected: uploads would be retrievable by anyone with the URL on a
  0.0.0.0-bound service, plus storage lifecycle concerns).
- Client-side GET navigation for set numbers (rejected in favor of the
  no-JavaScript-required PRG redirect).
- Batch mode, image resolution, timeout budgets — unchanged.
