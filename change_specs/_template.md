# <Change Title> Specification

Status: draft | approved | implemented (record sbt test and npm test
results when done).

## Goals

1. ...

## Requirements

- R1: ... (each requirement observable and testable)

## Characterization tests (required BEFORE implementation for
behavior-preserving changes)

If any part of this change moves, rewrites, or deletes existing
behavior, list the tests that pin the CURRENT observable output
before implementation starts:

- exact markup when a UI changes - exact strings, including styles
  and attributes (see RenderingGoldenSpec)
- exact message flows when an actor protocol changes (see
  ImageResolverSpec, DownloadQueueSpec)
- exact file output when a writer changes

A change is not behavior-preserving unless its characterization
tests exist, pass against the old code, and still pass against the
new code. Fixtures must carry production-shaped data (image fields,
dimensions, printed part numbers) - image-less fixtures cannot
express image invariants.

- C1: ...

## Design

- ...

## Test plan (TDD ordering)

1. ... (new behavior: red tests first, then implementation)

## Manual visual smoke (required before done for UI changes)

- Criterion: start web mode, submit set 21321, and confirm: sorted
  rows render; taxonomy images are inline and size-bounded; printed
  parts (e.g. 98138pr0035) show the printed image after the lazy
  LDraw-then-Brickset fallback; unmatched parts render an empty
  image cell; no image warps the table columns.
- Command: sbt "runMain com.wolfskeep.TaxonomySortMain -web", open
  http://localhost:37080/parts-sorter
- Both suites green as well: sbt test, npm test.

## Out of scope

- ...
