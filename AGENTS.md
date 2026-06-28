# `logpile` — developer guide for agents

`logpile` is a **content-addressed cache for expensive, deterministic simulations**. It
guarantees identical simulation inputs are never run twice, and was built for
simulation-based calibration of the [`plant`](https://github.com/traitecoevo/plant) forest
model. It is an R package (Imports `arrow`, `dplyr`, `fs`; `Remotes: traitecoevo/plant@develop`).

Key properties:
- **Content-addressed** — a run is identified by the SHA-256 hash of its inputs.
- **Fault-tolerant** — crashes/timeouts are recorded as results, so broken parameter sets
  aren't retried endlessly.
- **Resumable** — parallel campaigns resume from their last completed state (`crew` for
  parallel execution).

## Core concepts

- A **pile** is a cache directory (`create_pile()` / `set_active_pile()`), backed by Arrow/parquet.
- `resolve_request()` turns a model id + fixed inputs into a content-addressed request.
- `predicate_set()` defines ecological predicates that decide which parameter sets to keep.
- See the README quickstart for the full campaign loop.

## Layout & workflow

- `R/` — package code; `tests/` — `testthat`. Plain R package: use
  `devtools::load_all()`, `devtools::test()`, `devtools::document()`, `R CMD check`.
- Requires R ≥ 4.1, a working Apache Arrow build, and `plant`.

## Gotchas

- The cache keys on the **hash of inputs**. If a `plant` change alters simulation *outputs*
  for the *same inputs* (a behavioural change, not a refactor), cached results become stale
  silently — the hash won't notice. Invalidate/namespace affected piles deliberately.

## Plant family

`logpile` is part of the **plant family** in the [`traitecoevo`](https://github.com/traitecoevo)
org — a hub-and-spoke set of packages built around the
[`plant`](https://github.com/traitecoevo/plant) size- and trait-structured forest model.

- **Docs hub** — family user guides & theory: <https://traitecoevo.github.io/overstorey/>
- **Cross-package orientation** — how the family fits together (who depends on whom,
  source-of-truth rules, cross-repo gotchas) lives in
  [`plant-meta`](https://github.com/traitecoevo/plant-meta); start with its
  [`AGENTS.md`](https://github.com/traitecoevo/plant-meta/blob/main/AGENTS.md). Keep
  family-wide concerns there, not here.
- **Issues & board** — follow the
  [issue guide](https://github.com/traitecoevo/plant-meta/blob/main/governance/issue-guide.md);
  work is tracked on [board #5](https://github.com/orgs/traitecoevo/projects/5) (new issues
  auto-add with no Status = the triage queue). Labels: `bug` / `task` / `epic` plus `blocked`,
  `needs-info`, `cross-package`, `breaking`, `question`.
