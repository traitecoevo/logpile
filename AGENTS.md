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

- The cache keys on the **hash of inputs**, and `model_id` (`"FF16@v1"`) is one of those
  inputs. The scientific version in `model_id` is **not** hand-typed here: `resolve_request()`
  re-derives it from `plant::model_version()`, so a request carries whatever scientific
  version the installed `plant` declares. Bumping a model's scientific version in `plant`
  therefore changes the fingerprint and forces reruns; a software-only `plant` release leaves
  it untouched and cached results are reused. Callers may pass a bare name (`"FF16"`) or a
  full id; a pinned version that disagrees with the installed `plant` is a hard error. The
  version may be compound — e.g. `TF24f@v2.1`, since TF24f is a fast approximation of TF24
  (major = the TF24 version it tracks, minor = its own approximation revision).
- This covers *versioned* scientific changes. A `plant` maintainer who alters simulation
  *outputs* without bumping the model's `scientific_version` still produces silently stale
  hits — plant's drift-guard test catches the default-parameter case, but pure equation
  changes rely on the maintainer bumping the version. `build_hash` (R + plant package
  versions) is recorded per run for provenance but does not drive invalidation.

## Writing in this guide

This file is read in full at the start of every agent session, as current
instructions, by a reader who was not in your session and cannot tell which
parts are out of date. When you change it:

- Write what is true now, in the present tense. If something here turns out to
  be wrong, replace it; the story of how you found out belongs in the PR.
- Give each rule its reason, in plain case. A reason persuades where capitals
  and warning signs do not.
- Point to where a value lives (`DESCRIPTION`, a header, a Makefile target)
  rather than copying the value, and leave out anything a fresh clone cannot
  resolve, such as local paths or notes from one session.
- Write in complete sentences that say what happens and what it causes, using
  names that already exist in the code.

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
- **Commit messages** — the repo squash-merges, so a PR's title and body are copied verbatim into
  permanent history. Keep them short and durable, and put the working detail (measurements,
  alternatives rejected, what you tried first) in the first PR comment instead — see
  [`commit-messages.md`](https://github.com/traitecoevo/plant-meta/blob/main/governance/commit-messages.md).
