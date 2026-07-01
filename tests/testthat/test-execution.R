

# Generating a manifest must be completely deterministic. It should always
# expand to the exact same list of inputs and fingerprints. Additionally, the
# design coordinates must remain bounded within the declared prior support.
test_that("Manifest generation is deterministic and recoverable", {
  pile <- create_pile(tempfile("inv3_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)

  template <- list(model_id = "FF16@v1")
  priors <- list(lma = c(0.05, 0.15))

  # Run 1
  set.seed(42)
  m1 <- manifest(template, priors, n = 10)
  # Run 2
  set.seed(42)
  m2 <- manifest(template, priors, n = 10)

  expect_equal(nrow(m1$coords), 10L)
  expect_equal(nrow(m2$coords), 10L)

  # Fingerprints must be identical across invocations
  h1 <- vapply(seq_len(nrow(m1$coords)), function(i) request_fingerprint(place_coords(m1$template, m1$coords[i, , drop = FALSE])), character(1))
  h2 <- vapply(seq_len(nrow(m2$coords)), function(i) request_fingerprint(place_coords(m2$template, m2$coords[i, , drop = FALSE])), character(1))
  expect_equal(h1, h2)

  # Design coordinates must be perfectly bounded
  coords <- m1$coords[, "lma"]
  expect_true(all(coords >= 0.05 & coords <= 0.15))

  # Design coordinates must be recoverable from the pile after execution
  req1 <- place_coords(m1$template, m1$coords[1, , drop = FALSE])
  fp1 <- request_fingerprint(req1)
  log <- make_mock_log(fp1)
  put_log(pile, fp1, log, list(request = req1, design_coords = list(lma = coords[1])))

  idx <- pile$st$get(fp1, namespace = "index")
  # Extract design parameter value from design_coords
  expect_equal(idx$design_coords$lma, coords[1])
})

# The index must remain uncorrupted even when multiple agents write to the
# pile at the same time. There should be no phantom records, and every file
# referenced by the index must genuinely exist on disk.
test_that("Concurrent writes do not corrupt the pile", {
  skip_if_not_installed("callr")

  pile_path <- tempfile("inv4_")
  pile <- create_pile(pile_path)
  on.exit(unlink(pile_path, recursive = TRUE), add = TRUE)

  # Generate 20 distinct requests using manifest
  template <- list(model_id = "FF16@v1")
  priors <- list(lma = c(0.05, 0.15))
  set.seed(99)
  m <- manifest(template, priors, n = 20)

  # Resolve all requests and their fingerprints up front
  all_reqs <- lapply(seq_len(nrow(m$coords)), function(i) place_coords(m$template, m$coords[i, , drop = FALSE]))
  all_fps <- vapply(all_reqs, request_fingerprint, character(1))

  # Spawn 4 workers that each write a subset, with overlap for dedup contention
  pkg_root <- tryCatch({
    proj_root <- fs::path_abs(getwd())
    while (!file.exists(file.path(proj_root, "DESCRIPTION")) &&
           proj_root != fs::path_dir(proj_root)) {
      proj_root <- fs::path_dir(proj_root)
    }
    if (file.exists(file.path(proj_root, "DESCRIPTION"))) proj_root else NULL
  }, error = function(e) NULL)

  worker_fn <- function(indices, pile_path, pkg_root, reqs, fingerprints) {
    if (!is.null(pkg_root)) {
      pkgload::load_all(pkg_root, export_all = TRUE, attach_testthat = FALSE, quiet = TRUE)
    }
    # Re-define helpers inside the subprocess
    mk_log <- function(fingerprint, n_steps = 20L, n_cohorts = 3L) {
      n <- n_steps * n_cohorts
      tibble::tibble(
        step     = rep(seq_len(n_steps), each = n_cohorts),
        species  = 1L,
        node     = rep(seq_len(n_cohorts + 1L)[-1L], times = n_steps),
        time     = rep(seq(0, 100, length.out = n_steps), each = n_cohorts),
        height   = cumsum(runif(n, 0.01, 0.1)),
        mortality = runif(n, 0, 0.01),
        fecundity = runif(n, 0, 0.001),
        log_density = runif(n, -5, 0)
      )
    }
    pl <- create_pile(pile_path)
    for (i in indices) {
      req <- reqs[[i]]
      fp <- fingerprints[i]
      if (has_log(pl, fp)) next
      log <- mk_log(fp)
      put_log(pl, fp, log, list(request = req))
    }
  }

  # Worker 1: 1:10, Worker 2: 11:20, Worker 3: 1:20 (full overlap), Worker 4: 20:1 (reverse)
  bg_args <- function(idx) list(idx, pile_path, pkg_root, all_reqs, all_fps)
  bg_env <- callr::rcmd_safe_env()
  p1 <- callr::r_bg(worker_fn, args = bg_args(1:10), env = bg_env)
  p2 <- callr::r_bg(worker_fn, args = bg_args(11:20), env = bg_env)
  p3 <- callr::r_bg(worker_fn, args = bg_args(1:20), env = bg_env)
  p4 <- callr::r_bg(worker_fn, args = bg_args(20:1), env = bg_env)

  # Wait for all workers
  p1$wait(timeout = 60000)
  p2$wait(timeout = 60000)
  p3$wait(timeout = 60000)
  p4$wait(timeout = 60000)

  # Re-open the pile from disk (fresh view)
  pile2 <- create_pile(pile_path)

  # Check: every fingerprint in the index
  keys <- pile2$st$list(namespace = "index")
  expect_equal(length(keys), 20L)

  # Check: every file referenced by the index exists on disk
  for (k in keys) {
    rec <- pile2$st$get(k, namespace = "index")
    expect_equal(rec$status, "done")
    full_path <- file.path(pile2$path, rec$path)
    expect_true(file.exists(full_path))
  }
})

# Parameter coordinates that crash the model are an informative signal, not a
# system error. The pile must durably commit a 'failed' record to the index
# without halting the calling process or writing a log file.
test_that("Biological failures are recorded as data, not dropped", {
  pile <- create_pile(tempfile("inv6_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)

  req_fail <- make_mock_request(model_id = "FF16@v1", lma = -1.0)
  h_fail <- request_fingerprint(resolve_request(req_fail))

  # Construct a PlantFailure—the structured error condition
  failure <- PlantFailure(
    reason = "exception",
    message = "Negative LMA: biologically inviable"
  )

  # Execution (pile$put with PlantFailure) must not halt the R session
  expect_no_error(put_log(pile, h_fail, failure, list(request = req_fail)))

  # The index MUST record the failure
  rec <- pile$st$get(h_fail, namespace = "index")
  expect_equal(rec$status, "failed")
  expect_true(!is.null(rec$failure$message))
  expect_match(rec$failure$message, "inviable")

  # The raw pile MUST NOT contain a readable log for this run
  expect_error(get_log(pile, h_fail), "failed")
})
