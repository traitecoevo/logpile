# -- String concat helper -----------------------------------------------------
`%.%` <- function(a, b) paste0(a, b)

# -- Mock Helpers for Tests ---------------------------------------------------

# Synthetic log that mimics the cohort-major layout written by pile$put().
# Avoids any dependency on the plant C++ runtime.
make_mock_log <- function(fingerprint, n_steps = 20L, n_cohorts = 3L) {
  n <- n_steps * n_cohorts
  heights <- cumsum(runif(n, 0.01, 0.1))
  tibble::tibble(
    run_fingerprint = as.character(fingerprint),
    strategy_id = rep(0L, n),
    cohort_id = rep(seq_len(n_cohorts + 1L)[-1L], times = n_steps),
    birth_time = 0.0,
    t = rep(seq(0, 100, length.out = n_steps), each = n_cohorts),
    height   = heights,
    area_sapwood = 0.1 * heights^1.41,
    area_heartwood = 0.05 * heights^1.41,
    mortality = runif(n, 0, 0.01),
    fecundity = runif(n, 0, 0.001),
    log_density = runif(n, -5, 0)
  )
}

# In the new pile API, pile$put takes (fingerprint, log/failure, request).
# We define a helper that constructs a mock request carrying the design_coords attribute.
make_mock_request <- function(model_id = "FF16@v1", lma = 0.08, design_coords = list(lma = lma)) {
  req <- list(
    model_id = model_id,
    strategies = list(list(lma = lma))
  )
  attr(req, "design_coords") <- design_coords
  req
}
