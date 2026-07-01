
# ============================================================================
# PROJECTION PIPELINE TESTS
#
# Covers:
#   1. transform_ff16 adds basal_area, density, diameter columns
#   2. stand_summary produces density-weighted means over a transformed log
#   3. size_field(B) bins cohorts and back-fills empty bins with zeros
#   4. projection_of routes through transform -> projection and caches by version
# ============================================================================

# ---------------------------------------------------------------------------
# 1. transform_ff16
# ---------------------------------------------------------------------------
test_that("transform_ff16 adds basal_area, density, diameter", {
  h <- "aa"
  df <- make_mock_log(h, n_steps = 5L, n_cohorts = 4L)

  result <- transform_ff16(df)

  expect_true("basal_area" %in% names(result))
  expect_true("density"    %in% names(result))
  expect_true("diameter"   %in% names(result))

  # density = exp(log_density)
  expect_equal(result$density, exp(df$log_density))

  # diameter = 0.01 * height^1.4
  expect_equal(result$diameter, 0.01 * result$height^1.4)

  # basal_area > 0 when height > 0
  expect_true(all(result$basal_area > 0.0))
})

# ---------------------------------------------------------------------------
# 2. stand_summary — generic density-weighted aggregation
# ---------------------------------------------------------------------------
test_that("stand_summary produces _mean and _total columns over transformed log", {
  h <- "bb"
  df <- make_mock_log(h, n_steps = 10L, n_cohorts = 5L)
  transformed <- transform_ff16(df)

  result <- stand_summary(transformed)

  expect_s3_class(result, "data.frame")
  expect_true("t" %in% names(result))
  # stand_summary_fn produces {col}_mean and {col}_total for numeric cols
  expect_true("basal_area_mean"  %in% names(result))
  expect_true("basal_area_total" %in% names(result))
  expect_true("height_mean"      %in% names(result))
  expect_true("diameter_mean"    %in% names(result))

  # One row per time step
  expect_equal(nrow(result), length(unique(transformed$t)))
})

test_that("stand_summary aggregates an arbitrary extra column seamlessly", {
  h <- "cc"
  df <- make_mock_log(h, n_steps = 4L, n_cohorts = 3L)
  transformed <- transform_ff16(df)
  # Inject a novel column
  transformed$my_metric <- runif(nrow(transformed))

  result <- stand_summary(transformed)

  expect_true("my_metric_mean"  %in% names(result))
  expect_true("my_metric_total" %in% names(result))
})

# ---------------------------------------------------------------------------
# 3. size_field(B) — binning and empty-bin back-fill
# ---------------------------------------------------------------------------
test_that("size_field bins cohorts and back-fills empty bins with zeros", {
  h <- "dd"
  df <- make_mock_log(h, n_steps = 5L, n_cohorts = 6L)
  transformed <- transform_ff16(df)

  B <- 4L
  sf <- size_field(B)
  result <- sf(transformed)

  expect_s3_class(result, "data.frame")
  expect_true("bin" %in% names(result))
  # Every time step must have exactly B bin rows
  bins_per_t <- vapply(split(result, result$t), nrow, integer(1))
  expect_true(all(bins_per_t == B))

  # Empty bins must have density == 0.0, not NA
  expect_false(anyNA(result$density))
  expect_false(anyNA(result$mortality))
})

test_that("size_field handles fewer cohorts than B via rank-based assignment", {
  h <- "ee"
  # Only 2 cohorts per step, B = 5
  df <- make_mock_log(h, n_steps = 3L, n_cohorts = 2L)
  transformed <- transform_ff16(df)

  B <- 5L
  sf <- size_field(B)
  result <- sf(transformed)

  bins_per_t <- vapply(split(result, result$t), nrow, integer(1))
  expect_true(all(bins_per_t == B))
})

# ---------------------------------------------------------------------------
# 4. projection_of — pipeline routing and cache-by-version
# ---------------------------------------------------------------------------
test_that("projection_of routes log through transform_ff16 then projection and caches result", {
  pile <- create_pile(tempfile("proj_pipe_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  req <- make_mock_request(model_id = "FF16@v1", lma = 0.09)
  h <- request_fingerprint(req)
  log <- make_mock_log(h)
  put_log(pile, h, log, list(request = req))

  # stand_summary requires diameter, which transform_ff16 adds
  result <- projection_of(h, stand_summary, pile = pile)

  expect_s3_class(result, "data.frame")
  expect_true("diameter_mean"   %in% names(result))
  expect_true("basal_area_mean" %in% names(result))
  expect_true(nrow(result) > 0L)

  # Second call must return identical result from cache (not re-compute)
  result2 <- projection_of(h, stand_summary, pile = pile)
  expect_equal(result$diameter_mean, result2$diameter_mean)
})

test_that("projection_of isolates cache by version attribute", {
  pile <- create_pile(tempfile("proj_iso_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  req <- make_mock_request(model_id = "FF16@v1", lma = 0.11)
  h <- request_fingerprint(req)
  log <- make_mock_log(h)
  put_log(pile, h, log, list(request = req))

  proj_a <- projection(function(df) data.frame(col_a = 1), id = "a@v1")
  proj_b <- projection(function(df) data.frame(col_b = 2), id = "b@v1")

  pa <- projection_of(h, proj_a, pile = pile)
  pb <- projection_of(h, proj_b, pile = pile)

  expect_true("col_a" %in% names(pa))
  expect_false("col_b" %in% names(pa))
  expect_true("col_b" %in% names(pb))
  expect_false("col_a" %in% names(pb))
})

test_that("projection_of errors if proj_fn has no id attribute", {
  pile <- create_pile(tempfile("proj_nover_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)

  req <- make_mock_request(model_id = "FF16@v1")
  h <- request_fingerprint(req)
  log <- make_mock_log(h)
  put_log(pile, h, log, list(request = req))

  bare_fn <- function(df) df
  expect_error(projection_of(h, bare_fn, pile = pile), "id")
})

test_that("weighted_mean computes expected values and falls back correctly", {
  # Equal weights
  expect_equal(weighted_mean(c(1, 2, 3), c(1, 1, 1)), 2.0)
  
  # Different weights
  expect_equal(weighted_mean(c(10, 20), c(1, 3)), 17.5)
  
  # Sum of weights <= 0 falls back to mean
  expect_equal(weighted_mean(c(10, 20), c(-1, 0)), 15.0)
  expect_equal(weighted_mean(c(10, 20), c(0, 0)), 15.0)
  
  # All NA weights falls back to mean
  expect_equal(weighted_mean(c(10, 20), c(NA, NA)), 15.0)
})

test_that("project_logs bulk path computes and caches multiple runs", {
  pile <- create_pile(tempfile("proj_bulk_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  r1 <- make_mock_request(model_id = "FF16@v1", lma = 0.1)
  r2 <- make_mock_request(model_id = "FF16@v1", lma = 0.2)
  h1 <- request_fingerprint(r1); l1 <- make_mock_log(h1); put_log(pile, h1, l1, list(request=r1))
  h2 <- request_fingerprint(r2); l2 <- make_mock_log(h2); put_log(pile, h2, l2, list(request=r2))

  proj <- projection(function(df) data.frame(sum_h = sum(df$height, na.rm=TRUE)), id = "bulk_test@v1")

  fingerprints <- c(h1, h2)
  res <- project_logs(fingerprints, proj, "FF16@v1", pile)

  # assert combined frame has both fingerprints and the projection columns
  expect_s3_class(res, "data.frame")
  expect_true("fingerprint" %in% names(res))
  expect_true("sum_h" %in% names(res))
  expect_true(all(fingerprints %in% res$fingerprint))

  # a projection record exists per fp pointing at the written part file
  rec1 <- pile$st$get(h1, namespace = "bulk_test@v1")
  rec2 <- pile$st$get(h2, namespace = "bulk_test@v1")
  expect_equal(rec1$status, "done")
  expect_equal(rec2$status, "done")
  expect_true(grepl("^part-.*\\.parquet$", basename(rec1$path)))
  expect_equal(rec1$path, rec2$path)
})
