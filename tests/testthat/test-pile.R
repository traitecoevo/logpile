# Data writing must be idempotent. Once a record is marked as 'done', it
# should never be overwritten. Furthermore, retrieving data must yield
# byte-identical output whether it was just written or fetched from disk.
test_that("Pile is durable and execution is idempotent", {
  pile <- create_pile(tempfile("inv2_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  req <- make_mock_request(model_id = "FF16@v1", lma = 0.12)
  h <- request_fingerprint(req)
  log <- make_mock_log(h)

  # First write (cache miss)
  put_log(pile, h, log, list(request = req))
  expect_true(has_log(pile, h))

  rec1 <- pile$st$get(h, namespace = "index")
  expect_equal(rec1$status, "done")

  # Second write (cache hit) — must be a no-op
  log2 <- log
  log2$height <- log2$height + 100.0
  put_log(pile, h, log2, list(request = req))
  rec2 <- pile$st$get(h, namespace = "index")
  expect_equal(rec2$path, rec1$path)

  # Raw retrieval: schema
  df_disk <- get_log(pile, h)
  expect_s3_class(df_disk, "data.frame")
  expect_true("height" %in% names(df_disk))
  expect_true(nrow(df_disk) > 0L)
  expect_equal(df_disk$fingerprint[1], h)
  expect_equal(df_disk$height, log$height)
})

# Projections must compute on a cache miss, return cached data on a hit,
# and remain strictly isolated by their projection version. Once a projection
# is cached, it must never touch the raw log again.
test_that("Projections compute on miss, cache on hit, isolate by version", {
  pile <- create_pile(tempfile("inv7_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  req <- make_mock_request(model_id = "FF16@v1", lma = 0.08)
  h <- request_fingerprint(req)
  log <- make_mock_log(h)
  put_log(pile, h, log, list(request = req))

  # Two projection objects with different version strings and different outputs
  proj_mean <- projection(function(df) {
    data.frame(mean_h = mean(df$height, na.rm = TRUE))
  }, id = "test_mean@v1")

  proj_max <- projection(function(df) {
    data.frame(max_h = max(df$height, na.rm = TRUE))
  }, id = "test_max@v1")

  # 1. Compute on Miss
  p1 <- projection_of(h, proj_mean, pile = pile)
  expect_s3_class(p1, "data.frame")
  expect_true("mean_h" %in% names(p1))

  # 2. Cache on Hit (must read from cache, not raw)
  p1_cached <- projection_of(h, proj_mean, pile = pile)
  expect_equal(p1$mean_h, p1_cached$mean_h)

  # Verify the index record exists on disk
  ns <- "test_mean@v1"
  rec_proj <- pile$st$get(h, namespace = ns)
  expect_equal(rec_proj$status, "done")
  cache_file <- file.path(pile$path, rec_proj$path)
  expect_true(file.exists(cache_file))

  # 3. Isolation by version — different projection, different output columns
  p2 <- projection_of(h, proj_max, pile = pile)
  expect_true("max_h" %in% names(p2))
  expect_true("mean_h" %in% names(p1))
  expect_false("max_h" %in% names(p1))
  expect_false("mean_h" %in% names(p2))
})

test_that("compaction round-trips correctly", {
  pile <- create_pile(tempfile("compact_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  r1 <- make_mock_request(model_id = "FF16@v1", lma = 0.1)
  r2 <- make_mock_request(model_id = "FF16@v1", lma = 0.2)
  r3 <- make_mock_request(model_id = "FF16@v1", lma = 0.3)
  h1 <- request_fingerprint(r1); l1 <- make_mock_log(h1); put_log(pile, h1, l1, list(request=r1))
  h2 <- request_fingerprint(r2); l2 <- make_mock_log(h2); put_log(pile, h2, l2, list(request=r2))
  h3 <- request_fingerprint(r3); l3 <- make_mock_log(h3); put_log(pile, h3, l3, list(request=r3))

  compact_pile(pile, "FF16@v1")

  # assert the per-run log-*.parquet files are gone
  log_files <- list.files(file.path(pile$path, "raw", "model=FF16@v1"), pattern = "^log-.*\\.parquet$")
  expect_equal(length(log_files), 0L)

  # a single part-*.parquet exists
  part_files <- list.files(file.path(pile$path, "raw", "model=FF16@v1"), pattern = "^part-.*\\.parquet$")
  expect_equal(length(part_files), 1L)

  # get_log for each fp returns data equal to what was written
  expect_equal(sort(get_log(pile, h1)$height), sort(l1$height))
  expect_equal(sort(get_log(pile, h2)$height), sort(l2$height))
  expect_equal(sort(get_log(pile, h3)$height), sort(l3$height))
})
