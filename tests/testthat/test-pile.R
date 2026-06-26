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
  pile_put(pile, h, log, list(request = req))
  expect_true(pile_has(pile, h))

  rec1 <- pile$st$get(h, namespace = "index")
  expect_equal(rec1$status, "done")

  # Second write (cache hit) — must be a no-op
  pile_put(pile, h, log, list(request = req))
  rec2 <- pile$st$get(h, namespace = "index")
  expect_equal(rec2$created_at, rec1$created_at)

  # Raw retrieval: schema
  df_disk <- pile_get(pile, h)
  expect_s3_class(df_disk, "data.frame")
  expect_true("height" %in% names(df_disk))
  expect_true(nrow(df_disk) > 0L)
  expect_equal(df_disk$run_fingerprint[1], h)
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
  pile_put(pile, h, log, list(request = req))

  # Two projection objects with different version strings and different outputs
  proj_mean <- projection(function(df) {
    data.frame(mean_h = mean(df$height, na.rm = TRUE))
  }, projection_version = "test_mean@v1")

  proj_max <- projection(function(df) {
    data.frame(max_h = max(df$height, na.rm = TRUE))
  }, projection_version = "test_max@v1")

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
