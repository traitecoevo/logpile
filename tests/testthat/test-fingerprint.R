test_that("resolve_request validates keys and fills defaults", {
  req <- list(
    strategies = list(list(lma = 0.08))
  )
  res <- resolve_request(req)
  expect_equal(res$model_id, "FF16@v1")
  expect_true(is.list(res$global))
  expect_true(is.list(res$control))
  expect_equal(res$global$patch_area, 1.0)
})

test_that("resolve_request rejects unknown top-level keys", {
  req <- list(
    strategies = list(list(lma = 0.08)),
    exotic_parameter = 42
  )
  expect_error(resolve_request(req), "Unknown top-level fields")
})

test_that("resolve_request rejects invalid namespaces", {
  expect_error(resolve_request(list(model_id = "FF16")), "Unsupported model ID format")
  expect_error(resolve_request(list(model_id = "FF16@v0")), "Unsupported model ID")
  expect_error(resolve_request(list(model_id = "FF16@v")), "Unsupported model ID format")
})

test_that("request_fingerprint asserts resolved input", {
  req <- list(strategies = list(list(lma = 0.08)))
  expect_error(request_fingerprint(req), "requires a resolved request")
})

test_that("normalize_for_cbor handles matrices, floats, and rejects NaN/Inf/NA", {
  # Matrix to list of lists of doubles
  mat <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2)
  res <- normalize_for_cbor(mat)
  expect_type(res, "list")
  expect_equal(length(res), 2L)
  expect_equal(res[[1]], c(1.0, 3.0))

  # Float normalization
  expect_identical(normalize_for_cbor(-0.0), 0.0)

  # Reject invalid numerics
  expect_error(normalize_for_cbor(NaN), "NA/NaN/Inf not permitted")
  expect_error(normalize_for_cbor(Inf), "NA/NaN/Inf not permitted")
  expect_error(normalize_for_cbor(NA_real_), "NA/NaN/Inf not permitted")
})

test_that("golden test: canonical CBOR and key/strategy permutation stability", {
  req1 <- list(
    model_id = "FF16@v1",
    global = list(patch_area = 1.0, max_patch_lifetime = 100.0),
    control = list(refine_schedule = FALSE, timeout = 0.0),
    strategies = list(list(lma = 0.1, rho = 200.0), list(lma = 0.2, rho = 100.0)),
    schedule = list(c(0.0, 10.0, 20.0), c(0.0, 15.0, 30.0)),
    drivers = character(0)
  )

  req2 <- list(
    drivers = character(0),
    schedule = list(c(0.0, 15.0, 30.0), c(0.0, 10.0, 20.0)),
    strategies = list(list(rho = 100.0, lma = 0.2), list(rho = 200.0, lma = 0.1)),
    control = list(timeout = 0.0, refine_schedule = FALSE),
    global = list(max_patch_lifetime = 100.0, patch_area = 1.0),
    model_id = "FF16@v1"
  )

  golden_hash <- "46636c9d19ca2f2616f807c76744522307cf25960557e2feb7308880c027b333"

  h1 <- request_fingerprint(resolve_request(req1))
  h2 <- request_fingerprint(resolve_request(req2))

  expect_equal(h1, golden_hash)
  expect_equal(h2, golden_hash)

  cbor1 <- canonical_cbor(resolve_request(req1))
  cbor2 <- canonical_cbor(resolve_request(req2))
  expect_identical(cbor1, cbor2)
})
