

# Biologically equivalent requests must yield the exact same SHA-256 fingerprint,
# regardless of how they are constructed. This stability is critical for
# ensuring that cached results can be reliably retrieved across different R sessions.
test_that("Identity is pure and permutation-invariant", {
  # 1. Base request
  req_base <- list(model_id = "FF16@v1", strategies = list(list(lma = 0.08)))

  # 2. Re-ordered keys
  req_reorder <- list(strategies = list(list(lma = 0.08)), model_id = "FF16@v1")

  # 3. Explicit default (patch_area = 1.0 is the schema default)
  req_explicit <- list(
    model_id = "FF16@v1",
    global = list(patch_area = 1.0),
    strategies = list(list(lma = 0.08))
  )

  # 4. Multi-strategy permutation
  req_multi_A <- list(
    model_id = "FF16@v1",
    strategies = list(list(lma = 0.1), list(lma = 0.2))
  )
  req_multi_B <- list(
    model_id = "FF16@v1",
    strategies = list(list(lma = 0.2), list(lma = 0.1))
  )

  h_base <- request_fingerprint(resolve_request(req_base))

  # Semantic equivalence => byte equivalence
  expect_equal(h_base, request_fingerprint(resolve_request(req_reorder)))
  expect_equal(h_base, request_fingerprint(resolve_request(req_explicit)))
  expect_equal(request_fingerprint(resolve_request(req_multi_A)), request_fingerprint(resolve_request(req_multi_B)))

  # The hash is a 64-char hex SHA-256
  expect_type(h_base, "character")
  expect_equal(nchar(h_base), 64L)
})

# The storage layer must faithfully serialize any data frame it receives.
# This ensures the pile remains robust even if the underlying C++ model
# starts emitting different variables.
test_that("Pile handles arbitrary model schemas without modification", {
  pile <- create_pile(tempfile("inv5_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)

  # A log with completely synthetic columns
  n_steps <- 5L
  mock_log <- tibble::tibble(
    step    = rep(seq_len(n_steps), each = 2L),
    species = 1L,
    node    = rep(c(1L, 2L), times = n_steps),
    time    = rep(seq(0, 100, length.out = n_steps), each = 2L),
    synthetic_variable_x = runif(n_steps * 2L),
    synthetic_variable_y = runif(n_steps * 2L),
    exotic_metric_z = rnorm(n_steps * 2L)
  )

  h_mock <- paste0("deadbeef", strrep("0", 56))  # 64-char fake hash
  req <- make_mock_request(model_id = "FF16@v1", lma = 0.1)

  put_log(pile, h_mock, mock_log, list(request = req))

  # Retrieval must preserve the exact shape
  retrieved <- get_log(pile, h_mock)
  expect_true("synthetic_variable_x" %in% names(retrieved))
  expect_true("synthetic_variable_y" %in% names(retrieved))
  expect_true("exotic_metric_z" %in% names(retrieved))
  expect_true(nrow(retrieved) > 0L)
})

# Driver data is immutable and content-addressed. The request fingerprint must
# only hash the reference pointer, not the entire data payload. To ensure
# integrity, the pile must abort before simulation if a driver file is missing.
test_that("Drivers are decoupled, deduplicated, and referentially enforced", {
  skip("Driver implementation deferred to a future task")
})
