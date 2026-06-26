test_that("all_within: exact boundary values pass", {
  expect_true(logpile:::all_within(c(0, 500), 0, 500))
  expect_false(logpile:::all_within(c(0, 500.001), 0, 500))
  expect_false(logpile:::all_within(c(-0.001, 500), 0, 500))
})

test_that("all_within: empty vector passes", {
  expect_true(logpile:::all_within(numeric(0), 0, 500))
})

test_that("in_range: NA returns FALSE", {
  expect_false(logpile:::in_range(NA_real_, 0, 1))
})

test_that("in_range: boundary values are inclusive", {
  expect_true(logpile:::in_range(-4.0, -4.0, -0.2))
  expect_true(logpile:::in_range(-0.2, -4.0, -0.2))
  expect_false(logpile:::in_range(-4.001, -4.0, -0.2))
})

test_that("stable_tail: returns last frac rows", {
  df <- data.frame(x = 1:10)
  tail <- logpile:::stable_tail(df, frac = 0.2)
  expect_equal(nrow(tail), 2L)
  expect_equal(tail$x, 9:10)
})

test_that("stable_tail: fewer than 5 rows returns empty", {
  df <- data.frame(x = 1:4)
  expect_equal(nrow(logpile:::stable_tail(df)), 0L)
})

test_that("thinning_slope: known slope recovers -2", {
  # N = D^(-2) => log(N) = -2 * log(D) => slope = -2
  D <- seq(0.05, 0.5, length.out = 20)
  N <- D^(-2)
  # Prepend a rising phase so peak is exactly the first element of N, D
  N_rise <- seq(10, N[1] - 10, length.out = 5)
  D_rise <- seq(0.01, 0.04, length.out = 5)
  proj <- data.frame(
    density_total = c(N_rise, N),
    diameter_mean = c(D_rise, D)
  )
  s <- logpile:::thinning_slope(proj)
  expect_true(!is.na(s))
  expect_true(abs(s - (-2.0)) < 0.05)
})

test_that("thinning_slope: insufficient post-peak data returns NA", {
  proj <- data.frame(density_total = c(10, 9, 8, 7), diameter_mean = c(0.1, 0.2, 0.3, 0.4))
  expect_true(is.na(logpile:::thinning_slope(proj)))
})

test_that("basal_area_bounded: passes within bounds, fails outside", {
  proj_ok  <- data.frame(basal_area_total = c(10, 200, 499))
  proj_bad <- data.frame(basal_area_total = c(10, 200, 501))
  expect_true(logpile:::basal_area_bounded(proj_ok))
  expect_false(logpile:::basal_area_bounded(proj_bad))
})

test_that("steady_structure: stable tail passes", {
  # Constant basal area in last 20% — fully stable
  ba <- c(seq(10, 50, length.out = 20), rep(50, 5))
  proj <- data.frame(basal_area_total = ba)
  expect_true(logpile:::steady_structure(proj))
})

test_that("steady_structure: oscillating tail fails", {
  ba <- c(rep(50, 20), 10, 90, 10, 90, 10)   # range/mean = 80/42 >> 0.5
  proj <- data.frame(basal_area_total = ba)
  expect_false(logpile:::steady_structure(proj))
})

test_that("self_thinning: ecologically valid slope passes", {
  D <- seq(0.1, 0.5, length.out = 20)
  N <- D^(-1.5)
  proj <- data.frame(density_total = c(rev(N[1:5]) * 2, N), diameter_mean = c(D[1:5], D))
  expect_true(logpile:::self_thinning(proj))
})

test_that("predicate_set hash is order-insensitive", {
  s1 <- predicate_set(c("basal_area_bounded", "stem_density_bounded"))
  s2 <- predicate_set(c("stem_density_bounded", "basal_area_bounded"))
  expect_identical(attr(s1, "version"), attr(s2, "version"))
})

test_that("predicate_set carries projection_id per entry", {
  ps <- predicate_set(c("basal_area_bounded", "drought_mortality_size_structured"))
  expect_equal(ps[["basal_area_bounded"]]$projection_id,   "stand_summary@v1")
  expect_equal(ps[["drought_mortality_size_structured"]]$projection_id, "size_field@v1")
})

test_that("evaluate_predicates returns 'failed_predicate' on failure", {
  pile <- create_pile(tempfile("pred_eval_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  req <- make_mock_request(model_id = "FF16@v1", lma = 0.08)
  fp <- request_fingerprint(req)
  log <- make_mock_log(fp)
  pile_put(pile, fp, log, list(request = req))

  # register a trivially-failing predicate
  register_predicate("__test_fail__", function(proj) FALSE, "stand_summary@v1")
  on.exit(rm("__test_fail__", envir = logpile:::.predicate_registry), add = TRUE)
  
  ps <- predicate_set("__test_fail__")
  result <- evaluate_predicates(fp, pile, ps)
  expect_equal(result, "failed_predicate")
 })

test_that("evaluate_predicates returns 'passed' when all predicates pass", {
  pile <- create_pile(tempfile("pred_eval2_"))
  on.exit(unlink(pile$path, recursive = TRUE), add = TRUE)
  set_active_pile(pile)

  req <- make_mock_request(model_id = "FF16@v1", lma = 0.08)
  fp <- request_fingerprint(req)
  log <- make_mock_log(fp)
  pile_put(pile, fp, log, list(request = req))

  register_predicate("__test_pass__", function(proj) TRUE, "stand_summary@v1")
  on.exit(rm("__test_pass__", envir = logpile:::.predicate_registry), add = TRUE)

  ps <- predicate_set("__test_pass__")
  result <- evaluate_predicates(fp, pile, ps)
  expect_equal(result, "passed")
})
