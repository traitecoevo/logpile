test_that("run_plant successfully executes SCM and coerces the log", {
  req <- list(
    model_id = "FF16@v1",
    strategies = list(list(lma = 0.08)),
    control = list(refine_schedule = FALSE)
  )

  res <- run_plant(req)
  expect_type(res, "list")
  expect_false(inherits(res, "PlantFailure"))
  expect_s3_class(res$log, "data.frame")
  expect_true(all(c("fingerprint", "strategy_id", "cohort_id", "birth_time", "t", "height") %in% names(res$log)))
  
  resolved <- resolve_request(req)
  expect_equal(unique(res$log$fingerprint), request_fingerprint(resolved))
  expect_true(res$runtime_seconds > 0)
  expect_true(res$solver_steps > 0)
})

test_that("run_plant handles refine_schedule correctly", {
  req <- list(
    model_id = "FF16@v1",
    strategies = list(list(lma = 0.08)),
    control = list(refine_schedule = TRUE)
  )
  res <- run_plant(req)
  expect_false(inherits(res, "PlantFailure"))
  expect_true(length(res$realised_schedule[[1]]) > 20)
})

test_that("run_plant records failures as data via PlantFailure", {
  req <- list(
    model_id = "FF16@v1",
    strategies = list(list(lma = -0.5))
  )
  res <- suppressWarnings(run_plant(req))
  expect_s3_class(res, "PlantFailure")
  expect_match(res$reason, "setup_error|exception")
  expect_match(res$message, "inviable|negative|NaN|must be positive|non-positive|TRUE/FALSE", ignore.case = TRUE)
})

test_that("format_log maps cohorts, drops seed row, and casts types without plant", {
  schedule <- list(c(0.0, 1.0), c(0.0, 1.0, 2.0))
  fp <- "test_fp_123"

  df <- tibble::tibble(
    step = c(1, 1, 1,  1, 1, 1, 1),
    species = c(1, 1, 1,  2, 2, 2, 2),
    node = c(1, 2, 3,  1, 2, 3, 4),
    time = c(3.0, 3.0, 3.0,  3.0, 3.0, 3.0, 3.0),
    height = c(5L, 4L, 0L,  10L, 8L, 6L, 0L)
  )

  res <- format_log(df, fp, schedule)

  expect_equal(nrow(res), 5L)
  expect_equal(res$strategy_id, c(0L, 0L, 1L, 1L, 1L))
  expect_equal(res$cohort_id, c(1L, 2L, 1L, 2L, 3L))
  expect_equal(res$birth_time, c(0.0, 1.0, 0.0, 1.0, 2.0))
  expect_type(res$height, "double")
  expect_equal(res$height, c(5.0, 4.0, 10.0, 8.0, 6.0))
})
