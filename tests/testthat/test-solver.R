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
  expect_true(all(c("run_fingerprint", "strategy_id", "cohort_id", "birth_time", "t", "height") %in% names(res$log)))
  
  resolved <- resolve_request(req)
  expect_equal(unique(res$log$run_fingerprint), request_fingerprint(resolved))
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
