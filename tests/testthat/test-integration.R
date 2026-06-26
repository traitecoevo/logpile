test_that("happy path from README runs successfully", {
  pile_path <- tempfile("test_campaign_")
  pile <- create_pile(pile_path)
  set_active_pile(pile)
  on.exit(unlink(pile_path, recursive = TRUE), add = TRUE)
  
  # 1. A request template: which model, plus any fixed settings.
  # Note: The README uses FF16@v3, but the refactor branch uses v1 schema.
  template <- resolve_request(list(
    model_id = "FF16@v1",
    global = list(max_patch_lifetime = 105.32),
    control = list(refine_schedule = FALSE) # For fast test execution
  ))
  
  # 2. Priors: the trait ranges to sweep.
  # sobol expects vectors c(lo, hi) rather than list(lo =, hi =)
  priors <- list(
    rho  = c(500, 1200),   # wood density
    hmat = c(3, 15)        # height at maturity
  )
  
  # 3. Predicates: what makes a run ecologically plausible.
  keep <- predicate_set(c(
    "allometry_in_range",
    "basal_area_bounded",
    "stem_density_bounded",
    "steady_structure"
  ))
  
  # 4. Run draws and evaluate predicates.
  # (n=2 for speed, skipping compare)
  m <- manifest(template, priors, 2L)
  fps <- run(m, pile = pile)
  
  evals <- evaluate_predicates(fps, pile, keep)
  
  # 5. Inspect and verify
  expect_equal(length(evals), 2L)
  expect_true(all(evals %in% c("passed", "failed_predicate", "failed_run")))

  # coverage
  coords_df <- coords("FF16@v1", pile = pile)
  expect_s3_class(coords_df, "data.frame")
  expect_true(nrow(coords_df) >= 2L)
  expect_true("rho" %in% names(coords_df))

  # test knn
  query_val <- matrix(c(800, 10), nrow = 1)
  res_knn <- knn(query_val, k = 1, model = "FF16@v1", pile = pile)
  expect_s3_class(res_knn, "data.frame")
  expect_equal(nrow(res_knn), 1L)
  expect_true("run_fingerprint" %in% names(res_knn))

  # test gap
  candidates <- data.frame(rho = c(600, 1000), hmat = c(5, 12))
  res_gap <- gap(candidates, n = 1, model = "FF16@v1", pile = pile)
  expect_equal(nrow(res_gap), 1L)
  expect_true("gap_distance" %in% names(res_gap))
})
