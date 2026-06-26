test_that("manifest constructor works as expected", {
  template <- list(model_id = "FF16@v1")
  priors <- list(lma = c(0.05, 0.15), rho = c(400, 800))
  
  set.seed(123)
  m <- manifest(template, priors, n = 5)
  expect_type(m, "list")
  expect_equal(names(m), c("template", "coords"))
  expect_equal(nrow(m$coords), 5L)
  
  # Coordinate placement manually
  req1 <- place_coords(m$template, m$coords[1, , drop = FALSE])
  expect_type(req1, "list")
  expect_equal(req1$model_id, "FF16@v1")
  expect_equal(length(req1$strategies), 1L)
  expect_true("lma" %in% names(req1$strategies[[1]]))
  expect_true("rho" %in% names(req1$strategies[[1]]))
  expect_equal(attr(req1, "design_coords")$lma, m$coords[1, "lma"])
  expect_equal(attr(req1, "design_coords")$rho, m$coords[1, "rho"])
})

test_that("sobol generates bounded coordinates correctly", {
  priors <- list(lma = c(0.05, 0.15), hmat = c(10.0, 30.0))
  
  # Seeded determinism
  set.seed(42)
  coords1 <- sobol(priors, n = 10)
  set.seed(42)
  coords2 <- sobol(priors, n = 10)
  expect_equal(coords1, coords2)
  
  # Boundaries
  expect_true(all(coords1$lma >= 0.05 & coords1$lma <= 0.15))
  expect_true(all(coords1$hmat >= 10.0 & coords1$hmat <= 30.0))
  
  # Unsupported bounds error
  expect_error(sobol(list(lma = c(0.05, NA)), n = 5))
  expect_error(sobol(list(lma = 0.05), n = 5))
})

test_that("place_coords maps parameters to correct locations", {
  template <- resolve_request(list(
    model_id = "FF16@v1",
    global = list(patch_area = 1.0),
    control = list(timeout = 0.0),
    strategies = list(list(lma = 0.08, rho = 600.0))
  ))
  
  # Test mapping global parameter
  req_g <- place_coords(template, list(patch_area = 2.5))
  expect_equal(req_g$global$patch_area, 2.5)
  
  # Test mapping control parameter
  req_c <- place_coords(template, list(timeout = 5.0))
  expect_equal(req_c$control$timeout, 5.0)
  
  # Test mapping strategy parameter
  req_s <- place_coords(template, list(lma = 0.12, rho = 700.0))
  expect_equal(req_s$strategies[[1]]$lma, 0.12)
  expect_equal(req_s$strategies[[1]]$rho, 700.0)
  
  # Non-existent parameter
  expect_error(place_coords(template, list(invalid_param = 123)))
})

