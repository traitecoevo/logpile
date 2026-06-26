#' Generate Campaign Manifest
#'
#' A generative campaign representation combining a request template
#' and parameter design coordinates sampled from a prior support.
#'
#' @param template A request list template.
#' @param priors A named list of parameter ranges (lower and upper bounds).
#' @param n Integer number of samples to draw.
#' @param seed Optional integer seed for reproducible Sobol sequences (default: 42).
#' @return A list containing template and coords.
#' @export
manifest <- function(template, priors, n, seed = 42L) {
  list(
    template = resolve_request(template),
    coords = sobol(priors, n, seed)
  )
}

#' Sobol Space-Filling Proposal Generator
#'
#' Generates space-filling Sobol draws scaled to the prior supports,
#' using a randomized digital shift inheriting from the global RNG state.
#'
#' @param priors Named list of prior ranges.
#' @param n Number of samples.
#' @param seed Optional integer seed (default: 42).
#' @return A data frame of coordinates.
sobol <- function(priors, n, seed = 42L) {
  if (!is.null(seed)) {
    if (identical(as.integer(seed), 42L)) {
      message("Using default seed = 42 for reproducible coordinates")
    }
    set.seed(seed)
  }

  D <- length(priors)
  if (D == 0L) {
    return(data.frame(log_q = rep(0.0, n)))
  }

  for (p in names(priors)) {
    if (length(priors[[p]]) != 2L || any(is.na(priors[[p]]))) {
      stop(sprintf("Prior for '%s' must be a range of length 2", p), call. = FALSE)
    }
  }

  mat <- qrng::sobol(n, d = D, randomize = "digital.shift")
  if (!is.matrix(mat)) mat <- matrix(mat, ncol = D, nrow = n)

  p_min <- vapply(priors, min, double(1))
  p_max <- vapply(priors, max, double(1))
  
  df <- as.data.frame(sweep(sweep(mat, 2, p_max - p_min, `*`), 2, p_min, `+`))
  names(df) <- names(priors)
  df$log_q <- -sum(log(p_max - p_min))
  df
}

#' Populate Request Template with Coordinates
#'
#' Injects parameter values from a coordinate row into the request template.
#'
#' @param template A request template.
#' @param coords A list or single-row data frame of coordinates.
#' @return A resolved and populated request list.
#' @export
place_coords <- function(template, coords) {
  if (length(coords) == 0L) return(template)
  
  req <- template
  mid <- req$model_id %||% "FF16@v1"
  model_type <- parse_model_id(mid)$model
  schema <- get_model_schema(model_type)

  g_names <- c(names(schema$p0), schema$hp_names, "collect_all_auxiliary")
  c_names <- c(names(schema$s0$control), "refine_schedule", "timeout")
  s_names <- c(names(schema$s0), "collect_all_auxiliary")

  coords_list <- as.list(coords)
  coords_list$log_q <- NULL

  if (is.null(req$global)) req$global <- list()
  if (is.null(req$control)) req$control <- list()
  if (is.null(req$strategies) || length(req$strategies) == 0L) req$strategies <- list(list())

  for (nm in names(coords_list)) {
    val <- coords_list[[nm]]
    if (nm %in% g_names) req$global[[nm]] <- as.double(val)
    else if (nm %in% c_names) req$control[[nm]] <- val
    else if (nm %in% s_names) {
      for (i in seq_along(req$strategies)) req$strategies[[i]][[nm]] <- as.double(val)
    } else {
      stop(sprintf("Parameter '%s' is not present in global, control, or strategy schemas", nm), call. = FALSE)
    }
  }

  req <- resolve_request(req)
  attr(req, "design_coords") <- coords_list
  req
}
