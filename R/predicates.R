.predicate_registry <- new.env(parent = emptyenv())

#' Register a Calibration Predicate
#'
#' @param id Scalar character name.
#' @param fn A function `function(proj)` returning TRUE/FALSE.
#' @param projection_id The projection this predicate operates on (e.g. "stand_summary@v1").
#' @export
register_predicate <- function(id, fn, projection_id = "stand_summary@v1") {
  assert_scalar_character(id)
  assert_scalar_character(projection_id)
  if (!is.function(fn)) stop("'fn' must be a function", call. = FALSE)
  .predicate_registry[[id]] <- list(fn = fn, id = projection_id)
  invisible(id)
}

#' @keywords internal
get_predicate <- function(id) {
  p <- .predicate_registry[[id]]
  if (is.null(p)) stop(sprintf("predicate '%s' not found in registry", id), call. = FALSE)
  p
}

#' Define a Predicate Set
#'
#' @param ids Character vector of registered predicate ids.
#' @return A named list with class `predicate_set` and a `predicate_set_fingerprint` attribute.
#' @export
predicate_set <- function(ids) {
  entries <- lapply(ids, get_predicate)
  base::names(entries) <- ids
  # Hash sorted names only — deparse() is unstable across R versions
  set_fingerprint <- secretbase::sha256(
    secretbase::cborenc(normalize_for_cbor(as.list(sort(ids)))),
    convert = TRUE
  )
  structure(entries, predicate_set_fingerprint = set_fingerprint, class = "predicate_set")
}

#' @export
as.character.predicate_set <- function(x, ...) attr(x, "predicate_set_fingerprint")

all_within <- function(x, lo, hi) {
  if (length(x) == 0L) return(TRUE)
  !any(x < lo | x > hi, na.rm = TRUE)
}

in_range <- function(v, lo, hi) {
  !is.na(v) && v >= lo && v <= hi
}

# Returns the last `frac` fraction of rows — the settled-stand tail.
stable_tail <- function(proj, frac = 0.2) {
  n <- nrow(proj)
  if (n < 5L) return(proj[integer(0L), , drop = FALSE])
  proj[seq.int(n - floor(frac * n) + 1L, n), , drop = FALSE]
}

# OLS slope of log(N) ~ log(D) after peak density. NA if insufficient data.
thinning_slope <- function(proj) {
  N <- proj$density_total
  D <- proj$diameter_mean
  peak <- which.max(N)
  if (length(N) - peak + 1L < 5L) return(NA_real_)
  N <- N[peak:length(N)]
  D <- D[peak:length(D)]
  keep <- !is.na(N) & !is.na(D) & N > 1e-4 & D > 0.01
  if (sum(keep) < 5L) return(NA_real_)
  x <- log(D[keep]); y <- log(N[keep])
  if (max(x) - min(x) < 1e-4) return(NA_real_)
  stats::cov(x, y) / stats::var(x)
}

#' Evaluate a Predicate Set Against Fingerprints
#'
#' Fetches each predicate's required projection from the pile, evaluates the
#' conjunction, and returns "passed" or "failed_predicate" for each fingerprint.
#'
#' @param fingerprints Character vector of SHA-256 fingerprints.
#' @param pile A logpile_pile object.
#' @param pset A `predicate_set` object.
#' @return A character vector ("passed" or "failed_predicate") of the same length as `fingerprints`.
#' @export
evaluate_predicates <- function(fingerprints, pile, pset) {
  if (length(fingerprints) == 0L) return(character(0))
  cls     <- classify_logs(pile, fingerprints)
  results <- cls$results
  passing <- cls$valid_fps
  if (length(passing) == 0L) return(unname(results[fingerprints]))

  for (id in unique(vapply(pset, function(p) p$id, character(1)))) {
    if (length(passing) == 0L) break
    ensure_projections(pile, id, passing, cls$model_map)
    proj_list <- load_projections(pile, id, passing)
    if (is.null(proj_list)) {                 # projection dir absent
      results[passing] <- "failed_predicate"; passing <- character(0); break
    }
    for (nm in predicate_names_for(pset, id)) {
      if (length(passing) == 0L) break
      fn <- pset[[nm]]$fn
      ok <- vapply(passing, function(fp)
        isTRUE(tryCatch(fn(proj_list[[fp]] %||% data.frame()), error = function(e) FALSE)),
        logical(1))
      results[passing[!ok]] <- "failed_predicate"
      passing <- passing[ok]
    }
  }
  results[passing] <- "passed"
  unname(results[fingerprints])
}

classify_logs <- function(pile, fingerprints) {
  records <- pile$st$mget(fingerprints, namespace = "index")
  results <- stats::setNames(rep("missing_run", length(fingerprints)), fingerprints)
  valid_fps <- character(0); model_map <- list()
  for (i in seq_along(fingerprints)) {
    rec <- records[[i]]
    if (is.null(rec)) next
    if (identical(rec$status, "failed")) { results[fingerprints[i]] <- "failed_run"; next }
    if (is.null(record_path(pile, fingerprints[i], rec, "index"))) next
    valid_fps <- c(valid_fps, fingerprints[i]); model_map[[fingerprints[i]]] <- rec$model_id
  }
  list(results = results, valid_fps = valid_fps, model_map = model_map)
}

ensure_projections <- function(pile, id, fingerprints, model_map) {
  missing <- fingerprints[vapply(pile$st$mget(fingerprints, namespace = id), is.null, logical(1))]
  if (length(missing) == 0L) return(invisible())
  models <- vapply(missing, function(fp) model_map[[fp]], character(1))
  for (mod in unique(models)) project_logs(missing[models == mod], id, mod, pile)
  invisible()
}

load_projections <- function(pile, id, fingerprints) {
  dir <- fs::path(pile$path, "projections", sprintf("projection=%s", id))
  if (!fs::dir_exists(dir)) return(NULL)
  df <- dplyr::collect(dplyr::filter(arrow::open_dataset(dir, format = "parquet"),
                                     fingerprint %in% fingerprints))
  if (nrow(df) == 0L) list() else split(df, df$fingerprint)
}

predicate_names_for <- function(pset, id)
  names(pset)[vapply(pset, function(p) identical(p$id, id), logical(1))]

allometry_in_range <- function(proj) {
  h <- proj$height_mean
  d <- proj$diameter_mean
  if (is.null(h) || is.null(d) || length(h) == 0L) return(FALSE)
  all_within(h, 0, 50) &&
    all_within(d, 0, 3) &&
    all_within(h / (d + 1e-5), 5, 500)
}

basal_area_bounded <- function(proj) {
  all_within(proj$basal_area_total, 0, 500)
}

stem_density_bounded <- function(proj) {
  all_within(proj$density_total, 0, 1000)
}

steady_structure <- function(proj) {
  tail <- stable_tail(proj)
  if (nrow(tail) == 0L) return(FALSE)
  ba <- tail$basal_area_total
  mu <- mean(ba, na.rm = TRUE)
  if (is.na(mu) || mu < 1.0) return(FALSE)
  (max(ba, na.rm = TRUE) - min(ba, na.rm = TRUE)) / mu <= 0.5
}

self_thinning <- function(proj) {
  in_range(thinning_slope(proj), -4.0, -0.2)
}

drought_mortality_size_structured <- function(proj) {
  if (is.null(proj) || nrow(proj) == 0L || !"bin" %in% names(proj)) return(FALSE)
  t_max <- max(proj$t, na.rm = TRUE)
  final <- proj[proj$t == t_max, ]
  if (nrow(final) < 2L) return(FALSE)
  mort <- final$mortality
  if (all(is.na(mort))) return(FALSE)
  mort_sd <- stats::sd(mort, na.rm = TRUE)
  if (is.na(mort_sd) || mort_sd < 1e-4) return(FALSE)
  avg_mort <- mean(mort, na.rm = TRUE)
  if (is.na(avg_mort) || avg_mort < 1e-4 || avg_mort > 25.0) return(FALSE)
  (max(mort, na.rm = TRUE) - min(mort, na.rm = TRUE)) >= 1e-2
}

register_predicate("allometry_in_range",   allometry_in_range,   "stand_summary@v1")
register_predicate("basal_area_bounded",   basal_area_bounded,   "stand_summary@v1")
register_predicate("stem_density_bounded", stem_density_bounded,  "stand_summary@v1")
register_predicate("steady_structure",     steady_structure,      "stand_summary@v1")
register_predicate("self_thinning",        self_thinning,         "stand_summary@v1")
register_predicate("drought_mortality_size_structured",
                   drought_mortality_size_structured, "size_field@v1")
