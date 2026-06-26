.predicate_registry <- new.env(parent = emptyenv())

#' Register a Calibration Predicate
#'
#' @param name Scalar character name.
#' @param fn A function `function(proj)` returning TRUE/FALSE.
#' @param projection_id The projection this predicate operates on (e.g. "stand_summary@v1").
#' @export
register_predicate <- function(name, fn, projection_id = "stand_summary@v1") {
  assert_scalar_character(name)
  assert_scalar_character(projection_id)
  if (!is.function(fn)) stop("'fn' must be a function", call. = FALSE)
  .predicate_registry[[name]] <- list(fn = fn, projection_id = projection_id)
  invisible(name)
}

#' @keywords internal
get_predicate <- function(name) {
  p <- .predicate_registry[[name]]
  if (is.null(p)) stop(sprintf("predicate '%s' not found in registry", name), call. = FALSE)
  p
}

#' Define a Predicate Set
#'
#' @param names Character vector of registered predicate names.
#' @return A named list with class `predicate_set` and a `predicate_set_hash` attribute.
#' @export
predicate_set <- function(names) {
  entries <- lapply(names, get_predicate)
  base::names(entries) <- names
  # Hash sorted names only — deparse() is unstable across R versions
  set_hash <- secretbase::sha256(
    secretbase::cborenc(normalize_for_cbor(as.list(sort(names)))),
    convert = TRUE
  )
  structure(entries, predicate_set_hash = set_hash, class = "predicate_set")
}

#' @export
as.character.predicate_set <- function(x, ...) attr(x, "predicate_set_hash")

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
#' @param fps Character vector of SHA-256 fingerprints.
#' @param pile A logpile_pile object.
#' @param pset A `predicate_set` object.
#' @return A character vector ("passed" or "failed_predicate") of the same length as `fps`.
#' @export
evaluate_predicates <- function(fps, pile, pset) {
  if (length(fps) == 0L) return(character(0))
  
  records <- pile$st$mget(fps, namespace = "index")
  
  results <- rep("missing_run", length(fps))
  names(results) <- fps
  
  valid_fps <- character(0)
  model_map <- list()
  
  for (i in seq_along(fps)) {
    rec <- records[[i]]
    if (!is.null(rec)) {
      if (identical(rec$status, "failed")) {
        results[fps[i]] <- "failed_run"
      } else {
        full_path <- fs::path(pile$path, rec$path)
        if (!fs::file_exists(full_path)) {
          pile$st$del(fps[i], namespace = "index")
        } else {
          valid_fps <- c(valid_fps, fps[i])
          model_map[[fps[i]]] <- rec$model_id
        }
      }
    }
  }
  
  if (length(valid_fps) == 0L) {
    return(unname(results[fps]))
  }
  
  passing_fps <- valid_fps
  pids <- unique(vapply(pset, function(x) x$projection_id, character(1)))
  
  for (pid in pids) {
    if (length(passing_fps) == 0L) break
    
    ns <- pid
    
    proj_recs <- pile$st$mget(passing_fps, namespace = ns)
    missing_fps <- passing_fps[vapply(proj_recs, is.null, logical(1))]
    
    if (length(missing_fps) > 0L) {
      models_for_missing <- vapply(missing_fps, function(fp) model_map[[fp]], character(1))
      for (mod in unique(models_for_missing)) {
        mod_fps <- missing_fps[models_for_missing == mod]
        project_runs(mod_fps, pid, mod, pile)
      }
    }
    
    cache_dir <- fs::path(pile$path, "projections", sprintf("projection=%s", ns))
    if (!fs::dir_exists(cache_dir)) {
      results[passing_fps] <- "failed_predicate"
      passing_fps <- character(0)
      break
    }
    
    ds <- arrow::open_dataset(cache_dir, format = "parquet")
    ds_filtered <- ds |> dplyr::filter(run_fingerprint %in% passing_fps)
    proj_df <- dplyr::collect(ds_filtered)
    
    if (nrow(proj_df) > 0L) {
      proj_list <- split(proj_df, proj_df$run_fingerprint)
    } else {
      proj_list <- list()
    }
    
    preds_for_pid <- base::names(pset)[vapply(pset, function(x) identical(x$projection_id, pid), logical(1))]
    
    for (nm in preds_for_pid) {
      if (length(passing_fps) == 0L) break
      entry <- pset[[nm]]
      
      new_passing <- character(0)
      for (fp in passing_fps) {
        df_fp <- proj_list[[fp]]
        if (is.null(df_fp)) df_fp <- data.frame()
        
        passed <- tryCatch({
          entry$fn(df_fp)
        }, error = function(e) FALSE)
        
        if (isTRUE(passed)) {
          new_passing <- c(new_passing, fp)
        } else {
          results[fp] <- "failed_predicate"
        }
      }
      passing_fps <- new_passing
    }
  }
  
  for (fp in passing_fps) {
    results[fp] <- "passed"
  }
  
  unname(results[fps])
}

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
