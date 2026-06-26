.projection_registry <- new.env(parent = emptyenv())

#' Register a Projection Function
#'
#' @param id Versioned string id, e.g. "stand_summary@v1".
#' @param fn A function `function(log)` returning a data frame.
#' @param required_columns Optional character vector of raw column names.
#' @export
register_projection <- function(id, fn, required_columns = NULL) {
  assert_scalar_character(id)
  if (!is.function(fn)) stop("'fn' must be a function", call. = FALSE)
  if (!is.null(required_columns)) attr(fn, "required_columns") <- required_columns
  .projection_registry[[id]] <- fn
  invisible(id)
}

#' @keywords internal
get_projection <- function(id) {
  fn <- .projection_registry[[id]]
  if (is.null(fn)) stop(sprintf("projection '%s' not found in registry", id), call. = FALSE)
  fn
}

#' Create a Projection Function Wrapper
#'
#' @param fn A function mapping a transformed log dataframe to aggregated results.
#' @param projection_version Character string identifying the projection name/version (used for caching).
#' @param required_columns Optional character vector of raw column names.
#' @return A wrapped projection function.
#' @export
projection <- function(fn, projection_version, required_columns = NULL) {
  assert_scalar_character(projection_version)
  if (!is.function(fn)) {
    stop("'fn' must be a function", call. = FALSE)
  }
  if (!is.null(required_columns)) attr(fn, "required_columns") <- required_columns
  structure(fn, projection_version = projection_version, class = "logpile_projection")
}

weighted_mean <- function(x, w) {
  sum_w <- sum(w, na.rm = TRUE)
  if (!is.na(sum_w) && sum_w > 0.0) {
    sum(x * w, na.rm = TRUE) / sum_w
  } else {
    mean(x, na.rm = TRUE)
  }
}

#' Stand Summary Projection
#'
#' Generic projection that groups by t and computes a
#' density-weighted mean for numeric columns, and total density.
#'
#' @param df Transformed log dataframe.
#' @return Aggregated stand summary dataframe.
#' @export
stand_summary_fn <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }

  df$weight <- if ("density" %in% names(df)) df$density else 1.0

  exclude <- c("run_fingerprint", "strategy_id", "cohort_id", "birth_time", "t", "bucket", "patch_density", "weight", "bin", "density")
  vars <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], exclude)

  res_df <- df %>%
    dplyr::group_by(t) %>%
    dplyr::summarise(
      density_total = if ("density" %in% names(df)) sum(density, na.rm = TRUE) else sum(weight, na.rm = TRUE),
      dplyr::across(dplyr::all_of(vars), list(
        mean = ~ weighted_mean(.x, weight),
        total = ~ sum(.x * weight, na.rm = TRUE)
      ), .names = "{.col}_{.fn}"),
      .groups = "drop"
    ) %>%
    dplyr::arrange(t)

  as.data.frame(res_df, stringsAsFactors = FALSE)
}

#' @export
stand_summary <- projection(stand_summary_fn, projection_version = "stand_summary@v1", required_columns = c("height", "mortality", "fecundity", "area_heartwood", "mass_heartwood", "log_density", "offspring_produced_survival_weighted"))


assign_bins <- function(height, B, method) {
  n <- length(height)
  if (n == 0L) return(integer(0))
  
  if (identical(method, "quantile")) {
    if (n >= B) {
      q <- stats::quantile(height, probs = seq(0, 1, length.out = B + 1), names = FALSE)
      if (length(unique(q)) == 1L) return(rep(1L, n))
      findInterval(height, q, all.inside = TRUE)
    } else {
      ranks <- rank(height, ties.method = "first")
      as.integer(ceiling(ranks * B / n))
    }
  } else {
    h_min <- 0.0
    h_max <- max(height, na.rm = TRUE)
    if (is.na(h_max) || h_max <= 0.0) h_max <- 10.0
    breaks <- seq(h_min, h_max, length.out = B + 1)
    findInterval(height, breaks, all.inside = TRUE)
  }
}

weighted_bin_summary <- function(sub_df) {
  sub_df %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      mortality = weighted_mean(mortality, weight),
      basal_area_total = sum(basal_area * density, na.rm = TRUE),
      basal_area_mean = weighted_mean(basal_area, weight),
      height_mean = weighted_mean(height, weight),
      diameter_mean = weighted_mean(diameter, weight),
      density = sum(density, na.rm = TRUE),
      .groups = "drop"
    )
}

backfill_bins <- function(bin_df, t_val, B) {
  full_bins <- data.frame(t = t_val, bin = 1:B, stringsAsFactors = FALSE)
  merged <- dplyr::left_join(full_bins, bin_df, by = "bin")
  for (col in c("density", "mortality", "basal_area_total", "basal_area_mean", "height_mean", "diameter_mean")) {
    merged[[col]][is.na(merged[[col]])] <- 0.0
  }
  merged
}

#' Create Size-Resolved Field Projection
#'
#' @param B Integer number of size classes.
#' @param method String, "quantile" or "linear".
#' @return A projection function that resolves cohort state into B size classes.
#' @export
size_field <- function(B, method = "quantile") {
  B <- as.integer(B)
  assert_scalar_character(method)
  
  fn <- function(df) {
    if (is.null(df) || nrow(df) == 0L) return(df)

    df$weight <- if ("density" %in% names(df)) df$density else 1.0

    res_list <- lapply(split(df, df$t), function(sub_df) {
      if (nrow(sub_df) == 0L) return(NULL)
      sub_df$bin <- assign_bins(sub_df$height, B, method)
      bin_df <- weighted_bin_summary(sub_df)
      backfill_bins(bin_df, sub_df$t[1L], B)
    })

    res_df <- dplyr::bind_rows(res_list)
    cols_order <- c("t", "bin", "density", "mortality", "basal_area_total", "basal_area_mean", "height_mean", "diameter_mean")
    res_df <- res_df[, cols_order]
    
    as.data.frame(res_df %>% dplyr::arrange(t, bin), stringsAsFactors = FALSE)
  }

  projection(fn, projection_version = sprintf("size_field_B%d_%s@v1", B, method), required_columns = c("height", "mortality", "log_density"))
}

# Coerce a projection id string or wrapped function into (fn, version).
resolve_projection <- function(proj) {
  if (is.character(proj)) return(list(fn = get_projection(proj), version = proj))
  if (is.function(proj)) {
    version <- attr(proj, "projection_version")
    if (is.null(version)) stop("proj must have a 'projection_version' attribute", call. = FALSE)
    return(list(fn = proj, version = version))
  }
  stop("'proj' must be a projection id string or projection function", call. = FALSE)
}

#' Get Projection from Pile
#'
#' Retrieves the projection of a run by fingerprint and projection id or function.
#' Computes and caches it on miss.
#'
#' @param fingerprint Character SHA-256 fingerprint.
#' @param proj Projection id string (e.g. "stand_summary@v1") or a wrapped projection function.
#' @param pile Pile object.
#' @return A data frame representing the projection.
#' @export
projection_of <- function(fingerprint, proj, pile = get_active_pile()) {
  assert_scalar_character(fingerprint)

  p <- resolve_projection(proj)
  proj_fn <- p$fn
  ns <- p$version

  hit <- pile_get(pile, fingerprint, ns)
  if (!is.null(hit)) {
    return(hit)
  }

  transformed <- pile_get(pile, fingerprint)
  proj_df <- proj_fn(transformed)
  
  if (!is.data.frame(proj_df)) {
    stop("Projection function must return a data.frame", call. = FALSE)
  }

  proj_df$run_fingerprint <- fingerprint

  # Delegate to pile_put for both writing the parquet and updating the storr index
  pile_put(pile, fingerprint, proj_df, storr_namespace = ns)
  as.data.frame(proj_df, stringsAsFactors = FALSE)
}

# Register built-ins at load time
register_projection("stand_summary@v1", stand_summary_fn, required_columns = c("height", "mortality", "fecundity", "area_heartwood", "mass_heartwood", "log_density", "offspring_produced_survival_weighted"))
register_projection("size_field@v1",    size_field(10L))

#' Bulk Project Runs
#'
#' Evaluates a projection across multiple runs simultaneously using Arrow
#' dataset pushdown for column selection and row filtering.
#'
#' @param fps Character vector of run fingerprints.
#' @param proj Projection id string or function.
#' @param model Model ID string (e.g. "FF16@v1").
#' @param pile Pile object.
#' @return A combined data frame.
#' @export
project_runs <- function(fps, proj, model, pile = get_active_pile()) {
  p <- resolve_projection(proj)
  proj_fn <- p$fn
  ns <- p$version
  
  raw_dir <- fs::path(pile$path, "raw", sprintf("model=%s", model))
  if (!fs::dir_exists(raw_dir)) return(data.frame())
  
  ds <- arrow::open_dataset(raw_dir, format = "parquet")
  ds <- ds |> dplyr::filter(run_fingerprint %in% fps)
  
  req_cols <- attr(proj_fn, "required_columns")
  if (!is.null(req_cols)) {
    base_cols <- c("run_fingerprint", "strategy_id", "cohort_id", "birth_time", "t", "bucket", "patch_density")
    select_cols <- unique(c(base_cols, req_cols))
    select_cols <- intersect(select_cols, names(ds))
    ds <- ds |> dplyr::select(dplyr::all_of(select_cols))
  }
  
  raw_df <- dplyr::collect(ds)
  if (nrow(raw_df) == 0L) return(data.frame())
  
  transform_fn <- get_transform(model)
  
  res_list <- lapply(split(raw_df, raw_df$run_fingerprint), function(sub_df) {
    tr_df <- transform_fn(sub_df)
    p_df <- proj_fn(tr_df)
    p_df$run_fingerprint <- sub_df$run_fingerprint[1L]
    p_df
  })
  
  final_df <- dplyr::bind_rows(res_list)
  
  rel_path <- as.character(fs::path("projections", sprintf("projection=%s", ns), part_filename()))
  final_path <- fs::path(pile$path, rel_path)
  
  fs::dir_create(fs::path_dir(final_path))
  tmp <- paste0(final_path, ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(final_df, tmp)
  file.rename(tmp, final_path)
  
  fps <- unique(final_df$run_fingerprint)
  relink_records(pile, ns, fps, rel_path, create = TRUE)
  
  as.data.frame(final_df, stringsAsFactors = FALSE)
}
