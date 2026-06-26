.pile_env <- new.env(parent = emptyenv())

#' Set Active Pile
#'
#' Sets the global active pile object.
#'
#' @param pile A logpile_pile object.
#' @export
set_active_pile <- function(pile) {
  .pile_env$active_pile <- pile
}

#' Get Active Pile
#'
#' Retrieves the global active pile object.
#'
#' @return The active logpile_pile object.
#' @export
get_active_pile <- function() {
  .pile_env$active_pile %||% stop("No active pile set. Call set_active_pile() first.", call. = FALSE)
}

#' Create a Logpile Pile Interface
#'
#' Initializes the storage directory structure and returns
#' a handle encapsulating the storr index database.
#'
#' @param path Absolute or relative path to the pile root directory.
#' @return A `logpile_pile` object.
#' @export
create_pile <- function(path) {
  path <- fs::path_abs(path)

  # Ensure index path exists
  index_path <- file.path(path, "index")
  fs::dir_create(index_path)

  if (!requireNamespace("thor", quietly = TRUE)) {
    stop("Package 'thor' must be installed", call. = FALSE)
  }
  # Set a 1TB mapsize. LMDB memory-maps this but does not allocate it physically.
  env <- thor::mdb_env(index_path, mapsize = 1099511627776)
  st <- thor::storr_thor(env, default_namespace = "index")

  pile <- structure(
    list(
      path = path,
      st = st,
      cache = new.env(parent = emptyenv())
    ),
    class = "logpile_pile"
  )
  pile
}

#' Check if pile has fingerprint
pile_has <- function(pile, fingerprint, storr_namespace = "index") {
  pile$st$exists(fingerprint, namespace = storr_namespace)
}

pile_get_record <- function(pile, fingerprint, storr_namespace = "index") {
  if (!pile_has(pile, fingerprint, storr_namespace)) {
    return(NULL)
  }
  pile$st$get(fingerprint, namespace = storr_namespace)
}

#' Get log from pile
pile_get <- function(pile, fingerprint, storr_namespace = "index") {
  rec <- pile_get_record(pile, fingerprint, storr_namespace)
  if (is.null(rec)) {
    if (storr_namespace == "index") {
      stop(sprintf("Fingerprint '%s' not found in pile index", fingerprint), call. = FALSE)
    }
    return(NULL)
  }

  if (storr_namespace == "index") {
    if (identical(rec$status, "failed")) {
      stop(sprintf("Simulation for fingerprint '%s' failed: %s", fingerprint, rec$failure$message), call. = FALSE)
    }

    full_path <- file.path(pile$path, rec$path)
    if (!file.exists(full_path)) {
      pile$st$del(fingerprint, namespace = "index")
      stop(sprintf("Phantom index record detected and removed. Run %s will be re-executed.", fingerprint), call. = FALSE)
    }

    if (is.null(rec$row_selector)) {
      res <- arrow::read_parquet(full_path)
    } else {
      ds <- arrow::open_dataset(full_path, format = "parquet")
      ds <- ds |> dplyr::filter(run_fingerprint == !!rec$row_selector$run_fingerprint)
      res <- dplyr::collect(ds)
    }

    df <- as.data.frame(res, stringsAsFactors = FALSE)
    transform_fn <- get_transform(rec$model_id)
    return(transform_fn(df))
  }

  if (identical(rec$type, "inline")) {
    return(rec$value)
  }
  
  if (identical(rec$type, "parquet")) {
    full_path <- file.path(pile$path, rec$path)
    if (!file.exists(full_path)) {
      return(NULL)
    }
    if (is.null(rec$row_selector)) {
      res <- arrow::read_parquet(full_path)
    } else {
      ds <- arrow::open_dataset(full_path, format = "parquet")
      ds <- ds |> dplyr::filter(run_fingerprint == !!rec$row_selector$run_fingerprint)
      res <- dplyr::collect(ds)
    }
    return(as.data.frame(res, stringsAsFactors = FALSE))
  }
  
  NULL
}

#' Put value into pile
pile_put <- function(pile, fingerprint, value, meta = NULL, storr_namespace = "index") {
  existing <- pile_get_record(pile, fingerprint, storr_namespace)
  if (!is.null(existing) && identical(existing$status, "done")) {
    return(invisible(NULL))
  }
  attempts <- if (!is.null(existing) && !is.null(existing$attempts)) existing$attempts + 1L else 1L

  if (storr_namespace == "index") {
    is_failure <- inherits(value, "PlantFailure")
    model_id <- meta$request$model_id %||% "FF16@v1"
    
    if (is_failure) {
      rec <- list(
        status = "failed",
        design_coords = meta$design_coords,
        build_hash = meta$build_hash,
        failure = list(reason = value$reason, message = value$message, last_t = value$last_t),
        model_id = model_id,
        attempts = attempts
      )
    } else {
      bucket <- substr(fingerprint, 1, 2)
      
      rel_file <- write_parquet(
        df = value,
        pile_path = pile$path,
        prefix = "raw",
        model = model_id,
        bucket = bucket,
        filename = sprintf("run-%s-0.parquet", fingerprint)
      )
      
      rec <- list(
        status = "done",
        path = rel_file,
        design_coords = meta$design_coords,
        build_hash = meta$build_hash,
        runtime_seconds = meta$runtime_seconds,
        solver_steps = meta$solver_steps,
        model_id = model_id,
        attempts = attempts
      )
    }
  } else {
    if (is.data.frame(value)) {
      bucket <- substr(fingerprint, 1, 2)
      rel_file <- write_parquet(
        df = value,
        pile_path = pile$path,
        prefix = "projections",
        projection = storr_namespace,
        bucket = bucket,
        filename = sprintf("run-%s-0.parquet", fingerprint)
      )
      
      rec <- list(
        status = "done",
        type = "parquet",
        path = rel_file,
        row_selector = NULL
      )
    } else {
      rec <- list(
        status = "done",
        type = "inline",
        value = value
      )
    }
  }
  
  pile$st$set(fingerprint, rec, namespace = storr_namespace)
  invisible(NULL)
}


#' Put driver
#' @export
put_driver <- function(pile, driver_set) {
  stop("Driver implementation deferred", call. = FALSE)
}

#' Get driver
#' @export
get_driver <- function(pile, hash) {
  stop("Driver implementation deferred", call. = FALSE)
}

#' Construct a Hive Partition Path
partition_path <- function(prefix, ...) {
  args <- list(...)
  parts <- vapply(names(args), function(nm) paste0(nm, "=", args[[nm]]), character(1))
  as.character(do.call(fs::path, c(list(prefix), as.list(parts))))
}

#' Write Parquet Atomically to a Partitioned Path
#' 
#' Writes a data frame atomically to a hive-partitioned directory structure,
#' creating directories as needed and returning the relative path.
#' 
#' @param df Data frame to write.
#' @param pile_path Base path of the pile.
#' @param prefix Top-level directory (e.g. "raw" or "projections").
#' @param ... Named arguments forming the hive partitions.
#' @param filename Name of the final file.
#' @return The relative path to the written file.
write_parquet <- function(df, pile_path, prefix, ..., filename) {
  rel_path <- fs::path(partition_path(prefix, ...), filename)
  final_path <- fs::path(pile_path, rel_path)
  
  fs::dir_create(fs::path_dir(final_path))
  
  tmp <- paste0(final_path, ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(df, tmp)
  if (!file.rename(tmp, final_path)) {
    stop(sprintf("Failed to rename temporary file to '%s'", final_path), call. = FALSE)
  }
  as.character(rel_path)
}

#' Compact All Runs for a Model
#'
#' @param pile A logpile_pile object.
#' @param model Model ID string.
#' @export
compact_pile <- function(pile, model) {
  model_dir <- fs::path(pile$path, "raw", sprintf("model=%s", model))
  if (!fs::dir_exists(model_dir)) return(invisible(NULL))
  
  run_files <- fs::dir_ls(model_dir, recurse = TRUE, regexp = "run-.*\\.parquet$")
  if (length(run_files) == 0L) return(invisible(NULL))
  
  ds <- arrow::open_dataset(run_files, format = "parquet")
  
  sort_cols <- intersect(c("run_fingerprint", "strategy_id", "cohort_id", "t"), names(ds$schema))
  if (length(sort_cols) > 0L) {
    ds <- ds |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  }
  
  df <- dplyr::collect(ds)
  if (nrow(df) == 0L) {
    fs::file_delete(run_files)
    return(invisible(NULL))
  }
  
  uuid_str <- gsub("-", "", uuid::UUIDgenerate())
  part_name <- sprintf("part-%s.parquet", uuid_str)
  rel_path <- as.character(fs::path("raw", sprintf("model=%s", model), part_name))
  final_path <- fs::path(pile$path, rel_path)
  
  arrow::write_parquet(df, final_path)
  
  fps <- unique(df$run_fingerprint)
  records <- pile$st$mget(fps, namespace = "index")
  
  valid_idx <- vapply(records, function(x) !is.null(x) && identical(x$status, "done"), logical(1))
  
  if (any(valid_idx)) {
    valid_fps <- fps[valid_idx]
    valid_records <- records[valid_idx]
    for (i in seq_along(valid_records)) {
      valid_records[[i]]$path <- rel_path
      valid_records[[i]]$row_selector <- list(run_fingerprint = valid_fps[i])
    }
    pile$st$mset(valid_fps, valid_records, namespace = "index")
  }
  
  fs::file_delete(run_files)
  
  buckets <- fs::dir_ls(model_dir, type = "directory", regexp = "bucket=")
  for (b in buckets) {
    if (length(fs::dir_ls(b)) == 0L) fs::dir_delete(b)
  }
  
  invisible(NULL)
}

#' Compact Projections
#'
#' @param pile A logpile_pile object.
#' @param projection_version Projection version string.
#' @export
compact_projections <- function(pile, projection_version) {
  proj_dir <- fs::path(pile$path, "projections", sprintf("projection=%s", projection_version))
  if (!fs::dir_exists(proj_dir)) return(invisible(NULL))
  
  run_files <- fs::dir_ls(proj_dir, recurse = TRUE, regexp = "run-.*\\.parquet$")
  if (length(run_files) == 0L) return(invisible(NULL))
  
  ds <- arrow::open_dataset(run_files, format = "parquet")
  
  sort_cols <- intersect(c("run_fingerprint", "strategy_id", "cohort_id", "t", "bin"), names(ds$schema))
  if (length(sort_cols) > 0L) {
    ds <- ds |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  }
  
  df <- dplyr::collect(ds)
  if (nrow(df) == 0L) {
    fs::file_delete(run_files)
    return(invisible(NULL))
  }
  
  uuid_str <- gsub("-", "", uuid::UUIDgenerate())
  part_name <- sprintf("part-%s.parquet", uuid_str)
  rel_path <- as.character(fs::path("projections", sprintf("projection=%s", projection_version), part_name))
  final_path <- fs::path(pile$path, rel_path)
  
  arrow::write_parquet(df, final_path)
  
  fps <- unique(df$run_fingerprint)
  records <- pile$st$mget(fps, namespace = projection_version)
  
  valid_idx <- vapply(records, function(x) !is.null(x) && identical(x$status, "done"), logical(1))
  
  if (any(valid_idx)) {
    valid_fps <- fps[valid_idx]
    valid_records <- records[valid_idx]
    for (i in seq_along(valid_records)) {
      valid_records[[i]]$path <- rel_path
      valid_records[[i]]$row_selector <- list(run_fingerprint = valid_fps[i])
    }
    pile$st$mset(valid_fps, valid_records, namespace = projection_version)
  }
  
  fs::file_delete(run_files)
  
  buckets <- fs::dir_ls(proj_dir, type = "directory", regexp = "bucket=")
  for (b in buckets) {
    if (length(fs::dir_ls(b)) == 0L) fs::dir_delete(b)
  }
  
  invisible(NULL)
}

