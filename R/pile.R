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

# Resolve a record's file, repairing the index if it has gone missing.
# Returns the absolute path, or NULL after deleting the phantom record.
record_path <- function(pile, fingerprint, rec, namespace) {
  full <- file.path(pile$path, rec$path)
  if (file.exists(full)) return(full)
  pile$st$del(fingerprint, namespace = namespace)
  NULL
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

    path <- record_path(pile, fingerprint, rec, "index")
    if (is.null(path)) {
      stop(sprintf("Phantom index record detected and removed. Run %s will be re-executed.", fingerprint), call. = FALSE)
    }

    df <- as.data.frame(read_run(path, rec$row_selector), stringsAsFactors = FALSE)
    transform_fn <- get_transform(rec$model_id)
    return(transform_fn(df))
  }

  if (identical(rec$type, "inline")) {
    return(rec$value)
  }
  
  if (identical(rec$type, "parquet")) {
    path <- record_path(pile, fingerprint, rec, storr_namespace)
    if (is.null(path)) return(NULL)
    return(as.data.frame(read_run(path, rec$row_selector), stringsAsFactors = FALSE))
  }
  
  NULL
}

index_record <- function(pile, fingerprint, value, meta, attempts) {
  is_failure <- inherits(value, "PlantFailure")
  model_id <- meta$request$model_id %||% "FF16@v1"
  
  if (is_failure) {
    list(
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
    
    list(
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
}

projection_record <- function(pile, fingerprint, value, storr_namespace) {
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
    
    list(
      status = "done",
      type = "parquet",
      path = rel_file,
      row_selector = NULL
    )
  } else {
    list(
      status = "done",
      type = "inline",
      value = value
    )
  }
}

#' Put value into pile
pile_put <- function(pile, fingerprint, value, meta = NULL, storr_namespace = "index") {
  existing <- pile_get_record(pile, fingerprint, storr_namespace)
  if (!is.null(existing) && identical(existing$status, "done")) return(invisible(NULL))
  attempts <- (existing$attempts %||% 0L) + 1L

  rec <- if (storr_namespace == "index")
    index_record(pile, fingerprint, value, meta, attempts)
  else
    projection_record(pile, fingerprint, value, storr_namespace)

  pile$st$set(fingerprint, rec, namespace = storr_namespace)
  invisible(NULL)
}

#' Read a parquet file, optionally selecting a single run by fingerprint.
read_run <- function(path, row_selector = NULL) {
  if (is.null(row_selector)) {
    arrow::read_parquet(path)
  } else {
    ds <- arrow::open_dataset(path, format = "parquet")
    dplyr::collect(dplyr::filter(ds, run_fingerprint == !!row_selector$run_fingerprint))
  }
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

part_filename <- function() sprintf("part-%s.parquet", gsub("-", "", uuid::UUIDgenerate()))

prune_empty_buckets <- function(dir) {
  for (b in fs::dir_ls(dir, type = "directory", regexp = "bucket=")) {
    if (length(fs::dir_ls(b)) == 0L) fs::dir_delete(b)
  }
}

#' Point records at a compacted file
#' 
#' With create = FALSE, records that are absent or not "done" are skipped 
#' (index compaction). With create = TRUE, missing records are minted as 
#' parquet records (bulk projection).
relink_records <- function(pile, namespace, fps, rel_path, create = FALSE) {
  records <- pile$st$mget(fps, namespace = namespace)
  valid_idx <- if (create) {
    rep(TRUE, length(fps))
  } else {
    vapply(records, function(x) !is.null(x) && identical(x$status, "done"), logical(1))
  }
  
  if (!any(valid_idx)) return(invisible(NULL))
  
  valid_fps <- fps[valid_idx]
  valid_records <- records[valid_idx]
  
  for (i in seq_along(valid_records)) {
    if (is.null(valid_records[[i]])) {
      valid_records[[i]] <- list(status = "done", type = "parquet")
    }
    valid_records[[i]]$path <- rel_path
    valid_records[[i]]$row_selector <- list(run_fingerprint = valid_fps[i])
  }
  
  pile$st$mset(valid_fps, valid_records, namespace = namespace)
  invisible(NULL)
}

# Merge every per-run parquet under one partition into a single sorted file,
# then relink its index records. Raw runs and projections differ only in
# directory layout, sort keys, and namespace.
compact_partition <- function(pile, prefix, key, namespace, sort_cols) {
  dir <- fs::path(pile$path, prefix, key)
  if (!fs::dir_exists(dir)) return(invisible(NULL))
  
  run_files <- fs::dir_ls(dir, recurse = TRUE, regexp = "run-.*\\.parquet$")
  if (length(run_files) == 0L) return(invisible(NULL))
  
  ds <- arrow::open_dataset(run_files, format = "parquet")
  sort_cols <- intersect(sort_cols, names(ds$schema))
  if (length(sort_cols) > 0L) {
    ds <- ds |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  }
  
  df <- dplyr::collect(ds)
  if (nrow(df) == 0L) {
    fs::file_delete(run_files)
    return(invisible(NULL))
  }
  
  rel_path <- as.character(fs::path(prefix, key, part_filename()))
  final_path <- fs::path(pile$path, rel_path)
  
  fs::dir_create(fs::path_dir(final_path))
  tmp <- paste0(final_path, ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(df, tmp)
  file.rename(tmp, final_path)
  
  fps <- unique(df$run_fingerprint)
  relink_records(pile, namespace, fps, rel_path, create = FALSE)
  
  fs::file_delete(run_files)
  prune_empty_buckets(dir)
  invisible(NULL)
}

#' Compact All Runs for a Model
#'
#' @param pile A logpile_pile object.
#' @param model Model ID string.
#' @export
compact_pile <- function(pile, model) {
  compact_partition(pile, "raw", sprintf("model=%s", model), "index",
                    c("run_fingerprint", "strategy_id", "cohort_id", "t"))
}

#' Compact Projections
#'
#' @param pile A logpile_pile object.
#' @param projection_version Projection version string.
#' @export
compact_projections <- function(pile, projection_version) {
  compact_partition(pile, "projections", sprintf("projection=%s", projection_version),
                    projection_version,
                    c("run_fingerprint", "strategy_id", "cohort_id", "t", "bin"))
}

