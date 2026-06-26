#' Evaluate a Request Idempotently
#'
#' Checks if a request is already processed, and if not, runs and commits it.
#'
#' @param req A request list.
#' @param pile A `logpile_pile` object.
#' @return A character fingerprint (invisibly).
evaluate_request <- function(req, pile) {
  fp <- request_fingerprint(req)
  if (pile_has(pile, fp)) return(invisible(fp))

  res <- run_plant(req)
  meta <- list(
    request = req, 
    design_coords = attr(req, "design_coords"),
    build_hash = get_build_hash()
  )
  
  if (inherits(res, "PlantFailure")) {
    payload <- res
  } else {
    payload <- res$log
    meta$runtime_seconds <- res$runtime_seconds
    meta$solver_steps <- res$solver_steps
  }
  
  pile_put(pile, fp, payload, meta)
  invisible(fp)
}

.run_env <- new.env(parent = emptyenv())

get_build_hash <- function() {
  if (is.null(.run_env$build_hash)) {
    .run_env$build_hash <- sprintf("R-%s_plant-%s", getRversion(), utils::packageVersion("plant"))
  }
  .run_env$build_hash
}

write_coverage <- function(pile, model_id, coords, fps) {
  cov_df <- coords
  cov_df$log_q <- NULL
  cov_df$run_fingerprint <- fps
  cov_df$status <- "pending"
  
  uuid_str <- gsub("-", "", uuid::UUIDgenerate())
  cov_file <- fs::path(pile$path, "coverage", sprintf("model=%s", model_id), sprintf("part-%s.parquet", uuid_str))
  fs::dir_create(fs::path_dir(cov_file))
  arrow::write_parquet(cov_df, cov_file)
}

#' Run Campaign Manifest Idempotently
#'
#' Evaluates a manifest of campaigns, bypassing done runs.
#'
#' @param manifest A manifest list.
#' @param pile A `logpile_pile` object.
#' @param workers Integer number of parallel workers.
#' @return A character vector of request fingerprints (invisibly).
#' @export
run <- function(manifest, pile = get_active_pile(), workers = 1L) {
  if (!is.list(manifest) || is.null(manifest$template) || is.null(manifest$coords)) {
    stop("Input must be a manifest with $template and $coords.", call. = FALSE)
  }

  n <- nrow(manifest$coords)
  if (n == 0L) return(invisible(character(0)))
  
  fps <- vapply(seq_len(n), function(i) {
    req <- place_coords(manifest$template, manifest$coords[i, , drop = FALSE])
    request_fingerprint(req)
  }, character(1))
  
  cached <- vapply(fps, function(fp) pile_has(pile, fp), logical(1))
  if (all(cached)) {
    return(invisible(fps))
  }
  
  pending_idx <- which(!cached)
  model_id <- manifest$template$model_id %||% "FF16@v1"
  
  write_coverage(pile, model_id, manifest$coords[pending_idx, , drop = FALSE], fps[pending_idx])
  
  manifest$coords <- manifest$coords[pending_idx, , drop = FALSE]
  n_pending <- nrow(manifest$coords)
  
  if (workers == 1L) {
    for (i in seq_len(n_pending)) {
      req <- place_coords(manifest$template, manifest$coords[i, , drop = FALSE])
      evaluate_request(req, pile)
    }
  } else {
    map_workers(manifest, pile, workers)
  }
  
  if (exists("flush_cache", envir = pile$st)) {
    pile$st$flush_cache()
  }
  
  message(sprintf("Compacting parquet files for %s...", model_id))
  compact_pile(pile, model_id)
  
  invisible(fps)
}

# --- Internal Parallel Helpers ---

partition_vector <- function(x, n_chunks) {
  n <- length(x)
  if (n == 0L) return(list())
  n_chunks <- min(as.integer(n_chunks), n)
  unname(split(x, ((seq_len(n) - 1L) %% n_chunks) + 1L))
}

# Walk upward from `start` until `marker` is found; NULL if never.
find_up <- function(marker, start) {
  dir <- fs::path_abs(start)
  while (!file.exists(file.path(dir, marker)) && dir != fs::path_dir(dir))
    dir <- fs::path_dir(dir)
  if (file.exists(file.path(dir, marker))) dir else NULL
}

get_pkg_root <- function() {
  pkg <- find.package("logpile", quiet = TRUE)
  if (length(pkg) && file.exists(file.path(pkg, "DESCRIPTION"))) return(pkg)
  find_up("DESCRIPTION", getwd())
}

fallback_log_crash <- function(req, pile, err_msg) {
  h <- request_fingerprint(req)
  if (!pile_has(pile, h)) {
    meta <- list(
      request = req, 
      design_coords = attr(req, "design_coords"),
      build_hash = get_build_hash()
    )
    pile_put(pile, h, PlantFailure("crash", sprintf("Worker died: %s", err_msg)), meta)
  }
}

#' Map Workers
#' 
#' Uses crew to distribute requests across multiple R sessions.
#' 
#' @param manifest A manifest object
#' @param pile A pile object
#' @param workers Integer workers
#' @keywords internal
map_workers <- function(manifest, pile, workers) {
  if (!requireNamespace("crew", quietly = TRUE)) {
    stop("Package 'crew' must be installed for multi-worker execution", call. = FALSE)
  }
  
  n <- nrow(manifest$coords)
  chunks <- partition_vector(seq_len(n), workers * 4L)
  
  controller <- crew::crew_controller_local(
    name = sprintf("logpile_%s", tempfile("crew_")),
    workers = workers
  )
  controller$start()
  on.exit(controller$terminate(), add = TRUE)

  results <- controller$map(
    command = {
      if (!is.null(pkg_root)) {
        pkgload::load_all(pkg_root, export_all = TRUE, attach_testthat = FALSE, quiet = TRUE)
      } else {
        library(logpile)
      }
      if (!exists(".worker_pile", envir = globalenv())) {
        assign(".worker_pile", create_pile(pile_path), envir = globalenv())
      }
      worker_pile <- get(".worker_pile", envir = globalenv())
      
      for (i in chunk) {
        req <- place_coords(template, coords[i, , drop = FALSE])
        tryCatch(evaluate_request(req, worker_pile), error = function(e) {
          fallback_log_crash(req, worker_pile, conditionMessage(e))
        })
      }
      TRUE
    },
    iterate = list(chunk = chunks, chunk_idx = as.character(seq_along(chunks))),
    data = list(pile_path = pile$path, pkg_root = get_pkg_root(), 
                template = manifest$template, coords = manifest$coords),
    names = "chunk_idx",
    error = "silent"
  )

  crashes <- results[!is.na(results$error) & results$error != "", , drop = FALSE]
  if (nrow(crashes) > 0L) {
    for (i in seq_len(nrow(crashes))) {
      err_msg <- crashes$error[i]
      chunk_idx <- as.integer(crashes$name[i])
      for (idx in chunks[[chunk_idx]]) {
        req <- place_coords(manifest$template, manifest$coords[idx, , drop = FALSE])
        h <- request_fingerprint(req)
        if (!pile_has(pile, h)) {
          fallback_log_crash(req, pile, err_msg)
          break
        }
      }
    }
  }
  invisible(NULL)
}
