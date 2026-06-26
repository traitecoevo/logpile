#' Extract Design Space Coordinates from Pile
#'
#' Scans the index namespace and returns a data frame of evaluated coordinates.
#'
#' @param model The model ID string (e.g. "FF16@v1").
#' @param pile A `logpile_pile` object.
#' @return A data frame of design coordinates and run statuses.
#' @export
coords <- function(model, pile = get_active_pile()) {
  assert_scalar_character(model)
  cov_dir <- fs::path(pile$path, "coverage", sprintf("model=%s", model))
  
  if (!fs::dir_exists(cov_dir)) {
    return(data.frame(run_fingerprint = character(0), status = character(0), stringsAsFactors = FALSE))
  }
  
  ds <- arrow::open_dataset(cov_dir, format = "parquet")
  df <- dplyr::collect(ds)
  
  if (nrow(df) == 0L) {
    return(data.frame(run_fingerprint = character(0), status = character(0), stringsAsFactors = FALSE))
  }
  
  st <- pile$st
  if (exists("flush_cache", envir = st)) {
    st$flush_cache()
  }
  keys <- st$list(namespace = "index")
  
  valid_fps <- intersect(df$run_fingerprint, keys)
  if (length(valid_fps) > 0L) {
    records <- st$mget(valid_fps, namespace = "index")
    status_map <- vapply(records, function(x) as.character(x$status %||% NA_character_), character(1))
    
    match_idx <- match(df$run_fingerprint, valid_fps)
    has_match <- !is.na(match_idx)
    df$status[has_match] <- status_map[match_idx[has_match]]
  }
  
  as.data.frame(df, stringsAsFactors = FALSE)
}

#' Coverage KNN Query
#'
#' Performs a nearest-neighbor search of a query point against the existing
#' design coordinates in the pile.
#'
#' @param query Numeric vector, matrix, or data frame query.
#' @param k Number of neighbors to return.
#' @param model The model ID string.
#' @param pile A `logpile_pile` object.
#' @return A list with `nn.idx` and `nn.dists`.
#' @export
knn <- function(query, k = 1L, model, pile = get_active_pile()) {
  ref_df <- coords(model, pile)
  if (nrow(ref_df) == 0L) {
    stop("Cannot run KNN query on an empty pile", call. = FALSE)
  }

  num_cols <- vapply(ref_df, is.numeric, logical(1))
  numeric_names <- names(ref_df)[num_cols]
  
  if (length(numeric_names) == 0L) {
    stop("No numeric design coordinates found in pile to perform KNN search", call. = FALSE)
  }

  if (is.vector(query) && is.numeric(query)) {
    query <- matrix(query, nrow = 1L)
  }
  
  if (is.matrix(query)) {
    if (ncol(query) != length(numeric_names)) {
      stop(sprintf("Query matrix has %d columns, but reference design space has %d numeric dimensions", ncol(query), length(numeric_names)), call. = FALSE)
    }
    query_mat <- query
  } else if (is.data.frame(query)) {
    missing_cols <- setdiff(numeric_names, names(query))
    if (length(missing_cols) > 0L) {
      if (ncol(query) != length(numeric_names)) {
        stop(sprintf("Query data frame does not contain columns: %s, and column count (%d) does not match reference (%d)", 
                     paste(missing_cols, collapse = ", "), ncol(query), length(numeric_names)), call. = FALSE)
      }
      query_mat <- as.matrix(query)
    } else {
      query_mat <- as.matrix(query[, numeric_names, drop = FALSE])
    }
  } else {
    stop("Query must be a numeric vector, matrix, or data frame", call. = FALSE)
  }

  ref_mat <- as.matrix(ref_df[, numeric_names, drop = FALSE])
  if (storage.mode(ref_mat) != "double") storage.mode(ref_mat) <- "double"
  if (storage.mode(query_mat) != "double") storage.mode(query_mat) <- "double"
  
  cache <- pile$cache
  tree_key <- paste0("coverage_tree_", model)
  size_key <- paste0("coverage_tree_size_", model)
  
  tree <- cache[[tree_key]]
  tree_size <- cache[[size_key]] %||% 0L
  
  n_ref <- nrow(ref_mat)
  if (is.null(tree) || n_ref >= 2L * tree_size || n_ref < tree_size) {
    tree <- nabor::WKNND(ref_mat)
    cache[[tree_key]] <- tree
    cache[[size_key]] <- n_ref
  }
  
  res <- tree$query(query_mat, k = k, eps = 0, radius = 0)
  
  n_query <- nrow(query_mat)
  
  df_res <- data.frame(
    neighbor_rank = rep(seq_len(k), each = n_query),
    run_fingerprint = ref_df$run_fingerprint[res$nn.idx],
    distance = as.vector(res$nn.dists),
    stringsAsFactors = FALSE
  )
  
  query_df <- if (is.data.frame(query)) {
    query
  } else {
    df <- as.data.frame(query_mat)
    names(df) <- numeric_names
    df
  }
  
  query_expanded <- query_df[rep(seq_len(n_query), times = k), , drop = FALSE]
  rownames(query_expanded) <- NULL
  df_res <- cbind(query_expanded, df_res)
  
  df_res <- df_res[order(rep(seq_len(n_query), times = k), df_res$neighbor_rank), ]
  rownames(df_res) <- NULL
  
  df_res
}

#' Identify Coverage Gaps
#'
#' Ranks a set of candidate points by their distance to the nearest
#' existing coordinate in the pile (largest gap first).
#'
#' @param candidates Candidate coordinates to evaluate.
#' @param n Number of gap points to return.
#' @param model The model ID string.
#' @param pile A `logpile_pile` object.
#' @return The top candidates with a `gap_distance` attribute.
#' @export
gap <- function(candidates, n = 1L, model, pile = get_active_pile()) {
  res <- knn(candidates, k = 1L, model = model, pile = pile)
  
  n <- min(as.integer(n), nrow(res))
  top_indices <- order(res$distance, decreasing = TRUE)[seq_len(n)]
  
  res_top <- res[top_indices, , drop = FALSE]
  rownames(res_top) <- NULL
  
  res_top$gap_distance <- res_top$distance
  res_top$distance <- NULL
  res_top$neighbor_rank <- NULL
  
  res_top
}

