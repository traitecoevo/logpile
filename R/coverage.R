empty_coords <- function()
  data.frame(fingerprint = character(0), status = character(0), stringsAsFactors = FALSE)

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
    return(empty_coords())
  }
  
  ds <- arrow::open_dataset(cov_dir, format = "parquet")
  df <- dplyr::collect(ds)
  
  if (nrow(df) == 0L) {
    return(empty_coords())
  }
  
  st <- pile$st
  if (exists("flush_cache", envir = st)) {
    st$flush_cache()
  }
  keys <- st$list(namespace = "index")
  
  valid_fps <- intersect(df$fingerprint, keys)
  if (length(valid_fps) > 0L) {
    records <- st$mget(valid_fps, namespace = "index")
    status_map <- vapply(records, function(x) as.character(x$status %||% NA_character_), character(1))
    
    match_idx <- match(df$fingerprint, valid_fps)
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
# Coerce a query (numeric vector, matrix, or data frame) to a double matrix
# with one column per design dimension.
query_matrix <- function(query, dims) {
  if (is.numeric(query) && is.vector(query)) query <- matrix(query, nrow = 1L)
  if (is.data.frame(query))
    query <- as.matrix(if (all(dims %in% names(query))) query[, dims, drop = FALSE] else query)
  if (!is.matrix(query))
    stop("Query must be a numeric vector, matrix, or data frame", call. = FALSE)
  if (ncol(query) != length(dims))
    stop(sprintf("Query has %d columns, but design space has %d dimensions",
                 ncol(query), length(dims)), call. = FALSE)
  storage.mode(query) <- "double"
  query
}

# Cached k-d tree, rebuilt when the design set has grown 2x or shrunk.
coverage_tree <- function(pile, model, ref_mat) {
  cache <- pile$cache
  key <- paste0("coverage_tree_", model)
  skey <- paste0("coverage_tree_size_", model)
  n <- nrow(ref_mat); size <- cache[[skey]] %||% 0L
  if (is.null(cache[[key]]) || n >= 2L * size || n < size) {
    cache[[key]] <- nabor::WKNND(ref_mat)
    cache[[skey]] <- n
  }
  cache[[key]]
}

knn <- function(query, k = 1L, model, pile = get_active_pile()) {
  ref_df <- coords(model, pile)
  if (nrow(ref_df) == 0L) {
    stop("Cannot run KNN query on an empty pile", call. = FALSE)
  }

  numeric_names <- names(ref_df)[vapply(ref_df, is.numeric, logical(1))]
  if (length(numeric_names) == 0L) {
    stop("No numeric design coordinates found in pile to perform KNN search", call. = FALSE)
  }

  ref_mat <- as.matrix(ref_df[, numeric_names, drop = FALSE])
  storage.mode(ref_mat) <- "double"
  
  query_mat <- query_matrix(query, numeric_names)
  tree <- coverage_tree(pile, model, ref_mat)
  
  res <- tree$query(query_mat, k = k, eps = 0, radius = 0)
  
  n_query <- nrow(query_mat)
  
  df_res <- data.frame(
    neighbor_rank = rep(seq_len(k), each = n_query),
    fingerprint = ref_df$fingerprint[res$nn.idx],
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

