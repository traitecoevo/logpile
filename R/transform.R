.transform_registry <- new.env(parent = emptyenv())

#' Register a transform Function
#'
#' @param model_prefix Character prefix of the model, e.g. "FF16".
#' @param fn A function mapping a raw log to an enriched log.
register_transform <- function(model_prefix, fn) {
  if (!is.character(model_prefix) || length(model_prefix) != 1L) {
    stop("'model_prefix' must be a scalar character", call. = FALSE)
  }
  if (!is.function(fn)) {
    stop("'fn' must be a function", call. = FALSE)
  }
  .transform_registry[[model_prefix]] <- fn
  invisible(model_prefix)
}

#' Get transform Function for a Model ID
#'
#' @param model_id Character model identifier, e.g. "FF16@v1".
#' @return A transform function (or identity if none registered).
get_transform <- function(model_id) {
  if (is.null(model_id)) {
    return(identity)
  }
  if (!is.character(model_id) || length(model_id) != 1L) {
    stop("'model_id' must be a scalar character", call. = FALSE)
  }
  prefix <- parse_model_id(model_id)$model
  .transform_registry[[prefix]] %||% identity
}

#' transform FF16 Log
#'
#' Enriches an FF16 raw log with density, basal area, and diameter.
#'
#' @param log A raw log data frame.
#' @return Enriched data frame.
transform_ff16 <- function(log) {
  if (is.null(log) || nrow(log) == 0L) {
    return(log)
  }
  
  if (!("density" %in% names(log))) {
    log$density <- if ("log_density" %in% names(log)) exp(log$log_density) else 1.0
  }
  
  # Map height to a realistic diameter/basal area (since FF16 sapwood area is microscopic)
  log$diameter <- 0.01 * (log$height %||% 1.0)^1.4
  log$basal_area <- pi * (log$diameter / 2.0)^2
  
  log
}

# Register default FF16 transform function
register_transform("FF16", transform_ff16)
