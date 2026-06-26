#' Assert that an input is a scalar character
#'
#' @param x Input to check.
#' @param name Variable name for error message.
#' @export
assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x) || length(x) != 1) {
    stop(sprintf("'%s' must be a scalar character", name), call. = FALSE)
  }
}

#' Null-coalescing operator
#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a
