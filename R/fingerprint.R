#' Normalize R Objects for Canonical CBOR Encoding
#'
#' Recursively sorts map keys, normalizes float `-0.0` to `0.0`,
#' and rejects `NaN`, `Inf`, and `NA` values in request payloads.
#'
#' @param x An R object.
#' @return A normalized R object.
normalize_for_cbor <- function(x) {
  if (is.matrix(x)) {
    x <- lapply(seq_len(nrow(x)), function(i) as.double(x[i, ]))
  }
  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm) && any(nm != "")) {
      x <- x[order(nm, method = "radix")]
    }
    return(lapply(x, normalize_for_cbor))
  }
  if (is.numeric(x)) {
    if (length(x) > 0) {
      if (any(is.na(x) | is.infinite(x) | is.nan(x))) {
        stop("NA/NaN/Inf not permitted in canonical identity requests", call. = FALSE)
      }
      if (is.double(x)) {
        x[x == 0.0] <- 0.0
      }
    }
  }
  x
}

#' Assert secretbase emits FP64 for doubles
#'
#' secretbase 1.3.0 always emits CBOR major-type-7 additional-info-27
#' (0xfb, 9 bytes) for R doubles. Call once at package load.
#' @keywords internal
assert_cbor_fp64 <- function() {
  raw <- secretbase::cborenc(1.0)
  if (length(raw) != 9L || raw[1L] != as.raw(0xfb)) {
    stop(
      "secretbase::cborenc() did not emit fixed FP64 for 1.0. ",
      "The pile's canonical identity requires every double to encode as ",
      "exactly 9 bytes (0xfb + 8 IEEE-754 bytes). ",
      "secretbase version: ", utils::packageVersion("secretbase"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  assert_cbor_fp64()
}

#' Sort Strategies and Paired Schedule Entries
#'
#' Sorts the strategies list and their corresponding schedule entries
#' by the SHA-256 of their paired canonical CBOR representation.
#'
#' @param strategies A list of strategy trait lists.
#' @param schedule A list of node schedule numeric vectors.
#' @return A list containing sorted `strategies` and `schedule`.
sort_strategies_and_schedule <- function(strategies, schedule) {
  n <- length(strategies)
  if (n <= 1L) {
    return(list(strategies = strategies, schedule = schedule))
  }
  if (length(schedule) != n) {
    stop("strategies and schedule must have the same length", call. = FALSE)
  }

  keys <- vapply(seq_len(n), function(i) {
    pair <- normalize_for_cbor(list(sched = schedule[[i]], strat = strategies[[i]]))
    secretbase::sha256(secretbase::cborenc(pair), convert = TRUE)
  }, character(1L))

  ord <- order(keys, method = "radix")
  list(strategies = strategies[ord], schedule = schedule[ord])
}

#' Parse Request Model ID
#'
#' Accepts either a bare model name (`"FF16"`) or a fully-qualified id carrying
#' a scientific version (`"FF16@v1"`). The version may be compound for a model
#' defined relative to another — e.g. `"TF24f@v2.1"`, where TF24f is a fast
#' approximation of TF24 `v2`. The scientific version is authored in the `plant`
#' package (see [resolve_request()], which re-derives it); a bare name returns
#' `version = NA` and is resolved against `plant`.
#'
#' @param mid Model ID string (e.g. "FF16", "FF16@v1", or "TF24f@v2.1").
#' @return A list with `model` and `version` (a version string such as `"1"` or
#'   `"2.1"`, or `NA` when the id carries no `@vX` suffix) fields.
#' @keywords internal
parse_model_id <- function(mid) {
  if (!is.character(mid) || length(mid) != 1L) {
    stop("Model ID must be a single string.", call. = FALSE)
  }

  if (!grepl("@", mid, fixed = TRUE)) {
    # bare model name, no version pinned
    if (!nzchar(mid)) {
      stop(sprintf("Unsupported model ID format: '%s'. Expected 'Model' or 'Model@vX'.", mid), call. = FALSE)
    }
    return(list(model = mid, version = NA_character_))
  }

  parts <- strsplit(mid, "@v", fixed = TRUE)[[1L]]
  if (length(parts) != 2L || !nzchar(parts[1L]) || !nzchar(parts[2L])) {
    stop(sprintf("Unsupported model ID format: '%s'. Expected 'Model' or 'Model@vX'.", mid), call. = FALSE)
  }

  model <- parts[1L]
  version <- parts[2L]

  # One or more dot-separated integers, leading component >= 1 (e.g. "1", "2.1").
  if (!grepl("^[1-9][0-9]*(\\.[0-9]+)*$", version)) {
    stop(sprintf("Unsupported model ID: '%s'. Version must be a positive integer, optionally dot-separated (e.g. '2.1').", mid), call. = FALSE)
  }

  list(model = model, version = version)
}

#' Scientific version of a model, from the installed `plant`
#'
#' The scientific version is the single source of truth in `plant`
#' (`plant::model_version()`); logpile reads it so cache invalidation tracks
#' scientific changes rather than software releases.
#'
#' @param model Bare model name, e.g. "FF16".
#' @return The scientific version string reported by `plant` (e.g. `"1"` or,
#'   for a compound version, `"2.1"`).
#' @keywords internal
plant_model_version <- function(model) {
  if (!exists("model_version", where = asNamespace("plant"), mode = "function")) {
    stop(
      "The installed 'plant' does not provide model_version(); ",
      "update plant to a version with per-model scientific versioning.",
      call. = FALSE
    )
  }
  plant::model_version(model)
}

is_resolved_request <- function(x) {
  if (!is.list(x) || is.null(x$model_id)) return(FALSE)
  tryCatch({
    parse_model_id(x$model_id)
    TRUE
  }, error = function(e) FALSE)
}

#' Canonical CBOR Encoding
#'
#' Recursively sorts named list keys, normalizes float -0.0 to 0.0,
#' and produces a definite-length CBOR representation.
#'
#' @param x An R object.
#' @return A raw vector representing the canonical CBOR.
canonical_cbor <- function(x) {
  if (is_resolved_request(x)) {
    ord <- sort_strategies_and_schedule(x$strategies, x$schedule)
    x$strategies <- ord$strategies
    x$schedule <- ord$schedule
  }

  secretbase::cborenc(normalize_for_cbor(x))
}

#' Compute Request Fingerprint
#'
#' Computes the SHA-256 hash of the canonical CBOR representation of a resolved request.
#'
#' @param request A list representing the request.
#' @return A 64-character hex string SHA-256 fingerprint.
#' @export
request_fingerprint <- function(request) {
  if (!is_resolved_request(request)) {
    stop("request_fingerprint requires a resolved request", call. = FALSE)
  }
  secretbase::sha256(canonical_cbor(request), convert = TRUE)
}

#' Compute Driver Set Hash
#'
#' Computes the SHA-256 hash of the canonical CBOR representation of a driver set.
#'
#' @param driver_set A named list of spline knots.
#' @return A 64-character hex string SHA-256 hash.
driver_set_hash <- function(driver_set) {
  raw_cbor <- canonical_cbor(driver_set)
  secretbase::sha256(raw_cbor, convert = TRUE)
}

#' Retrieve Model Schema and Defaults from plant Namespace
#'
#' @param model_type Character string for model (e.g., "FF16", "TF24").
#' @return A list containing base parameters, strategy defaults, and hyperparameter names.
#' @keywords internal
get_model_schema <- function(model_type) {
  p0 <- plant::scm_base_parameters(model_type)
  s0_obj <- get(paste0(model_type, "_Strategy"), asNamespace("plant"))()
  
  s0 <- as.list(s0_obj)
  if (!is.null(s0$pars)) {
    for (nm in names(s0$pars)) {
      s0[[nm]] <- s0$pars[[nm]]
    }
    s0$pars <- NULL
  }

  make_hp_fun <- tryCatch(plant::make_hyperpar(model_type), error = function(e) NULL)
  hp_names <- if (!is.null(make_hp_fun)) names(formals(make_hp_fun)) else character(0)

  list(p0 = p0, s0 = s0, hp_names = hp_names)
}

#' Extract length-1 numeric or logical parameters from a list
#' @param lst A list of parameters.
#' @return A list of scalar parameters.
#' @keywords internal
extract_scalars <- function(lst) {
  lst <- as.list(lst)
  is_scalar <- vapply(lst, function(x) length(x) == 1L && (is.numeric(x) || is.logical(x)), logical(1L))
  lst[is_scalar]
}

#' Safely Cast and Merge Input Values with Defaults
#'
#' @param target Sparse user input list.
#' @param defaults Default configuration list.
#' @param allowed_extra Vector of valid extra parameter names.
#' @param context_name Context string for error reporting.
#' @return A type-safe representation of the parameters.
#' @keywords internal
merge_and_cast <- function(target, defaults, allowed_extra = character(0), context_name = "list") {
  target <- target %||% list()
  if (!is.list(target)) {
    stop(sprintf("'%s' must be a list", context_name), call. = FALSE)
  }

  valid_names <- c(names(defaults), allowed_extra)
  extra <- setdiff(names(target), valid_names)
  if (length(extra) > 0) {
    stop(sprintf("Unknown fields in %s: %s", context_name, paste(extra, collapse = ", ")), call. = FALSE)
  }

  res <- lapply(stats::setNames(valid_names, valid_names), function(nm) {
    val <- target[[nm]] %||% defaults[[nm]]
    if (is.null(val)) return(NULL)

    ref <- defaults[[nm]]
    if (is.null(ref)) return(as.double(val))
    if (is.logical(ref)) return(as.logical(val))
    if (is.integer(ref)) return(as.integer(val))
    if (is.character(ref)) return(as.character(val))
    as.double(val)
  })

  res[!vapply(res, is.null, logical(1L))]
}

#' Resolve Global Parameters
#'
#' @param req_global Request global list.
#' @param schema Model schema list.
#' @return A resolved global settings list.
#' @keywords internal
resolve_global <- function(req_global, schema) {
  defaults <- extract_scalars(schema$p0)
  defaults$collect_all_auxiliary <- TRUE
  merge_and_cast(req_global, defaults, schema$hp_names, "global")
}

#' Resolve Control Settings
#'
#' @param req_control Request control list.
#' @param schema Model schema list.
#' @return A resolved control settings list.
#' @keywords internal
resolve_control <- function(req_control, schema) {
  defaults <- as.list(schema$s0$control)
  defaults$refine_schedule <- FALSE
  defaults$timeout <- 0.0
  merge_and_cast(req_control, defaults, context_name = "control")
}

#' Resolve Strategy Parameter Maps
#'
#' @param req_strategies Request strategies list.
#' @param schema Model schema list.
#' @return A resolved list of strategy parameter maps.
#' @keywords internal
resolve_strategies <- function(req_strategies, schema) {
  req_strategies <- req_strategies %||% list(list())
  if (!is.list(req_strategies)) {
    stop("'strategies' must be a list of strategy parameter maps", call. = FALSE)
  }

  defaults <- extract_scalars(schema$s0)
  if (!is.null(schema$s0$pars)) {
    defaults <- c(defaults, extract_scalars(schema$s0$pars))
  }
  defaults$collect_all_auxiliary <- TRUE

  lapply(req_strategies, merge_and_cast, defaults = defaults, allowed_extra = schema$hp_names, context_name = "strategy")
}

#' Resolve Node Schedule
#'
#' @param req_schedule Request schedule.
#' @param n_strats Integer count of strategy parameter maps.
#' @param max_lifetime Numeric maximum patch lifetime.
#' @return A resolved node schedule list.
#' @keywords internal
resolve_schedule <- function(req_schedule, n_strats, max_lifetime) {
  default_times <- plant::node_schedule_times_default(max_lifetime)
  if (is.null(req_schedule)) {
    return(lapply(seq_len(n_strats), function(i) as.double(default_times)))
  }

  if (!is.list(req_schedule)) {
    req_schedule <- list(req_schedule)
  }
  if (length(req_schedule) != n_strats) {
    req_schedule <- rep_len(req_schedule, n_strats)
  }

  lapply(req_schedule, function(times) {
    times_f <- as.double(times)
    times_f <- times_f[times_f <= max_lifetime]
    if (length(times_f) == 0L) 0.0 else times_f
  })
}

#' Resolve Driver Sets
#'
#' @param req_drivers Request drivers.
#' @return A resolved driver representation (hash string or nested list).
#' @keywords internal
resolve_drivers <- function(req_drivers) {
  if (is.null(req_drivers)) return(character(0))
  if (is.character(req_drivers) && length(req_drivers) == 0L) return(character(0))
  if (is.character(req_drivers) && length(req_drivers) == 1L) return(req_drivers)
  stop("'drivers' must be a single hash string (pointer). Use put_driver(pile, ...) first.", call. = FALSE)
}

#' Resolve and Validate a Request against the plant Schema
#'
#' @param request A list representing the request.
#' @return A resolved and type-safe request list.
#' @export
resolve_request <- function(request) {
  request <- request %||% list()

  valid_keys <- c("model_id", "global", "control", "strategies", "schedule", "drivers")
  extra_keys <- setdiff(names(request), valid_keys)
  if (length(extra_keys) > 0) {
    stop(sprintf("Unknown top-level fields in request: %s", paste(extra_keys, collapse = ", ")), call. = FALSE)
  }

  requested <- request$model_id %||% "FF16"
  parsed_id <- parse_model_id(requested)
  model <- parsed_id$model

  # The scientific version is authoritative in `plant`, not in the request:
  # re-derive it so the fingerprint tracks scientific changes (rerun) and
  # ignores software-only releases (reuse). A caller may still pass a bare
  # name ("FF16") or a full id ("FF16@v1"); a pinned version that disagrees
  # with the installed plant is a hard error rather than a silent stale hit.
  plant_version <- plant_model_version(model)
  if (!is.na(parsed_id$version) &&
      !identical(parsed_id$version, plant_version)) {
    stop(sprintf(
      paste0("Requested model_id '%s' pins scientific version %s, but the ",
             "installed plant provides %s. Use '%s' (or a bare '%s'), or ",
             "install the matching plant."),
      requested, parsed_id$version,
      plant::model_id(model), plant::model_id(model), model),
      call. = FALSE)
  }
  mid <- plant::model_id(model)
  schema <- get_model_schema(model)

  global <- resolve_global(request$global, schema)
  strats <- resolve_strategies(request$strategies, schema)

  list(
    model_id = mid,
    global = global,
    control = resolve_control(request$control, schema),
    strategies = strats,
    schedule = resolve_schedule(request$schedule, length(strats), global$max_patch_lifetime),
    drivers = resolve_drivers(request$drivers)
  )
}
