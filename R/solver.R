#' Structured S3 Plant Solver Failure Object
#'
#' @param reason Character string describing the failure reason.
#' @param message Character string carrying the solver error message.
#' @param last_t Numeric or NULL indicating the last solver time reached before divergence.
#' @param partial_log Data frame or NULL indicating the partial log.
#' @return A structured S3 PlantFailure error condition.
#' @export
PlantFailure <- function(reason, message, last_t = NULL, partial_log = NULL) {
  structure(
    list(
      reason = reason,
      message = message,
      last_t = last_t,
      partial_log = partial_log
    ),
    class = c("PlantFailure", "error", "condition")
  )
}

# The model's hyperparameter function, configured from global settings.
hyperpar <- function(model, global) {
  hp <- tryCatch(plant::make_hyperpar(model), error = function(e) NULL)
  if (is.null(hp)) return(function(m, s) m)
  do.call(hp, global[intersect(names(global), names(formals(hp)))])
}

# Trait columns the hyperpar function synthesises (must not be fed as inputs).
generated_cols <- function(hp_fn, strategy) {
  dummy <- matrix(0.1, 1L, 1L, dimnames = list(NULL, "lma"))
  gen <- tryCatch(hp_fn(dummy, strategy, filter = FALSE), error = function(e) NULL)
  if (is.null(gen)) character(0) else setdiff(colnames(gen), "lma")
}

#' Build SCM parameters from request list
#'
#' Helper that handles trait matrix construction, hyperparameter expansion,
#' and birth rate drivers.
#'
#' @param req Resolved request list.
#' @return A list containing elements `p` (Parameters), `env` (Environment), and `ctrl` (Control).
#' @keywords internal
build_scm <- function(req) {
  model <- parse_model_id(req$model_id)$model

  hp_fn <- hyperpar(model, req$global)

  p <- plant::scm_base_parameters(model)
  p$patch_area <- req$global$patch_area
  p$max_patch_lifetime <- req$global$max_patch_lifetime

  if (length(req$strategies) > 0L) {
    s0 <- get(paste0(model, "_Strategy"), asNamespace("plant"))()
    gen_cols <- generated_cols(hp_fn, s0)

    traits <- as.matrix(dplyr::bind_rows(req$strategies))
    valid_names <- c(names(s0), if (!is.null(s0$pars)) names(s0$pars) else character(0))
    input_traits <- intersect(colnames(traits), valid_names)
    input_traits <- setdiff(input_traits, gen_cols)
    traits <- traits[, input_traits, drop = FALSE]

    br <- if (is.list(req$drivers) && !is.null(req$drivers$birth_rate)) {
      list(
        x = purrr::map_dbl(req$drivers$birth_rate, 1L),
        y = purrr::map_dbl(req$drivers$birth_rate, 2L)
      )
    } else {
      1.0
    }

    p <- plant::add_strategies(
      p, traits, hp_fn,
      birth_rate = rep(list(br), nrow(traits)),
      keep_existing = FALSE
    )
    p$node_schedule_times <- req$schedule
  } else {
    p$strategies <- list()
    p$node_schedule_times <- list()
  }

  valid_ctrl_keys <- intersect(names(req$control), names(plant::Control()))
  list(
    p = p,
    env = plant::Environment(model),
    ctrl = plant::Control(values = req$control[valid_ctrl_keys])
  )
}

#' Run Plant SCM Solver
#'
#' Maps a resolved request to Parameters, executes SCM, and returns results.
#' Enforces strict type coercion boundaries.
#'
#' @param resolved_request A resolved request list (e.g. from resolve_request).
#' @return A list containing elements `log`, `realised_schedule`, `runtime_seconds`,
#'   and `solver_steps` on success, or a structured S3 PlantFailure error on failure.
#' @export
run_plant <- function(resolved_request) {
  req <- resolve_request(resolved_request)

  args <- tryCatch({
    build_scm(req)
  }, error = function(e) {
    PlantFailure("setup_error", e$message)
  })
  if (inherits(args, "PlantFailure")) return(args)

  t_start <- proc.time()

  res <- tryCatch({
    plant::run_scm(
      p = args$p,
      env = args$env,
      ctrl = args$ctrl,
      refine_schedule = req$control$refine_schedule,
      collect = TRUE
    )
  }, error = function(e) {
    PlantFailure("exception", e$message)
  })

  if (inherits(res, "PlantFailure")) return(res)

  fp <- request_fingerprint(req)
  coerced_log <- coerce_log(res$species, fp, res$p$node_schedule_times)

  list(
    log = coerced_log,
    realised_schedule = res$p$node_schedule_times,
    runtime_seconds = as.double((proc.time() - t_start)["elapsed"]),
    solver_steps = nrow(res$steps)
  )
}

add_double_cols <- function(tbl, df, exclude) {
  for (col in setdiff(names(df), exclude)) tbl[[col]] <- as.double(df[[col]])
  tbl
}

#' Coerce raw species log to cohort-major layout
#'
#' Filters out the seedling/seed row and maps active cohorts to their
#' strategy_id, cohort_id, and birth_time using the realised_schedule.
#' Enforces rigid types at the boundary.
#'
#' @param df Raw species log data frame from run_plant.
#' @param run_fingerprint Character SHA-256 fingerprint of the run.
#' @param realised_schedule List of realised schedule times per strategy.
#' @return A coerced tibble with explicit schemas for Parquet serialization.
#' @importFrom dplyr %>%
coerce_log <- function(df, run_fingerprint, realised_schedule) {
  exclude <- c("step", "species", "node", "time")
  if (is.null(df) || nrow(df) == 0L) {
    skel <- tibble::tibble(run_fingerprint = character(0), strategy_id = integer(0),
                           cohort_id = integer(0), birth_time = double(0), t = double(0))
    return(if (is.null(df)) skel else add_double_cols(skel, df, exclude))
  }
  df <- df %>% dplyr::group_by(step, species) %>%
    dplyr::filter(if (length(node) == 0L || all(is.na(node))) FALSE else node < max(node, na.rm = TRUE)) %>%
    dplyr::ungroup()
  strategy_id <- as.integer(df$species) - 1L
  cohort_id   <- as.integer(df$node)
  offsets     <- cumsum(c(0L, vapply(realised_schedule, length, integer(1L))))
  birth_time  <- unlist(realised_schedule)[offsets[strategy_id + 1L] + cohort_id]
  tbl <- tibble::tibble(run_fingerprint = as.character(run_fingerprint),
                        strategy_id = strategy_id, cohort_id = cohort_id,
                        birth_time = as.double(birth_time), t = as.double(df$time))
  add_double_cols(tbl, df, exclude)
}

