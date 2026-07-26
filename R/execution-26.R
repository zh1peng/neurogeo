#' Declare hard execution resource limits
#'
#' @param memory_bytes Maximum estimated in-memory bytes.
#' @param elapsed_seconds Maximum elapsed time.
#' @param blocks Maximum scheduled blocks.
#' @param materialized_elements Maximum explicitly materialized elements.
#'
#' @return An `ngeo_resource_budget`.
#' @export
ngeo_resource_budget <- function(
    memory_bytes = Inf,
    elapsed_seconds = Inf,
    blocks = Inf,
    materialized_elements = Inf) {
  value <- c(
    memory_bytes = memory_bytes,
    elapsed_seconds = elapsed_seconds,
    blocks = blocks,
    materialized_elements = materialized_elements
  )
  if (!is.numeric(value) || anyNA(value) ||
      any(value <= 0) || any(!is.finite(value) & value != Inf)) {
    .ngeo_abort("Resource limits must be positive numbers or `Inf`.",
                "ngeo_error_argument")
  }
  structure(as.list(value), class = "ngeo_resource_budget")
}

.ngeo_budget_assert <- function(budget, field, value) {
  if (!inherits(budget, "ngeo_resource_budget")) {
    .ngeo_abort("A valid resource budget is required.",
                "ngeo_error_argument")
  }
  if (value > budget[[field]]) {
    .ngeo_abort(
      sprintf(
        "Estimated `%s` (%s) exceeds the declared budget (%s).",
        field, format(value, scientific = FALSE),
        format(budget[[field]], scientific = FALSE)
      ),
      "ngeo_error_resource"
    )
  }
  invisible(TRUE)
}

#' @export
print.ngeo_resource_budget <- function(x, ...) {
  cat("<ngeo_resource_budget>\n")
  for (name in names(x)) cat("  ", name, ": ", x[[name]], "\n", sep = "")
  invisible(x)
}

.ngeo_function_identity <- function(value, declared = NULL) {
  if (!is.function(value)) {
    .ngeo_abort("A function identity requires a function.",
                "ngeo_error_argument")
  }
  if (!is.null(declared)) {
    .ngeo_assert_scalar_character(declared, "declared")
    return(declared)
  }
  digest::digest(
    list(
      formals = formals(value),
      body = body(value),
      environment = environmentName(environment(value)),
      package_version = .ngeo_package_version()
    ),
    algo = "sha256"
  )
}

#' Create a deterministic resumable execution plan
#'
#' @param operation Stable operation name.
#' @param tasks Ordered serializable task descriptors.
#' @param executor Function receiving one task and its one-based index.
#' @param executor_id Optional stable identifier for the executor
#'   implementation. By default it is derived from the function formals,
#'   body, environment name, and package version.
#' @param identity Inputs included in the plan hash.
#' @param budget Resource budget.
#' @param checkpoint Optional JSON checkpoint path.
#'
#' @return An `ngeo_execution_plan`.
#' @export
ngeo_execution_plan <- function(
    operation,
    tasks,
    executor,
    identity = list(),
    budget = ngeo_resource_budget(),
    checkpoint = NULL,
    executor_id = NULL) {
  .ngeo_assert_scalar_character(operation, "operation")
  if (!is.list(tasks) || !length(tasks) || !is.function(executor) ||
      !is.list(identity)) {
    .ngeo_abort("Plan tasks, executor, and identity are invalid.",
                "ngeo_error_argument")
  }
  .ngeo_budget_assert(budget, "blocks", length(tasks))
  executor_id <- .ngeo_function_identity(executor, executor_id)
  hash <- digest::digest(
    list(
      operation = operation,
      tasks = tasks,
      executor_id = executor_id,
      identity = identity
    ),
    algo = "sha256"
  )
  structure(
    list(
      operation = operation,
      tasks = tasks,
      executor = executor,
      executor_id = executor_id,
      identity = identity,
      plan_hash = hash,
      budget = budget,
      checkpoint = checkpoint
    ),
    class = "ngeo_execution_plan"
  )
}

.ngeo_atomic_json <- function(value, path) {
  .ngeo_require("jsonlite", "atomic checkpoint writing")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE,
    null = "null", digits = NA
  )
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    .ngeo_abort("Could not atomically replace checkpoint.",
                "ngeo_error_io")
  }
  invisible(path)
}

#' Execute or resume a deterministic plan
#'
#' @param plan An `ngeo_execution_plan`.
#' @param resume Whether to use a matching checkpoint.
#' @param stop_after Optional testing/debug limit on newly executed tasks.
#'
#' @return An `ngeo_execution_result`.
#' @export
ngeo_execute <- function(plan, resume = TRUE, stop_after = Inf) {
  if (!inherits(plan, "ngeo_execution_plan")) {
    .ngeo_abort("`plan` must be an execution plan.", "ngeo_error_argument")
  }
  completed <- integer()
  results <- vector("list", length(plan$tasks))
  if (isTRUE(resume) && !is.null(plan$checkpoint) &&
      file.exists(plan$checkpoint)) {
    .ngeo_require("jsonlite", "checkpoint reading")
    state <- jsonlite::fromJSON(plan$checkpoint, simplifyVector = FALSE)
    if (!identical(state$plan_hash, plan$plan_hash)) {
      .ngeo_abort("Checkpoint identity does not match the plan.",
                  "ngeo_error_cache_mismatch")
    }
    completed <- as.integer(unlist(state$completed))
    for (i in completed) results[[i]] <- state$results[[as.character(i)]]
  }
  started <- proc.time()[["elapsed"]]
  executed <- 0L
  for (i in setdiff(seq_along(plan$tasks), completed)) {
    .ngeo_budget_assert(
      plan$budget, "elapsed_seconds",
      proc.time()[["elapsed"]] - started
    )
    results[[i]] <- plan$executor(plan$tasks[[i]], i)
    completed <- c(completed, i)
    executed <- executed + 1L
    if (!is.null(plan$checkpoint)) {
      named <- results[completed]
      names(named) <- as.character(completed)
      .ngeo_atomic_json(
        list(
          schema = "NGCS-execution-checkpoint-1",
          plan_hash = plan$plan_hash,
          completed = completed,
          results = named
        ),
        plan$checkpoint
      )
    }
    if (executed >= stop_after) break
  }
  result <- list(
    plan_hash = plan$plan_hash,
    completed = sort(unique(completed)),
    results = results,
    complete = length(unique(completed)) == length(plan$tasks),
    elapsed_seconds = proc.time()[["elapsed"]] - started
  )
  class(result) <- "ngeo_execution_result"
  result
}

#' Construct a content-addressed local cache
#'
#' @param path Cache directory.
#' @return An `ngeo_cache`.
#' @export
ngeo_cache <- function(path) {
  .ngeo_assert_scalar_character(path, "path")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  structure(
    list(path = normalizePath(path, winslash = "/", mustWork = TRUE)),
    class = "ngeo_cache"
  )
}

#' Read or populate a content-addressed cache entry
#'
#' @param cache An `ngeo_cache`.
#' @param identity Serializable identity including all scientific inputs.
#' @param compute Function used on a cache miss.
#' @param compute_id Optional stable identifier for the compute
#'   implementation. By default it is derived from the function formals,
#'   body, environment name, and package version.
#'
#' @return An `ngeo_cache_result`.
#' @export
ngeo_cache_compute <- function(cache, identity, compute, compute_id = NULL) {
  if (!inherits(cache, "ngeo_cache") || !is.list(identity) ||
      !is.function(compute)) {
    .ngeo_abort("Cache inputs are invalid.", "ngeo_error_argument")
  }
  compute_id <- .ngeo_function_identity(compute, compute_id)
  key <- digest::digest(
    list(identity = identity, compute_id = compute_id),
    algo = "sha256"
  )
  path <- file.path(cache$path, paste0(key, ".rds"))
  hit <- file.exists(path)
  if (hit) {
    value <- readRDS(path)
  } else {
    value <- compute()
    temporary <- tempfile(paste0(".", key, "-"), tmpdir = cache$path)
    on.exit(unlink(temporary), add = TRUE)
    saveRDS(value, temporary, version = 3L)
    if (!file.rename(temporary, path)) {
      .ngeo_abort("Could not atomically populate cache.", "ngeo_error_io")
    }
  }
  structure(
    list(
      value = value,
      key = key,
      compute_id = compute_id,
      hit = hit,
      path = path
    ),
    class = "ngeo_cache_result"
  )
}

#' Atomically create one output file
#'
#' @param path Final output path.
#' @param writer Function receiving a temporary sibling path.
#' @param overwrite Whether to replace an existing output.
#'
#' @return Output metadata.
#' @export
ngeo_atomic_write <- function(path, writer, overwrite = FALSE) {
  .ngeo_assert_scalar_character(path, "path")
  if (!is.function(writer)) {
    .ngeo_abort("`writer` must be a function.", "ngeo_error_argument")
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    .ngeo_abort("Atomic output exists; use `overwrite = TRUE`.",
                "ngeo_error_overwrite")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary), add = TRUE)
  writer(temporary)
  if (!file.exists(temporary)) {
    .ngeo_abort("Atomic writer did not create its temporary output.",
                "ngeo_error_io")
  }
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    .ngeo_abort("Could not atomically replace output.", "ngeo_error_io")
  }
  structure(
    list(
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      size = file.info(path)$size,
      sha256 = digest::digest(
        path, algo = "sha256", file = TRUE, serialize = FALSE
      )
    ),
    class = "ngeo_atomic_output"
  )
}

#' @export
print.ngeo_execution_plan <- function(x, ...) {
  cat("<ngeo_execution_plan>\n  operation: ", x$operation,
      "\n  tasks: ", length(x$tasks), "\n  hash: ",
      x$plan_hash, "\n", sep = "")
  invisible(x)
}
