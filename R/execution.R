#' Declare hard execution resource limits
#'
#' @param memory_bytes Maximum estimated in-memory bytes.
#' @param elapsed_seconds Maximum elapsed time.
#' @param blocks Maximum scheduled chunks.
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
    .ngeo_abort(
      "Resource limits must be positive numbers or `Inf`.",
      "ngeo_error_argument"
    )
  }
  structure(as.list(value), class = "ngeo_resource_budget")
}

.ngeo_budget_assert <- function(budget, field, value) {
  if (!inherits(budget, "ngeo_resource_budget")) {
    .ngeo_abort(
      "A valid resource budget is required.",
      "ngeo_error_argument"
    )
  }
  if (value > budget[[field]]) {
    .ngeo_abort(
      sprintf(
        "Estimated `%s` (%s) exceeds the declared budget (%s).",
        field,
        format(value, scientific = FALSE),
        format(budget[[field]], scientific = FALSE)
      ),
      "ngeo_error_resource"
    )
  }
  invisible(TRUE)
}

.ngeo_atomic_write <- function(path, writer, overwrite = FALSE) {
  .ngeo_assert_scalar_character(path, "path")
  if (!is.function(writer)) {
    .ngeo_abort("`writer` must be a function.", "ngeo_error_argument")
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    .ngeo_abort(
      "Atomic output exists; use `overwrite = TRUE`.",
      "ngeo_error_overwrite"
    )
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary), add = TRUE)
  writer(temporary)
  if (!file.exists(temporary)) {
    .ngeo_abort(
      "Atomic writer did not create its temporary output.",
      "ngeo_error_io"
    )
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
        path,
        algo = "sha256",
        file = TRUE,
        serialize = FALSE
      )
    ),
    class = "ngeo_atomic_output"
  )
}

#' @export
print.ngeo_resource_budget <- function(x, ...) {
  cat("<ngeo_resource_budget>\n")
  for (name in names(x)) {
    cat("  ", name, ": ", x[[name]], "\n", sep = "")
  }
  invisible(x)
}
