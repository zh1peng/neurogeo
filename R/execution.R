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

.ngeo_atomic_backup_path <- function(path) {
  file.path(dirname(path), paste0(".", basename(path), ".ngeo-backup"))
}

.ngeo_atomic_operations <- function(operations) {
  defaults <- list(
    rename = file.rename,
    copy = file.copy,
    unlink = unlink
  )
  if (is.null(operations)) {
    return(defaults)
  }
  if (!is.list(operations) ||
      !all(names(defaults) %in% names(operations)) ||
      !all(vapply(operations[names(defaults)], is.function, logical(1)))) {
    .ngeo_abort(
      "Internal file operations must provide rename, copy, and unlink.",
      "ngeo_error_argument"
    )
  }
  operations[names(defaults)]
}

.ngeo_atomic_remove <- function(path, operations) {
  if (!file.exists(path)) {
    return(TRUE)
  }
  operations$unlink(path)
  !file.exists(path)
}

.ngeo_atomic_restore <- function(backup, path, operations) {
  if (isTRUE(operations$rename(backup, path))) {
    return(TRUE)
  }
  copied <- isTRUE(operations$copy(backup, path, overwrite = FALSE)) &&
    file.exists(path)
  if (copied) {
    .ngeo_atomic_remove(backup, operations)
  }
  copied
}

.ngeo_atomic_recover <- function(path, backup, operations) {
  if (!file.exists(backup)) {
    return(invisible(FALSE))
  }
  if (!file.exists(path)) {
    if (!.ngeo_atomic_restore(backup, path, operations)) {
      .ngeo_abort(
        sprintf(
          "Could not recover the previous output; backup remains at `%s`.",
          backup
        ),
        "ngeo_error_io_recovery"
      )
    }
    return(invisible(TRUE))
  }
  if (!.ngeo_atomic_remove(backup, operations)) {
    .ngeo_warn(
      sprintf("A completed output backup remains at `%s`.", backup),
      "ngeo_warning_io_cleanup"
    )
  }
  invisible(TRUE)
}

.ngeo_atomic_write <- function(
    path,
    writer,
    overwrite = FALSE,
    .operations = NULL) {
  .ngeo_assert_scalar_character(path, "path")
  if (!is.function(writer)) {
    .ngeo_abort("`writer` must be a function.", "ngeo_error_argument")
  }
  operations <- .ngeo_atomic_operations(.operations)
  backup <- .ngeo_atomic_backup_path(path)
  .ngeo_atomic_recover(path, backup, operations)
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
  on.exit(.ngeo_atomic_remove(temporary, operations), add = TRUE)
  writer(temporary)
  if (!file.exists(temporary)) {
    .ngeo_abort(
      "Atomic writer did not create its temporary output.",
      "ngeo_error_io"
    )
  }
  had_output <- file.exists(path)
  if (had_output && !isTRUE(operations$rename(path, backup))) {
    .ngeo_abort(
      "Could not prepare the existing output for safe replacement.",
      "ngeo_error_io"
    )
  }
  if (!isTRUE(operations$rename(temporary, path))) {
    restored <- !had_output || .ngeo_atomic_restore(
      backup, path, operations
    )
    if (!restored) {
      .ngeo_abort(
        sprintf(
          "Output replacement and rollback failed; backup remains at `%s`.",
          backup
        ),
        "ngeo_error_io_recovery"
      )
    }
    .ngeo_abort(
      "Could not replace output; the previous output was restored.",
      "ngeo_error_io"
    )
  }
  if (had_output && !.ngeo_atomic_remove(backup, operations)) {
    .ngeo_warn(
      sprintf("Output was replaced, but its backup remains at `%s`.", backup),
      "ngeo_warning_io_cleanup"
    )
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
