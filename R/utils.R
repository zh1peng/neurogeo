`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.ngeo_abort <- function(message, class = "ngeo_error") {
  condition <- structure(
    list(message = message, call = NULL),
    class = c(class, "ngeo_error", "error", "condition")
  )
  stop(condition)
}

.ngeo_warn <- function(message, class = "ngeo_warning") {
  condition <- structure(
    list(message = message, call = NULL),
    class = c(class, "ngeo_warning", "warning", "condition")
  )
  warning(condition)
}

.ngeo_assert_scalar_character <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .ngeo_abort(
      sprintf("`%s` must be one non-missing character value.", name),
      "ngeo_error_argument"
    )
  }
  invisible(x)
}

.ngeo_as_integer <- function(x, name) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != floor(x))) {
    .ngeo_abort(
      sprintf("`%s` must contain finite integer values.", name),
      "ngeo_error_index"
    )
  }
  as.integer(x)
}

.ngeo_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("neurogeo")),
    error = function(...) "4.0.0"
  )
}

.ngeo_operation <- function(operation, parameters = list()) {
  list(
    operation = operation,
    software = list(
      package = "neurogeo",
      version = .ngeo_package_version()
    ),
    timestamp_utc = format(
      Sys.time(),
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    ),
    parameters = parameters
  )
}
