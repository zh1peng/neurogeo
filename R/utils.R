`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.ngeo_logical_source_id <- function(path, sha256 = NULL, size = NULL) {
  name <- basename(path)
  identity <- if (!is.null(sha256)) {
    sha256
  } else {
    digest::digest(
      list(name = name, size = as.numeric(size)),
      algo = "sha256"
    )
  }
  paste0("file:", name, "#sha256:", substr(identity, 1L, 16L))
}

.ngeo_condition_code <- function(class) {
  primary <- class[[1L]]
  prefix <- if (grepl("^ngeo_warning", primary)) "NGEO_WARNING" else
    "NGEO_ERROR"
  suffix <- sub("^ngeo_(error|warning)_?", "", primary)
  suffix <- toupper(gsub("[^A-Za-z0-9]+", "_", suffix))
  if (nzchar(suffix)) paste(prefix, suffix, sep = "_") else prefix
}

.ngeo_condition_field <- function(message) {
  matched <- regmatches(
    message,
    regexec("`([^`]+)`", message, perl = TRUE)
  )[[1L]]
  if (length(matched) >= 2L && nzchar(matched[[2L]])) {
    matched[[2L]]
  } else {
    "object"
  }
}

.ngeo_condition_hint <- function(class, field) {
  primary <- class[[1L]]
  if (grepl("backend|dependency", primary)) {
    return("Install the named optional package, or use the documented native-data fallback.")
  }
  if (grepl("io|format|manifest|schema", primary)) {
    return("Verify the path, format contract, permissions, and checksum before retrying.")
  }
  if (grepl("layer", primary)) {
    return("Inspect ngeo_layers(x), select an existing layer, and retry explicitly.")
  }
  if (grepl("measure", primary)) {
    return("Inspect ngeo_measures(x) and use ngeo_update_measure() for a safe correction.")
  }
  if (grepl("coordinate_space|metric|distance|transform", primary)) {
    return("Confirm coordinate space, units, and transform direction before retrying.")
  }
  if (grepl("argument|index|missing", primary)) {
    return(sprintf(
      "Review `%s` and retry with a value matching the documented contract.",
      field
    ))
  }
  "Inspect the named object and the documented contract before retrying."
}

.ngeo_abort <- function(message, class = "ngeo_error", code = NULL,
                        field = NULL, hint = NULL) {
  field <- field %||% .ngeo_condition_field(message)
  code <- code %||% .ngeo_condition_code(class)
  hint <- hint %||% .ngeo_condition_hint(class, field)
  condition <- structure(
    list(
      message = message,
      call = NULL,
      code = code,
      field = field,
      hint = hint
    ),
    class = c(class, "ngeo_error", "error", "condition")
  )
  stop(condition)
}

.ngeo_warn <- function(message, class = "ngeo_warning", code = NULL,
                       field = NULL, hint = NULL) {
  field <- field %||% .ngeo_condition_field(message)
  code <- code %||% .ngeo_condition_code(class)
  hint <- hint %||% .ngeo_condition_hint(class, field)
  condition <- structure(
    list(
      message = message,
      call = NULL,
      code = code,
      field = field,
      hint = hint
    ),
    class = c(class, "ngeo_warning", "warning", "condition")
  )
  warning(condition)
}

.ngeo_assert_scalar_character <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .ngeo_abort(
      sprintf("`%s` must be one non-missing character value.", name),
      "ngeo_error_argument",
      code = "NGEO_ERROR_ARGUMENT_SCALAR_CHARACTER",
      field = name,
      hint = sprintf(
        "Supply `%s` as one non-missing, non-empty character value.", name
      )
    )
  }
  invisible(x)
}

.ngeo_as_integer <- function(x, name) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != floor(x)) || any(abs(x) > .Machine$integer.max)) {
    .ngeo_abort(
      sprintf(
        "`%s` must contain finite integer values representable by R.", name
      ),
      "ngeo_error_index",
      code = "NGEO_ERROR_INDEX_INTEGER",
      field = name,
      hint = sprintf(
        "Remove missing, non-integer, or out-of-range values from `%s`.", name
      )
    )
  }
  as.integer(x)
}

.ngeo_as_dgCMatrix <- function(x) {
  if (!inherits(x, "dMatrix")) {
    x <- methods::as(x, "dMatrix")
  }
  x <- methods::as(x, "generalMatrix")
  methods::as(x, "CsparseMatrix")
}

.ngeo_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("neurogeo")),
    error = function(...) "6.3.0"
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
