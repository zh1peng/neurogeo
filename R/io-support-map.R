.ngeo_support_prefix <- function(path) {
  .ngeo_assert_scalar_character(path, "path")
  sub(
    "([.]variance)?[.](json|mtx)$",
    "",
    path,
    ignore.case = TRUE
  )
}

#' Write a support map as Matrix Market plus JSON
#'
#' @param x An `ngeo_support_map`.
#' @param path Output prefix or JSON/MTX path.
#' @param overwrite Whether to replace existing sidecars.
#'
#' @return Paths to the written files, invisibly.
#' @export
write_ngeo_support_map <- function(x, path, overwrite = FALSE) {
  ngeo_validate_support_map(x)
  .ngeo_require("jsonlite", "support-map metadata writing")
  prefix <- .ngeo_support_prefix(path)
  matrix_path <- paste0(prefix, ".mtx")
  json_path <- paste0(prefix, ".json")
  variance_path <- if (is.null(x$weight_variance)) {
    NULL
  } else {
    paste0(prefix, ".variance.mtx")
  }
  output <- c(matrix_path, json_path, variance_path)
  if (!isTRUE(overwrite) && any(file.exists(output))) {
    .ngeo_abort(
      "Support-map output already exists; use `overwrite = TRUE`.",
      "ngeo_error_io"
    )
  }
  dir.create(dirname(json_path), recursive = TRUE, showWarnings = FALSE)
  Matrix::writeMM(x$operator, matrix_path)
  if (!is.null(variance_path)) {
    Matrix::writeMM(x$weight_variance, variance_path)
  }
  metadata <- list(
    schema = "NGCS-support-map-exchange-1",
    spec_version = x$spec_version,
    direction = x$direction,
    type = x$type,
    coverage = x$coverage,
    source_domain_hash = x$source_domain_hash,
    target_domain_hash = x$target_domain_hash,
    source_element_id = x$source_element_id,
    target_element_id = x$target_element_id,
    source_support = x$source_support,
    target_support = x$target_support,
    operator_dimnames = dimnames(x$operator),
    operator_file = basename(matrix_path),
    weight_variance_file = if (is.null(variance_path)) {
      NULL
    } else {
      basename(variance_path)
    },
    support_map_hash = ngeo_support_map_hash(x),
    provenance = x$provenance
  )
  jsonlite::write_json(
    metadata,
    json_path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  invisible(stats::setNames(
    output,
    c("operator", "metadata", if (!is.null(variance_path)) {
      "weight_variance"
    })
  ))
}

.ngeo_json_vector <- function(x, mode = c("character", "numeric")) {
  mode <- match.arg(mode)
  if (is.null(x)) {
    return(NULL)
  }
  value <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (identical(mode, "numeric")) {
    as.numeric(value)
  } else {
    as.character(value)
  }
}

.ngeo_json_dimnames <- function(x) {
  if (is.null(x)) {
    return(list(NULL, NULL))
  }
  result <- lapply(x, function(value) {
    if (is.null(value)) NULL else as.character(unlist(value))
  })
  length(result) <- 2L
  result
}

.ngeo_exchange_file <- function(directory, file, field) {
  if (!is.character(file) || length(file) != 1L ||
      is.na(file) || !nzchar(file) ||
      !identical(file, basename(file))) {
    .ngeo_abort(
      sprintf("Support-map `%s` must be a sidecar-local file name.", field),
      "ngeo_error_io"
    )
  }
  file.path(directory, file)
}

#' Read a Matrix Market plus JSON support map
#'
#' @param path Exchange prefix or JSON sidecar path.
#'
#' @return An `ngeo_support_map` bound to the stored ordered domain IDs.
#' @export
read_ngeo_support_map <- function(path) {
  if (dir.exists(path) ||
      tolower(basename(path)) == "manifest.json") {
    return(read_ngeo_support_bundle(path))
  }
  .ngeo_require("jsonlite", "support-map metadata reading")
  prefix <- .ngeo_support_prefix(path)
  json_path <- paste0(prefix, ".json")
  if (!file.exists(json_path)) {
    .ngeo_abort(
      sprintf("Support-map sidecar does not exist: %s", json_path),
      "ngeo_error_io"
    )
  }
  metadata <- tryCatch(
    jsonlite::fromJSON(json_path, simplifyVector = FALSE),
    error = function(error) {
      .ngeo_abort(
        paste("Could not parse support-map JSON:", conditionMessage(error)),
        "ngeo_error_io"
      )
    }
  )
  if (!identical(metadata$schema, "NGCS-support-map-exchange-1") ||
      !identical(metadata$direction, "target_by_source")) {
    .ngeo_abort(
      "Unsupported support-map exchange schema or orientation.",
      "ngeo_error_io"
    )
  }
  directory <- dirname(json_path)
  operator_path <- .ngeo_exchange_file(
    directory,
    metadata$operator_file,
    "operator_file"
  )
  if (!file.exists(operator_path)) {
    .ngeo_abort(
      "The support-map Matrix Market operator is missing.",
      "ngeo_error_io"
    )
  }
  operator <- tryCatch(
    methods::as(Matrix::readMM(operator_path), "CsparseMatrix"),
    error = function(error) {
      .ngeo_abort(
        paste("Could not read support operator:", conditionMessage(error)),
        "ngeo_error_io"
      )
    }
  )
  dimnames(operator) <- .ngeo_json_dimnames(
    metadata$operator_dimnames
  )
  variance <- NULL
  if (!is.null(metadata$weight_variance_file)) {
    variance_path <- .ngeo_exchange_file(
      directory,
      metadata$weight_variance_file,
      "weight_variance_file"
    )
    if (!file.exists(variance_path)) {
      .ngeo_abort(
        "The support-map variance Matrix Market file is missing.",
        "ngeo_error_io"
      )
    }
    variance <- tryCatch(
      methods::as(Matrix::readMM(variance_path), "CsparseMatrix"),
      error = function(error) {
        .ngeo_abort(
          paste("Could not read support variance:", conditionMessage(error)),
          "ngeo_error_io"
        )
      }
    )
    dimnames(variance) <- dimnames(operator)
  }
  result <- .ngeo_support_map_structure(
    operator = operator,
    type = metadata$type,
    source_hash = metadata$source_domain_hash,
    target_hash = metadata$target_domain_hash,
    source_id = .ngeo_json_vector(
      metadata$source_element_id, "character"
    ),
    target_id = .ngeo_json_vector(
      metadata$target_element_id, "character"
    ),
    source_support = .ngeo_json_vector(
      metadata$source_support, "numeric"
    ),
    target_support = .ngeo_json_vector(
      metadata$target_support, "numeric"
    ),
    weight_variance = variance,
    coverage = metadata$coverage,
    provenance = metadata$provenance %||% list()
  )
  result$spec_version <- metadata$spec_version %||% "2.0"
  if (!identical(
    ngeo_support_map_hash(result),
    metadata$support_map_hash
  )) {
    .ngeo_abort(
      "Support-map exchange hash verification failed.",
      "ngeo_error_io"
    )
  }
  result
}
