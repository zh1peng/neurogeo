# Portable sparse support-map bundles.
.ngeo_support_bundle_directory <- function(path) {
  .ngeo_assert_scalar_character(path, "path")
  if (tolower(basename(path)) == "manifest.json") dirname(path) else path
}

.ngeo_file_sha256 <- function(path) {
  digest::digest(
    path, algo = "sha256", file = TRUE, serialize = FALSE
  )
}

#' Write an atomic NGCS support-map exchange schema 2 bundle
#'
#' @param x An `ngeo_support_map`.
#' @param path Output bundle directory.
#' @param chunk_size Number of ordered source columns per Matrix Market chunk.
#' @param overwrite Whether to replace an existing complete bundle.
#' @return Bundle path and manifest checksum.
#' @export
write_ngeo_support_bundle <- function(
    x,
    path,
    chunk_size = 100000L,
    overwrite = FALSE) {
  ngeo_validate_support_map(x)
  .ngeo_require("jsonlite", "support bundle writing")
  chunk_size <- .ngeo_as_integer(chunk_size, "chunk_size")
  if (length(chunk_size) != 1L || chunk_size < 1L) {
    .ngeo_abort("Bundle chunk size must be positive.",
                "ngeo_error_argument")
  }
  directory <- .ngeo_support_bundle_directory(path)
  parent <- dirname(directory)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(directory) && !dir.exists(directory)) {
    .ngeo_abort("Support bundle path exists and is not a directory.",
                "ngeo_error_io")
  }
  if (dir.exists(directory) && !isTRUE(overwrite)) {
    .ngeo_abort("Support bundle exists; use `overwrite = TRUE`.",
                "ngeo_error_overwrite")
  }
  temporary <- tempfile(".ngeo-support-bundle-", tmpdir = parent)
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  groups <- split(
    seq_len(ncol(x$operator)),
    ceiling(seq_len(ncol(x$operator)) / chunk_size)
  )
  chunks <- lapply(seq_along(groups), function(i) {
    columns <- groups[[i]]
    operator_file <- sprintf("operator-%05d.mtx", i)
    operator_path <- file.path(temporary, operator_file)
    operator <- x$operator[, columns, drop = FALSE]
    Matrix::writeMM(operator, operator_path)
    variance_file <- NULL
    variance_sha <- NULL
    variance_size <- NULL
    if (!is.null(x$weight_variance)) {
      variance_file <- sprintf("variance-%05d.mtx", i)
      variance_path <- file.path(temporary, variance_file)
      Matrix::writeMM(
        x$weight_variance[, columns, drop = FALSE],
        variance_path
      )
      variance_sha <- .ngeo_file_sha256(variance_path)
      variance_size <- file.info(variance_path)$size
    }
    list(
      index = i,
      source_start = min(columns),
      source_end = max(columns),
      dimensions = c(nrow(x$operator), length(columns)),
      nonzero = length(operator@x),
      operator_file = operator_file,
      operator_size = file.info(operator_path)$size,
      operator_sha256 = .ngeo_file_sha256(operator_path),
      variance_file = variance_file,
      variance_size = variance_size,
      variance_sha256 = variance_sha
    )
  })
  metadata <- list(
    schema = "NGCS-support-map-exchange-2",
    spec_version = x$spec_version,
    direction = x$direction,
    type = x$type,
    coverage = x$coverage,
    source_base_hash = x$source_base_hash,
    target_base_hash = x$target_base_hash,
    source_element_id = x$source_element_id,
    target_element_id = x$target_element_id,
    source_support = x$source_support,
    target_support = x$target_support,
    operator_dimensions = dim(x$operator),
    operator_dimnames = dimnames(x$operator),
    chunk_axis = "source_columns",
    chunk_size = chunk_size,
    chunks = chunks,
    support_map_hash = ngeo_support_map_hash(x),
    history = x$history
  )
  manifest_path <- file.path(temporary, "manifest.json")
  jsonlite::write_json(
    metadata,
    manifest_path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  backup <- tempfile(".ngeo-support-backup-", tmpdir = parent)
  if (dir.exists(directory) && !file.rename(directory, backup)) {
    .ngeo_abort("Could not stage existing support bundle.",
                "ngeo_error_io")
  }
  success <- FALSE
  on.exit({
    if (!success && dir.exists(backup) && !dir.exists(directory)) {
      file.rename(backup, directory)
    }
    if (success && dir.exists(backup)) unlink(backup, recursive = TRUE)
  }, add = TRUE)
  if (!file.rename(temporary, directory)) {
    .ngeo_abort("Could not atomically promote support bundle.",
                "ngeo_error_io")
  }
  success <- TRUE
  manifest_path <- file.path(directory, "manifest.json")
  structure(
    list(
      path = normalizePath(directory, winslash = "/", mustWork = TRUE),
      manifest = normalizePath(
        manifest_path, winslash = "/", mustWork = TRUE
      ),
      sha256 = .ngeo_file_sha256(manifest_path),
      chunks = length(chunks)
    ),
    class = "ngeo_support_bundle_output"
  )
}

.ngeo_read_support_bundle_manifest <- function(path) {
  .ngeo_require("jsonlite", "support bundle reading")
  directory <- .ngeo_support_bundle_directory(path)
  manifest_path <- file.path(directory, "manifest.json")
  if (!file.exists(manifest_path)) {
    .ngeo_abort("Support bundle manifest is missing.", "ngeo_error_io")
  }
  metadata <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(error) {
      .ngeo_abort(
        paste("Could not parse support bundle:", conditionMessage(error)),
        "ngeo_error_io"
      )
    }
  )
  if (!identical(metadata$schema, "NGCS-support-map-exchange-2") ||
      !identical(metadata$direction, "target_by_source") ||
      !identical(metadata$chunk_axis, "source_columns") ||
      !is.list(metadata$chunks) || !length(metadata$chunks)) {
    .ngeo_abort("Unsupported or malformed support bundle schema.",
                "ngeo_error_io")
  }
  list(
    directory = directory,
    manifest_path = manifest_path,
    metadata = metadata
  )
}

#' Validate a schema 2 support-map bundle and checksums
#'
#' @param path Bundle directory or manifest path.
#' @return The parsed manifest, invisibly.
#' @export
ngeo_validate_support_bundle <- function(path) {
  bundle <- .ngeo_read_support_bundle_manifest(path)
  metadata <- bundle$metadata
  dimensions <- as.integer(unlist(metadata$operator_dimensions))
  if (length(dimensions) != 2L || any(dimensions < 1L)) {
    .ngeo_abort("Support bundle dimensions are invalid.",
                "ngeo_error_io")
  }
  expected_start <- 1L
  for (i in seq_along(metadata$chunks)) {
    chunk <- metadata$chunks[[i]]
    start <- as.integer(chunk$source_start)
    end <- as.integer(chunk$source_end)
    chunk_dimensions <- as.integer(unlist(chunk$dimensions))
    if (!identical(as.integer(chunk$index), as.integer(i)) ||
        start != expected_start || end < start ||
        !identical(
          chunk_dimensions,
          c(dimensions[[1L]], end - start + 1L)
        )) {
      .ngeo_abort("Support bundle chunks are not ordered and contiguous.",
                  "ngeo_error_io")
    }
    operator_path <- .ngeo_exchange_file(
      bundle$directory, chunk$operator_file, "operator_file"
    )
    if (!file.exists(operator_path) ||
        !identical(.ngeo_file_sha256(operator_path),
                   chunk$operator_sha256) ||
        !identical(
          as.numeric(file.info(operator_path)$size),
          as.numeric(chunk$operator_size)
        )) {
      .ngeo_abort("Support bundle operator checksum failed.",
                  "ngeo_error_io")
    }
    if (!is.null(chunk$variance_file)) {
      variance_path <- .ngeo_exchange_file(
        bundle$directory, chunk$variance_file, "variance_file"
      )
      if (!file.exists(variance_path) ||
          !identical(.ngeo_file_sha256(variance_path),
                     chunk$variance_sha256) ||
          !identical(
            as.numeric(file.info(variance_path)$size),
            as.numeric(chunk$variance_size)
          )) {
        .ngeo_abort("Support bundle variance checksum failed.",
                    "ngeo_error_io")
      }
    }
    expected_start <- end + 1L
  }
  if (expected_start != dimensions[[2L]] + 1L) {
    .ngeo_abort("Support bundle source coverage is incomplete.",
                "ngeo_error_io")
  }
  invisible(metadata)
}

#' Read an NGCS support-map exchange schema 2 bundle
#'
#' @param path Bundle directory or manifest path.
#' @return A verified `ngeo_support_map`.
#' @export
read_ngeo_support_bundle <- function(path) {
  metadata <- ngeo_validate_support_bundle(path)
  directory <- .ngeo_support_bundle_directory(path)
  operators <- lapply(metadata$chunks, function(chunk) {
    methods::as(
      methods::as(
        Matrix::readMM(file.path(directory, chunk$operator_file)),
        "dMatrix"
      ),
      "CsparseMatrix"
    )
  })
  operator <- methods::as(do.call(cbind, operators), "CsparseMatrix")
  dimnames(operator) <- .ngeo_json_dimnames(metadata$operator_dimnames)
  variance <- NULL
  if (any(vapply(
    metadata$chunks,
    function(chunk) !is.null(chunk$variance_file),
    logical(1)
  ))) {
    if (!all(vapply(
      metadata$chunks,
      function(chunk) !is.null(chunk$variance_file),
      logical(1)
    ))) {
      .ngeo_abort("Support variance chunks are incomplete.",
                  "ngeo_error_io")
    }
    variance <- methods::as(do.call(cbind, lapply(
      metadata$chunks,
      function(chunk) {
        methods::as(
          Matrix::readMM(file.path(directory, chunk$variance_file)),
          "dMatrix"
        )
      }
    )), "CsparseMatrix")
    dimnames(variance) <- dimnames(operator)
  }
  result <- .ngeo_support_map_structure(
    operator = operator,
    type = metadata$type,
    source_hash = metadata$source_base_hash,
    target_hash = metadata$target_base_hash,
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
    history = metadata$history %||% list()
  )
  result$spec_version <- metadata$spec_version %||% "2.0"
  if (!identical(ngeo_support_map_hash(result),
                 metadata$support_map_hash)) {
    .ngeo_abort("Support bundle logical hash verification failed.",
                "ngeo_error_io")
  }
  result
}

#' Migrate schema 1 or 2 support-map exchange to schema 2
#'
#' @param path Existing schema 1 prefix/JSON or schema 2 bundle.
#' @param output New schema 2 bundle directory.
#' @param chunk_size Ordered source columns per chunk.
#' @param overwrite Whether to replace an existing bundle.
#' @return A schema 2 bundle output.
#' @export
ngeo_migrate_support_map_exchange <- function(
    path,
    output,
    chunk_size = 100000L,
    overwrite = FALSE) {
  map <- if (dir.exists(path) ||
      tolower(basename(path)) == "manifest.json") {
    read_ngeo_support_bundle(path)
  } else {
    read_ngeo_support_map(path)
  }
  write_ngeo_support_bundle(
    map, output, chunk_size = chunk_size, overwrite = overwrite
  )
}

#' @export
print.ngeo_support_bundle_output <- function(x, ...) {
  cat("<ngeo_support_bundle_output>\n  chunks: ", x$chunks,
      "\n  path: ", x$path, "\n", sep = "")
  invisible(x)
}
