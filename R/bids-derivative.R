# BIDS derivative publication.
#' Build a scoped BIDS derivative sidecar
#'
#' @param x An `ngeo` dataset.
#' @param entities Named BIDS spatial/file entities.
#' @param sources Optional source-file paths.
#' @param generated_by Optional generator metadata.
#'
#' @return A JSON-ready list.
#' @export
ngeo_bids_sidecar <- function(
    x,
    entities = list(),
    sources = character(),
    generated_by = NULL) {
  ngeo_validate(x, "basic")
  if (!is.list(entities) || is.null(names(entities)) ||
      any(!nzchar(names(entities)))) {
    .ngeo_abort("`entities` must be a named list.", "ngeo_error_argument")
  }
  list(
    Specification = "BIDS derivative sidecar subset",
    SpatialReference = x$base$coordinate_space$space_id %||% "unknown",
    DomainType = x$base$type,
    DomainHash = base_hash(x),
    Entities = entities,
    MeasurementSemantics = lapply(seq_len(nrow(x$layers)), function(i) {
      measure <- .ngeo_measures_for_layers(x, i)
      list(
        Name = x$layers$name[[i]],
        ValueType = measure$value_type[[1L]],
        SpatialSemantics = measure$support_behavior[[1L]],
        Units = measure$unit[[1L]]
      )
    }),
    Sources = as.character(sources),
    GeneratedBy = generated_by %||% list(
      Name = "neurogeo",
      Version = .ngeo_package_version()
    ),
    Provenance = x$history
  ) |>
    (\(sidecar) {
      ngeo_validate_bids_sidecar(sidecar, x)
      sidecar
    })()
}

#' Write a neurogeo object and BIDS derivative sidecar
#'
#' This writes one explicitly named derivative. It does not index or
#' orchestrate a BIDS dataset.
#'
#' @param x An `ngeo` dataset.
#' @param path Derivative data path.
#' @param entities Named BIDS entities.
#' @param sources Optional source files.
#' @param overwrite Whether to replace outputs.
#' @param collision Collision policy: error, overwrite, or choose the first
#'   available canonical run entity.
#' @param strict_name Whether path entities must exactly match `entities`.
#' @param ... Passed to the selected writer.
#'
#' @return Data and JSON paths.
#' @export
write_ngeo_bids_derivative <- function(
    x,
    path,
    entities = list(),
    sources = character(),
    overwrite = FALSE,
    collision = NULL,
    strict_name = FALSE,
    ...) {
  .ngeo_require("jsonlite", "BIDS derivative sidecar writing")
  collision <- collision %||% if (isTRUE(overwrite)) "overwrite" else "error"
  collision <- match.arg(collision, c("error", "overwrite", "version"))
  json_path <- .ngeo_bids_json_path(path)
  if (any(file.exists(c(path, json_path)))) {
    if (collision == "error") {
      .ngeo_abort("Derivative output already exists.", "ngeo_error_io")
    }
    if (collision == "version") {
      path <- .ngeo_bids_version_path(path)
      json_path <- .ngeo_bids_json_path(path)
      entities <- ngeo_bids_parse_name(path)$entities
    }
  }
  if (isTRUE(strict_name)) {
    parsed <- ngeo_bids_parse_name(path)
    .ngeo_validate_bids_entities(entities)
    if (!identical(
      unlist(parsed$entities, use.names = TRUE),
      unlist(entities, use.names = TRUE)
    )) {
      .ngeo_abort("Derivative path and declared entities differ.",
                  "ngeo_error_bids")
    }
  }
  sidecar <- ngeo_bids_sidecar(x, entities, sources)
  ngeo_validate_bids_sidecar(
    sidecar, x, if (isTRUE(strict_name)) path else NULL
  )
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  transaction <- tempfile(".ngeo-bids-", tmpdir = directory)
  dir.create(transaction)
  on.exit(unlink(transaction, recursive = TRUE), add = TRUE)
  temporary_data <- file.path(transaction, basename(path))
  temporary_json <- file.path(transaction, basename(json_path))
  lower <- tolower(temporary_data)
  if (grepl("\\.(dscalar|dlabel|dtseries)\\.nii$", lower)) {
    type <- sub("^.*\\.(dscalar|dlabel|dtseries)\\.nii$", "\\1", lower)
    write_ngeo_cifti(x, temporary_data, type = type)
  } else {
    write_ngeo(x, temporary_data, ...)
  }
  jsonlite::write_json(
    sidecar,
    temporary_json,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  backup_data <- file.path(transaction, ".backup-data")
  backup_json <- file.path(transaction, ".backup-json")
  if (file.exists(path) && !file.rename(path, backup_data)) {
    .ngeo_abort("Could not stage existing derivative.", "ngeo_error_io")
  }
  if (file.exists(json_path) && !file.rename(json_path, backup_json)) {
    if (file.exists(backup_data)) file.rename(backup_data, path)
    .ngeo_abort("Could not stage existing sidecar.", "ngeo_error_io")
  }
  success <- FALSE
  on.exit({
    if (!success) {
      if (file.exists(path)) unlink(path)
      if (file.exists(json_path)) unlink(json_path)
      if (file.exists(backup_data)) file.rename(backup_data, path)
      if (file.exists(backup_json)) file.rename(backup_json, json_path)
    }
  }, add = TRUE)
  if (!file.rename(temporary_data, path) ||
      !file.rename(temporary_json, json_path)) {
    .ngeo_abort("Could not atomically promote derivative transaction.",
                "ngeo_error_io")
  }
  success <- TRUE
  output <- c(
    data = normalizePath(path, winslash = "/", mustWork = TRUE),
    sidecar = normalizePath(json_path, winslash = "/", mustWork = TRUE)
  )
  attr(output, "sha256") <- vapply(
    output,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  invisible(output)
}
