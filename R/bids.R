# BIDS filename and sidecar contracts.
.ngeo_bids_entity_order <- c(
  "sub", "ses", "task", "acq", "ce", "rec", "dir", "run", "mod",
  "echo", "flip", "inv", "mt", "part", "recording", "proc",
  "space", "atlas", "hemi", "res", "den", "label", "desc"
)

.ngeo_bids_extensions <- c(
  ".dtseries.nii", ".dscalar.nii", ".dlabel.nii",
  ".nii.gz", ".surf.gii", ".shape.gii", ".label.gii", ".func.gii",
  ".nii", ".gii", ".tsv", ".json"
)

.ngeo_validate_bids_entities <- function(entities) {
  if (!is.list(entities) || is.null(names(entities)) ||
      any(!nzchar(names(entities))) || anyDuplicated(names(entities)) ||
      any(!names(entities) %in% .ngeo_bids_entity_order) ||
      any(vapply(entities, length, integer(1)) != 1L) ||
      any(vapply(entities, function(value) {
        !is.character(value) || is.na(value) ||
          !grepl("^[A-Za-z0-9]+$", value)
      }, logical(1)))) {
    .ngeo_abort(
      "BIDS entities must be known, unique, named alphanumeric scalars.",
      "ngeo_error_bids"
    )
  }
  invisible(entities)
}

#' Parse a scoped BIDS derivative file name
#'
#' @param path File path or basename.
#' @return An `ngeo_bids_name`.
#' @export
ngeo_bids_parse_name <- function(path) {
  .ngeo_assert_scalar_character(path, "path")
  name <- basename(path)
  extension <- .ngeo_bids_extensions[endsWith(
    tolower(name), .ngeo_bids_extensions
  )]
  if (!length(extension)) {
    .ngeo_abort("Unsupported BIDS derivative extension.",
                "ngeo_error_bids")
  }
  extension <- extension[[which.max(nchar(extension))]]
  stem <- substr(name, 1L, nchar(name) - nchar(extension))
  parts <- strsplit(stem, "_", fixed = TRUE)[[1L]]
  if (!length(parts) || any(!nzchar(parts))) {
    .ngeo_abort("BIDS derivative name is empty or malformed.",
                "ngeo_error_bids")
  }
  suffix <- parts[[length(parts)]]
  entity_parts <- parts[-length(parts)]
  if (!grepl("^[A-Za-z0-9]+$", suffix) || !length(entity_parts)) {
    .ngeo_abort("BIDS derivative requires entities and a suffix.",
                "ngeo_error_bids")
  }
  entities <- lapply(entity_parts, function(part) {
    match <- regmatches(
      part,
      regexec("^([A-Za-z0-9]+)-([A-Za-z0-9]+)$", part)
    )[[1L]]
    if (length(match) != 3L) {
      .ngeo_abort("Malformed BIDS entity.", "ngeo_error_bids")
    }
    stats::setNames(list(match[[3L]]), match[[2L]])
  })
  entities <- do.call(c, entities)
  .ngeo_validate_bids_entities(entities)
  order <- match(names(entities), .ngeo_bids_entity_order)
  if (is.unsorted(order, strictly = TRUE)) {
    .ngeo_abort("BIDS entities are not in canonical order.",
                "ngeo_error_bids")
  }
  result <- list(
    entities = entities,
    suffix = suffix,
    extension = extension,
    basename = name,
    directory = dirname(path)
  )
  class(result) <- "ngeo_bids_name"
  result
}

#' Build a canonical scoped BIDS derivative file name
#'
#' @param entities Named BIDS entities.
#' @param suffix Derivative suffix.
#' @param extension Supported extension, including its leading dot.
#' @return A canonical basename.
#' @export
ngeo_bids_build_name <- function(entities, suffix, extension) {
  .ngeo_validate_bids_entities(entities)
  .ngeo_assert_scalar_character(suffix, "suffix")
  .ngeo_assert_scalar_character(extension, "extension")
  extension <- tolower(extension)
  if (!grepl("^[A-Za-z0-9]+$", suffix) ||
      !extension %in% .ngeo_bids_extensions) {
    .ngeo_abort("BIDS suffix or extension is unsupported.",
                "ngeo_error_bids")
  }
  order <- order(match(names(entities), .ngeo_bids_entity_order))
  entities <- entities[order]
  paste0(
    paste0(names(entities), "-", unlist(entities), collapse = "_"),
    "_", suffix, extension
  )
}

.ngeo_bids_json_path <- function(path) {
  lower <- tolower(path)
  for (extension in .ngeo_bids_extensions) {
    if (endsWith(lower, extension)) {
      return(paste0(
        substr(path, 1L, nchar(path) - nchar(extension)),
        ".json"
      ))
    }
  }
  paste0(path, ".json")
}

#' Validate a scoped BIDS derivative sidecar
#'
#' @param sidecar JSON-ready sidecar list.
#' @param x Optional matching `ngeo` dataset.
#' @param path Optional canonical derivative path.
#' @return `sidecar`, invisibly.
#' @export
ngeo_validate_bids_sidecar <- function(sidecar, x = NULL, path = NULL) {
  required <- c(
    "Specification", "SpatialReference", "DomainType", "DomainHash",
    "Entities", "MeasurementSemantics", "Sources", "GeneratedBy",
    "Provenance"
  )
  valid <- is.list(sidecar) && all(required %in% names(sidecar)) &&
    is.character(sidecar$SpatialReference) &&
    length(sidecar$SpatialReference) == 1L &&
    is.character(sidecar$DomainType) &&
    length(sidecar$DomainType) == 1L &&
    is.character(sidecar$DomainHash) &&
    length(sidecar$DomainHash) == 1L &&
    is.list(sidecar$Entities) &&
    is.list(sidecar$MeasurementSemantics) &&
    is.list(sidecar$GeneratedBy) &&
    is.list(sidecar$Provenance)
  if (!valid) {
    .ngeo_abort("BIDS derivative sidecar structure is invalid.",
                "ngeo_error_bids")
  }
  .ngeo_validate_bids_entities(sidecar$Entities)
  semantics_valid <- all(vapply(
    sidecar$MeasurementSemantics,
    function(current) {
      is.list(current) &&
        all(c(
          "Name", "ValueType", "SpatialSemantics", "Units"
        ) %in% names(current))
    },
    logical(1)
  ))
  if (!semantics_valid) {
    .ngeo_abort("BIDS measurement semantics are incomplete.",
                "ngeo_error_bids")
  }
  if (!is.null(x)) {
    ngeo_validate(x, "basic")
    if (!identical(sidecar$DomainHash, base_hash(x)) ||
        !identical(sidecar$DomainType, x$base$type) ||
        length(sidecar$MeasurementSemantics) != nrow(x$layers)) {
      .ngeo_abort("BIDS sidecar does not match the dataset.",
                  "ngeo_error_base_mismatch")
    }
  }
  if (!is.null(path)) {
    parsed <- ngeo_bids_parse_name(path)
    if (!identical(
      unlist(sidecar$Entities, use.names = TRUE),
      unlist(parsed$entities, use.names = TRUE)
    )) {
      .ngeo_abort("BIDS path entities and sidecar entities differ.",
                  "ngeo_error_bids")
    }
  }
  invisible(sidecar)
}

.ngeo_bids_version_path <- function(path) {
  parsed <- ngeo_bids_parse_name(path)
  for (run in seq_len(9999L)) {
    entities <- parsed$entities
    entities$run <- as.character(run)
    candidate <- file.path(
      dirname(path),
      ngeo_bids_build_name(entities, parsed$suffix, parsed$extension)
    )
    if (!file.exists(candidate) &&
        !file.exists(.ngeo_bids_json_path(candidate))) {
      return(candidate)
    }
  }
  .ngeo_abort("No available BIDS run entity for collision versioning.",
              "ngeo_error_bids")
}

#' @export
print.ngeo_bids_name <- function(x, ...) {
  cat("<ngeo_bids_name> ", x$basename, "\n", sep = "")
  invisible(x)
}
