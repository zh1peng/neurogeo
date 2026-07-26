.ngeo_example_manifest_path <- function() {
  path <- system.file(
    "extdata", "reference-4.1", "manifest.csv",
    package = "neurogeo"
  )
  if (!nzchar(path) || !file.exists(path)) {
    .ngeo_abort(
      "Bundled example-data manifest is unavailable.",
      "ngeo_error_io"
    )
  }
  path
}

#' Discover auditable neuroimaging format examples
#'
#' Returns metadata and installed paths for the small upstream reference
#' files used by the format tutorials and conformance tests. Each file is
#' pinned by source commit, byte size, license, and SHA-256.
#'
#' @param name Optional exact fixture name. Use `NULL` to list every fixture.
#' @param verify Whether to verify byte size and SHA-256 before returning.
#'
#' @return A data frame with source metadata, local paths, and verification
#'   status.
#' @export
ngeo_example_data <- function(name = NULL, verify = TRUE) {
  manifest_path <- .ngeo_example_manifest_path()
  manifest <- utils::read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) ||
        !nzchar(name)) {
      .ngeo_abort(
        "`name` must be one non-empty fixture name.",
        "ngeo_error_argument"
      )
    }
    manifest <- manifest[manifest$name == name, , drop = FALSE]
    if (!nrow(manifest)) {
      .ngeo_abort(
        sprintf("Unknown example-data fixture `%s`.", name),
        "ngeo_error_argument"
      )
    }
  }

  root <- dirname(manifest_path)
  paths <- file.path(root, manifest$file)
  present <- file.exists(paths)
  if (any(!present)) {
    .ngeo_abort(
      paste0(
        "Bundled example-data file is missing: ",
        manifest$file[which(!present)[[1L]]]
      ),
      "ngeo_error_io"
    )
  }
  manifest$path <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  manifest$verified <- NA
  if (isTRUE(verify)) {
    sizes <- unname(file.info(paths)$size)
    hashes <- vapply(
      paths,
      digest::digest,
      character(1),
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    )
    manifest$verified <- sizes == as.numeric(manifest$size) &
      hashes == manifest$sha256
    if (any(!manifest$verified)) {
      .ngeo_abort(
        paste0(
          "Example-data integrity check failed: ",
          manifest$name[which(!manifest$verified)[[1L]]]
        ),
        "ngeo_error_io"
      )
    }
  }
  manifest
}
