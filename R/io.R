.ngeo_require <- function(package, feature) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .ngeo_abort(
      sprintf(
        "`%s` is required for %s. Install it with install.packages(\"%s\").",
        package, feature, package
      ),
      "ngeo_error_backend"
    )
  }
  invisible(TRUE)
}

.ngeo_backend_read <- function(format, path, code) {
  backend_warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      code(),
      warning = function(warning) {
        backend_warnings <<- c(
          backend_warnings,
          conditionMessage(warning)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) {
      if (inherits(error, "ngeo_error")) {
        stop(error)
      }
      .ngeo_abort(
        sprintf(
          "Failed to read %s `%s`: %s",
          format,
          basename(path),
          conditionMessage(error)
        ),
        "ngeo_error_io"
      )
    }
  )
  if (length(backend_warnings)) {
    .ngeo_warn(
      sprintf(
        "%s reader warning for `%s`: %s",
        format,
        basename(path),
        paste(unique(backend_warnings), collapse = "; ")
      ),
      "ngeo_warning_io"
    )
  }
  value
}

.ngeo_source_record <- function(path, importer, checksum = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  info <- file.info(path)
  list(
    source_id = path,
    size = unname(info$size),
    checksum_md5 = if (isTRUE(checksum)) {
      unname(tools::md5sum(path))
    } else {
      NULL
    },
    bids_entities = .ngeo_bids_entities(path),
    importer = importer,
    read_time_utc = format(
      Sys.time(),
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    )
  )
}

.ngeo_bids_entities <- function(path) {
  tokens <- strsplit(basename(path), "_", fixed = TRUE)[[1L]]
  entities <- list()
  for (name in c("space", "hemi", "den", "res")) {
    token <- tokens[startsWith(tokens, paste0(name, "-"))]
    if (length(token)) {
      value <- sub(paste0("^", name, "-"), "", token[[1L]])
      value <- sub("\\..*$", "", value)
      entities[[name]] <- value
    }
  }
  entities
}

.ngeo_append_import_provenance <- function(x,
                                           paths,
                                           importer,
                                           metadata = list(),
                                           checksum = TRUE) {
  sources <- lapply(
    paths,
    .ngeo_source_record,
    importer = importer,
    checksum = checksum
  )
  x$provenance$sources <- c(x$provenance$sources %||% list(), sources)
  x$provenance$operations <- c(
    x$provenance$operations %||% list(),
    list(.ngeo_operation(
      importer,
      list(
        source_count = length(paths),
        metadata = metadata
      )
    ))
  )
  x
}

.ngeo_read_sidecar <- function(path) {
  .ngeo_require("jsonlite", "JSON sidecar reading")
  .ngeo_backend_read(
    "JSON sidecar",
    path,
    function() jsonlite::fromJSON(path, simplifyVector = FALSE)
  )
}

.ngeo_bids_sidecar <- function(path) {
  sidecar <- sub("\\.nii(\\.gz)?$", ".json", path, ignore.case = TRUE)
  if (identical(sidecar, path) || !file.exists(sidecar)) {
    return(NULL)
  }
  .ngeo_read_sidecar(sidecar)
}

.ngeo_path_extension <- function(path) {
  lower <- tolower(basename(path))
  if (grepl("\\.nii\\.gz$", lower)) {
    return("nii.gz")
  }
  tools::file_ext(lower)
}

#' Read a supported neuroimaging object
#'
#' @param x Primary input path.
#' @param geometry Optional geometry path.
#' @param data Optional data paths.
#' @param labels Optional labels path.
#' @param surfaces Optional CIFTI surface paths.
#' @param mask Optional volume mask.
#' @param format Explicit format, or automatic dispatch.
#' @param ... Format-specific arguments.
#'
#' @return An `ngeo` object.
#' @export
read_ngeo <- function(x = NULL,
                      geometry = NULL,
                      data = NULL,
                      labels = NULL,
                      surfaces = NULL,
                      mask = NULL,
                      format = c(
                        "auto", "nifti", "gifti", "cifti", "freesurfer"
                      ),
                      ...) {
  format <- match.arg(format)
  primary <- x %||% geometry
  if (is.null(primary) || !is.character(primary) || !length(primary)) {
    .ngeo_abort(
      "Provide `x` or `geometry` as a file path.",
      "ngeo_error_argument"
    )
  }

  if (identical(format, "auto")) {
    lower <- tolower(primary[[1L]])
    format <- if (grepl(
      "\\.(dscalar|dlabel|dtseries)\\.nii$",
      lower
    )) {
      "cifti"
    } else if (grepl("\\.gii$", lower)) {
      "gifti"
    } else if (grepl("\\.nii(\\.gz)?$", lower)) {
      "nifti"
    } else {
      "freesurfer"
    }
  }

  switch(
    format,
    nifti = read_ngeo_nifti(
      primary,
      mask = mask,
      ...
    ),
    gifti = read_ngeo_gifti(
      geometry = geometry %||% primary,
      data = data,
      labels = labels,
      ...
    ),
    cifti = read_ngeo_cifti(
      primary,
      surfaces = surfaces,
      ...
    ),
    freesurfer = read_ngeo_freesurfer(
      x = primary,
      geometry = geometry,
      data = data,
      labels = labels,
      ...
    )
  )
}
