.ngeo_assert_output <- function(path, overwrite) {
  .ngeo_assert_scalar_character(path, "path")
  if (!is.logical(overwrite) || length(overwrite) != 1L ||
      is.na(overwrite)) {
    .ngeo_abort(
      "`overwrite` must be TRUE or FALSE.",
      "ngeo_error_argument"
    )
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    .ngeo_abort(
      sprintf("Output directory `%s` does not exist.", parent),
      "ngeo_error_argument"
    )
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    .ngeo_abort(
      sprintf("Output `%s` already exists.", path),
      "ngeo_error_overwrite"
    )
  }
  invisible(path)
}

.ngeo_backend_write <- function(format, path, code) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      code(),
      warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) {
      if (inherits(error, "ngeo_error")) {
        stop(error)
      }
      .ngeo_abort(
        sprintf(
          "Failed to write %s `%s`: %s",
          format,
          basename(path),
          conditionMessage(error)
        ),
        "ngeo_error_io"
      )
    }
  )
  if (length(warnings)) {
    .ngeo_warn(
      sprintf(
        "%s writer warning for `%s`: %s",
        format,
        basename(path),
        paste(unique(warnings), collapse = "; ")
      ),
      "ngeo_warning_io"
    )
  }
  value
}

.ngeo_safe_name <- function(x) {
  result <- gsub("[^A-Za-z0-9._-]+", "_", x)
  result[!nzchar(result)] <- "map"
  make.unique(result, sep = "_")
}

.ngeo_insert_suffix <- function(path, suffix, extension) {
  pattern <- paste0(gsub("[.]", "[.]", extension), "$")
  if (!grepl(pattern, path, ignore.case = TRUE)) {
    return(paste0(path, suffix))
  }
  sub(pattern, paste0(suffix, extension), path, ignore.case = TRUE)
}

.ngeo_gifti_output <- function(path, suffix, extension) {
  sub(
    "[.]surf[.]gii$",
    paste0(suffix, extension),
    path,
    ignore.case = TRUE
  )
}

.ngeo_volume_arrays <- function(x, fill = 0) {
  n_map <- nrow(x$maps)
  dimensions <- c(x$domain$dim, if (n_map > 1L) n_map)
  values <- array(fill, dim = dimensions)
  if (!is.null(x$values)) {
    for (map in seq_len(n_map)) {
      if (n_map > 1L) {
        values[cbind(x$domain$voxel_index, map)] <- x$values[, map]
      } else {
        values[x$domain$voxel_index] <- x$values[, map]
      }
    }
  }
  mask <- array(FALSE, dim = x$domain$dim)
  mask[x$domain$voxel_index] <- TRUE
  list(values = values, mask = mask)
}

.ngeo_write_json <- function(value, path, overwrite) {
  .ngeo_require("jsonlite", "JSON metadata writing")
  .ngeo_assert_output(path, overwrite)
  .ngeo_backend_write(
    "JSON",
    path,
    function() jsonlite::write_json(
      value,
      path,
      pretty = TRUE,
      auto_unbox = TRUE,
      na = "null",
      digits = NA
    )
  )
  path
}

#' Write an NGCS volume to NIfTI
#'
#' The returned mask path is part of the round-trip contract: active domain
#' membership is not inferred from zero or missing values.
#'
#' @param x An `ngeo_volume`.
#' @param path Output `.nii` or `.nii.gz` path.
#' @param mask_path Optional mask output path.
#' @param datatype RNifti output datatype.
#' @param overwrite Replace existing outputs.
#'
#' @return A file manifest with data, mask, and sidecar paths.
#' @export
write_ngeo_nifti <- function(x,
                             path,
                             mask_path = NULL,
                             datatype = "auto",
                             overwrite = FALSE) {
  .ngeo_require("RNifti", "NIfTI writing")
  ngeo_validate(x, "strict")
  if (!inherits(x, "ngeo_volume")) {
    .ngeo_abort(
      "NIfTI writing requires an `ngeo_volume`.",
      "ngeo_error_domain"
    )
  }
  if (!grepl("[.]nii([.]gz)?$", path, ignore.case = TRUE)) {
    .ngeo_abort(
      "NIfTI output must end in `.nii` or `.nii.gz`.",
      "ngeo_error_argument"
    )
  }
  mask_path <- mask_path %||% sub(
    "[.]nii([.]gz)?$",
    "_mask.nii.gz",
    path,
    ignore.case = TRUE
  )
  sidecar <- sub("[.]nii([.]gz)?$", ".json", path, ignore.case = TRUE)
  for (output in c(path, mask_path, sidecar)) {
    .ngeo_assert_output(output, overwrite)
  }
  arrays <- .ngeo_volume_arrays(x)
  transform <- structure(x$domain$affine, code = 2L)
  image <- RNifti::asNifti(arrays$values)
  image <- RNifti::`sform<-`(image, transform)
  mask_image <- RNifti::asNifti(array(
    as.integer(arrays$mask),
    dim = dim(arrays$mask)
  ))
  mask_image <- RNifti::`sform<-`(mask_image, transform)
  .ngeo_backend_write(
    "NIfTI",
    path,
    function() RNifti::writeNifti(
      image,
      path,
      datatype = datatype
    )
  )
  .ngeo_backend_write(
    "NIfTI mask",
    mask_path,
    function() RNifti::writeNifti(
      mask_image,
      mask_path,
      datatype = "uint8"
    )
  )
  .ngeo_write_json(
    list(
      Neurogeo = list(
        specification = x$provenance$spec_version,
        domain_hash = ngeo_domain_hash(x),
        maps = x$maps,
        measures = x$measures,
        mask = basename(mask_path)
      )
    ),
    sidecar,
    overwrite = overwrite
  )
  list(
    format = "nifti",
    data = normalizePath(path, winslash = "/"),
    mask = normalizePath(mask_path, winslash = "/"),
    sidecar = normalizePath(sidecar, winslash = "/"),
    capabilities = c(
      "voxel_mapping", "affine", "map_order", "values", "mask"
    )
  )
}

.ngeo_label_colortable <- function(label) {
  table <- label$table
  if (is.null(table) || !is.data.frame(table)) {
    .ngeo_abort(
      "Label writing requires an explicit color table.",
      "ngeo_error_labels"
    )
  }
  if (all(c("struct_name", "r", "g", "b", "a") %in% names(table))) {
    result <- table[, c("struct_name", "r", "g", "b", "a"), drop = FALSE]
  } else if (all(c(
    "label", "Red", "Green", "Blue", "Alpha"
  ) %in% names(table))) {
    result <- data.frame(
      struct_name = table$label,
      r = as.integer(table$Red),
      g = as.integer(table$Green),
      b = as.integer(table$Blue),
      a = as.integer(table$Alpha),
      stringsAsFactors = FALSE
    )
  } else {
    .ngeo_abort(
      "Label color table columns are not supported for writing.",
      "ngeo_error_labels"
    )
  }
  result$struct_index <- seq_len(nrow(result)) - 1L
  result
}

.ngeo_write_annot <- function(label, path, overwrite) {
  .ngeo_assert_output(path, overwrite)
  colortable <- .ngeo_label_colortable(label)
  .ngeo_backend_write(
    "FreeSurfer annotation",
    path,
    function() freesurferformats::write.fs.annot(
      path,
      num_vertices = length(label$values),
      colortable = colortable,
      labels_as_colorcodes = as.integer(label$values)
    )
  )
  path
}

#' Write a surface to GIFTI geometry, metric, and label files
#'
#' @param x An `ngeo_surface`.
#' @param path Primary geometry `.surf.gii` path.
#' @param overwrite Replace existing outputs.
#'
#' @return A manifest of named geometry, data, label, and sidecar paths.
#' @export
write_ngeo_gifti <- function(x, path, overwrite = FALSE) {
  .ngeo_require("freesurferformats", "GIFTI writing")
  ngeo_validate(x, "strict")
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "GIFTI writing requires an `ngeo_surface`.",
      "ngeo_error_domain"
    )
  }
  if (!grepl("[.]surf[.]gii$", path, ignore.case = TRUE)) {
    .ngeo_abort(
      "Primary GIFTI geometry must end in `.surf.gii`.",
      "ngeo_error_argument"
    )
  }
  coordinate_names <- names(x$domain$coordinates)
  paths <- vapply(coordinate_names, function(name) {
    if (identical(name, x$domain$active_coordinates)) {
      path
    } else {
      .ngeo_gifti_output(
        path,
        paste0(".", .ngeo_safe_name(name)),
        ".surf.gii"
      )
    }
  }, character(1))
  names(paths) <- coordinate_names
  for (i in seq_along(paths)) {
    .ngeo_assert_output(paths[[i]], overwrite)
    .ngeo_backend_write(
      "GIFTI geometry",
      paths[[i]],
      function() freesurferformats::write.fs.surface.gii(
        paths[[i]],
        if (ncol(x$domain$coordinates[[i]]) == 2L) {
          cbind(x$domain$coordinates[[i]], 0)
        } else {
          x$domain$coordinates[[i]]
        },
        x$domain$faces
      )
    )
  }
  data_paths <- character()
  if (!is.null(x$values)) {
    safe_maps <- .ngeo_safe_name(x$maps$name)
    data_paths <- vapply(seq_len(nrow(x$maps)), function(i) {
      output <- .ngeo_gifti_output(
        path,
        paste0(".", safe_maps[[i]]),
        ".shape.gii"
      )
      .ngeo_assert_output(output, overwrite)
      .ngeo_backend_write(
        "GIFTI metric",
        output,
        function() freesurferformats::write.fs.morph.gii(
          output,
          x$values[, i]
        )
      )
      output
    }, character(1))
    names(data_paths) <- x$maps$name
  }
  label_paths <- character()
  if (length(x$labels)) {
    safe_labels <- .ngeo_safe_name(names(x$labels))
    label_paths <- vapply(seq_along(x$labels), function(i) {
      output <- .ngeo_gifti_output(
        path,
        paste0(".", safe_labels[[i]]),
        ".label.gii"
      )
      temporary <- tempfile(fileext = ".annot")
      on.exit(unlink(temporary), add = TRUE)
      .ngeo_write_annot(x$labels[[i]], temporary, overwrite = TRUE)
      annot <- freesurferformats::read.fs.annot(temporary)
      .ngeo_assert_output(output, overwrite)
      .ngeo_backend_write(
        "GIFTI label",
        output,
        function() freesurferformats::write.fs.annot.gii(output, annot)
      )
      output
    }, character(1))
    names(label_paths) <- names(x$labels)
  }
  sidecar <- .ngeo_gifti_output(path, "", ".json")
  .ngeo_write_json(
    list(
      Neurogeo = list(
        specification = x$provenance$spec_version,
        domain_hash = ngeo_domain_hash(x),
        active_coordinates = x$domain$active_coordinates,
        coordinate_roles = stats::setNames(
          x$domain$coordinate_meta$role,
          x$domain$coordinate_meta$name
        ),
        maps = x$maps,
        measures = x$measures
      )
    ),
    sidecar,
    overwrite
  )
  list(
    format = "gifti",
    geometry = stats::setNames(
      normalizePath(paths, winslash = "/"),
      names(paths)
    ),
    data = stats::setNames(
      normalizePath(data_paths, winslash = "/", mustWork = FALSE),
      names(data_paths)
    ),
    labels = stats::setNames(
      normalizePath(label_paths, winslash = "/", mustWork = FALSE),
      names(label_paths)
    ),
    sidecar = normalizePath(sidecar, winslash = "/"),
    coordinate_roles = stats::setNames(
      x$domain$coordinate_meta$role,
      x$domain$coordinate_meta$name
    ),
    capabilities = c(
      "surface_geometry", "coordinate_sets", "map_order",
      "values", "labels"
    )
  )
}

#' Write an NGCS surface or volume to FreeSurfer formats
#'
#' @param x An `ngeo_surface` or `ngeo_volume`.
#' @param path Primary surface or MGH/MGZ path.
#' @param overwrite Replace existing outputs.
#'
#' @return A file manifest.
#' @export
write_ngeo_freesurfer <- function(x, path, overwrite = FALSE) {
  .ngeo_require("freesurferformats", "FreeSurfer format writing")
  ngeo_validate(x, "strict")
  if (inherits(x, "ngeo_volume")) {
    if (!grepl("[.]m(gh|gz)$", path, ignore.case = TRUE)) {
      .ngeo_abort(
        "FreeSurfer volume output must end in `.mgh` or `.mgz`.",
        "ngeo_error_argument"
      )
    }
    .ngeo_assert_output(path, overwrite)
    arrays <- .ngeo_volume_arrays(x)
    .ngeo_backend_write(
      "FreeSurfer MGH/MGZ",
      path,
      function() freesurferformats::write.fs.mgh(
        path,
        arrays$values,
        vox2ras_matrix = x$domain$affine
      )
    )
    return(list(
      format = "freesurfer",
      data = normalizePath(path, winslash = "/"),
      capabilities = c("affine", "map_order", "values")
    ))
  }
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "FreeSurfer writing requires a surface or volume domain.",
      "ngeo_error_domain"
    )
  }
  .ngeo_assert_output(path, overwrite)
  .ngeo_backend_write(
    "FreeSurfer surface",
    path,
    function() freesurferformats::write.fs.surface(
      path,
      x$domain$coordinates[[x$domain$active_coordinates]],
      x$domain$faces
    )
  )
  data_paths <- character()
  if (!is.null(x$values)) {
    data_paths <- vapply(seq_len(nrow(x$maps)), function(i) {
      output <- paste0(path, ".", .ngeo_safe_name(x$maps$name[[i]]), ".curv")
      .ngeo_assert_output(output, overwrite)
      .ngeo_backend_write(
        "FreeSurfer morphometry",
        output,
        function() freesurferformats::write.fs.morph(
          output,
          x$values[, i],
          format = "curv"
        )
      )
      output
    }, character(1))
    names(data_paths) <- x$maps$name
  }
  label_paths <- character()
  if (length(x$labels)) {
    label_paths <- vapply(seq_along(x$labels), function(i) {
      output <- paste0(path, ".", .ngeo_safe_name(names(x$labels)[[i]]), ".annot")
      .ngeo_write_annot(x$labels[[i]], output, overwrite)
    }, character(1))
    names(label_paths) <- names(x$labels)
  }
  list(
    format = "freesurfer",
    geometry = normalizePath(path, winslash = "/"),
    data = stats::setNames(
      normalizePath(data_paths, winslash = "/", mustWork = FALSE),
      names(data_paths)
    ),
    labels = stats::setNames(
      normalizePath(label_paths, winslash = "/", mustWork = FALSE),
      names(label_paths)
    ),
    capabilities = c("surface_geometry", "map_order", "values", "labels")
  )
}

#' Write a supported NGCS dataset
#'
#' @param x An `ngeo` dataset.
#' @param path Primary output path.
#' @param format Output format.
#' @param ... Format-specific writer arguments.
#'
#' @return A writer manifest.
#' @export
write_ngeo <- function(x,
                       path,
                       format = c("auto", "nifti", "gifti", "freesurfer"),
                       ...) {
  format <- match.arg(format)
  if (identical(format, "auto")) {
    format <- if (grepl("[.]nii([.]gz)?$", path, ignore.case = TRUE)) {
      "nifti"
    } else if (grepl("[.]gii$", path, ignore.case = TRUE)) {
      "gifti"
    } else {
      "freesurfer"
    }
  }
  switch(
    format,
    nifti = write_ngeo_nifti(x, path, ...),
    gifti = write_ngeo_gifti(x, path, ...),
    freesurfer = write_ngeo_freesurfer(x, path, ...)
  )
}

.ngeo_redact_provenance <- function(x, policy, name = NULL) {
  if (is.list(x)) {
    fields <- names(x)
    result <- lapply(seq_along(x), function(i) {
      field <- if (is.null(fields)) NULL else fields[[i]]
      .ngeo_redact_provenance(x[[i]], policy, field)
    })
    names(result) <- names(x)
    return(result)
  }
  path_field <- !is.null(name) && (
    name %in% c("source", "source_id", "path", "paths") ||
      grepl("(_path|_file)$", name)
  )
  checksum_field <- !is.null(name) && grepl("^checksum", name)
  if (path_field && is.character(x)) {
    return(if (identical(policy, "paths")) basename(x) else "<redacted>")
  }
  if (checksum_field && identical(policy, "all")) {
    return(NULL)
  }
  x
}

#' Export auditable provenance as JSON-compatible data
#'
#' @param x An `ngeo` dataset.
#' @param path Optional JSON output path. If `NULL`, return the record only.
#' @param redact `"none"`, basename-only `"paths"`, or `"all"` sensitive
#'   source identifiers and checksums.
#' @param overwrite Replace an existing JSON output.
#'
#' @return A JSON-compatible provenance record, invisibly when written.
#' @export
ngeo_export_provenance <- function(
    x,
    path = NULL,
    redact = c("none", "paths", "all"),
    overwrite = FALSE) {
  ngeo_validate(x, "basic")
  redact <- match.arg(redact)
  provenance <- if (identical(redact, "none")) {
    x$provenance
  } else {
    .ngeo_redact_provenance(x$provenance, redact)
  }
  record <- list(
    schema = "neurogeo-provenance-1.0",
    specification = x$provenance$spec_version,
    package_version = x$provenance$package_version,
    domain_type = x$domain$type,
    domain_hash = ngeo_domain_hash(x),
    element_count = nrow(x$domain$elements),
    maps = x$maps,
    measures = x$measures,
    provenance = provenance,
    redaction = redact
  )
  if (is.null(path)) {
    return(record)
  }
  .ngeo_write_json(record, path, overwrite)
  invisible(record)
}
