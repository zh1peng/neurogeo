.ngeo_gifti_indices <- function(gii, intent) {
  which(toupper(gii$data_info$Intent) %in% toupper(intent))
}

.ngeo_gifti_coordinate_role <- function(name) {
  lower <- tolower(name)
  if (grepl("inflated", lower)) {
    "visualization"
  } else if (grepl("sphere", lower)) {
    "registration"
  } else if (grepl("(^|[._-])flat([._-]|$)", lower)) {
    "chart"
  } else {
    "anatomical"
  }
}

.ngeo_gifti_geometry <- function(paths, coordinate_roles = NULL) {
  if (is.null(names(paths)) || any(!nzchar(names(paths)))) {
    names(paths) <- tools::file_path_sans_ext(basename(paths))
  }
  coordinates <- vector("list", length(paths))
  metadata <- vector("list", length(paths))
  faces <- NULL

  for (i in seq_along(paths)) {
    gii <- .ngeo_backend_read(
      "GIFTI geometry",
      paths[[i]],
      function() gifti::read_gifti(paths[[i]])
    )
    point_index <- .ngeo_gifti_indices(
      gii,
      "NIFTI_INTENT_POINTSET"
    )
    triangle_index <- .ngeo_gifti_indices(
      gii,
      "NIFTI_INTENT_TRIANGLE"
    )
    if (length(point_index) != 1L || length(triangle_index) != 1L) {
      .ngeo_abort(
        sprintf(
          "GIFTI geometry `%s` must contain one pointset and one triangle array.",
          paths[[i]]
        ),
        "ngeo_error_format"
      )
    }
    coordinates[[i]] <- as.matrix(gii$data[[point_index]])
    current_faces <- as.matrix(gii$data[[triangle_index]])
    storage.mode(current_faces) <- "integer"
    if (is.null(faces)) {
      faces <- current_faces
    } else if (!identical(faces, current_faces)) {
      .ngeo_abort(
        "GIFTI coordinate files do not share identical topology.",
        "ngeo_error_alignment"
      )
    }
    metadata[[i]] <- list(
      file_meta = gii$file_meta,
      pointset_meta = gii$data_meta[[point_index]],
      pointset_transform = gii$parsed_transformations[[point_index]]
    )
  }
  names(coordinates) <- names(paths)
  names(metadata) <- names(paths)
  roles <- coordinate_roles %||%
    vapply(names(paths), .ngeo_gifti_coordinate_role, character(1))
  if (length(roles) != length(paths)) {
    .ngeo_abort(
      "`coordinate_roles` must align with GIFTI geometry files.",
      "ngeo_error_alignment"
    )
  }
  chart_index <- which(roles == "chart")
  for (i in chart_index) {
    if (ncol(coordinates[[i]]) == 3L &&
        all(abs(coordinates[[i]][, 3L]) <= sqrt(.Machine$double.eps))) {
      coordinates[[i]] <- coordinates[[i]][, 1:2, drop = FALSE]
    }
  }
  list(
    coordinates = coordinates,
    faces = faces,
    roles = roles,
    metadata = metadata
  )
}

.ngeo_gifti_data <- function(paths, n_vertex) {
  if (is.null(paths)) {
    return(list(values = NULL, maps = NULL, metadata = list()))
  }
  if (!is.character(paths) || any(!file.exists(paths))) {
    .ngeo_abort(
      "All GIFTI `data` paths must exist.",
      "ngeo_error_argument"
    )
  }

  columns <- list()
  map_names <- character()
  metadata <- list()
  for (file_index in seq_along(paths)) {
    path <- paths[[file_index]]
    gii <- .ngeo_backend_read(
      "GIFTI data",
      path,
      function() gifti::read_gifti(path)
    )
    array_index <- which(!toupper(gii$data_info$Intent) %in% c(
      "NIFTI_INTENT_POINTSET",
      "NIFTI_INTENT_TRIANGLE",
      "NIFTI_INTENT_LABEL"
    ))
    if (!length(array_index)) {
      .ngeo_abort(
        sprintf("GIFTI data file `%s` has no scalar data arrays.", paths[[file_index]]),
        "ngeo_error_format"
      )
    }
    file_name <- names(paths)[file_index]
    if (is.null(file_name) || is.na(file_name) || !nzchar(file_name)) {
      file_name <- tools::file_path_sans_ext(basename(paths[[file_index]]))
    }

    for (j in array_index) {
      value <- as.matrix(gii$data[[j]])
      if (nrow(value) != n_vertex && length(value) == n_vertex) {
        value <- matrix(value, ncol = 1L)
      }
      if (nrow(value) != n_vertex) {
        .ngeo_abort(
          sprintf(
            "GIFTI data `%s` has %d rows but geometry has %d vertices.",
            paths[[file_index]], nrow(value), n_vertex
          ),
          "ngeo_error_alignment"
        )
      }
      for (column in seq_len(ncol(value))) {
        columns[[length(columns) + 1L]] <- value[, column]
        suffix <- if (ncol(value) > 1L) paste0("_", column) else ""
        map_names <- c(map_names, paste0(file_name, suffix))
      }
      metadata[[length(metadata) + 1L]] <- list(
        source = paths[[file_index]],
        intent = gii$data_info$Intent[[j]],
        data_meta = gii$data_meta[[j]]
      )
    }
  }

  values <- do.call(cbind, columns)
  colnames(values) <- make.unique(map_names)
  list(
    values = values,
    maps = data.frame(
      name = colnames(values),
      stringsAsFactors = FALSE
    ),
    metadata = metadata
  )
}

.ngeo_gifti_labels <- function(paths, n_vertex) {
  if (is.null(paths)) {
    return(list())
  }
  if (!is.character(paths) || any(!file.exists(paths))) {
    .ngeo_abort(
      "All GIFTI `labels` paths must exist.",
      "ngeo_error_argument"
    )
  }

  result <- list()
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    gii <- .ngeo_backend_read(
      "GIFTI label",
      path,
      function() gifti::read_gifti(path)
    )
    label_index <- .ngeo_gifti_indices(gii, "NIFTI_INTENT_LABEL")
    if (length(label_index) != 1L) {
      .ngeo_abort(
        sprintf("GIFTI label `%s` must contain one label array.", paths[[i]]),
        "ngeo_error_format"
      )
    }
    values <- as.integer(gii$data[[label_index]])
    if (length(values) != n_vertex) {
      .ngeo_abort(
        sprintf(
          "GIFTI label `%s` has %d values but geometry has %d vertices.",
          paths[[i]], length(values), n_vertex
        ),
        "ngeo_error_alignment"
      )
    }
    name <- names(paths)[i]
    if (is.null(name) || is.na(name) || !nzchar(name)) {
      name <- tools::file_path_sans_ext(basename(paths[[i]]))
    }
    table <- if (is.null(gii$label)) {
      NULL
    } else {
      data.frame(
        label = rownames(gii$label),
        gii$label,
        row.names = NULL,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    result[[name]] <- list(
      values = values,
      table = table,
      source = paths[[i]]
    )
  }
  result
}

#' Read GIFTI geometry, metric, and labels
#'
#' @param geometry One or more GIFTI surface paths.
#' @param data Optional metric/shape/functional GIFTI paths.
#' @param labels Optional label GIFTI paths.
#' @param maps Optional map metadata overriding generated names.
#' @param measures Optional measurement semantics.
#' @param space Optional `ngeo_space`.
#' @param coordinate_roles Optional roles for coordinate files.
#' @param strict Whether to run strict validation.
#' @param checksum Whether to record MD5 checksums.
#'
#' @return An `ngeo_surface` object.
#' @export
read_ngeo_gifti <- function(geometry,
                            data = NULL,
                            labels = NULL,
                            maps = NULL,
                            measures = NULL,
                            space = NULL,
                            coordinate_roles = NULL,
                            strict = TRUE,
                            checksum = TRUE) {
  .ngeo_require("gifti", "GIFTI reading")
  if (!is.character(geometry) || !length(geometry) ||
      any(!file.exists(geometry))) {
    .ngeo_abort(
      "All GIFTI `geometry` paths must exist.",
      "ngeo_error_argument"
    )
  }

  geometry_data <- .ngeo_gifti_geometry(
    geometry,
    coordinate_roles = coordinate_roles
  )
  n_vertex <- nrow(geometry_data$coordinates[[1L]])
  value_data <- .ngeo_gifti_data(data, n_vertex)
  label_data <- .ngeo_gifti_labels(labels, n_vertex)
  maps <- maps %||% value_data$maps

  space <- space %||% ngeo_space(
    "unknown",
    kind = "surface",
    source_metadata = list(gifti = geometry_data$metadata)
  )
  x <- ngeo_surface(
    coordinates = geometry_data$coordinates,
    faces = geometry_data$faces,
    values = value_data$values,
    maps = maps,
    measures = measures,
    labels = label_data,
    space = space,
    coordinate_roles = geometry_data$roles,
    index_base = "zero",
    source_index_base = 0L
  )
  x <- .ngeo_append_import_provenance(
    x,
    paths = c(geometry, data, labels),
    importer = "read_ngeo_gifti",
    metadata = list(
      geometry = geometry_data$metadata,
      data = value_data$metadata
    ),
    checksum = checksum
  )
  if (isTRUE(strict)) {
    ngeo_validate(x, "strict")
  }
  x
}
