.ngeo_fs_surface_geometry <- function(geometry, coordinates = NULL) {
  paths <- c(active = geometry, coordinates)
  if (any(!file.exists(paths))) {
    .ngeo_abort(
      "All FreeSurfer surface coordinate files must exist.",
      "ngeo_error_argument"
    )
  }

  coordinate_values <- vector("list", length(paths))
  faces <- NULL
  metadata <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    surface <- .ngeo_backend_read(
      "FreeSurfer surface",
      path,
      function() freesurferformats::read.fs.surface(path)
    )
    coordinate_values[[i]] <- surface$vertices
    if (is.null(faces)) {
      faces <- surface$faces
    } else if (!identical(faces, surface$faces)) {
      .ngeo_abort(
        "FreeSurfer coordinate files do not share identical faces.",
        "ngeo_error_alignment"
      )
    }
    metadata[i] <- list(surface$internal %||% list())
  }
  names(coordinate_values) <- names(paths)
  names(metadata) <- names(paths)
  roles <- vapply(names(paths), .ngeo_gifti_coordinate_role, character(1))
  list(
    paths = paths,
    coordinates = coordinate_values,
    faces = faces,
    roles = roles,
    metadata = metadata
  )
}

.ngeo_fs_surface_data <- function(paths, n_vertex) {
  if (is.null(paths)) {
    return(list(values = NULL, layers = NULL))
  }
  if (!is.character(paths) || any(!file.exists(paths))) {
    .ngeo_abort(
      "All FreeSurfer `data` paths must exist.",
      "ngeo_error_argument"
    )
  }

  columns <- list()
  layer_names <- character()
  for (i in seq_along(paths)) {
    extension <- .ngeo_path_extension(paths[[i]])
    path <- paths[[i]]
    value <- if (extension %in% c("mgh", "mgz")) {
      .ngeo_backend_read(
        "FreeSurfer MGH/MGZ morphometry",
        path,
        function() freesurferformats::read.fs.mgh(path, flatten = TRUE)
      )
    } else {
      .ngeo_backend_read(
        "FreeSurfer morphometry",
        path,
        function() freesurferformats::read.fs.morph(path)
      )
    }
    value <- as.matrix(value)
    if (nrow(value) != n_vertex && length(value) %% n_vertex == 0L) {
      value <- matrix(value, nrow = n_vertex)
    }
    if (nrow(value) != n_vertex) {
      .ngeo_abort(
        sprintf(
          "FreeSurfer data `%s` has %d rows but geometry has %d vertices.",
          paths[[i]], nrow(value), n_vertex
        ),
        "ngeo_error_alignment"
      )
    }
    base_name <- names(paths)[i]
    if (is.null(base_name) || is.na(base_name) || !nzchar(base_name)) {
      base_name <- basename(paths[[i]])
    }
    for (column in seq_len(ncol(value))) {
      columns[[length(columns) + 1L]] <- value[, column]
      suffix <- if (ncol(value) > 1L) paste0("_", column) else ""
      layer_names <- c(layer_names, paste0(base_name, suffix))
    }
  }
  values <- do.call(cbind, columns)
  colnames(values) <- make.unique(layer_names)
  list(
    values = values,
    layers = data.frame(name = colnames(values), stringsAsFactors = FALSE)
  )
}

.ngeo_fs_annot <- function(path, n_vertex) {
  if (is.null(path)) {
    return(list())
  }
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ngeo_abort(
      "`labels` must name an existing FreeSurfer annotation.",
      "ngeo_error_argument"
    )
  }
  annot <- .ngeo_backend_read(
    "FreeSurfer annotation",
    path,
    function() freesurferformats::read.fs.annot(path)
  )
  if (length(annot$label_codes) != n_vertex) {
    .ngeo_abort(
      sprintf(
        "FreeSurfer annotation has %d vertices but geometry has %d.",
        length(annot$label_codes), n_vertex
      ),
      "ngeo_error_alignment"
    )
  }
  list(
    annot = list(
      values = annot$label_codes,
      names = annot$label_names,
      table = annot$colortable_df,
      source = path
    )
  )
}

.ngeo_read_freesurfer_surface <- function(geometry,
                                          coordinates,
                                          data,
                                          labels,
                                          layers,
                                          measures,
                                          coordinate_space,
                                          strict,
                                          checksum) {
  geometry_data <- .ngeo_fs_surface_geometry(geometry, coordinates)
  n_vertex <- nrow(geometry_data$coordinates[[1L]])
  value_data <- .ngeo_fs_surface_data(data, n_vertex)
  label_data <- .ngeo_fs_annot(labels, n_vertex)
  layers <- layers %||% value_data$layers
  coordinate_space <- coordinate_space %||% ngeo_coordinate_space(
    "unknown",
    kind = "surface",
    source_metadata = list(freesurfer = geometry_data$metadata)
  )

  x <- ngeo_surface(
    coordinates = geometry_data$coordinates,
    faces = geometry_data$faces,
    values = value_data$values,
    layers = layers,
    measures = measures,
    labels = label_data,
    coordinate_space = coordinate_space,
    coordinate_roles = geometry_data$roles,
    index_base = "one",
    source_index_base = 0L
  )
  x <- .ngeo_append_import_provenance(
    x,
    paths = c(geometry_data$paths, data, labels),
    importer = "read_ngeo_freesurfer_surface",
    metadata = list(geometry = geometry_data$metadata),
    checksum = checksum
  )
  if (isTRUE(strict)) {
    ngeo_validate(x, "strict")
  }
  x
}

.ngeo_read_freesurfer_volume <- function(path,
                                         affine,
                                         mask,
                                         layers,
                                         measures,
                                         coordinate_space,
                                         load_data,
                                         strict,
                                         checksum) {
  volume <- .ngeo_backend_read(
    "FreeSurfer MGH/MGZ",
    path,
    function() freesurferformats::read.fs.mgh(
      path,
      with_header = TRUE,
      drop_empty_dims = FALSE
    )
  )
  header <- volume$header
  data <- volume$data
  lattice_dim <- as.integer(dim(data)[1:3])
  n_layer <- if (length(dim(data)) <= 3L) {
    1L
  } else {
    prod(dim(data)[-seq_len(3L)])
  }
  if (is.null(layers)) {
    layers <- data.frame(
      name = paste0("frame_", seq_len(n_layer)),
      source_frame = seq_len(n_layer) - 1L,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(affine)) {
    affine <- tryCatch(
      freesurferformats::mghheader.vox2ras(header),
      error = function(error) NULL
    )
    if (is.null(affine)) {
      .ngeo_abort(
        paste0(
          "MGH/MGZ header has no valid RAS affine. ",
          "Provide `affine=` explicitly; no identity transform was assumed."
        ),
        "ngeo_error_transform"
      )
    }
  }
  coordinate_space <- coordinate_space %||% ngeo_coordinate_space(
    "unknown",
    kind = "volume",
    source_metadata = list(
      ras_good_flag = header$ras_good_flag,
      mgh_dimensions = header$voldim
    )
  )
  x <- ngeo_volume(
    values = if (isTRUE(load_data)) data else NULL,
    dim = lattice_dim,
    affine = affine,
    mask = mask,
    layers = layers,
    measures = measures,
    coordinate_space = coordinate_space,
    index_base = "zero"
  )
  x$history$header_summary <- header
  x <- .ngeo_append_import_provenance(
    x,
    paths = path,
    importer = "read_ngeo_freesurfer_volume",
    metadata = list(load_data = load_data),
    checksum = checksum
  )
  if (isTRUE(strict)) {
    ngeo_validate(x, "strict")
  }
  x
}

#' Read FreeSurfer surface, morphometry, annotation, or MGH/MGZ
#'
#' @param x Primary FreeSurfer path.
#' @param geometry Surface geometry path when reading surface data.
#' @param coordinates Optional additional surface coordinate paths.
#' @param data Optional morphometry paths.
#' @param labels Optional annotation path.
#' @param base Explicit surface/volume base, or safe automatic detection.
#' @param affine Optional explicit MGH/MGZ affine.
#' @param mask Optional volume mask.
#' @param layers Optional layer metadata.
#' @param measures Optional measurement semantics.
#' @param coordinate_space Optional `ngeo_coordinate_space`.
#' @param load_data Whether to retain volume data.
#' @param strict Whether to run strict validation.
#' @param checksum Whether to record MD5 checksums.
#'
#' @return An `ngeo_surface` or `ngeo_volume` object.
#' @export
read_ngeo_freesurfer <- function(x = NULL,
                                 geometry = NULL,
                                 coordinates = NULL,
                                 data = NULL,
                                 labels = NULL,
                                 base = c("auto", "surface", "volume"),
                                 affine = NULL,
                                 mask = NULL,
                                 layers = NULL,
                                 measures = NULL,
                                 coordinate_space = NULL,
                                 load_data = TRUE,
                                 strict = TRUE,
                                 checksum = TRUE) {
  .ngeo_require("freesurferformats", "FreeSurfer format reading")
  base <- match.arg(base)
  primary <- x %||% geometry
  if (is.null(primary) || !is.character(primary) ||
      length(primary) != 1L || !file.exists(primary)) {
    .ngeo_abort(
      "Provide an existing FreeSurfer path.",
      "ngeo_error_argument"
    )
  }

  extension <- .ngeo_path_extension(primary)
  is_mgh <- extension %in% c("mgh", "mgz")
  if (identical(base, "auto")) {
    base <- if (!is.null(geometry) && !identical(primary, geometry)) {
      "surface"
    } else if (!is_mgh) {
      "surface"
    } else {
      probe <- .ngeo_backend_read(
        "FreeSurfer MGH/MGZ",
        primary,
        function() freesurferformats::read.fs.mgh(
          primary,
          with_header = TRUE,
          drop_empty_dims = FALSE
        )
      )
      dims <- dim(probe$data)
      if (sum(dims[1:3] > 1L) >= 2L) {
        "volume"
      } else {
        .ngeo_abort(
          paste0(
            "MGH/MGZ base is ambiguous. Specify `base = \"surface\"` ",
            "with matching `geometry`, or `base = \"volume\"`."
          ),
          "ngeo_error_base_ambiguous"
        )
      }
    }
  }

  if (identical(base, "volume")) {
    if (!is_mgh) {
      .ngeo_abort(
        "FreeSurfer volume input must be MGH or MGZ.",
        "ngeo_error_format"
      )
    }
    return(.ngeo_read_freesurfer_volume(
      primary,
      affine = affine,
      mask = mask,
      layers = layers,
      measures = measures,
      coordinate_space = coordinate_space,
      load_data = load_data,
      strict = strict,
      checksum = checksum
    ))
  }

  if (is_mgh) {
    data <- c(data, primary)
    if (is.null(geometry)) {
      .ngeo_abort(
        "Surface MGH/MGZ requires matching `geometry`.",
        "ngeo_error_argument"
      )
    }
  } else {
    geometry <- geometry %||% primary
  }
  .ngeo_read_freesurfer_surface(
    geometry = geometry,
    coordinates = coordinates,
    data = data,
    labels = labels,
    layers = layers,
    measures = measures,
    coordinate_space = coordinate_space,
    strict = strict,
    checksum = checksum
  )
}
