.ngeo_cifti_structure <- function(value) {
  sub("^CIFTI_STRUCTURE_", "", value)
}

.ngeo_cifti_volume_info <- function(x) {
  if (is.list(x) && all(c("mat", "VolumeDimensions") %in% names(x))) {
    return(x)
  }
  if (!is.list(x)) {
    return(NULL)
  }
  for (item in x) {
    found <- .ngeo_cifti_volume_info(item)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

.ngeo_cifti_surface_path <- function(surfaces, structure) {
  if (is.null(surfaces)) {
    return(NULL)
  }
  if (is.list(surfaces)) {
    surfaces <- unlist(surfaces, use.names = TRUE)
  }
  if (!is.character(surfaces)) {
    .ngeo_abort(
      "`surfaces` must be a named character vector/list of paths.",
      "ngeo_error_argument"
    )
  }
  if (is.null(names(surfaces))) {
    if (length(surfaces) == 2L) {
      names(surfaces) <- c("left", "right")
    } else {
      .ngeo_abort(
        "Name CIFTI surfaces by structure.",
        "ngeo_error_argument"
      )
    }
  }
  keys <- tolower(gsub("^cifti_structure_", "", names(surfaces)))
  target <- tolower(.ngeo_cifti_structure(structure))
  aliases <- if (identical(target, "cortex_left")) {
    c("cortex_left", "left", "lh")
  } else if (identical(target, "cortex_right")) {
    c("cortex_right", "right", "rh")
  } else {
    target
  }
  index <- which(keys %in% aliases)
  if (!length(index)) NULL else surfaces[[index[[1L]]]]
}

.ngeo_cifti_surface_geometry <- function(path) {
  if (is.null(path)) {
    return(NULL)
  }
  if (!file.exists(path)) {
    .ngeo_abort(
      sprintf("CIFTI surface `%s` does not exist.", path),
      "ngeo_error_argument"
    )
  }
  if (grepl("\\.gii$", path, ignore.case = TRUE)) {
    read_ngeo_gifti(path, strict = TRUE, checksum = FALSE)
  } else {
    read_ngeo_freesurfer(
      geometry = path,
      base = "surface",
      strict = TRUE,
      checksum = FALSE
    )
  }
}

.ngeo_cifti_values <- function(cifti, brain_axis, load_data) {
  attributes <- cifti$matrix_indices_attributes
  axis_count <- length(attributes)
  original_dim <- attr(cifti$data, "orig_dim")
  if (length(original_dim) < axis_count) {
    .ngeo_abort(
      "CIFTI data dimensions do not match matrix-index metadata.",
      "ngeo_error_format"
    )
  }
  axis_dim <- as.integer(original_dim[seq_len(axis_count)])
  array_data <- array(cifti$data, dim = axis_dim)
  permutation <- c(brain_axis, setdiff(seq_len(axis_count), brain_axis))
  array_data <- aperm(array_data, permutation)
  values <- matrix(array_data, nrow = axis_dim[[brain_axis]])
  if (isTRUE(load_data)) values else NULL
}

.ngeo_cifti_maps <- function(cifti, brain_axis, n_layer) {
  layer_names <- cifti$NamedMap$map_names %||% character()
  if (length(layer_names) != n_layer) {
    layer_names <- paste0("map_", seq_len(n_layer))
  }
  layers <- data.frame(
    name = layer_names,
    source_frame = seq_len(n_layer) - 1L,
    stringsAsFactors = FALSE
  )

  other_axes <- setdiff(
    seq_along(cifti$matrix_indices_attributes),
    brain_axis
  )
  if (length(other_axes) == 1L) {
    attributes <- cifti$matrix_indices_attributes[[other_axes]]
    if (identical(
      unname(attributes[["IndicesMapToDataType"]]),
      "CIFTI_INDEX_TYPE_SERIES"
    )) {
      start <- as.numeric(attributes[["SeriesStart"]] %||% 0)
      step <- as.numeric(attributes[["SeriesStep"]] %||% 1)
      layers$time <- start + (seq_len(n_layer) - 1L) * step
      layers$time_unit <- attributes[["SeriesUnit"]] %||% "unknown"
    }
  }
  layers
}

.ngeo_cifti_components <- function(cifti, brain_axis, surfaces) {
  models <- cifti$BrainModel
  if (!is.list(models) || !length(models)) {
    .ngeo_abort(
      "CIFTI has no brain-model mapping.",
      "ngeo_error_format"
    )
  }
  offsets <- vapply(
    models,
    function(model) as.numeric(attr(model, "IndexOffset")),
    numeric(1)
  )
  models <- models[order(offsets)]
  offsets <- sort(offsets)
  counts <- vapply(
    models,
    function(model) as.numeric(attr(model, "IndexCount")),
    numeric(1)
  )
  if (!identical(offsets, cumsum(c(0, utils::head(counts, -1L))))) {
    .ngeo_abort(
      "CIFTI brain models are not a contiguous ordered mapping.",
      "ngeo_error_alignment"
    )
  }

  volume_info <- .ngeo_cifti_volume_info(cifti$Volume)
  lapply(seq_along(models), function(i) {
    model <- models[[i]]
    structure <- attr(model, "BrainStructure")
    model_type <- attr(model, "ModelType")
    component_id <- tolower(.ngeo_cifti_structure(structure))
    if (identical(model_type, "CIFTI_MODEL_TYPE_SURFACE")) {
      path <- .ngeo_cifti_surface_path(surfaces, structure)
      geometry <- .ngeo_cifti_surface_geometry(path)
      list(
        component_id = component_id,
        kind = "surface",
        structure = .ngeo_cifti_structure(structure),
        vertex_index = as.integer(model),
        surface_vertex_count = as.integer(
          attr(model, "SurfaceNumberOfVertices")
        ),
        source_index_base = 0L,
        geometry = geometry
      )
    } else if (identical(model_type, "CIFTI_MODEL_TYPE_VOXELS")) {
      voxel_index <- attr(model, "VoxelIndicesIJK")
      if (is.null(volume_info) || is.null(volume_info$mat)) {
        .ngeo_abort(
          "CIFTI voxel brain model has no volume transform.",
          "ngeo_error_transform"
        )
      }
      list(
        component_id = component_id,
        kind = "volume",
        structure = .ngeo_cifti_structure(structure),
        voxel_index = as.matrix(voxel_index),
        affine = as.matrix(volume_info$mat),
        source_index_base = 0L
      )
    } else {
      .ngeo_abort(
        sprintf("Unsupported CIFTI brain model `%s`.", model_type),
        "ngeo_error_format"
      )
    }
  })
}

#' Read CIFTI dscalar, dlabel, or dtseries without Workbench
#'
#' @param path CIFTI path.
#' @param surfaces Optional left/right surface geometry paths.
#' @param frames Optional map/frame selection.
#' @param layers Optional layer metadata overriding the file metadata.
#' @param measures Optional measurement semantics.
#' @param coordinate_space Optional hybrid `ngeo_coordinate_space`.
#' @param load_data Whether to retain the matrix values.
#' @param strict Whether to run strict validation.
#' @param checksum Whether to record a SHA-256 source identity. A legacy MD5
#'   field is retained through 6.x for history compatibility.
#'
#' @return An `ngeo_grayordinate` object.
#' @export
read_ngeo_cifti <- function(path,
                            surfaces = NULL,
                            frames = NULL,
                            layers = NULL,
                            measures = NULL,
                            coordinate_space = NULL,
                            load_data = TRUE,
                            strict = TRUE,
                            checksum = TRUE) {
  .ngeo_require("cifti", "CIFTI reading")
  if (is.list(surfaces)) {
    surfaces <- unlist(surfaces, use.names = TRUE)
  }
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ngeo_abort("`path` must name an existing CIFTI file.", "ngeo_error_argument")
  }
  if (!grepl(
    "\\.(dscalar|dlabel|dtseries)\\.nii$",
    path,
    ignore.case = TRUE
  )) {
    .ngeo_abort(
      "CIFTI input must be dscalar, dlabel, or dtseries.",
      "ngeo_error_format"
    )
  }

  cifti <- .ngeo_backend_read(
    "CIFTI",
    path,
    function() {
      if (isTRUE(load_data)) {
        cifti::read_cifti(
          path,
          drop_data = FALSE,
          trans_data = FALSE,
          verbose = FALSE
        )
      } else {
        .ngeo_cifti_metadata_only(path)
      }
    }
  )
  mapping_type <- vapply(
    cifti$matrix_indices_attributes,
    function(attributes) unname(attributes[["IndicesMapToDataType"]]),
    character(1)
  )
  brain_axis <- which(mapping_type == "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  if (length(brain_axis) != 1L) {
    .ngeo_abort(
      "CIFTI input requires exactly one brain-model axis.",
      "ngeo_error_format"
    )
  }

  components <- .ngeo_cifti_components(cifti, brain_axis, surfaces)
  n_element <- sum(vapply(
    components,
    function(component) {
      if (identical(component$kind, "surface")) {
        length(component$vertex_index)
      } else {
        nrow(component$voxel_index)
      }
    },
    integer(1)
  ))
  axis_dim <- if (isTRUE(load_data)) {
    original_dim <- attr(cifti$data, "orig_dim")
    axis_count <- length(cifti$matrix_indices_attributes)
    as.integer(original_dim[seq_len(axis_count)])
  } else {
    .ngeo_cifti_axis_dimensions(cifti)
  }
  n_layer <- prod(axis_dim[-brain_axis])
  if (axis_dim[[brain_axis]] != n_element) {
    .ngeo_abort(
      "CIFTI matrix rows do not match brain-model element counts.",
      "ngeo_error_alignment"
    )
  }

  values <- if (isTRUE(load_data)) {
    .ngeo_cifti_values(cifti, brain_axis, TRUE)
  } else {
    NULL
  }
  file_maps <- .ngeo_cifti_maps(cifti, brain_axis, n_layer)
  layers <- layers %||% file_maps
  named_metadata <- .ngeo_cifti_read_named_metadata(
    path, file_maps$name
  )
  if (any(lengths(named_metadata) > 0L)) {
    layers[["metadata"]] <- named_metadata
  }
  if (is.null(measures)) {
    is_label <- grepl("\\.dlabel\\.nii$", path, ignore.case = TRUE)
    measure <- ngeo_measure(
      value_type = if (is_label) "label" else "continuous",
      support_behavior = if (is_label) "categorical" else "unknown"
    )
    measures <- measure[rep.int(1L, n_layer), , drop = FALSE]
    rownames(measures) <- NULL
  }
  coordinate_space <- coordinate_space %||% ngeo_coordinate_space(
    "unknown",
    kind = "hybrid",
    source_metadata = list(
      matrix_indices_attributes = cifti$matrix_indices_attributes
    )
  )
  x <- ngeo_grayordinate(
    components = components,
    values = values,
    layers = layers,
    measures = measures,
    coordinate_space = coordinate_space
  )

  lookup <- cifti$NamedMap$look_up_table %||% NULL
  if (!is.null(lookup)) {
    label_names <- file_maps$name
    x$base$labels <- stats::setNames(
      lapply(seq_along(lookup), function(i) {
        list(
          table = lookup[[i]],
          layer_id = x$layers$layer_id[[i]]
        )
      }),
      label_names[seq_along(lookup)]
    )
  }
  x$history$cifti <- list(
    matrix_indices_attributes = cifti$matrix_indices_attributes,
    brain_model_count = length(cifti$BrainModel),
    named_layers = cifti$NamedMap,
    named_map_metadata = named_metadata,
    datatype = .ngeo_cifti_header_datatype(path)
  )
  x <- .ngeo_append_import_provenance(
    x,
    paths = c(path, unname(surfaces)),
    importer = "read_ngeo_cifti",
    metadata = list(
      load_data = load_data,
      mapping_type = mapping_type
    ),
    checksum = checksum
  )
  if (!is.null(frames)) {
    x <- ngeo_subset(x, layers = frames)
  }
  if (isTRUE(strict)) {
    ngeo_validate(x, "strict")
  }
  x
}
