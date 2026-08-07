# Format-aware file-backed values.
.ngeo_file_source_identity <- function(path, checksum = TRUE) {
  info <- file.info(path)
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime),
    sha256 = if (isTRUE(checksum)) {
      digest::digest(
        path, algo = "sha256", file = TRUE, serialize = FALSE
      )
    } else {
      NULL
    }
  )
}

.ngeo_binary_connection <- function(path, compressed) {
  if (isTRUE(compressed)) gzfile(path, "rb") else file(path, "rb")
}

.ngeo_binary_read <- function(connection, specification, n) {
  if (!n) return(numeric())
  value <- readBin(
    connection,
    what = specification$what,
    n = n,
    size = specification$bytes,
    signed = specification$signed,
    endian = specification$endian
  )
  if (length(value) != n) {
    .ngeo_abort(
      "File-backed source ended before the requested values.",
      "ngeo_error_io"
    )
  }
  as.numeric(value)
}

.ngeo_binary_discard <- function(connection, specification, n) {
  remaining <- n
  while (remaining > 0L) {
    current <- min(remaining, 65536L)
    .ngeo_binary_read(connection, specification, current)
    remaining <- remaining - current
  }
  invisible(TRUE)
}

.ngeo_contiguous_runs <- function(positions) {
  if (!length(positions)) return(list())
  group <- cumsum(c(TRUE, diff(positions) != 1))
  split(positions, group)
}

.ngeo_binary_positions <- function(path, specification, positions) {
  positions <- as.numeric(positions)
  if (anyNA(positions) || any(positions < 0) ||
      any(positions != floor(positions))) {
    .ngeo_abort("Binary positions are invalid.", "ngeo_error_index")
  }
  unique_positions <- sort(unique(positions))
  values <- numeric(length(unique_positions))
  connection <- .ngeo_binary_connection(
    path, specification$compressed
  )
  on.exit(close(connection), add = TRUE)
  if (isTRUE(specification$compressed)) {
    header <- readBin(
      connection,
      what = "raw",
      n = specification$data_offset
    )
    if (length(header) != specification$data_offset) {
      .ngeo_abort("Compressed header is truncated.", "ngeo_error_io")
    }
    cursor <- 0
    output_start <- 1L
    for (run in .ngeo_contiguous_runs(unique_positions)) {
      gap <- run[[1L]] - cursor
      .ngeo_binary_discard(connection, specification, gap)
      count <- length(run)
      values[seq.int(output_start, length.out = count)] <-
        .ngeo_binary_read(connection, specification, count)
      cursor <- run[[count]] + 1
      output_start <- output_start + count
    }
  } else {
    output_start <- 1L
    for (run in .ngeo_contiguous_runs(unique_positions)) {
      seek(
        connection,
        where = specification$data_offset +
          run[[1L]] * specification$bytes,
        origin = "start"
      )
      count <- length(run)
      values[seq.int(output_start, length.out = count)] <-
        .ngeo_binary_read(connection, specification, count)
      output_start <- output_start + count
    }
  }
  slope <- specification$slope %||% 0
  intercept <- specification$intercept %||% 0
  if (is.finite(slope) && slope != 0) {
    values <- values * slope + intercept
  }
  values[match(positions, unique_positions)]
}

.ngeo_nifti_datatype <- function(code, endian) {
  value <- switch(
    as.character(as.integer(code)),
    "2" = list(what = "integer", bytes = 1L, signed = FALSE),
    "4" = list(what = "integer", bytes = 2L, signed = TRUE),
    "8" = list(what = "integer", bytes = 4L, signed = TRUE),
    "16" = list(what = "numeric", bytes = 4L, signed = TRUE),
    "64" = list(what = "numeric", bytes = 8L, signed = TRUE),
    "256" = list(what = "integer", bytes = 1L, signed = TRUE),
    "512" = list(what = "integer", bytes = 2L, signed = FALSE),
    "768" = list(what = "integer", bytes = 4L, signed = FALSE),
    NULL
  )
  if (is.null(value)) {
    .ngeo_abort(
      sprintf("File-backed NIfTI datatype `%s` is unsupported.", code),
      "ngeo_error_format"
    )
  }
  c(value, list(endian = endian))
}

.ngeo_file_endian <- function(path, compressed, expected_header) {
  read_header <- function(endian) {
    connection <- .ngeo_binary_connection(path, compressed)
    on.exit(close(connection))
    readBin(connection, integer(), n = 1L, size = 4L, endian = endian)
  }
  if (identical(read_header("little"), as.integer(expected_header))) {
    "little"
  } else if (identical(
    read_header("big"), as.integer(expected_header)
  )) {
    "big"
  } else {
    .ngeo_abort("Could not determine file header endianness.",
                "ngeo_error_format")
  }
}

.ngeo_nifti_binary_spec <- function(path, header) {
  compressed <- grepl("[.]gz$", path, ignore.case = TRUE)
  version <- attr(header, "version") %||% 1L
  endian <- .ngeo_file_endian(
    path, compressed, if (version == 2L) 540L else 348L
  )
  c(
    .ngeo_nifti_datatype(header$datatype, endian),
    list(
      data_offset = as.numeric(header$vox_offset),
      compressed = compressed,
      slope = as.numeric(header$scl_slope),
      intercept = as.numeric(header$scl_inter)
    )
  )
}

.ngeo_cifti_binary_spec <- function(path, header) {
  endian <- .ngeo_file_endian(path, FALSE, 540L)
  c(
    .ngeo_nifti_datatype(header@datatype, endian),
    list(
      data_offset = as.numeric(header@vox_offset),
      compressed = FALSE,
      slope = as.numeric(header@scl_slope),
      intercept = as.numeric(header@scl_inter)
    )
  )
}

.ngeo_mgh_header <- function(path) {
  compressed <- grepl("[.]mgz$", path, ignore.case = TRUE)
  connection <- .ngeo_binary_connection(path, compressed)
  on.exit(close(connection))
  version <- readBin(
    connection, integer(), n = 1L, size = 4L, endian = "big"
  )
  dimensions <- readBin(
    connection, integer(), n = 4L, size = 4L, endian = "big"
  )
  dtype <- readBin(
    connection, integer(), n = 1L, size = 4L, endian = "big"
  )
  dof <- readBin(
    connection, integer(), n = 1L, size = 4L, endian = "big"
  )
  ras_good_flag <- readBin(
    connection, integer(), n = 1L, size = 2L, endian = "big"
  )
  used <- 2L
  affine <- NULL
  delta <- NULL
  if (ras_good_flag == 1L) {
    delta <- readBin(
      connection, numeric(), n = 3L, size = 4L, endian = "big"
    )
    mdc <- readBin(
      connection, numeric(), n = 9L, size = 4L, endian = "big"
    )
    mdc <- matrix(mdc, nrow = 3L, byrow = FALSE)
    center <- readBin(
      connection, numeric(), n = 3L, size = 4L, endian = "big"
    )
    scaled <- mdc %*% diag(delta)
    origin <- center - scaled %*% (dimensions[1:3] / 2)
    affine <- diag(4)
    affine[1:3, 1:3] <- scaled
    affine[1:3, 4L] <- origin
    used <- used + 60L
  }
  if (version != 1L || any(dimensions < 1L)) {
    .ngeo_abort("Invalid MGH/MGZ header.", "ngeo_error_format")
  }
  list(
    version = version,
    dimensions = as.integer(dimensions),
    dtype = as.integer(dtype),
    dof = dof,
    ras_good_flag = ras_good_flag,
    delta = delta,
    affine = affine,
    data_offset = 28L + 256L,
    compressed = compressed,
    unused_header_bytes = 256L - used
  )
}

.ngeo_mgh_binary_spec <- function(path, header) {
  datatype <- switch(
    as.character(header$dtype),
    "0" = list(what = "integer", bytes = 1L, signed = FALSE),
    "1" = list(what = "integer", bytes = 4L, signed = TRUE),
    "3" = list(what = "numeric", bytes = 4L, signed = TRUE),
    "4" = list(what = "integer", bytes = 2L, signed = TRUE),
    NULL
  )
  if (is.null(datatype)) {
    .ngeo_abort("Unsupported MGH/MGZ datatype.", "ngeo_error_format")
  }
  c(
    datatype,
    list(
      endian = "big",
      data_offset = header$data_offset,
      compressed = header$compressed,
      slope = 0,
      intercept = 0
    )
  )
}

.ngeo_validate_file_identity <- function(identity, verify) {
  info <- file.info(identity$path)
  valid <- file.exists(identity$path) &&
    identical(as.numeric(info$size), identity$size)
  if (valid && identical(verify, "checksum")) {
    valid <- identical(
      digest::digest(
        identity$path,
        algo = "sha256",
        file = TRUE,
        serialize = FALSE
      ),
      identity$sha256
    )
  } else if (valid && identical(verify, "metadata")) {
    valid <- identical(as.numeric(info$mtime), identity$mtime)
  }
  if (!valid) {
    .ngeo_abort(
      "File-backed source identity changed after construction.",
      "ngeo_error_file_mutation"
    )
  }
  invisible(TRUE)
}

.ngeo_file_value_positions <- function(specification, rows, columns) {
  if (identical(specification$layout, "volume")) {
    return(as.vector(outer(
      specification$element_index[rows],
      specification$layer_index[columns] *
        specification$full_element_count,
      "+"
    )))
  }
  row_position <- specification$element_index[rows]
  map_position <- specification$layer_index[columns]
  as.vector(outer(
    row_position * specification$strides[
      specification$brain_axis
    ],
    map_position * specification$strides[
      specification$map_axis
    ],
    "+"
  ))
}

#' Construct one verified file-backed aligned values block
#'
#' @param path Source neuroimaging file.
#' @param dim Selected element-by-map dimensions.
#' @param layer_names Selected map names.
#' @param format NIfTI, CIFTI, MGH, or MGZ.
#' @param selection Internal auditable binary selection specification.
#' @param binary Internal binary datatype specification.
#' @param verify Source mutation verification policy.
#' @param budget Resource limits applied to every materialized value block.
#' @param complete_selection Whether the values represent the complete file.
#' @return An `ngeo_file_values` and `ngeo_delayed_values`.
#' @export
ngeo_file_values <- function(
    path,
    dim,
    layer_names,
    format,
    selection,
    binary,
    verify = c("checksum", "metadata", "none"),
    budget = ngeo_resource_budget(),
    complete_selection = FALSE) {
  verify <- match.arg(verify)
  .ngeo_assert_scalar_character(path, "path")
  if (!file.exists(path) || !is.list(selection) || !is.list(binary)) {
    .ngeo_abort("File-backed values specification is invalid.",
                "ngeo_error_argument")
  }
  dim <- .ngeo_as_integer(dim, "dim")
  if (length(dim) != 2L || any(dim < 1L) ||
      length(layer_names) != dim[[2L]]) {
    .ngeo_abort("File-backed dimensions or map names are invalid.",
                "ngeo_error_alignment")
  }
  identity <- .ngeo_file_source_identity(
    path, checksum = identical(verify, "checksum")
  )
  if (!inherits(budget, "ngeo_resource_budget")) {
    .ngeo_abort("`budget` must be an `ngeo_resource_budget`.",
                "ngeo_error_argument")
  }
  reader <- function(rows, columns) {
    .ngeo_validate_file_identity(identity, verify)
    count <- length(rows) * length(columns)
    .ngeo_budget_assert(budget, "materialized_elements", count)
    .ngeo_budget_assert(budget, "memory_bytes", count * 8)
    positions <- .ngeo_file_value_positions(
      selection, rows, columns
    )
    matrix(
      .ngeo_binary_positions(identity$path, binary, positions),
      nrow = length(rows),
      ncol = length(columns)
    )
  }
  structure(
    list(
      reader = reader,
      dim = dim,
      dimnames = list(NULL, layer_names),
      source = identity$path,
      source_identity = identity,
      format = format,
      selection = selection,
      binary = binary,
      verify = verify,
      budget = budget,
      complete_selection = isTRUE(complete_selection)
    ),
    class = c("ngeo_file_values", "ngeo_delayed_values")
  )
}

#' Validate and identify a file-backed values block
#'
#' @param x An `ngeo_file_values`.
#' @return `ngeo_validate_file_values()` returns `x` invisibly;
#' `ngeo_file_values_identity()` returns a SHA-256 identity.
#' @name ngeo_file_values_validation
NULL

#' @rdname ngeo_file_values_validation
#' @export
ngeo_validate_file_values <- function(x) {
  required_binary <- c(
    "what", "bytes", "signed", "endian", "data_offset", "compressed"
  )
  if (!inherits(x, "ngeo_file_values") ||
      !inherits(x, "ngeo_delayed_values") ||
      length(x$dim) != 2L || any(x$dim < 1L) ||
      !is.function(x$reader) ||
      !x$verify %in% c("checksum", "metadata", "none") ||
      !inherits(x$budget, "ngeo_resource_budget") ||
      !is.list(x$selection) ||
      length(x$selection$element_index) != x$dim[[1L]] ||
      length(x$selection$layer_index) != x$dim[[2L]] ||
      !is.list(x$binary) ||
      any(!required_binary %in% names(x$binary))) {
    .ngeo_abort("Invalid file-backed values block.",
                "ngeo_error_values")
  }
  .ngeo_validate_file_identity(x$source_identity, x$verify)
  invisible(x)
}

#' @rdname ngeo_file_values_validation
#' @export
ngeo_file_values_identity <- function(x) {
  ngeo_validate_file_values(x)
  digest::digest(
    list(
      source_path = x$source_identity$path,
      source_mtime = x$source_identity$mtime,
      source_sha256 = x$source_identity$sha256,
      source_size = x$source_identity$size,
      format = x$format,
      dim = x$dim,
      layer_names = x$dimnames[[2L]],
      selection = x$selection,
      binary = x$binary,
      verification = x$verify,
      complete_selection = x$complete_selection
    ),
    algo = "sha256"
  )
}

.ngeo_filebacked_attach <- function(
    x,
    values,
    format,
    selected_elements,
    selected_layers) {
  x$values <- values
  colnames(x$values) <- x$layers$name
  x$history$file_backed <- list(
    format = format,
    source = values$source,
    source_identity = ngeo_file_values_identity(values),
    verification = values$verify,
    selected_elements = selected_elements,
    selected_layers = selected_layers,
    materialized = FALSE
  )
  ngeo_validate(x, "strict")
  x
}

.ngeo_selection <- function(index, n, name) {
  if (is.null(index)) return(seq_len(n))
  value <- .ngeo_as_integer(index, name)
  if (!length(value) || any(value < 1L | value > n) ||
      anyDuplicated(value)) {
    .ngeo_abort(
      sprintf("`%s` must uniquely select indices in range.", name),
      "ngeo_error_index"
    )
  }
  value
}

#' Read NIfTI using a verified file-backed values block
#'
#' @inheritParams read_ngeo_nifti
#' @param elements Optional one-based active-base element selection.
#' @param verify Source mutation verification policy.
#' @param budget Resource limits for materialized value blocks.
#' @return An `ngeo_volume` with `ngeo_file_values`.
#' @export
read_ngeo_nifti_filebacked <- function(
    path,
    mask = NULL,
    frames = NULL,
    elements = NULL,
    layers = NULL,
    measures = NULL,
    coordinate_space = NULL,
    affine = c("auto", "sform", "qform"),
    strict = TRUE,
    checksum = TRUE,
    verify = c("checksum", "metadata", "none"),
    budget = ngeo_resource_budget()) {
  .ngeo_require("RNifti", "file-backed NIfTI reading")
  verify <- match.arg(verify)
  if (!file.exists(path)) {
    .ngeo_abort("NIfTI source does not exist.", "ngeo_error_argument")
  }
  if (identical(mask, "nonzero")) {
    .ngeo_abort(
      "`mask = \"nonzero\"` requires full data inspection; supply a mask.",
      "ngeo_error_resource"
    )
  }
  header <- RNifti::niftiHeader(path, unused = TRUE)
  lattice_dim <- as.integer(header$dim[2:4])
  image_dim <- as.integer(
    header$dim[2:(as.integer(header$dim[[1L]]) + 1L)]
  )
  n_layer <- if (length(image_dim) <= 3L) 1L else
    prod(image_dim[-seq_len(3L)])
  frames <- .ngeo_selection(frames, n_layer, "frames")
  transforms <- list(
    qform = if (header$qform_code > 0L) {
      matrix(
        as.numeric(RNifti::xform(
          path, useQuaternionFirst = TRUE
        )),
        nrow = 4L
      )
    } else NULL,
    qform_code = header$qform_code,
    sform = if (header$sform_code > 0L) {
      matrix(
        as.numeric(RNifti::xform(
          path, useQuaternionFirst = FALSE
        )),
        nrow = 4L
      )
    } else NULL,
    sform_code = header$sform_code
  )
  active <- .ngeo_nifti_affine(transforms, affine)
  unit <- (attr(header, "pixunits") %||% "mm")[[1L]]
  coordinate_space <- coordinate_space %||% ngeo_coordinate_space(
    "unknown",
    kind = "volume",
    unit = unit,
    source_metadata = list(
      qform_code = header$qform_code,
      sform_code = header$sform_code
    )
  )
  active_mask <- .ngeo_nifti_mask(mask, NULL, lattice_dim)
  if (is.null(layers)) {
    layers <- data.frame(
      name = paste0("frame_", frames),
      source_frame = frames - 1L,
      stringsAsFactors = FALSE
    )
  }
  x <- ngeo_volume(
    values = NULL,
    dim = lattice_dim,
    affine = active$matrix,
    mask = active_mask,
    layers = layers,
    measures = measures,
    coordinate_space = coordinate_space,
    index_base = "zero"
  )
  source_elements <- seq_len(nrow(x$base$elements))
  elements <- .ngeo_selection(
    elements, length(source_elements), "elements"
  )
  if (!identical(elements, source_elements)) {
    x <- ngeo_subset(x, elements = elements)
  }
  voxel <- x$base$geometry$voxel_index
  linear <- (voxel[, 1L] - 1L) +
    (voxel[, 2L] - 1L) * lattice_dim[[1L]] +
    (voxel[, 3L] - 1L) * prod(lattice_dim[1:2])
  values <- ngeo_file_values(
    path = path,
    dim = c(nrow(x$base$elements), length(frames)),
    layer_names = x$layers$name,
    format = "nifti",
    selection = list(
      layout = "volume",
      element_index = as.numeric(linear),
      layer_index = as.numeric(frames - 1L),
      full_element_count = prod(lattice_dim)
    ),
    binary = .ngeo_nifti_binary_spec(path, header),
    verify = verify,
    budget = budget,
    complete_selection = is.null(mask) &&
      identical(elements, seq_len(prod(lattice_dim))) &&
      identical(frames, seq_len(n_layer))
  )
  x$base$geometry$header_transforms <- c(
    transforms, list(active = active$source)
  )
  x$history$header_summary <- list(
    version = attr(header, "version"),
    dim = header$dim,
    pixdim = header$pixdim,
    datatype = header$datatype
  )
  x <- .ngeo_append_import_provenance(
    x, path, "read_ngeo_nifti_filebacked",
    metadata = list(load_data = FALSE, frames = frames),
    checksum = checksum
  )
  x <- .ngeo_filebacked_attach(
    x, values, "nifti", elements, frames
  )
  if (isTRUE(strict)) ngeo_validate(x, "strict")
  x
}

.ngeo_cifti_metadata_only <- function(path) {
  .ngeo_require("xml2", "file-backed CIFTI metadata reading")
  result <- cifti::get_cifti_type(path, verbose = FALSE)
  result$hdr <- cifti::nifti_2_hdr(path)
  nodes <- cifti::matrix_ind_map_nodes(path)
  result$matrix_indices_attributes <- lapply(nodes, xml2::xml_attrs)
  result$data <- NULL
  result$filename <- path
  class(result) <- "cifti"
  result
}

.ngeo_cifti_axis_dimensions <- function(cifti) {
  axes <- vapply(
    cifti$matrix_indices_attributes,
    function(attributes) {
      as.integer(attributes[["AppliesToMatrixDimension"]]) + 1L
    },
    integer(1)
  )
  header_dim <- as.integer(cifti$hdr@dim_)
  dimensions <- header_dim[5L + axes]
  dimensions[order(axes)]
}

#' Read CIFTI using a verified file-backed values block
#'
#' @inheritParams read_ngeo_cifti
#' @param structures Optional brain-structure names to retain.
#' @param elements Optional one-based grayordinate selection after structure
#' filtering.
#' @param verify Source mutation verification policy.
#' @param budget Resource limits for materialized value blocks.
#' @return An `ngeo_grayordinate` with `ngeo_file_values`.
#' @export
read_ngeo_cifti_filebacked <- function(
    path,
    surfaces = NULL,
    frames = NULL,
    structures = NULL,
    elements = NULL,
    layers = NULL,
    measures = NULL,
    coordinate_space = NULL,
    strict = TRUE,
    checksum = TRUE,
    verify = c("checksum", "metadata", "none"),
    budget = ngeo_resource_budget()) {
  .ngeo_require("cifti", "file-backed CIFTI reading")
  verify <- match.arg(verify)
  if (!file.exists(path) ||
      !grepl("\\.(dscalar|dlabel|dtseries)\\.nii$",
             path, ignore.case = TRUE)) {
    .ngeo_abort("Unsupported or missing CIFTI source.",
                "ngeo_error_format")
  }
  cifti <- .ngeo_cifti_metadata_only(path)
  mapping_type <- vapply(
    cifti$matrix_indices_attributes,
    function(attributes) unname(attributes[["IndicesMapToDataType"]]),
    character(1)
  )
  brain_axis <- which(mapping_type == "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  if (length(brain_axis) != 1L || length(mapping_type) != 2L) {
    .ngeo_abort(
      "File-backed CIFTI requires one brain-model and one map axis.",
      "ngeo_error_format"
    )
  }
  axis_dim <- .ngeo_cifti_axis_dimensions(cifti)
  map_axis <- setdiff(seq_along(axis_dim), brain_axis)
  components <- .ngeo_cifti_components(
    cifti, brain_axis, surfaces
  )
  n_element <- axis_dim[[brain_axis]]
  n_layer <- axis_dim[[map_axis]]
  file_maps <- .ngeo_cifti_maps(cifti, brain_axis, n_layer)
  named_metadata <- .ngeo_cifti_read_named_metadata(
    path, file_maps$name
  )
  if (any(lengths(named_metadata) > 0L)) {
    file_maps[["metadata"]] <- named_metadata
  }
  frames <- .ngeo_selection(frames, n_layer, "frames")
  file_maps <- file_maps[frames, , drop = FALSE]
  rownames(file_maps) <- NULL
  layers <- layers %||% file_maps
  if (is.null(measures)) {
    is_label <- grepl("\\.dlabel\\.nii$", path, ignore.case = TRUE)
    measure <- ngeo_measure(
      value_type = if (is_label) "label" else "continuous",
      support_behavior = if (is_label) "categorical" else "unknown"
    )
    measures <- measure[rep.int(1L, length(frames)), , drop = FALSE]
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
    values = NULL,
    layers = layers,
    measures = measures,
    coordinate_space = coordinate_space
  )
  source_rows <- seq_len(n_element)
  if (!is.null(structures)) {
    structures <- as.character(structures)
    keep <- x$base$elements$structure %in% structures |
      x$base$elements$component_id %in% tolower(structures)
    if (!any(keep)) {
      .ngeo_abort("No requested CIFTI structure is present.",
                  "ngeo_error_index")
    }
    source_rows <- source_rows[keep]
    x <- ngeo_subset(x, elements = which(keep))
  }
  elements <- .ngeo_selection(
    elements, nrow(x$base$elements), "elements"
  )
  if (!identical(elements, seq_len(nrow(x$base$elements)))) {
    source_rows <- source_rows[elements]
    x <- ngeo_subset(x, elements = elements)
  }
  strides <- cumprod(c(1, utils::head(axis_dim, -1L)))
  values <- ngeo_file_values(
    path = path,
    dim = c(nrow(x$base$elements), length(frames)),
    layer_names = x$layers$name,
    format = "cifti",
    selection = list(
      layout = "cifti",
      element_index = as.numeric(source_rows - 1L),
      layer_index = as.numeric(frames - 1L),
      axis_dimensions = axis_dim,
      strides = strides,
      brain_axis = brain_axis,
      map_axis = map_axis
    ),
    binary = .ngeo_cifti_binary_spec(path, cifti$hdr),
    verify = verify,
    budget = budget,
    complete_selection = identical(
      source_rows, seq_len(n_element)
    ) && identical(frames, seq_len(n_layer))
  )
  lookup <- cifti$NamedMap$look_up_table %||% NULL
  if (!is.null(lookup)) {
    lookup <- lookup[frames]
    x$base$labels <- stats::setNames(
      lapply(seq_along(lookup), function(i) {
        list(table = lookup[[i]], layer_id = x$layers$layer_id[[i]])
      }),
      x$layers$name[seq_along(lookup)]
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
    x, c(path, unname(surfaces)), "read_ngeo_cifti_filebacked",
    metadata = list(load_data = FALSE, mapping_type = mapping_type),
    checksum = checksum
  )
  x <- .ngeo_filebacked_attach(
    x, values, "cifti", source_rows, frames
  )
  if (isTRUE(strict)) ngeo_validate(x, "strict")
  x
}

#' Read an MGH or MGZ volume using a file-backed values block
#'
#' @param path MGH/MGZ path.
#' @param affine Optional affine when the header has no valid RAS transform.
#' @param mask Optional logical volume mask.
#' @param frames,elements Optional one-based selections.
#' @param layers,measures,coordinate_space Optional NGCS metadata.
#' @param strict,checksum Validation and history controls.
#' @param verify Source mutation verification policy.
#' @param budget Resource limits for materialized value blocks.
#' @return An `ngeo_volume` with `ngeo_file_values`.
#' @export
read_ngeo_mgh_filebacked <- function(
    path,
    affine = NULL,
    mask = NULL,
    frames = NULL,
    elements = NULL,
    layers = NULL,
    measures = NULL,
    coordinate_space = NULL,
    strict = TRUE,
    checksum = TRUE,
    verify = c("checksum", "metadata", "none"),
    budget = ngeo_resource_budget()) {
  verify <- match.arg(verify)
  if (!file.exists(path) ||
      !grepl("[.](mgh|mgz)$", path, ignore.case = TRUE)) {
    .ngeo_abort("MGH/MGZ source does not exist.",
                "ngeo_error_argument")
  }
  header <- .ngeo_mgh_header(path)
  lattice_dim <- header$dimensions[1:3]
  n_layer <- header$dimensions[[4L]]
  frames <- .ngeo_selection(frames, n_layer, "frames")
  affine <- affine %||% header$affine
  if (is.null(affine)) {
    .ngeo_abort(
      "MGH/MGZ header has no RAS affine; provide `affine=`.",
      "ngeo_error_transform"
    )
  }
  if (is.null(layers)) {
    layers <- data.frame(
      name = paste0("frame_", frames),
      source_frame = frames - 1L,
      stringsAsFactors = FALSE
    )
  }
  coordinate_space <- coordinate_space %||% ngeo_coordinate_space(
    "unknown",
    kind = "volume",
    source_metadata = list(
      ras_good_flag = header$ras_good_flag,
      mgh_dimensions = header$dimensions
    )
  )
  x <- ngeo_volume(
    values = NULL,
    dim = lattice_dim,
    affine = affine,
    mask = mask,
    layers = layers,
    measures = measures,
    coordinate_space = coordinate_space,
    index_base = "zero"
  )
  source_elements <- seq_len(nrow(x$base$elements))
  elements <- .ngeo_selection(
    elements, length(source_elements), "elements"
  )
  if (!identical(elements, source_elements)) {
    x <- ngeo_subset(x, elements = elements)
  }
  voxel <- x$base$geometry$voxel_index
  linear <- (voxel[, 1L] - 1L) +
    (voxel[, 2L] - 1L) * lattice_dim[[1L]] +
    (voxel[, 3L] - 1L) * prod(lattice_dim[1:2])
  values <- ngeo_file_values(
    path = path,
    dim = c(nrow(x$base$elements), length(frames)),
    layer_names = x$layers$name,
    format = if (header$compressed) "mgz" else "mgh",
    selection = list(
      layout = "volume",
      element_index = as.numeric(linear),
      layer_index = as.numeric(frames - 1L),
      full_element_count = prod(lattice_dim)
    ),
    binary = .ngeo_mgh_binary_spec(path, header),
    verify = verify,
    budget = budget,
    complete_selection = is.null(mask) &&
      identical(elements, seq_len(prod(lattice_dim))) &&
      identical(frames, seq_len(n_layer))
  )
  x$history$header_summary <- header
  x <- .ngeo_append_import_provenance(
    x, path, "read_ngeo_mgh_filebacked",
    metadata = list(load_data = FALSE, frames = frames),
    checksum = checksum
  )
  x <- .ngeo_filebacked_attach(
    x, values, values$format, elements, frames
  )
  if (isTRUE(strict)) ngeo_validate(x, "strict")
  x
}

#' Dispatch to a supported file-backed neuroimaging reader
#'
#' @param path Neuroimaging file.
#' @param format Auto-detected or explicit NIfTI, CIFTI, MGH, or MGZ.
#' @param ... Arguments passed to the format-specific reader.
#' @return An `ngeo` object with file-backed aligned values.
#' @export
read_ngeo_filebacked <- function(
    path,
    format = c("auto", "nifti", "cifti", "mgh", "mgz"),
    ...) {
  format <- match.arg(format)
  if (identical(format, "auto")) {
    lower <- tolower(path)
    format <- if (grepl(
      "\\.(dscalar|dlabel|dtseries)\\.nii$", lower
    )) {
      "cifti"
    } else if (grepl("\\.nii(\\.gz)?$", lower)) {
      "nifti"
    } else if (grepl("\\.mgz$", lower)) {
      "mgz"
    } else if (grepl("\\.mgh$", lower)) {
      "mgh"
    } else {
      .ngeo_abort("Could not detect a file-backed format.",
                  "ngeo_error_format")
    }
  }
  switch(
    format,
    nifti = read_ngeo_nifti_filebacked(path, ...),
    cifti = read_ngeo_cifti_filebacked(path, ...),
    mgh = read_ngeo_mgh_filebacked(path, ...),
    mgz = read_ngeo_mgh_filebacked(path, ...)
  )
}

#' Atomically copy a complete file-backed neuroimaging source in chunks
#'
#' @param x An `ngeo` object with a complete `ngeo_file_values` selection.
#' @param path Output path with the same format suffix.
#' @param chunk_bytes Positive copy-buffer size.
#' @param overwrite Whether to replace the output.
#' @return An `ngeo_atomic_output`.
#' @export
write_ngeo_filebacked <- function(
    x,
    path,
    chunk_bytes = 1048576L,
    overwrite = FALSE) {
  ngeo_validate(x, "strict")
  values <- x$values
  ngeo_validate_file_values(values)
  if (!isTRUE(values$complete_selection)) {
    .ngeo_abort(
      "Chunked pass-through requires the complete source selection.",
      "ngeo_error_partial_selection"
    )
  }
  chunk_bytes <- .ngeo_as_integer(chunk_bytes, "chunk_bytes")
  if (length(chunk_bytes) != 1L || chunk_bytes < 1L) {
    .ngeo_abort("`chunk_bytes` must be positive.",
                "ngeo_error_argument")
  }
  source <- values$source_identity$path
  suffix <- function(value) {
    lower <- tolower(basename(value))
    match <- regmatches(
      lower,
      regexpr(
        "(\\.dscalar\\.nii|\\.dlabel\\.nii|\\.dtseries\\.nii|\\.nii\\.gz|\\.nii|\\.mgz|\\.mgh)$",
        lower
      )
    )
    if (!length(match) || !nzchar(match)) NA_character_ else match
  }
  if (is.na(suffix(source)) ||
      !identical(suffix(source), suffix(path))) {
    .ngeo_abort(
      "File-backed pass-through output must keep the source format suffix.",
      "ngeo_error_format"
    )
  }
  if (identical(
    normalizePath(source, winslash = "/", mustWork = TRUE),
    normalizePath(
      path, winslash = "/", mustWork = FALSE
    )
  )) {
    .ngeo_abort("File-backed output must differ from its source.",
                "ngeo_error_argument")
  }
  output <- .ngeo_atomic_write(
    path,
    function(temporary) {
      input <- file(source, "rb")
      on.exit(close(input), add = TRUE)
      destination <- file(temporary, "wb")
      on.exit(close(destination), add = TRUE)
      repeat {
        block <- readBin(input, "raw", n = chunk_bytes)
        if (!length(block)) break
        writeBin(block, destination)
      }
    },
    overwrite = overwrite
  )
  attr(output, "source_identity") <- ngeo_file_values_identity(values)
  attr(output, "format") <- values$format
  output
}

#' @export
print.ngeo_file_values <- function(x, ...) {
  cat("<ngeo_file_values>\n  format: ", x$format,
      "\n  dimensions: ", paste(x$dim, collapse = " x "),
      "\n  verification: ", x$verify,
      "\n  source: ", x$source, "\n", sep = "")
  invisible(x)
}
