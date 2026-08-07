.ngeo_xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.ngeo_xml_unescape <- function(x) {
  x <- gsub("&quot;", '"', x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

.ngeo_cifti_metadata_xml <- function(metadata) {
  if (!length(metadata)) return("")
  paste0(
    "<MetaData>",
    paste0(
      "<MD><Name>", .ngeo_xml_escape(names(metadata)),
      "</Name><Value>",
      .ngeo_xml_escape(vapply(metadata, as.character, character(1))),
      "</Value></MD>",
      collapse = ""
    ),
    "</MetaData>"
  )
}

.ngeo_cifti_named_metadata <- function(x, metadata) {
  if (is.null(metadata) && "metadata" %in% names(x$layers)) {
    metadata <- x$layers$metadata
  }
  if (is.null(metadata)) {
    metadata <- rep(list(list()), nrow(x$layers))
  }
  if (!is.list(metadata) || length(metadata) != nrow(x$layers)) {
    .ngeo_abort("NamedMap metadata must align with layers.",
                "ngeo_error_alignment")
  }
  lapply(metadata, function(current) {
    if (!is.list(current) || (
      length(current) && (
        is.null(names(current)) ||
          any(!nzchar(names(current))) ||
          anyDuplicated(names(current)) ||
          any(vapply(current, length, integer(1)) != 1L) ||
          any(vapply(current, function(value) {
            is.list(value) || is.na(value)
          }, logical(1)))
      )
    )) {
      .ngeo_abort("NamedMap metadata entries must be named scalars.",
                  "ngeo_error_format")
    }
    current
  })
}

.ngeo_validate_cifti_labels <- function(x) {
  for (i in seq_len(nrow(x$layers))) {
    stored <- x$base$labels[[x$layers$name[[i]]]]$table %||% NULL
    if (is.null(stored)) next
    required <- c("Key", "Red", "Green", "Blue", "Alpha", "Label")
    if (!is.data.frame(stored) || any(!required %in% names(stored)) ||
        !is.numeric(stored$Key) || any(!is.finite(stored$Key)) ||
        any(stored$Key != round(stored$Key)) ||
        anyDuplicated(stored$Key) ||
        !all(vapply(
          stored[, c("Red", "Green", "Blue", "Alpha"), drop = FALSE],
          is.numeric,
          logical(1)
        )) ||
        any(!is.finite(as.matrix(
          stored[, c("Red", "Green", "Blue", "Alpha"), drop = FALSE]
        ))) ||
        any(as.matrix(
          stored[, c("Red", "Green", "Blue", "Alpha"), drop = FALSE]
        ) < 0) ||
        any(as.matrix(
          stored[, c("Red", "Green", "Blue", "Alpha"), drop = FALSE]
        ) > 1) ||
        !is.character(stored$Label) || anyNA(stored$Label)) {
      .ngeo_abort("CIFTI label table is invalid.", "ngeo_error_format")
    }
  }
  invisible(TRUE)
}

#' Validate the supported CIFTI-2 output contract
#'
#' @param x An `ngeo_grayordinate`.
#' @param type Dense scalar, label, or time series.
#' @param datatype Float32, float64, or int32; `NULL` selects the normative
#'   default.
#' @param named_map_metadata Optional metadata list aligned with scalar or
#'   label layers.
#' @return A normalized CIFTI output contract.
#' @export
ngeo_validate_cifti_contract <- function(
    x,
    type = c("dscalar", "dlabel", "dtseries"),
    datatype = NULL,
    named_map_metadata = NULL) {
  ngeo_validate(x, "strict")
  type <- match.arg(type)
  if (!inherits(x, "ngeo_grayordinate") || is.null(x$values)) {
    .ngeo_abort("CIFTI output requires loaded grayordinate values.",
                "ngeo_error_argument")
  }
  datatype <- datatype %||% if (type == "dlabel") "int32" else "float32"
  datatype <- match.arg(datatype, c("float32", "float64", "int32"))
  values <- as.matrix(x$values)
  if (datatype == "int32" && (
      any(!is.finite(values)) || any(values != round(values)) ||
        any(values < -.Machine$integer.max) ||
        any(values > .Machine$integer.max)
  )) {
    .ngeo_abort("int32 CIFTI output requires finite in-range integers.",
                "ngeo_error_measure")
  }
  if (type == "dlabel") {
    if (datatype != "int32") {
      .ngeo_abort("dlabel output requires `datatype = \"int32\"`.",
                  "ngeo_error_format")
    }
    .ngeo_validate_cifti_labels(x)
  }
  metadata <- .ngeo_cifti_named_metadata(x, named_map_metadata)
  if (type == "dtseries") {
    if (any(lengths(metadata) > 0L)) {
      .ngeo_abort("dtseries uses a series axis, not NamedMap metadata.",
                  "ngeo_error_format")
    }
    time <- x$layers$time %||% (seq_len(nrow(x$layers)) - 1L)
    unit <- x$layers$time_unit %||% rep("SECOND", nrow(x$layers))
    if (!is.numeric(time) || length(time) != nrow(x$layers) ||
        any(!is.finite(time)) ||
        (length(time) > 2L &&
          max(abs(diff(time) - diff(time)[[1L]])) > 1e-10) ||
        !is.character(unit) || length(unit) != nrow(x$layers) ||
        length(unique(unit)) != 1L ||
        !unit[[1L]] %in% c("SECOND", "HERTZ", "METER", "RADIAN")) {
      .ngeo_abort("CIFTI series axis is invalid or not equally spaced.",
                  "ngeo_error_format")
    }
  }
  list(
    type = type,
    datatype = datatype,
    named_map_metadata = metadata,
    dimensions = dim(values),
    brain_models = length(x$base$geometry$components),
    layer_names = x$layers$name
  )
}

.ngeo_cifti_brain_xml <- function(x) {
  offset <- 0L
  volume <- Filter(
    function(component) component$kind == "volume",
    x$base$geometry$components
  )
  brain <- vapply(x$base$geometry$components, function(component) {
    count <- component$n_element
    structure <- paste0("CIFTI_STRUCTURE_", toupper(component$structure))
    result <- if (component$kind == "surface") {
      index <- component$vertex_index - component$source_index_base
      paste0(
        '<BrainModel IndexOffset="', offset, '" IndexCount="', count,
        '" ModelType="CIFTI_MODEL_TYPE_SURFACE" BrainStructure="',
        structure, '" SurfaceNumberOfVertices="',
        component$surface_vertex_count, '"><VertexIndices>',
        paste(index, collapse = " "), "</VertexIndices></BrainModel>"
      )
    } else {
      index <- component$voxel_index - component$source_index_base
      paste0(
        '<BrainModel IndexOffset="', offset, '" IndexCount="', count,
        '" ModelType="CIFTI_MODEL_TYPE_VOXELS" BrainStructure="',
        structure, '"><VoxelIndicesIJK>',
        paste(apply(index, 1L, paste, collapse = " "), collapse = "\n"),
        "</VoxelIndicesIJK></BrainModel>"
      )
    }
    offset <<- offset + count
    result
  }, character(1))
  volume_xml <- ""
  if (length(volume)) {
    affine <- volume[[1L]]$affine
    if (any(vapply(volume, function(z) {
      !isTRUE(all.equal(z$affine, affine))
    }, logical(1)))) {
      .ngeo_abort("CIFTI volume components require one affine.",
                  "ngeo_error_transform")
    }
    all_index <- do.call(rbind, lapply(volume, function(z) {
      z$voxel_index - z$source_index_base
    }))
    dimensions <- apply(all_index, 2L, max) + 1L
    volume_xml <- paste0(
      '<Volume VolumeDimensions="', paste(dimensions, collapse = ","),
      '"><TransformationMatrixVoxelIndicesIJKtoXYZ MeterExponent="-3">',
      paste(apply(affine, 1L, paste, collapse = " "), collapse = "\n"),
      "</TransformationMatrixVoxelIndicesIJKtoXYZ></Volume>"
    )
  }
  paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="1" ',
    'IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS">',
    paste0(brain, collapse = ""), volume_xml, "</MatrixIndicesMap>"
  )
}

.ngeo_cifti_map_xml <- function(x, type, named_map_metadata) {
  if (type == "dtseries") {
    time <- x$layers$time %||% (seq_len(nrow(x$layers)) - 1L)
    step <- if (length(time) > 1L) time[[2L]] - time[[1L]] else 1
    unit <- x$layers$time_unit[[1L]] %||% "SECOND"
    return(paste0(
      '<MatrixIndicesMap AppliesToMatrixDimension="0" ',
      'IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES" ',
      'NumberOfSeriesPoints="', nrow(x$layers),
      '" SeriesStart="', time[[1L]], '" SeriesStep="', step,
      '" SeriesUnit="', .ngeo_xml_escape(unit), '"/>'
    ))
  }
  layers <- vapply(seq_len(nrow(x$layers)), function(i) {
    label_xml <- ""
    if (type == "dlabel") {
      stored <- x$base$labels[[x$layers$name[[i]]]]$table %||% NULL
      if (is.data.frame(stored) &&
          all(c("Key", "Red", "Green", "Blue", "Alpha", "Label") %in%
              names(stored))) {
        keys <- stored$Key
        red <- stored$Red
        green <- stored$Green
        blue <- stored$Blue
        alpha <- stored$Alpha
        label <- stored$Label
      } else {
        keys <- sort(unique(as.integer(x$values[, i])))
        red <- green <- blue <- rep(0.5, length(keys))
        alpha <- rep(1, length(keys))
        label <- paste0("label_", keys)
      }
      label_xml <- paste0(
        "<LabelTable>",
        paste0(
          '<Label Key="', keys, '" Red="', red,
          '" Green="', green, '" Blue="', blue,
          '" Alpha="', alpha, '">',
          .ngeo_xml_escape(label), "</Label>",
          collapse = ""
        ),
        "</LabelTable>"
      )
    }
    paste0(
      "<NamedMap>",
      .ngeo_cifti_metadata_xml(named_map_metadata[[i]]),
      "<MapName>", .ngeo_xml_escape(x$layers$name[[i]]),
      "</MapName>", label_xml, "</NamedMap>"
    )
  }, character(1))
  paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="0" ',
    'IndicesMapToDataType="',
    if (type == "dlabel") {
      "CIFTI_INDEX_TYPE_LABELS"
    } else {
      "CIFTI_INDEX_TYPE_SCALARS"
    },
    '">',
    paste0(layers, collapse = ""), "</MatrixIndicesMap>"
  )
}

.ngeo_write_fixed <- function(connection, value, size) {
  raw <- charToRaw(enc2utf8(value))
  writeBin(c(raw[seq_len(min(length(raw), size))],
             raw(max(0L, size - length(raw)))), connection)
}

.ngeo_write_int64 <- function(connection, value) {
  for (current in value) {
    if (current < 0 || current >= 2^31) {
      .ngeo_abort("CIFTI writer dimension exceeds int64 subset.",
                  "ngeo_error_resource")
    }
    writeBin(c(as.integer(current), 0L), connection,
             size = 4L, endian = "little")
  }
}

#' Write CIFTI-2 dscalar, dlabel, or dtseries using pure R
#'
#' @param x An `ngeo_grayordinate` with loaded values.
#' @param path Output path.
#' @param type Dense scalar, label, or time series.
#' @param overwrite Whether to replace an existing file.
#' @param datatype Optional `float32`, `float64`, or `int32` datatype.
#' @param named_map_metadata Optional metadata aligned with scalar/label layers.
#'
#' @return `path`, invisibly.
#' @export
write_ngeo_cifti <- function(
    x,
    path,
    type = c("dscalar", "dlabel", "dtseries"),
    overwrite = FALSE,
    datatype = NULL,
    named_map_metadata = NULL) {
  contract <- ngeo_validate_cifti_contract(
    x, type, datatype, named_map_metadata
  )
  type <- contract$type
  expected <- paste0(".", type, ".nii")
  if (!endsWith(tolower(path), expected)) {
    .ngeo_abort(paste0("CIFTI path must end in `", expected, "`."),
                "ngeo_error_format")
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    .ngeo_abort("CIFTI output exists; use `overwrite = TRUE`.",
                "ngeo_error_io")
  }
  values <- as.matrix(x$values)
  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<CIFTI Version="2"><Matrix>',
    .ngeo_cifti_map_xml(x, type, contract$named_map_metadata),
    .ngeo_cifti_brain_xml(x),
    "</Matrix></CIFTI>"
  )
  xml_raw <- charToRaw(enc2utf8(xml))
  extension_size <- ceiling((8L + length(xml_raw)) / 16L) * 16L
  vox_offset <- 544L + extension_size
  intent <- switch(
    type,
    dscalar = c(3006L, "ConnDenseScalar"),
    dlabel = c(3007L, "ConnDenseLabel"),
    dtseries = c(3002L, "ConnDenseSeries")
  )
  storage <- switch(
    contract$datatype,
    float32 = list(code = 16L, bits = 32L, size = 4L, mode = "double"),
    float64 = list(code = 64L, bits = 64L, size = 8L, mode = "double"),
    int32 = list(code = 8L, bits = 32L, size = 4L, mode = "integer")
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  connection <- file(path, "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(540L, connection, size = 4L, endian = "little")
  .ngeo_write_fixed(connection, "n+2\r\n\032\n", 8L)
  writeBin(c(storage$code, storage$bits), connection,
           size = 2L, endian = "little")
  .ngeo_write_int64(
    connection,
    c(6L, 1L, 1L, 1L, 1L, ncol(values), nrow(values), 1L)
  )
  writeBin(rep(0, 3L), connection, size = 8L, endian = "little")
  writeBin(c(0, rep(1, 7L)), connection, size = 8L, endian = "little")
  .ngeo_write_int64(connection, vox_offset)
  writeBin(rep(0, 6L), connection, size = 8L, endian = "little")
  .ngeo_write_int64(connection, c(0L, 0L))
  .ngeo_write_fixed(connection, "neurogeo pure-R CIFTI-2", 80L)
  .ngeo_write_fixed(connection, "", 24L)
  writeBin(c(0L, 0L), connection, size = 4L, endian = "little")
  writeBin(rep(0, 18L), connection, size = 8L, endian = "little")
  writeBin(c(0L, 0L, as.integer(intent[[1L]])), connection,
           size = 4L, endian = "little")
  .ngeo_write_fixed(connection, intent[[2L]], 16L)
  writeBin(raw(16L), connection)
  writeBin(as.raw(c(1L, 0L, 0L, 0L)), connection)
  writeBin(as.integer(extension_size), connection,
           size = 4L, endian = "little")
  writeBin(32L, connection, size = 4L, endian = "little")
  writeBin(c(xml_raw, raw(extension_size - 8L - length(xml_raw))), connection)
  output_values <- if (storage$mode == "integer") {
    as.integer(t(values))
  } else {
    as.numeric(t(values))
  }
  writeBin(
    output_values,
    connection,
    size = storage$size,
    endian = "little"
  )
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

.ngeo_cifti_extension_xml <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  seek(connection, where = 540L, origin = "start")
  extension <- readBin(connection, "raw", n = 4L)
  if (!length(extension) || extension[[1L]] == as.raw(0L)) return(NULL)
  size <- readBin(
    connection, "integer", n = 1L, size = 4L, endian = "little"
  )
  code <- readBin(
    connection, "integer", n = 1L, size = 4L, endian = "little"
  )
  if (!length(size) || size < 8L || !identical(code, 32L)) return(NULL)
  value <- readBin(connection, "raw", n = size - 8L)
  value <- value[value != as.raw(0L)]
  if (!length(value)) NULL else rawToChar(value)
}

.ngeo_cifti_xml_value <- function(x, tag) {
  pattern <- paste0("(?s)<", tag, ">(.*?)</", tag, ">")
  match <- regmatches(x, regexec(pattern, x, perl = TRUE))[[1L]]
  if (length(match) < 2L) NULL else .ngeo_xml_unescape(match[[2L]])
}

.ngeo_cifti_read_named_metadata <- function(path, layer_names) {
  xml <- .ngeo_cifti_extension_xml(path)
  if (is.null(xml)) return(rep(list(list()), length(layer_names)))
  position <- gregexpr(
    "(?s)<NamedMap>.*?</NamedMap>", xml, perl = TRUE
  )
  blocks <- regmatches(xml, position)[[1L]]
  result <- stats::setNames(rep(list(list()), length(layer_names)), layer_names)
  for (block in blocks) {
    layer_name <- .ngeo_cifti_xml_value(block, "MapName")
    if (is.null(layer_name) || !layer_name %in% layer_names) next
    md_position <- gregexpr("(?s)<MD>.*?</MD>", block, perl = TRUE)
    entries <- regmatches(block, md_position)[[1L]]
    metadata <- list()
    for (entry in entries) {
      name <- .ngeo_cifti_xml_value(entry, "Name")
      value <- .ngeo_cifti_xml_value(entry, "Value")
      if (!is.null(name) && !is.null(value)) metadata[[name]] <- value
    }
    result[[layer_name]] <- metadata
  }
  unname(result)
}

.ngeo_cifti_header_datatype <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  seek(connection, where = 12L, origin = "start")
  code <- readBin(
    connection, "integer", n = 1L, size = 2L, endian = "little",
    signed = TRUE
  )
  switch(
    as.character(code),
    `8` = "int32",
    `16` = "float32",
    `64` = "float64",
    paste0("unsupported:", code)
  )
}
