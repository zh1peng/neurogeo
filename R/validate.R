.ngeo_validate_coordinate_space <- function(coordinate_space) {
  if (!inherits(coordinate_space, "ngeo_coordinate_space") ||
      !is.list(coordinate_space)) {
    .ngeo_abort(
      "Domain coordinate_space fields are invalid.",
      "ngeo_error_coordinate_space"
    )
  }
  required <- c(
    "space_id", "kind", "unit", "structure", "template", "density",
    "resolution", "source_metadata"
  )
  scalar_character <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  optional_scalar_character <- function(value) {
    is.null(value) || scalar_character(value)
  }
  valid_resolution <- is.null(coordinate_space$resolution) ||
    (is.atomic(coordinate_space$resolution) &&
      length(coordinate_space$resolution) %in% c(1L, 3L) &&
      !anyNA(coordinate_space$resolution) &&
      if (is.numeric(coordinate_space$resolution)) {
        all(is.finite(coordinate_space$resolution)) &&
          all(coordinate_space$resolution > 0)
      } else {
        is.character(coordinate_space$resolution) &&
          all(nzchar(coordinate_space$resolution))
      })
  if (any(!required %in% names(coordinate_space)) ||
      !scalar_character(coordinate_space$space_id) ||
      !scalar_character(coordinate_space$kind) ||
      !coordinate_space$kind %in% c(
        "unknown", "surface", "volume", "hybrid"
      ) ||
      !scalar_character(coordinate_space$unit) ||
      !optional_scalar_character(coordinate_space$structure) ||
      !optional_scalar_character(coordinate_space$template) ||
      !optional_scalar_character(coordinate_space$density) ||
      !valid_resolution ||
      !is.list(coordinate_space$source_metadata)) {
    .ngeo_abort(
      "Domain coordinate_space fields are invalid.",
      "ngeo_error_coordinate_space"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_measures <- function(measures) {
  required <- c(
    "measure_id", "unit", "value_type", "support_behavior",
    "missing_policy", "aggregation"
  )
  if (any(!required %in% names(measures)) ||
      any(vapply(measures[required], function(value) {
        !is.character(value) || anyNA(value) || any(!nzchar(value))
      }, logical(1))) ||
      any(!measures$support_behavior %in% c(
        "unknown", "intensive", "extensive", "count", "categorical"
      )) ||
      any(!measures$missing_policy %in% c("preserve", "exclude"))) {
    .ngeo_abort(
      "Measurement metadata contains invalid fields or vocabulary.",
      "ngeo_error_measure"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_history <- function(history) {
  scalar_character <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  if (!is.list(history) ||
      !scalar_character(history$spec_version) ||
      !scalar_character(history$package_version) ||
      (!is.null(history$operations) && !is.list(history$operations))) {
    .ngeo_abort("Dataset history is invalid.", "ngeo_error_history")
  }
  for (operation in history$operations %||% list()) {
    valid <- is.list(operation) &&
      scalar_character(operation$operation) &&
      is.list(operation$software) &&
      scalar_character(operation$software$package) &&
      scalar_character(operation$software$version) &&
      scalar_character(operation$timestamp_utc) &&
      grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        operation$timestamp_utc
      ) &&
      is.list(operation$parameters)
    if (!valid) {
      .ngeo_abort(
        "Dataset history contains an invalid operation record.",
        "ngeo_error_history"
      )
    }
  }
  invisible(TRUE)
}

.ngeo_validate_common <- function(x) {
  required <- c("base", "values", "layers", "measures", "history")
  if (!inherits(x, "ngeo") || !is.list(x) ||
      any(!required %in% names(x))) {
    .ngeo_abort(
      "Object does not have the required `ngeo` top-level structure.",
      "ngeo_error_object"
    )
  }
  if (!inherits(x$base, "ngeo_base") ||
      !is.data.frame(x$base$elements)) {
    .ngeo_abort(
      "`base` must contain an element table.",
      "ngeo_error_base"
    )
  }

  elements <- x$base$elements
  if (!"element_id" %in% names(elements) ||
      anyNA(elements$element_id) ||
      any(!nzchar(elements$element_id)) ||
      anyDuplicated(elements$element_id)) {
    .ngeo_abort(
      "`element_id` must be non-missing and unique.",
      "ngeo_error_index"
    )
  }

  n_element <- nrow(elements)
  if (!is.null(x$values)) {
    if (!(is.matrix(x$values) || inherits(x$values, "Matrix") ||
        inherits(x$values, "ngeo_delayed_values"))) {
      .ngeo_abort("`values` must be a matrix or NULL.", "ngeo_error_values")
    }
    if (nrow(x$values) != n_element) {
      .ngeo_abort(
        "`values` is not aligned with base elements.",
        "ngeo_error_alignment"
      )
    }
  }

  if (!is.data.frame(x$layers) ||
      any(!c("layer_id", "measure_id") %in% names(x$layers)) ||
      anyNA(x$layers$layer_id) || anyDuplicated(x$layers$layer_id)) {
    .ngeo_abort(
      "`layers` must define unique layer IDs and a measure reference.",
      "ngeo_error_alignment"
    )
  }
  if (!is.null(x$values) && ncol(x$values) != nrow(x$layers)) {
    .ngeo_abort(
      "`layers` must align with value columns.",
      "ngeo_error_alignment"
    )
  }
  if (!is.data.frame(x$measures) ||
      !"measure_id" %in% names(x$measures) ||
      anyNA(x$measures$measure_id) || anyDuplicated(x$measures$measure_id) ||
      any(!x$layers$measure_id %in% x$measures$measure_id)) {
    .ngeo_abort(
      "Every layer must reference exactly one defined measure.",
      "ngeo_error_measure"
    )
  }
  if (!is.list(x$history)) {
    .ngeo_abort("`history` must be a list.", "ngeo_error_history")
  }
  .ngeo_validate_labels(
    x$base$labels %||% list(),
    n_element,
    as.character(x$layers$layer_id)
  )
  invisible(TRUE)
}

.ngeo_validate_point <- function(x) {
  base <- x$base
  coordinates <- base$geometry$coordinates
  if (!inherits(base, "ngeo_point_base") ||
      !is.matrix(coordinates) || !is.numeric(coordinates) ||
      !ncol(coordinates) %in% c(2L, 3L) ||
      nrow(coordinates) != nrow(base$elements) ||
      anyNA(coordinates) || any(!is.finite(coordinates))) {
    .ngeo_abort("Invalid point base.", "ngeo_error_base")
  }
  if (!is.null(base$geometry$uncertainty) &&
      (length(base$geometry$uncertainty) != nrow(base$elements) ||
        anyNA(base$geometry$uncertainty) ||
        any(base$geometry$uncertainty < 0))) {
    .ngeo_abort(
      "Point uncertainty must align with elements and be non-negative.",
      "ngeo_error_alignment"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_grayordinate <- function(x) {
  base <- x$base
  if (!inherits(base, "ngeo_grayordinate_base") ||
      !is.list(base$geometry$components) || !length(base$geometry$components)) {
    .ngeo_abort("Invalid grayordinate base.", "ngeo_error_base")
  }

  n <- nrow(base$elements)
  rows <- unlist(lapply(base$geometry$components, `[[`, "global_rows"))
  if (length(rows) != n || anyDuplicated(rows) ||
      !identical(sort(as.integer(rows)), seq_len(n))) {
    .ngeo_abort(
      "Grayordinate component rows must partition the global element order.",
      "ngeo_error_alignment"
    )
  }

  ids <- vapply(base$geometry$components, `[[`, character(1), "component_id")
  if (anyDuplicated(ids) ||
      any(!base$elements$component_id %in% ids)) {
    .ngeo_abort(
      "Grayordinate component IDs are inconsistent.",
      "ngeo_error_base"
    )
  }

  for (component in base$geometry$components) {
    if (length(component$global_rows) != component$n_element) {
      .ngeo_abort(
        "Grayordinate component size is inconsistent.",
        "ngeo_error_alignment"
      )
    }
    if (identical(component$kind, "surface")) {
      if (length(component$vertex_index) != component$n_element ||
          anyDuplicated(component$vertex_index)) {
        .ngeo_abort(
          "Grayordinate surface indices are inconsistent.",
          "ngeo_error_index"
        )
      }
      if (!is.null(component$geometry) &&
          (!inherits(component$geometry, "ngeo_surface") ||
            nrow(component$geometry$base$elements) !=
              component$surface_vertex_count)) {
        .ngeo_abort(
          "Attached grayordinate surface geometry is inconsistent.",
          "ngeo_error_alignment"
        )
      }
    } else if (identical(component$kind, "volume")) {
      if (!is.matrix(component$voxel_index) ||
          ncol(component$voxel_index) != 3L ||
          nrow(component$voxel_index) != component$n_element ||
          !is.matrix(component$affine) ||
          !identical(dim(component$affine), c(4L, 4L))) {
        .ngeo_abort(
          "Grayordinate volume component is inconsistent.",
          "ngeo_error_base"
        )
      }
    } else {
      .ngeo_abort(
        "Unknown grayordinate component kind.",
        "ngeo_error_base"
      )
    }
  }
  invisible(TRUE)
}

.ngeo_validate_parcellation <- function(x) {
  base <- x$base
  n <- nrow(base$elements)
  if (!inherits(base, "ngeo_parcellation_base") ||
      !"region_id" %in% names(base$elements) ||
      anyDuplicated(base$elements$region_id) ||
      length(base$geometry$support_size) != n) {
    .ngeo_abort("Invalid parcellation base.", "ngeo_error_base")
  }
  if (!is.null(base$geometry$centroid) &&
      (!is.matrix(base$geometry$centroid) ||
        nrow(base$geometry$centroid) != n ||
        !ncol(base$geometry$centroid) %in% c(2L, 3L))) {
    .ngeo_abort(
      "Region centroids must align with elements.",
      "ngeo_error_alignment"
    )
  }
  if (!is.null(base$topology$adjacency) &&
      !identical(dim(base$topology$adjacency), c(n, n))) {
    .ngeo_abort(
      "Region adjacency must align with elements.",
      "ngeo_error_alignment"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_surface_coordinates <- function(base, n_vertex) {
  coordinate_rows <- vapply(base$geometry$coordinates, nrow, integer(1))
  if (any(coordinate_rows != n_vertex)) {
    .ngeo_abort(
      "Surface coordinate rows must align with elements.",
      "ngeo_error_alignment"
    )
  }
  coordinate_meta <- base$geometry$coordinate_meta
  required_meta <- c(
    "name", "dimension", "role", "unit", "metric_eligible"
  )
  if (!is.data.frame(coordinate_meta) ||
      any(!required_meta %in% names(coordinate_meta)) ||
      nrow(coordinate_meta) != length(base$geometry$coordinates) ||
      anyNA(coordinate_meta[required_meta]) ||
      !identical(
        as.character(coordinate_meta$name),
        names(base$geometry$coordinates)
      ) ||
      any(coordinate_meta$dimension != vapply(
        base$geometry$coordinates,
        ncol,
        integer(1)
      )) ||
      any(!coordinate_meta$role %in% c(
        "anatomical", "registration", "visualization", "chart"
      )) ||
      !is.logical(coordinate_meta$metric_eligible) ||
      anyNA(coordinate_meta$metric_eligible)) {
    .ngeo_abort(
      "Surface coordinate metadata is inconsistent.",
      "ngeo_error_geometry"
    )
  }
  active_coordinates <- base$geometry$active_coordinates
  if (!is.character(active_coordinates) ||
      length(active_coordinates) != 1L || is.na(active_coordinates) ||
      !active_coordinates %in% names(base$geometry$coordinates)) {
    .ngeo_abort(
      "Surface active coordinates must name one available coordinate set.",
      "ngeo_error_geometry"
    )
  }
  chart_rows <- coordinate_meta$role == "chart"
  if (any(coordinate_meta$dimension[chart_rows] != 2L) ||
      any(coordinate_meta$metric_eligible[chart_rows])) {
    .ngeo_abort(
      "Computational charts must be 2D and not metric-eligible.",
      "ngeo_error_chart"
    )
  }
  if (!is.null(base$charts)) {
    if (!is.list(base$charts) ||
        any(!names(base$charts) %in% coordinate_meta$name[chart_rows])) {
      .ngeo_abort(
        "Surface chart metadata does not match declared chart coordinates.",
        "ngeo_error_chart"
      )
    }
  }
  invisible(TRUE)
}

.ngeo_validate_surface_faces <- function(base, n_vertex, strict) {
  faces <- base$geometry$faces
  if (!is.matrix(faces) || ncol(faces) != 3L ||
      (length(faces) && (min(faces) < 1L || max(faces) > n_vertex))) {
    .ngeo_abort("Surface face indices are invalid.", "ngeo_error_index")
  }
  if (nrow(faces) && any(
    faces[, 1L] == faces[, 2L] |
      faces[, 1L] == faces[, 3L] |
      faces[, 2L] == faces[, 3L]
  )) {
    .ngeo_abort(
      "Surface contains a face with repeated vertices.",
      "ngeo_error_geometry"
    )
  }

  if (strict && nrow(faces)) {
    canonical <- t(apply(faces, 1L, sort))
    if (anyDuplicated(data.frame(canonical))) {
      .ngeo_warn(
        "Surface contains duplicate faces.",
        "ngeo_warning_duplicate_faces"
      )
    }
  }
  invisible(TRUE)
}

.ngeo_validate_surface <- function(x, strict) {
  base <- x$base
  if (!inherits(base, "ngeo_surface_base") ||
      !is.list(base$geometry$coordinates) || !length(base$geometry$coordinates)) {
    .ngeo_abort("Invalid surface base.", "ngeo_error_base")
  }
  n_vertex <- nrow(base$elements)
  .ngeo_validate_surface_coordinates(base, n_vertex)
  .ngeo_validate_surface_faces(base, n_vertex, strict)
  invisible(TRUE)
}

.ngeo_validate_volume <- function(x) {
  base <- x$base
  if (!inherits(base, "ngeo_volume_base") ||
      length(base$geometry$dim) != 3L ||
      !is.matrix(base$geometry$affine) ||
      !identical(dim(base$geometry$affine), c(4L, 4L))) {
    .ngeo_abort("Invalid volume base.", "ngeo_error_base")
  }
  if (abs(det(base$geometry$affine[1:3, 1:3, drop = FALSE])) <=
      .Machine$double.eps) {
    .ngeo_abort(
      "Volume affine linear component is singular.",
      "ngeo_error_geometry"
    )
  }

  index <- base$geometry$voxel_index
  if (!is.matrix(index) || ncol(index) != 3L ||
      nrow(index) != nrow(base$elements)) {
    .ngeo_abort(
      "Volume voxel indices must align with elements.",
      "ngeo_error_alignment"
    )
  }
  for (axis in seq_len(3L)) {
    if (any(index[, axis] < 1L | index[, axis] > base$geometry$dim[axis])) {
      .ngeo_abort(
        "Volume voxel index is outside the lattice.",
        "ngeo_error_index"
      )
    }
  }
  if (anyDuplicated(data.frame(index))) {
    .ngeo_abort(
      "Volume voxel indices must be unique.",
      "ngeo_error_index"
    )
  }
  invisible(TRUE)
}

#' Validate an NGCS object
#'
#' @param x An `ngeo` object or another registered NGCS object.
#' @param level Validation level: basic structural invariants, strict base
#'   checks, or scientific semantic diagnostics.
#'
#' @section When to use and when not to use:
#' Use strict validation after reading, constructing, migrating, or safely
#' updating an object. Scientific validation diagnoses declared semantics; it
#' does not prove that a method is appropriate for a study design.
#' @section Units and assumptions:
#' Validation preserves declared units and reports unknown semantics rather
#' than inventing them. Passing validation assumes input metadata are truthful.
#' @section Validation:
#' Schema, golden-format, migration, and conformance fixtures exercise all
#' registered base types and validation levels.
#' @return `x`, invisibly.
#' @seealso [ngeo_capabilities()], [ngeo_coordinate_space()],
#'   [ngeo_measures()]
#' @references Neuroimaging Geoinformatics Core Specification 6.0,
#'   `inst/spec/NGCS-6.0.md`.
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 1, 1), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 3))
#' )
#' ngeo_validate(point, "basic")
#' ngeo_validate(point, "strict")
#' \dontshow{
#' suppressWarnings(ngeo_validate(point, "scientific"))
#' }
#' @export
ngeo_validate <- function(x, level = c("basic", "strict", "scientific")) {
  level <- match.arg(level)
  if (!inherits(x, "ngeo")) {
    schema <- .ngeo_schema(x)
    .ngeo_schema_validate_one(x, schema$schema_id[[1L]])
    return(invisible(x))
  }
  .ngeo_validate_common(x)

  type <- x$base$type
  if (identical(type, "surface")) {
    .ngeo_validate_surface(x, strict = level != "basic")
  } else if (identical(type, "volume")) {
    .ngeo_validate_volume(x)
  } else if (identical(type, "point")) {
    .ngeo_validate_point(x)
  } else if (identical(type, "grayordinate")) {
    .ngeo_validate_grayordinate(x)
  } else if (identical(type, "parcellation")) {
    .ngeo_validate_parcellation(x)
  } else {
    .ngeo_abort(
      sprintf("Domain type `%s` is not supported.", type),
      "ngeo_error_base"
    )
  }

  if (level != "basic") {
    .ngeo_validate_coordinate_space(x$base$coordinate_space)
    .ngeo_validate_measures(x$measures)
    .ngeo_validate_history(x$history)
  }
  if (level == "scientific") {
    if (identical(x$base$coordinate_space$space_id, "unknown")) {
      .ngeo_warn(
        "Coordinate coordinate_space is unknown; no named coordinate_space was assumed.",
        "ngeo_warning_space_unknown"
      )
    }
    if (nrow(x$measures) &&
        any(x$measures$support_behavior == "unknown")) {
      .ngeo_warn(
        "At least one map has unknown measurement semantics.",
        "ngeo_warning_measure_unknown"
      )
    }
  }
  invisible(x)
}
