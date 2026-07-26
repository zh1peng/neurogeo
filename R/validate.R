.ngeo_validate_common <- function(x) {
  required <- c("domain", "values", "maps", "measures", "labels", "provenance")
  if (!inherits(x, "ngeo") || !is.list(x) ||
      any(!required %in% names(x))) {
    .ngeo_abort(
      "Object does not have the required `ngeo` top-level structure.",
      "ngeo_error_object"
    )
  }
  if (!inherits(x$domain, "ngeo_domain") ||
      !is.data.frame(x$domain$elements)) {
    .ngeo_abort(
      "`domain` must contain an element table.",
      "ngeo_error_domain"
    )
  }

  elements <- x$domain$elements
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
        "`values` is not aligned with domain elements.",
        "ngeo_error_alignment"
      )
    }
  }

  if (!is.data.frame(x$maps) || !is.data.frame(x$measures) ||
      nrow(x$measures) != nrow(x$maps)) {
    .ngeo_abort(
      "`maps` and `measures` must align with value columns.",
      "ngeo_error_alignment"
    )
  }
  if (!is.null(x$values) && ncol(x$values) != nrow(x$maps)) {
    .ngeo_abort(
      "`maps` and `measures` must align with value columns.",
      "ngeo_error_alignment"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_points <- function(x) {
  domain <- x$domain
  coordinates <- domain$coordinates
  if (!inherits(domain, "ngeo_points_domain") ||
      !is.matrix(coordinates) || !is.numeric(coordinates) ||
      !ncol(coordinates) %in% c(2L, 3L) ||
      nrow(coordinates) != nrow(domain$elements) ||
      anyNA(coordinates) || any(!is.finite(coordinates))) {
    .ngeo_abort("Invalid points domain.", "ngeo_error_domain")
  }
  if (!is.null(domain$uncertainty) &&
      (length(domain$uncertainty) != nrow(domain$elements) ||
        anyNA(domain$uncertainty) ||
        any(domain$uncertainty < 0))) {
    .ngeo_abort(
      "Point uncertainty must align with elements and be non-negative.",
      "ngeo_error_alignment"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_grayordinates <- function(x) {
  domain <- x$domain
  if (!inherits(domain, "ngeo_grayordinates_domain") ||
      !is.list(domain$components) || !length(domain$components)) {
    .ngeo_abort("Invalid grayordinates domain.", "ngeo_error_domain")
  }

  n <- nrow(domain$elements)
  rows <- unlist(lapply(domain$components, `[[`, "global_rows"))
  if (length(rows) != n || anyDuplicated(rows) ||
      !identical(sort(as.integer(rows)), seq_len(n))) {
    .ngeo_abort(
      "Grayordinate component rows must partition the global element order.",
      "ngeo_error_alignment"
    )
  }

  ids <- vapply(domain$components, `[[`, character(1), "component_id")
  if (anyDuplicated(ids) ||
      any(!domain$elements$component_id %in% ids)) {
    .ngeo_abort(
      "Grayordinate component IDs are inconsistent.",
      "ngeo_error_domain"
    )
  }

  for (component in domain$components) {
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
            nrow(component$geometry$domain$elements) !=
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
          "ngeo_error_domain"
        )
      }
    } else {
      .ngeo_abort(
        "Unknown grayordinate component kind.",
        "ngeo_error_domain"
      )
    }
  }
  invisible(TRUE)
}

.ngeo_validate_regions <- function(x) {
  domain <- x$domain
  n <- nrow(domain$elements)
  if (!inherits(domain, "ngeo_regions_domain") ||
      !"region_id" %in% names(domain$elements) ||
      anyDuplicated(domain$elements$region_id) ||
      length(domain$support_size) != n) {
    .ngeo_abort("Invalid regions domain.", "ngeo_error_domain")
  }
  if (!is.null(domain$centroid) &&
      (!is.matrix(domain$centroid) ||
        nrow(domain$centroid) != n ||
        !ncol(domain$centroid) %in% c(2L, 3L))) {
    .ngeo_abort(
      "Region centroids must align with elements.",
      "ngeo_error_alignment"
    )
  }
  if (!is.null(domain$adjacency) &&
      !identical(dim(domain$adjacency), c(n, n))) {
    .ngeo_abort(
      "Region adjacency must align with elements.",
      "ngeo_error_alignment"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_surface <- function(x, strict) {
  domain <- x$domain
  if (!inherits(domain, "ngeo_surface_domain") ||
      !is.list(domain$coordinates) || !length(domain$coordinates)) {
    .ngeo_abort("Invalid surface domain.", "ngeo_error_domain")
  }

  n_vertex <- nrow(domain$elements)
  coordinate_rows <- vapply(domain$coordinates, nrow, integer(1))
  if (any(coordinate_rows != n_vertex)) {
    .ngeo_abort(
      "Surface coordinate rows must align with elements.",
      "ngeo_error_alignment"
    )
  }
  coordinate_meta <- domain$coordinate_meta
  required_meta <- c(
    "name", "dimension", "role", "units", "metric_eligible"
  )
  if (!is.data.frame(coordinate_meta) ||
      any(!required_meta %in% names(coordinate_meta)) ||
      nrow(coordinate_meta) != length(domain$coordinates) ||
      !identical(
        as.character(coordinate_meta$name),
        names(domain$coordinates)
      ) ||
      any(coordinate_meta$dimension != vapply(
        domain$coordinates,
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
  chart_rows <- coordinate_meta$role == "chart"
  if (any(coordinate_meta$dimension[chart_rows] != 2L) ||
      any(coordinate_meta$metric_eligible[chart_rows])) {
    .ngeo_abort(
      "Computational charts must be 2D and not metric-eligible.",
      "ngeo_error_chart"
    )
  }
  if (!is.null(domain$charts)) {
    if (!is.list(domain$charts) ||
        any(!names(domain$charts) %in% coordinate_meta$name[chart_rows])) {
      .ngeo_abort(
        "Surface chart metadata does not match declared chart coordinates.",
        "ngeo_error_chart"
      )
    }
  }
  faces <- domain$faces
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

.ngeo_validate_volume <- function(x) {
  domain <- x$domain
  if (!inherits(domain, "ngeo_volume_domain") ||
      length(domain$dim) != 3L ||
      !is.matrix(domain$affine) ||
      !identical(dim(domain$affine), c(4L, 4L))) {
    .ngeo_abort("Invalid volume domain.", "ngeo_error_domain")
  }
  if (abs(det(domain$affine[1:3, 1:3, drop = FALSE])) <=
      .Machine$double.eps) {
    .ngeo_abort(
      "Volume affine linear component is singular.",
      "ngeo_error_geometry"
    )
  }

  index <- domain$voxel_index
  if (!is.matrix(index) || ncol(index) != 3L ||
      nrow(index) != nrow(domain$elements)) {
    .ngeo_abort(
      "Volume voxel indices must align with elements.",
      "ngeo_error_alignment"
    )
  }
  for (axis in seq_len(3L)) {
    if (any(index[, axis] < 1L | index[, axis] > domain$dim[axis])) {
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
#' @param x An `ngeo` object.
#' @param level Validation level: basic structural invariants, strict domain
#'   checks, or scientific semantic diagnostics.
#'
#' @return `x`, invisibly.
#' @export
ngeo_validate <- function(x, level = c("basic", "strict", "scientific")) {
  level <- match.arg(level)
  .ngeo_validate_common(x)

  type <- x$domain$type
  if (identical(type, "surface")) {
    .ngeo_validate_surface(x, strict = level != "basic")
  } else if (identical(type, "volume")) {
    .ngeo_validate_volume(x)
  } else if (identical(type, "points")) {
    .ngeo_validate_points(x)
  } else if (identical(type, "grayordinates")) {
    .ngeo_validate_grayordinates(x)
  } else if (identical(type, "regions")) {
    .ngeo_validate_regions(x)
  } else {
    .ngeo_abort(
      sprintf("Domain type `%s` is not implemented by this prototype.", type),
      "ngeo_error_domain"
    )
  }

  if (level != "basic" && !inherits(x$domain$space, "ngeo_space")) {
    .ngeo_abort(
      "Domain space must be an `ngeo_space` object.",
      "ngeo_error_space"
    )
  }
  if (level == "scientific") {
    if (identical(x$domain$space$space_id, "unknown")) {
      .ngeo_warn(
        "Coordinate space is unknown; no named space was assumed.",
        "ngeo_warning_space_unknown"
      )
    }
    if (nrow(x$measures) &&
        any(x$measures$spatial_semantics == "unknown")) {
      .ngeo_warn(
        "At least one map has unknown measurement semantics.",
        "ngeo_warning_measure_unknown"
      )
    }
  }
  invisible(x)
}
