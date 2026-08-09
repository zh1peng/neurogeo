.ngeo_chart_coordinates <- function(x, chart = NULL) {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "Computational charts are currently defined for surface bases.",
      "ngeo_error_capability"
    )
  }
  meta <- x$base$geometry$coordinate_meta
  available <- meta$name[meta$role == "chart"]
  if (is.null(chart)) {
    if (length(available) != 1L) {
      .ngeo_abort(
        "Select one chart explicitly; the surface does not have exactly one chart.",
        "ngeo_error_chart"
      )
    }
    chart <- available[[1L]]
  }
  .ngeo_assert_scalar_character(chart, "chart")
  if (!chart %in% available) {
    .ngeo_abort(
      sprintf("`%s` is not a declared 2D computational chart.", chart),
      "ngeo_error_chart"
    )
  }
  coordinates <- x$base$geometry$coordinates[[chart]]
  if (ncol(coordinates) != 2L) {
    .ngeo_abort(
      "A computational chart must have exactly two coordinates.",
      "ngeo_error_chart"
    )
  }
  list(
    name = chart,
    coordinates = coordinates,
    metadata = x$base$charts[[chart]]
  )
}

#' Add a two-dimensional computational chart
#'
#' A chart is an auxiliary coordinate set for planar display and
#' interoperability. It never replaces anatomical coordinates, support,
#' topology, or distance_method eligibility.
#'
#' @param x An `ngeo_surface`.
#' @param coordinates Finite vertex-aligned two-dimensional coordinates.
#' @param name Unique chart name.
#' @param distortion Optional aligned or global distortion metadata.
#' @param source Optional source description.
#'
#' @return A new `ngeo_surface` with the chart and history appended.
#' @templateVar example_call ngeo_set_chart(surface_data, "flat", flat_coordinates)
#' @template stable-geometry-core
#' @export
ngeo_set_chart <- function(x,
                           coordinates,
                           name = "chart",
                           distortion = NULL,
                           source = NULL) {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "`ngeo_set_chart()` requires an `ngeo_surface`.",
      "ngeo_error_capability"
    )
  }
  ngeo_validate(x, "basic")
  .ngeo_assert_scalar_character(name, "name")
  if (name %in% names(x$base$geometry$coordinates)) {
    .ngeo_abort(
      sprintf("Coordinate set `%s` already exists.", name),
      "ngeo_error_chart"
    )
  }
  if (!is.matrix(coordinates) || !is.numeric(coordinates) ||
      ncol(coordinates) != 2L ||
      nrow(coordinates) != nrow(x$base$elements) ||
      anyNA(coordinates) || any(!is.finite(coordinates))) {
    .ngeo_abort(
      "`coordinates` must be a finite n_vertex by 2 numeric matrix.",
      "ngeo_error_alignment"
    )
  }
  if (!is.null(distortion)) {
    valid_distortion <- is.list(distortion) ||
      is.atomic(distortion) ||
      is.data.frame(distortion) ||
      is.matrix(distortion)
    aligned <- if (is.list(distortion) &&
        !is.data.frame(distortion) && !is.matrix(distortion)) {
      TRUE
    } else {
      NROW(distortion) %in% c(1L, nrow(coordinates))
    }
    if (!valid_distortion || !aligned) {
      .ngeo_abort(
        "`distortion` must be global or aligned with surface vertices.",
        "ngeo_error_alignment"
      )
    }
  }
  if (!is.null(source)) {
    .ngeo_assert_scalar_character(source, "source")
  }

  original_hash <- base_hash(x)
  result <- x
  result$base$geometry$coordinates[[name]] <- coordinates
  result$base$geometry$coordinate_meta <- rbind(
    result$base$geometry$coordinate_meta,
    data.frame(
      name = name,
      dimension = 2L,
      role = "chart",
      unit = result$base$coordinate_space$unit,
      metric_eligible = FALSE,
      stringsAsFactors = FALSE
    )
  )
  result$base$charts <- result$base$charts %||% list()
  result$base$charts[[name]] <- list(
    source_base_hash = original_hash,
    source = source,
    distortion = distortion
  )
  result$history$operations <- c(
    result$history$operations,
    list(.ngeo_operation(
      "ngeo_set_chart",
      list(
        name = name,
        source_base_hash = original_hash,
        source = source,
        distortion_recorded = !is.null(distortion)
      )
    ))
  )
  ngeo_validate(result, "strict")
  result
}

#' Return chart distortion metadata
#'
#' @param x An `ngeo_surface` with a chart.
#' @param chart Optional chart name.
#'
#' @return The stored distortion metadata, or `NULL`.
#' @templateVar example_call ngeo_chart_distortion(surface_data, "flat")
#' @template stable-geometry-core
#' @export
ngeo_chart_distortion <- function(x, chart = NULL) {
  .ngeo_chart_coordinates(x, chart)$metadata$distortion
}

.ngeo_sf_values <- function(x, include_values) {
  data <- x$base$elements
  if (isTRUE(include_values) && !is.null(x$values)) {
    data <- cbind(data, as.data.frame(as.matrix(x$values)))
  }
  data
}

#' Export an NGCS base to simple features
#'
#' This is an explicit interoperability copy. The returned object is not the
#' core geometry representation and must not be used to replace anatomical
#' support or topology.
#'
#' @param x An `ngeo_surface`, `ngeo_point`, or `ngeo_parcellation`.
#' @param feature Elements/vertices or surface faces.
#' @param chart Surface chart name.
#' @param include_values Include aligned values as ordinary columns.
#' @param max_features Explicit feature-count guard.
#'
#' @return An `sf` object with NGCS identity attributes.
#' @templateVar example_call ngeo_as_sf(parcellation_data)
#' @template stable-geometry-core
#' @export
ngeo_as_sf <- function(x,
                       feature = c("element", "face"),
                       chart = NULL,
                       include_values = TRUE,
                       max_features = getOption(
                         "neurogeo.max_sf_features",
                         100000L
                       )) {
  .ngeo_require("sf", "simple-features interoperability")
  ngeo_validate(x, "basic")
  feature <- match.arg(feature)
  if (!is.numeric(max_features) || length(max_features) != 1L ||
      is.na(max_features) || max_features < 1 ||
      max_features != floor(max_features)) {
    .ngeo_abort(
      "`max_features` must be one positive integer.",
      "ngeo_error_argument"
    )
  }

  chart_info <- NULL
  coordinates <- if (inherits(x, "ngeo_surface")) {
    chart_info <- .ngeo_chart_coordinates(x, chart)
    chart_info$coordinates
  } else if (inherits(x, "ngeo_point")) {
    x$base$geometry$coordinates
  } else if (inherits(x, "ngeo_parcellation")) {
    x$base$geometry$centroid
  } else {
    .ngeo_abort(
      "`ngeo_as_sf()` supports surfaces with charts, point, and parcellation with centroids.",
      "ngeo_error_capability"
    )
  }
  if (is.null(coordinates) || ncol(coordinates) != 2L ||
      anyNA(coordinates)) {
    .ngeo_abort(
      "Simple-features export requires complete planar 2D coordinates.",
      "ngeo_error_capability"
    )
  }

  if (identical(feature, "face")) {
    if (!inherits(x, "ngeo_surface")) {
      .ngeo_abort(
        "Face export requires a surface chart.",
        "ngeo_error_capability"
      )
    }
    faces <- x$base$geometry$faces
    if (nrow(faces) > max_features) {
      .ngeo_abort(
        "Surface face export exceeds `max_features`.",
        "ngeo_error_resource"
      )
    }
    geometry <- sf::st_sfc(lapply(seq_len(nrow(faces)), function(i) {
      vertices <- coordinates[faces[i, ], , drop = FALSE]
      sf::st_polygon(list(rbind(vertices, vertices[1L, ])))
    }))
    data <- data.frame(
      face_id = sprintf("face_%08d", seq_len(nrow(faces))),
      stringsAsFactors = FALSE
    )
  } else {
    if (nrow(coordinates) > max_features) {
      .ngeo_abort(
        "Element export exceeds `max_features`.",
        "ngeo_error_resource"
      )
    }
    geometry <- sf::st_sfc(lapply(seq_len(nrow(coordinates)), function(i) {
      sf::st_point(coordinates[i, ])
    }))
    data <- .ngeo_sf_values(x, include_values)
  }
  result <- sf::st_sf(data, geometry = geometry)
  attr(result, "base_hash") <- base_hash(x)
  attr(result, "ngeo_feature") <- feature
  attr(result, "ngeo_chart") <- chart_info$name %||% NULL
  attr(result, "ngeo_chart_metadata") <- chart_info$metadata %||% NULL
  result
}
