.ngeo_element_table <- function(n, source_index_base = 1L) {
  source_index_base <- as.integer(source_index_base)
  source_index <- seq_len(n) - 1L + source_index_base

  data.frame(
    element_id = sprintf("element_%08d", seq_len(n)),
    source_index = source_index,
    source_index_base = rep.int(source_index_base, n),
    included = rep.int(TRUE, n),
    stringsAsFactors = FALSE
  )
}

.ngeo_values <- function(values, n_element) {
  if (is.null(values)) {
    return(NULL)
  }
  if (is.data.frame(values)) {
    values <- as.matrix(values)
  } else if (is.atomic(values) && is.null(dim(values))) {
    values <- matrix(values, ncol = 1L)
  }

  is_delayed <- inherits(values, "ngeo_delayed_values")
  is_matrix <- is.matrix(values) || inherits(values, "Matrix") || is_delayed
  valid_storage <- if (inherits(values, "Matrix")) {
    methods::is(values, "dMatrix") ||
      methods::is(values, "iMatrix") ||
      methods::is(values, "lMatrix") ||
      methods::is(values, "nMatrix")
  } else if (is_delayed) {
    TRUE
  } else {
    is.numeric(values) || is.integer(values) || is.logical(values)
  }
  if (!is_matrix || !valid_storage) {
    .ngeo_abort(
      "`values` must be a numeric, integer, or logical matrix/vector.",
      "ngeo_error_values"
    )
  }
  if (nrow(values) != n_element) {
    .ngeo_abort(
      sprintf(
        "`values` has %d rows but the domain has %d elements. No implicit resampling was performed.",
        nrow(values), n_element
      ),
      "ngeo_error_alignment"
    )
  }
  values
}

.ngeo_maps <- function(maps, n_map, value_names = NULL) {
  if (n_map == 0L) {
    if (!is.null(maps) && NROW(maps) != 0L) {
      .ngeo_abort(
        "`maps` must be empty when `values` is NULL.",
        "ngeo_error_alignment"
      )
    }
    return(data.frame(
      map_id = character(),
      name = character(),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(maps)) {
    names <- value_names
    if (is.null(names) || length(names) != n_map ||
        anyNA(names) || any(!nzchar(names))) {
      names <- paste0("map_", seq_len(n_map))
    }
    return(data.frame(
      map_id = sprintf("map_%04d", seq_len(n_map)),
      name = names,
      stringsAsFactors = FALSE
    ))
  }

  if (is.character(maps)) {
    maps <- data.frame(name = maps, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(maps) || nrow(maps) != n_map) {
    .ngeo_abort(
      sprintf("`maps` must have exactly %d rows.", n_map),
      "ngeo_error_alignment"
    )
  }
  if (!"name" %in% names(maps)) {
    maps$name <- paste0("map_", seq_len(n_map))
  }
  if (!"map_id" %in% names(maps)) {
    maps$map_id <- sprintf("map_%04d", seq_len(n_map))
  }
  maps
}

.ngeo_measures <- function(measures, maps) {
  n_map <- nrow(maps)
  if (n_map == 0L) {
    return(data.frame(
      map_id = character(),
      value_type = character(),
      spatial_semantics = character(),
      units = character(),
      missing_policy = character(),
      default_aggregation = character(),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(measures)) {
    return(data.frame(
      map_id = maps$map_id,
      value_type = rep.int("continuous", n_map),
      spatial_semantics = rep.int("unknown", n_map),
      units = rep.int("unknown", n_map),
      missing_policy = rep.int("preserve", n_map),
      default_aggregation = rep.int("none", n_map),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(measures) || nrow(measures) != n_map) {
    .ngeo_abort(
      sprintf("`measures` must have exactly %d rows.", n_map),
      "ngeo_error_alignment"
    )
  }

  required <- c(
    "value_type", "spatial_semantics", "units",
    "missing_policy", "default_aggregation"
  )
  missing <- setdiff(required, names(measures))
  if (length(missing)) {
    .ngeo_abort(
      sprintf(
        "`measures` is missing required columns: %s.",
        paste(missing, collapse = ", ")
      ),
      "ngeo_error_measure"
    )
  }
  if (!"map_id" %in% names(measures)) {
    measures$map_id <- maps$map_id
  }
  measures
}

.new_ngeo <- function(domain,
                      values = NULL,
                      maps = NULL,
                      measures = NULL,
                      labels = list(),
                      provenance = list(),
                      class) {
  n_element <- nrow(domain$elements)
  values <- .ngeo_values(values, n_element)
  n_map <- if (is.null(values)) {
    if (is.null(maps)) 0L else NROW(maps)
  } else {
    ncol(values)
  }
  maps <- .ngeo_maps(maps, n_map, colnames(values))
  measures <- .ngeo_measures(measures, maps)
  if (!is.null(values)) {
    colnames(values) <- maps$name
  }

  if (!is.list(labels)) {
    .ngeo_abort("`labels` must be a list.", "ngeo_error_labels")
  }
  if (!is.list(provenance)) {
    .ngeo_abort("`provenance` must be a list.", "ngeo_error_provenance")
  }

  provenance$spec_version <- provenance$spec_version %||% "2.0"
  provenance$package_version <- provenance$package_version %||%
    .ngeo_package_version()

  x <- structure(
    list(
      domain = domain,
      values = values,
      maps = maps,
      measures = measures,
      labels = labels,
      provenance = provenance
    ),
    class = c(class, "ngeo")
  )
  ngeo_validate(x, level = "basic")
  x
}

#' Return the domain type
#'
#' @param x An `ngeo` object.
#' @return A scalar domain type.
#' @export
ngeo_domain_type <- function(x) {
  if (!inherits(x, "ngeo")) {
    .ngeo_abort("`x` must inherit from `ngeo`.", "ngeo_error_argument")
  }
  x$domain$type
}

#' Access core NGCS fields
#'
#' @param x An `ngeo` object.
#' @param maps Optional map selection for `ngeo_values()`.
#' @return The requested field.
#' @name ngeo_accessors
NULL

#' @rdname ngeo_accessors
#' @export
ngeo_domain <- function(x) {
  ngeo_validate(x, "basic")
  x$domain
}

#' @rdname ngeo_accessors
#' @export
ngeo_elements <- function(x) {
  ngeo_validate(x, "basic")
  x$domain$elements
}

#' @rdname ngeo_accessors
#' @export
ngeo_values <- function(x, maps = NULL) {
  ngeo_validate(x, "basic")
  if (is.null(x$values) || is.null(maps)) {
    return(x$values)
  }
  index <- .ngeo_map_selection(x, maps)
  x$values[, index, drop = FALSE]
}

#' @rdname ngeo_accessors
#' @export
ngeo_maps <- function(x) {
  ngeo_validate(x, "basic")
  x$maps
}

#' @rdname ngeo_accessors
#' @export
ngeo_measures <- function(x) {
  ngeo_validate(x, "basic")
  x$measures
}

#' @rdname ngeo_accessors
#' @export
ngeo_labels <- function(x) {
  ngeo_validate(x, "basic")
  x$labels
}

#' @rdname ngeo_accessors
#' @export
ngeo_provenance <- function(x) {
  ngeo_validate(x, "basic")
  x$provenance
}

#' Compute an implementation domain hash
#'
#' The hash identifies an R domain representation. It is not a replacement
#' for language-independent conformance fixtures.
#'
#' @param x An `ngeo` object or `ngeo_domain`.
#' @return A hexadecimal xxHash64 digest.
#' @export
ngeo_domain_hash <- function(x) {
  domain <- if (inherits(x, "ngeo")) x$domain else x
  if (!inherits(domain, "ngeo_domain")) {
    .ngeo_abort(
      "`x` must be an `ngeo` object or `ngeo_domain`.",
      "ngeo_error_argument"
    )
  }
  if (identical(domain$type, "grayordinates")) {
    domain$components <- lapply(domain$components, function(component) {
      if (inherits(component$geometry, "ngeo")) {
        component$geometry <- component$geometry$domain
      }
      component
    })
  }
  digest::digest(domain, algo = "xxhash64", serialize = TRUE)
}

#' Report available object capabilities
#'
#' @param x An `ngeo` object.
#' @return A named logical vector.
#' @export
ngeo_capabilities <- function(x) {
  ngeo_validate(x, "basic")
  type <- x$domain$type
  coordinates <- x$domain$coordinates %||% list()
  if (identical(type, "regions") && !is.null(x$domain$centroid)) {
    coordinates <- x$domain$centroid
  }
  if (is.matrix(coordinates)) {
    coordinates <- list(active = coordinates)
  }
  if (identical(type, "grayordinates")) {
    surface_components <- Filter(
      function(component) identical(component$kind, "surface"),
      x$domain$components
    )
    volume_components <- Filter(
      function(component) identical(component$kind, "volume"),
      x$domain$components
    )
    surface_geometry <- length(surface_components) > 0L &&
      all(vapply(
        surface_components,
        function(component) inherits(component$geometry, "ngeo_surface"),
        logical(1)
      ))
    volume_affine <- length(volume_components) > 0L &&
      all(vapply(
        volume_components,
        function(component) {
          is.matrix(component$affine) &&
            identical(dim(component$affine), c(4L, 4L))
        },
        logical(1)
      ))
    geometry_coordinates <- unlist(lapply(
      surface_components,
      function(component) {
        if (!inherits(component$geometry, "ngeo_surface")) {
          return(integer())
        }
        vapply(
          component$geometry$domain$coordinates,
          ncol,
          integer(1)
        )
      }
    ))
  } else {
    surface_geometry <- identical(type, "surface") &&
      !is.null(x$domain$faces)
    volume_affine <- identical(type, "volume") &&
      !is.null(x$domain$affine)
    geometry_coordinates <- integer()
  }
  dimensions <- if (length(coordinates)) {
    vapply(coordinates, ncol, integer(1))
  } else {
    integer()
  }
  dimensions <- c(dimensions, geometry_coordinates)

  c(
    coordinates_2d = any(dimensions == 2L),
    coordinates_3d = any(dimensions == 3L),
    surface_topology = surface_geometry,
    voxel_affine = volume_affine,
    adjacency = identical(type, "surface") ||
      identical(type, "volume") ||
      (identical(type, "grayordinates") &&
        surface_geometry &&
        (length(volume_components) == 0L || volume_affine)) ||
      (identical(type, "regions") && !is.null(x$domain$adjacency)),
    surface_area = surface_geometry &&
      any(dimensions %in% c(2L, 3L)),
    voxel_volume = volume_affine,
    geodesic = surface_geometry,
    partition = !is.null(x$domain$membership),
    labels = length(x$labels) > 0L,
    chart = any(dimensions == 2L)
  )
}

#' @export
print.ngeo <- function(x, ...) {
  n_element <- nrow(x$domain$elements)
  n_map <- nrow(x$maps)
  cat(
    "<", class(x)[1L], ">\n",
    "  domain: ", x$domain$type, "\n",
    "  elements: ", n_element, "\n",
    "  maps: ", n_map, "\n",
    "  space: ", x$domain$space$space_id, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
summary.ngeo <- function(object, ...) {
  capabilities <- ngeo_capabilities(object)
  list(
    class = class(object)[1L],
    domain = object$domain$type,
    elements = nrow(object$domain$elements),
    maps = nrow(object$maps),
    space = object$domain$space$space_id,
    capabilities = capabilities
  )
}
