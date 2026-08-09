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

.values <- function(values, n_element) {
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
        "`values` has %d rows but the base has %d elements. No implicit resampling was performed.",
        nrow(values), n_element
      ),
      "ngeo_error_alignment"
    )
  }
  values
}

.layers <- function(layers, n_layer, value_names = NULL, measures = NULL) {
  if (n_layer == 0L) {
    if (!is.null(layers) && NROW(layers) != 0L) {
      .ngeo_abort(
        "`layers` must be empty when `values` is NULL.",
        "ngeo_error_alignment"
      )
    }
    return(data.frame(
      layer_id = character(),
      name = character(),
      measure_id = character(),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(layers)) {
    names <- value_names
    if (is.null(names) || length(names) != n_layer ||
        anyNA(names) || any(!nzchar(names))) {
      names <- paste0("layer_", seq_len(n_layer))
    }
    layers <- data.frame(
      layer_id = sprintf("layer_%04d", seq_len(n_layer)),
      name = names,
      stringsAsFactors = FALSE
    )
  }
  if (is.character(layers)) {
    layers <- data.frame(name = layers, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(layers) || nrow(layers) != n_layer) {
    .ngeo_abort(
      sprintf("`layers` must have exactly %d rows.", n_layer),
      "ngeo_error_alignment"
    )
  }
  if (!"name" %in% names(layers)) {
    layers$name <- paste0("layer_", seq_len(n_layer))
  }
  if (!"layer_id" %in% names(layers)) {
    layers$layer_id <- sprintf("layer_%04d", seq_len(n_layer))
  }
  if (anyNA(layers$layer_id) || any(!nzchar(layers$layer_id)) ||
      anyDuplicated(layers$layer_id)) {
    .ngeo_abort(
      "`layers$layer_id` must contain non-missing unique identifiers.",
      "ngeo_error_layer"
    )
  }
  if (!"measure_id" %in% names(layers)) {
    supplied_measure_ids <- if (is.data.frame(measures) &&
        "measure_id" %in% names(measures)) {
      unique(as.character(measures$measure_id))
    } else {
      character()
    }
    layers$measure_id <- if (length(supplied_measure_ids) == n_layer) {
      supplied_measure_ids
    } else if (length(supplied_measure_ids) == 1L) {
      rep.int(supplied_measure_ids, n_layer)
    } else {
      rep.int("measure_unknown", n_layer)
    }
  }
  if (anyNA(layers$measure_id) || any(!nzchar(layers$measure_id))) {
    .ngeo_abort(
      "`layers$measure_id` must contain non-missing identifiers.",
      "ngeo_error_measure"
    )
  }
  layers
}

.measures <- function(measures, layers) {
  measure_ids <- unique(as.character(layers$measure_id))
  if (!length(measure_ids)) {
    return(data.frame(
      measure_id = character(),
      name = character(),
      unit = character(),
      value_type = character(),
      support_behavior = character(),
      missing_policy = character(),
      aggregation = character(),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(measures)) {
    return(data.frame(
      measure_id = measure_ids,
      name = measure_ids,
      unit = rep.int("unknown", length(measure_ids)),
      value_type = rep.int("unknown", length(measure_ids)),
      support_behavior = rep.int("unknown", length(measure_ids)),
      missing_policy = rep.int("preserve", length(measure_ids)),
      aggregation = rep.int("unknown", length(measure_ids)),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(measures) || !"measure_id" %in% names(measures)) {
    .ngeo_abort(
      "`measures` must be a data frame containing `measure_id`.",
      "ngeo_error_measure"
    )
  }
  if (anyNA(measures$measure_id) || any(!nzchar(measures$measure_id)) ||
      anyDuplicated(measures$measure_id)) {
    .ngeo_abort(
      "`measures$measure_id` must contain non-missing unique identifiers.",
      "ngeo_error_measure"
    )
  }
  missing_measures <- setdiff(measure_ids, measures$measure_id)
  if (length(missing_measures)) {
    .ngeo_abort(
      sprintf(
        "`layers$measure_id` references undefined measures: %s.",
        paste(missing_measures, collapse = ", ")
      ),
      "ngeo_error_measure"
    )
  }
  defaults <- list(
    name = as.character(measures$measure_id),
    unit = rep.int("unknown", nrow(measures)),
    value_type = rep.int("unknown", nrow(measures)),
    support_behavior = rep.int("unknown", nrow(measures)),
    missing_policy = rep.int("preserve", nrow(measures)),
    aggregation = rep.int("unknown", nrow(measures))
  )
  for (field in names(defaults)) {
    if (!field %in% names(measures)) {
      measures[[field]] <- defaults[[field]]
    }
  }
  measures
}

.ngeo_measures_for_layers <- function(x, layer_index, unique = FALSE) {
  ids <- as.character(x$layers$measure_id[layer_index])
  if (isTRUE(unique)) {
    ids <- unique(ids)
  }
  rows <- match(ids, x$measures$measure_id)
  if (anyNA(rows)) {
    .ngeo_abort(
      "A layer references an undefined measure.",
      "ngeo_error_measure"
    )
  }
  x$measures[rows, , drop = FALSE]
}

.ngeo_validate_labels <- function(labels, n_element, layer_ids = character()) {
  if (!is.list(labels)) {
    .ngeo_abort("`labels` must be a list.", "ngeo_error_labels")
  }
  for (label in labels) {
    if (!is.list(label)) {
      .ngeo_abort(
        "Every label resource must be a list.",
        "ngeo_error_labels"
      )
    }
    if (!is.null(label$values) && length(label$values) != n_element) {
      .ngeo_abort(
        "Label values must align with base elements.",
        "ngeo_error_alignment"
      )
    }
    if (!is.null(label$layer_id) &&
        (!is.character(label$layer_id) || length(label$layer_id) != 1L ||
          is.na(label$layer_id) || !nzchar(label$layer_id) ||
          !label$layer_id %in% layer_ids)) {
      .ngeo_abort(
        "A label resource references an undefined layer.",
        "ngeo_error_labels"
      )
    }
  }
  invisible(TRUE)
}

.ngeo_subset_labels <- function(labels, index, n_element, layer_ids) {
  labels <- Filter(function(label) {
    is.null(label$layer_id) || label$layer_id %in% layer_ids
  }, labels)
  lapply(labels, function(label) {
    if (!is.null(label$values) && length(label$values) == n_element) {
      label$values <- label$values[index]
    }
    label
  })
}

.new_ngeo <- function(base,
                      values = NULL,
                      layers = NULL,
                      measures = NULL,
                      labels = list(),
                      history = list(),
                      class) {
  n_element <- nrow(base$elements)
  values <- .values(values, n_element)
  n_layer <- if (is.null(values)) {
    if (is.null(layers)) 0L else NROW(layers)
  } else {
    ncol(values)
  }
  if (is.data.frame(measures) && !"measure_id" %in% names(measures)) {
    measures$measure_id <- sprintf("measure_%04d", seq_len(nrow(measures)))
  }
  layers <- .layers(layers, n_layer, colnames(values), measures)
  measures <- .measures(measures, layers)
  if (!is.null(values)) {
    colnames(values) <- layers$name
  }

  .ngeo_validate_labels(labels, n_element, layers$layer_id)
  if (!is.list(history)) {
    .ngeo_abort("`history` must be a list.", "ngeo_error_history")
  }

  history$spec_version <- history$spec_version %||% "6.0"
  history$package_version <- history$package_version %||%
    .ngeo_package_version()

  base$labels <- labels
  x <- structure(
    list(
      base = base,
      values = values,
      layers = layers,
      measures = measures,
      history = history
    ),
    class = c(class, "ngeo")
  )
  ngeo_validate(x, level = "basic")
  x
}

#' Return the base type
#'
#' @param x An `ngeo` object.
#' @return A scalar base type.
#' @export
base_type <- function(x) {
  if (!inherits(x, "ngeo")) {
    .ngeo_abort("`x` must inherit from `ngeo`.", "ngeo_error_argument")
  }
  x$base$type
}

#' Access core NGCS fields
#'
#' @param x An `ngeo` object.
#' @param layers Optional layer selection for `values()`.
#' @return The requested field.
#' @name ngeo_accessors
NULL

#' @rdname ngeo_accessors
#' @export
spatial_base <- function(x) {
  ngeo_validate(x, "basic")
  x$base
}

#' @rdname ngeo_accessors
#' @export
base_elements <- function(x) {
  ngeo_validate(x, "basic")
  x$base$elements
}

#' @rdname ngeo_accessors
#' @export
values <- function(x, layers = NULL) {
  ngeo_validate(x, "basic")
  if (is.null(x$values) || is.null(layers)) {
    return(x$values)
  }
  index <- .ngeo_layer_selection(x, layers)
  x$values[, index, drop = FALSE]
}

#' @rdname ngeo_accessors
#' @export
layers <- function(x) {
  ngeo_validate(x, "basic")
  x$layers
}

#' @rdname ngeo_accessors
#' @export
measures <- function(x) {
  ngeo_validate(x, "basic")
  x$measures
}

#' @rdname ngeo_accessors
#' @export
ngeo_labels <- function(x) {
  ngeo_validate(x, "basic")
  x$base$labels %||% list()
}

#' @rdname ngeo_accessors
#' @export
history <- function(x) {
  ngeo_validate(x, "basic")
  x$history
}

#' Prefixed accessors for core NGCS fields
#'
#' These are the preferred 6.x names. The original unprefixed accessors remain
#' available throughout 6.x for compatibility.
#'
#' @section When to use and when not to use:
#' Use these read-only accessors in user code and tutorials. Do not mutate the
#' returned internal tables to update an object; use a constructor or a safe
#' update function such as `ngeo_update_measure()`.
#' @section Units and assumptions:
#' Values retain their measure units; spatial-base coordinates retain their
#' coordinate-space unit. Accessors do not transform, copy, or reinterpret
#' either.
#' @section Validation:
#' Core alignment, immutability, and lifecycle tests validate every prefixed
#' accessor against its 6.x compatibility accessor.
#' @inheritParams ngeo_accessors
#' @return The requested field.
#' @examples
#' x <- ngeo_point(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
#' ngeo_base_type(x)
#' ngeo_base_elements(x)
#' ngeo_layers(x)
#' @seealso [ngeo_validate()], [ngeo_update_measure()], [ngeo_point()]
#' @references Neuroimaging Geoinformatics Core Specification 6.0,
#'   `inst/spec/API-6.0.md`.
#' @name ngeo_prefixed_accessors
NULL

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_spatial_base <- function(x) spatial_base(x)

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_base_elements <- function(x) base_elements(x)

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_values <- function(x, layers = NULL) values(x, layers = layers)

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_layers <- function(x) layers(x)

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_measures <- function(x) measures(x)

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_history <- function(x) history(x)

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_base_type <- function(x) base_type(x)

#' @rdname ngeo_prefixed_accessors
#' @export
ngeo_base_hash <- function(x) base_hash(x)

#' Compute an implementation base hash
#'
#' The hash identifies the ordered spatial base while intentionally excluding
#' `base$labels`. Label resources may therefore be added or merged without
#' changing spatial compatibility. It is not a replacement for
#' language-independent conformance fixtures.
#'
#' @param x An `ngeo` object or `ngeo_base`.
#' @return A hexadecimal xxHash64 digest.
#' @export
base_hash <- function(x) {
  base <- if (inherits(x, "ngeo")) x$base else x
  if (!inherits(base, "ngeo_base")) {
    .ngeo_abort(
      "`x` must be an `ngeo` object or `ngeo_base`.",
      "ngeo_error_argument"
    )
  }
  if (identical(base$type, "grayordinate")) {
    base$geometry$components <- lapply(base$geometry$components, function(component) {
      if (inherits(component$geometry, "ngeo")) {
        component$geometry <- component$geometry$base
      }
      component
    })
  }
  base$labels <- NULL
  digest::digest(base, algo = "xxhash64", serialize = TRUE)
}

#' Report available object capabilities
#'
#' @param x An `ngeo` object.
#' @return A named logical vector.
#' @export
ngeo_capabilities <- function(x) {
  ngeo_validate(x, "basic")
  type <- x$base$type
  coordinates <- x$base$geometry$coordinates %||% list()
  if (identical(type, "parcellation") && !is.null(x$base$geometry$centroid)) {
    coordinates <- x$base$geometry$centroid
  }
  if (is.matrix(coordinates)) {
    coordinates <- list(active = coordinates)
  }
  if (identical(type, "grayordinate")) {
    surface_components <- Filter(
      function(component) identical(component$kind, "surface"),
      x$base$geometry$components
    )
    volume_components <- Filter(
      function(component) identical(component$kind, "volume"),
      x$base$geometry$components
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
          component$geometry$base$geometry$coordinates,
          ncol,
          integer(1)
        )
      }
    ))
  } else {
    surface_geometry <- identical(type, "surface") &&
      !is.null(x$base$geometry$faces)
    volume_affine <- identical(type, "volume") &&
      !is.null(x$base$geometry$affine)
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
      (identical(type, "grayordinate") &&
        surface_geometry &&
        (length(volume_components) == 0L || volume_affine)) ||
      (identical(type, "parcellation") && !is.null(x$base$topology$adjacency)),
    surface_area = surface_geometry &&
      any(dimensions %in% c(2L, 3L)),
    voxel_volume = volume_affine,
    geodesic = surface_geometry,
    partition = !is.null(x$base$geometry$membership),
    labels = length(x$base$labels %||% list()) > 0L,
    chart = any(dimensions == 2L)
  )
}

#' @export
print.ngeo <- function(x, ...) {
  n_element <- nrow(x$base$elements)
  n_layer <- nrow(x$layers)
  cat(
    "<", class(x)[1L], ">\n",
    "  base: ", x$base$type, "\n",
    "  elements: ", n_element, "\n",
    "  layers: ", n_layer, "\n",
    "  coordinate_space: ", x$base$coordinate_space$space_id, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
summary.ngeo <- function(object, ...) {
  capabilities <- ngeo_capabilities(object)
  list(
    class = class(object)[1L],
    base = object$base$type,
    elements = nrow(object$base$elements),
    layers = nrow(object$layers),
    coordinate_space = object$base$coordinate_space$space_id,
    capabilities = capabilities
  )
}
