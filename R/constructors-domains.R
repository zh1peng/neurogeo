#' Construct a point dataset
#'
#' @param coordinates A finite numeric matrix with two or three columns.
#' @param values Optional point-aligned values.
#' @param layers Optional layer metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param coordinate_space An `ngeo_coordinate_space`.
#' @param structure Optional structure label per point.
#' @param uncertainty Optional non-negative uncertainty per point.
#' @param index_base Source index base.
#'
#' @section When to use and when not to use:
#' Use this constructor for observations located at explicit 2-D or 3-D point
#' coordinates. Do not use it for voxels with an affine, mesh vertices with
#' faces, parcels, or grayordinates; use their type-specific constructors.
#' @section Units and assumptions:
#' Coordinate columns use `coordinate_space$unit`. Rows of `values` must align
#' exactly with coordinate rows, and columns must align with `layers`.
#' @section Validation:
#' Construction enforces alignment; use `ngeo_validate(..., "strict")` before
#' analysis. The contract is validated by the point and quickstart fixtures.
#' @return An `ngeo_point` object.
#' @seealso [ngeo_coordinate_space()], [ngeo_validate()],
#'   [ngeo_spatial_weights()]
#' @references Neuroimaging Geoinformatics Core Specification 6.0,
#'   `inst/spec/NGCS-6.0.md`.
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 3))
#' )
#' base_hash(point)
#' ngeo_distance(point, from = 1, to = 3, distance_method = "euclidean")
#' ngeo_spatial_weights(point, method = "knn", k = 2)
#' @export
ngeo_point <- function(coordinates,
                        values = NULL,
                        layers = NULL,
                        measures = NULL,
                        labels = list(),
                        coordinate_space = ngeo_coordinate_space(),
                        structure = NULL,
                        uncertainty = NULL,
                        index_base = c("one", "zero")) {
  index_base <- match.arg(index_base)
  if (!is.matrix(coordinates) || !is.numeric(coordinates) ||
      !ncol(coordinates) %in% c(2L, 3L) ||
      anyNA(coordinates) || any(!is.finite(coordinates))) {
    .ngeo_abort(
      "`coordinates` must be a finite numeric matrix with 2 or 3 columns.",
      "ngeo_error_geometry"
    )
  }
  if (!inherits(coordinate_space, "ngeo_coordinate_space")) {
    .ngeo_abort("`coordinate_space` must be an `ngeo_coordinate_space` object.", "ngeo_error_coordinate_space")
  }

  n <- nrow(coordinates)
  if (!is.null(structure) &&
      (!is.character(structure) || length(structure) != n || anyNA(structure))) {
    .ngeo_abort(
      sprintf("`structure` must be a character vector of length %d.", n),
      "ngeo_error_alignment"
    )
  }
  if (!is.null(uncertainty) &&
      (!is.numeric(uncertainty) || length(uncertainty) != n ||
        anyNA(uncertainty) || any(!is.finite(uncertainty)) ||
        any(uncertainty < 0))) {
    .ngeo_abort(
      sprintf("`uncertainty` must be non-negative and have length %d.", n),
      "ngeo_error_alignment"
    )
  }

  source_base <- if (identical(index_base, "zero")) 0L else 1L
  elements <- .ngeo_element_table(n, source_base)
  if (!is.null(structure)) {
    elements$structure <- structure
  }

  base <- base::structure(
    list(
      type = "point",
      elements = elements,
      geometry = list(
        coordinates = coordinates,
        uncertainty = uncertainty
      ),
      coordinate_space = coordinate_space,
      topology = NULL
    ),
    class = c("ngeo_point_base", "ngeo_base")
  )

  .new_ngeo(
    base = base,
    values = values,
    layers = layers,
    measures = measures,
    labels = labels,
    history = list(
      operations = list(.ngeo_operation(
        "ngeo_point",
        list(source_index_base = source_base)
      ))
    ),
    class = "ngeo_point"
  )
}

.ngeo_component_scalar <- function(component, field) {
  value <- component[[field]]
  .ngeo_assert_scalar_character(value, paste0("component$", field))
  value
}

.ngeo_gray_surface_component <- function(component, component_id) {
  vertex_index <- .ngeo_as_integer(
    component$vertex_index,
    paste0(component_id, "$vertex_index")
  )
  if (!length(vertex_index)) {
    .ngeo_abort(
      sprintf("Surface component `%s` is empty.", component_id),
      "ngeo_error_base"
    )
  }
  source_base <- as.integer(component$source_index_base %||% 0L)
  if (!source_base %in% c(0L, 1L)) {
    .ngeo_abort(
      "`source_index_base` must be 0 or 1.",
      "ngeo_error_index"
    )
  }
  vertex_count <- .ngeo_as_integer(
    component$surface_vertex_count,
    paste0(component_id, "$surface_vertex_count")
  )
  if (length(vertex_count) != 1L || vertex_count <= 0L) {
    .ngeo_abort(
      "`surface_vertex_count` must be one positive integer.",
      "ngeo_error_base"
    )
  }
  internal_vertex <- vertex_index - source_base + 1L
  if (any(internal_vertex < 1L | internal_vertex > vertex_count)) {
    .ngeo_abort(
      sprintf("Surface indices in `%s` exceed the full surface.", component_id),
      "ngeo_error_index"
    )
  }
  if (anyDuplicated(vertex_index)) {
    .ngeo_abort(
      sprintf("Surface indices in `%s` must be unique.", component_id),
      "ngeo_error_index"
    )
  }

  geometry <- component$geometry %||% NULL
  if (!is.null(geometry)) {
    if (!inherits(geometry, "ngeo_surface")) {
      .ngeo_abort(
        sprintf("Geometry for `%s` must be an `ngeo_surface`.", component_id),
        "ngeo_error_geometry"
      )
    }
    if (nrow(geometry$base$elements) != vertex_count) {
      .ngeo_abort(
        sprintf(
          "Geometry for `%s` has %d vertices; expected %d.",
          component_id,
          nrow(geometry$base$elements),
          vertex_count
        ),
        "ngeo_error_alignment"
      )
    }
  }

  list(
    kind = "surface",
    vertex_index = vertex_index,
    internal_vertex_index = internal_vertex,
    surface_vertex_count = vertex_count,
    source_index_base = source_base,
    geometry = geometry,
    n_element = length(vertex_index)
  )
}

.ngeo_gray_volume_component <- function(component, component_id) {
  voxel_index <- component$voxel_index
  if (is.data.frame(voxel_index)) {
    voxel_index <- as.matrix(voxel_index)
  }
  if (!is.matrix(voxel_index) || ncol(voxel_index) != 3L) {
    .ngeo_abort(
      sprintf("`voxel_index` for `%s` must have three columns.", component_id),
      "ngeo_error_index"
    )
  }
  original_dim <- dim(voxel_index)
  voxel_index <- .ngeo_as_integer(
    voxel_index,
    paste0(component_id, "$voxel_index")
  )
  dim(voxel_index) <- original_dim
  if (!nrow(voxel_index) || anyDuplicated(data.frame(voxel_index))) {
    .ngeo_abort(
      sprintf("Voxel indices in `%s` must be non-empty and unique.", component_id),
      "ngeo_error_index"
    )
  }

  affine <- component$affine
  if (!is.matrix(affine) || !is.numeric(affine) ||
      !identical(dim(affine), c(4L, 4L)) ||
      anyNA(affine) || any(!is.finite(affine)) ||
      abs(det(affine[1:3, 1:3, drop = FALSE])) <= .Machine$double.eps) {
    .ngeo_abort(
      sprintf("Affine for `%s` must be finite and non-singular.", component_id),
      "ngeo_error_geometry"
    )
  }
  source_base <- as.integer(component$source_index_base %||% 0L)
  if (!source_base %in% c(0L, 1L)) {
    .ngeo_abort(
      "`source_index_base` must be 0 or 1.",
      "ngeo_error_index"
    )
  }

  list(
    kind = "volume",
    voxel_index = voxel_index,
    affine = affine,
    source_index_base = source_base,
    n_element = nrow(voxel_index)
  )
}

.ngeo_gray_component <- function(component, global_start) {
  if (!is.list(component)) {
    .ngeo_abort("Every grayordinate component must be a list.", "ngeo_error_base")
  }
  component_id <- .ngeo_component_scalar(component, "component_id")
  kind <- match.arg(component$kind, c("surface", "volume"))
  structure_name <- .ngeo_component_scalar(component, "structure")

  normalized <- if (identical(kind, "surface")) {
    .ngeo_gray_surface_component(component, component_id)
  } else {
    .ngeo_gray_volume_component(component, component_id)
  }
  normalized$component_id <- component_id
  normalized$structure <- structure_name
  normalized$global_rows <- seq.int(
    global_start,
    length.out = normalized$n_element
  )
  normalized
}

#' Construct a grayordinate dataset
#'
#' @param components Ordered surface and volume component definitions.
#' @param values Optional grayordinate-aligned values.
#' @param layers Optional layer metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param coordinate_space A hybrid `ngeo_coordinate_space`.
#'
#' @return An `ngeo_grayordinate` object.
#' @examples
#' gray <- ngeo_grayordinate(
#'   components = list(
#'     list(
#'       component_id = "left",
#'       kind = "surface",
#'       structure = "CORTEX_LEFT",
#'       vertex_index = c(0L, 2L),
#'       surface_vertex_count = 4L,
#'       source_index_base = 0L
#'     )
#'   ),
#'   values = cbind(statistic = c(1.2, 0.7))
#' )
#' base_elements(gray)
#' @export
ngeo_grayordinate <- function(components,
                               values = NULL,
                               layers = NULL,
                               measures = NULL,
                               labels = list(),
                               coordinate_space = ngeo_coordinate_space(kind = "hybrid")) {
  if (!is.list(components) || !length(components)) {
    .ngeo_abort("`components` must be a non-empty list.", "ngeo_error_base")
  }
  if (!inherits(coordinate_space, "ngeo_coordinate_space") ||
      !coordinate_space$kind %in% c("hybrid", "unknown")) {
    .ngeo_abort(
      "Grayordinates require a hybrid or unknown coordinate_space.",
      "ngeo_error_coordinate_space"
    )
  }

  normalized <- vector("list", length(components))
  next_row <- 1L
  for (i in seq_along(components)) {
    normalized[[i]] <- .ngeo_gray_component(components[[i]], next_row)
    next_row <- next_row + normalized[[i]]$n_element
  }
  ids <- vapply(normalized, `[[`, character(1), "component_id")
  if (anyDuplicated(ids)) {
    .ngeo_abort("Grayordinate component IDs must be unique.", "ngeo_error_base")
  }
  names(normalized) <- ids

  element_parts <- lapply(normalized, function(component) {
    n <- component$n_element
    source_index <- if (identical(component$kind, "surface")) {
      component$vertex_index
    } else {
      seq_len(n) - 1L + component$source_index_base
    }
    data.frame(
      element_id = paste0(
        component$component_id, ":",
        seq_len(n) - 1L
      ),
      source_index = source_index,
      source_index_base = rep.int(component$source_index_base, n),
      structure = rep.int(component$structure, n),
      included = rep.int(TRUE, n),
      component_id = rep.int(component$component_id, n),
      component_index = seq_len(n),
      stringsAsFactors = FALSE
    )
  })
  elements <- do.call(rbind, element_parts)
  rownames(elements) <- NULL

  base <- base::structure(
    list(
      type = "grayordinate",
      elements = elements,
      geometry = list(components = normalized),
      coordinate_space = coordinate_space,
      topology = NULL
    ),
    class = c("ngeo_grayordinate_base", "ngeo_base")
  )

  .new_ngeo(
    base = base,
    values = values,
    layers = layers,
    measures = measures,
    labels = labels,
    history = list(
      operations = list(.ngeo_operation(
        "ngeo_grayordinate",
        list(components = ids)
      ))
    ),
    class = "ngeo_grayordinate"
  )
}

#' Construct a parcellation dataset
#'
#' @param parcellation Region metadata containing a unique `region_id`.
#' @param values Optional region-aligned values.
#' @param membership Optional base-element membership vector or sparse matrix.
#' @param source_base Optional base `ngeo` object or its base hash.
#' @param centroid Optional region centroid matrix.
#' @param support_size Optional region support sizes.
#' @param adjacency Optional region adjacency matrix.
#' @param layers Optional layer metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param coordinate_space An `ngeo_coordinate_space`.
#'
#' @return An `ngeo_parcellation` object.
#' @examples
#' parcellation <- ngeo_parcellation(
#'   data.frame(region_id = c("A", "B"), name = c("anterior", "posterior")),
#'   values = cbind(mean_signal = c(1.4, 2.1)),
#'   centroid = matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
#'   support_size = c(10, 15)
#' )
#' base_elements(parcellation)
#' ngeo_support_size(parcellation)
#' @export
ngeo_parcellation <- function(parcellation,
                         values = NULL,
                         membership = NULL,
                         source_base = NULL,
                         centroid = NULL,
                         support_size = NULL,
                         adjacency = NULL,
                         layers = NULL,
                         measures = NULL,
                         labels = list(),
                         coordinate_space = ngeo_coordinate_space()) {
  if (!is.data.frame(parcellation) || !"region_id" %in% names(parcellation) ||
      !nrow(parcellation) || anyNA(parcellation$region_id) ||
      anyDuplicated(parcellation$region_id)) {
    .ngeo_abort(
      "`parcellation` must contain non-missing unique `region_id` values.",
      "ngeo_error_base"
    )
  }
  if (!inherits(coordinate_space, "ngeo_coordinate_space")) {
    .ngeo_abort("`coordinate_space` must be an `ngeo_coordinate_space` object.", "ngeo_error_coordinate_space")
  }
  n <- nrow(parcellation)

  if (!is.null(centroid) &&
      (!is.matrix(centroid) || !is.numeric(centroid) ||
        nrow(centroid) != n || !ncol(centroid) %in% c(2L, 3L) ||
        anyNA(centroid) || any(!is.finite(centroid)))) {
    .ngeo_abort(
      "`centroid` must be a finite n_region by 2/3 matrix.",
      "ngeo_error_geometry"
    )
  }
  if (is.null(support_size)) {
    support_size <- rep.int(NA_real_, n)
  }
  if (!is.numeric(support_size) || length(support_size) != n ||
      any(support_size < 0, na.rm = TRUE)) {
    .ngeo_abort(
      "`support_size` must be a non-negative vector aligned with parcellation.",
      "ngeo_error_alignment"
    )
  }
  if (!is.null(adjacency)) {
    is_matrix <- is.matrix(adjacency) || inherits(adjacency, "Matrix")
    if (!is_matrix || !identical(dim(adjacency), c(n, n))) {
      .ngeo_abort(
        "`adjacency` must be an n_region by n_region matrix.",
        "ngeo_error_alignment"
      )
    }
  }
  if (!is.null(membership) &&
      !(is.atomic(membership) || is.matrix(membership) ||
        inherits(membership, "Matrix"))) {
    .ngeo_abort(
      "`membership` must be a vector or matrix.",
      "ngeo_error_base"
    )
  }

  source_base_hash <- if (inherits(source_base, "ngeo") ||
      inherits(source_base, "ngeo_base")) {
    base_hash(source_base)
  } else {
    source_base
  }
  if (!is.null(source_base_hash)) {
    .ngeo_assert_scalar_character(source_base_hash, "source_base")
  }

  elements <- .ngeo_element_table(n, 1L)
  elements$element_id <- paste0("region:", as.character(parcellation$region_id))
  elements$region_id <- parcellation$region_id
  for (name in setdiff(names(parcellation), "region_id")) {
    elements[[name]] <- parcellation[[name]]
  }

  base <- base::structure(
    list(
      type = "parcellation",
      elements = elements,
      geometry = list(
        source_base_hash = source_base_hash,
        membership = membership,
        centroid = centroid,
        support_size = as.numeric(support_size)
      ),
      coordinate_space = coordinate_space,
      topology = if (is.null(adjacency)) NULL else list(adjacency = adjacency)
    ),
    class = c("ngeo_parcellation_base", "ngeo_base")
  )

  .new_ngeo(
    base = base,
    values = values,
    layers = layers,
    measures = measures,
    labels = labels,
    history = list(
      operations = list(.ngeo_operation(
        "ngeo_parcellation",
        list(source_base_hash = source_base_hash)
      ))
    ),
    class = "ngeo_parcellation"
  )
}
