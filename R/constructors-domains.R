#' Construct a points dataset
#'
#' @param coordinates A finite numeric matrix with two or three columns.
#' @param values Optional point-aligned values.
#' @param maps Optional map metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param space An `ngeo_space`.
#' @param structure Optional structure label per point.
#' @param uncertainty Optional non-negative uncertainty per point.
#' @param index_base Source index base.
#'
#' @return An `ngeo_points` object.
#' @export
ngeo_points <- function(coordinates,
                        values = NULL,
                        maps = NULL,
                        measures = NULL,
                        labels = list(),
                        space = ngeo_space(),
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
  if (!inherits(space, "ngeo_space")) {
    .ngeo_abort("`space` must be an `ngeo_space` object.", "ngeo_error_space")
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

  domain <- base::structure(
    list(
      type = "points",
      elements = elements,
      coordinates = coordinates,
      space = space,
      uncertainty = uncertainty
    ),
    class = c("ngeo_points_domain", "ngeo_domain")
  )

  .new_ngeo(
    domain = domain,
    values = values,
    maps = maps,
    measures = measures,
    labels = labels,
    provenance = list(
      operations = list(.ngeo_operation(
        "ngeo_points",
        list(source_index_base = source_base)
      ))
    ),
    class = "ngeo_points"
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
      "ngeo_error_domain"
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
      "ngeo_error_domain"
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
    if (nrow(geometry$domain$elements) != vertex_count) {
      .ngeo_abort(
        sprintf(
          "Geometry for `%s` has %d vertices; expected %d.",
          component_id,
          nrow(geometry$domain$elements),
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
    .ngeo_abort("Every grayordinate component must be a list.", "ngeo_error_domain")
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

#' Construct a grayordinates dataset
#'
#' @param components Ordered surface and volume component definitions.
#' @param values Optional grayordinate-aligned values.
#' @param maps Optional map metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param space A hybrid `ngeo_space`.
#'
#' @return An `ngeo_grayordinates` object.
#' @export
ngeo_grayordinates <- function(components,
                               values = NULL,
                               maps = NULL,
                               measures = NULL,
                               labels = list(),
                               space = ngeo_space(kind = "hybrid")) {
  if (!is.list(components) || !length(components)) {
    .ngeo_abort("`components` must be a non-empty list.", "ngeo_error_domain")
  }
  if (!inherits(space, "ngeo_space") ||
      !space$kind %in% c("hybrid", "unknown")) {
    .ngeo_abort(
      "Grayordinates require a hybrid or unknown space.",
      "ngeo_error_space"
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
    .ngeo_abort("Grayordinate component IDs must be unique.", "ngeo_error_domain")
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

  domain <- base::structure(
    list(
      type = "grayordinates",
      elements = elements,
      components = normalized,
      space = space
    ),
    class = c("ngeo_grayordinates_domain", "ngeo_domain")
  )

  .new_ngeo(
    domain = domain,
    values = values,
    maps = maps,
    measures = measures,
    labels = labels,
    provenance = list(
      operations = list(.ngeo_operation(
        "ngeo_grayordinates",
        list(components = ids)
      ))
    ),
    class = "ngeo_grayordinates"
  )
}

#' Construct a regions dataset
#'
#' @param regions Region metadata containing a unique `region_id`.
#' @param values Optional region-aligned values.
#' @param membership Optional base-element membership vector or sparse matrix.
#' @param base_domain Optional base `ngeo` object or its domain hash.
#' @param centroid Optional region centroid matrix.
#' @param support_size Optional region support sizes.
#' @param adjacency Optional region adjacency matrix.
#' @param maps Optional map metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param space An `ngeo_space`.
#'
#' @return An `ngeo_regions` object.
#' @export
ngeo_regions <- function(regions,
                         values = NULL,
                         membership = NULL,
                         base_domain = NULL,
                         centroid = NULL,
                         support_size = NULL,
                         adjacency = NULL,
                         maps = NULL,
                         measures = NULL,
                         labels = list(),
                         space = ngeo_space()) {
  if (!is.data.frame(regions) || !"region_id" %in% names(regions) ||
      !nrow(regions) || anyNA(regions$region_id) ||
      anyDuplicated(regions$region_id)) {
    .ngeo_abort(
      "`regions` must contain non-missing unique `region_id` values.",
      "ngeo_error_domain"
    )
  }
  if (!inherits(space, "ngeo_space")) {
    .ngeo_abort("`space` must be an `ngeo_space` object.", "ngeo_error_space")
  }
  n <- nrow(regions)

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
      "`support_size` must be a non-negative vector aligned with regions.",
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
      "ngeo_error_domain"
    )
  }

  base_domain_hash <- if (inherits(base_domain, "ngeo") ||
      inherits(base_domain, "ngeo_domain")) {
    ngeo_domain_hash(base_domain)
  } else {
    base_domain
  }
  if (!is.null(base_domain_hash)) {
    .ngeo_assert_scalar_character(base_domain_hash, "base_domain")
  }

  elements <- .ngeo_element_table(n, 1L)
  elements$element_id <- paste0("region:", as.character(regions$region_id))
  elements$region_id <- regions$region_id
  for (name in setdiff(names(regions), "region_id")) {
    elements[[name]] <- regions[[name]]
  }

  domain <- base::structure(
    list(
      type = "regions",
      elements = elements,
      base_domain_hash = base_domain_hash,
      membership = membership,
      centroid = centroid,
      support_size = as.numeric(support_size),
      adjacency = adjacency,
      space = space
    ),
    class = c("ngeo_regions_domain", "ngeo_domain")
  )

  .new_ngeo(
    domain = domain,
    values = values,
    maps = maps,
    measures = measures,
    labels = labels,
    provenance = list(
      operations = list(.ngeo_operation(
        "ngeo_regions",
        list(base_domain_hash = base_domain_hash)
      ))
    ),
    class = "ngeo_regions"
  )
}

