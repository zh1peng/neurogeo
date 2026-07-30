.ngeo_element_selection <- function(x, elements) {
  n <- nrow(x$domain$elements)
  if (is.null(elements)) {
    return(seq_len(n))
  }
  if (is.character(elements)) {
    index <- match(elements, x$domain$elements$element_id)
    if (anyNA(index)) {
      missing <- unique(elements[is.na(index)])
      .ngeo_abort(
        sprintf(
          "Unknown element IDs: %s.",
          paste(missing, collapse = ", ")
        ),
        "ngeo_error_index"
      )
    }
  } else if (is.logical(elements)) {
    if (length(elements) != n || anyNA(elements)) {
      .ngeo_abort(
        sprintf("Logical element selection must have length %d.", n),
        "ngeo_error_index"
      )
    }
    index <- which(elements)
  } else {
    index <- .ngeo_as_integer(elements, "elements")
    if (any(index < 1L | index > n)) {
      .ngeo_abort(
        sprintf("Element positions must be between 1 and %d.", n),
        "ngeo_error_index"
      )
    }
  }
  if (!length(index)) {
    .ngeo_abort(
      "An `ngeo` domain cannot be subset to zero elements.",
      "ngeo_error_index"
    )
  }
  if (anyDuplicated(index)) {
    .ngeo_abort(
      "Element selection must not contain duplicates.",
      "ngeo_error_index"
    )
  }
  as.integer(index)
}

.ngeo_map_selection <- function(x, maps) {
  n <- nrow(x$maps)
  if (is.null(maps)) {
    return(seq_len(n))
  }
  if (n == 0L) {
    .ngeo_abort(
      "Cannot select maps from a geometry-only object.",
      "ngeo_error_index"
    )
  }
  if (is.character(maps)) {
    id_match <- match(maps, x$maps$map_id)
    name_match <- match(maps, x$maps$name)
    index <- ifelse(!is.na(id_match), id_match, name_match)
    if (anyNA(index)) {
      .ngeo_abort(
        sprintf(
          "Unknown maps: %s.",
          paste(unique(maps[is.na(index)]), collapse = ", ")
        ),
        "ngeo_error_index"
      )
    }
  } else if (is.logical(maps)) {
    if (length(maps) != n || anyNA(maps)) {
      .ngeo_abort(
        sprintf("Logical map selection must have length %d.", n),
        "ngeo_error_index"
      )
    }
    index <- which(maps)
  } else {
    index <- .ngeo_as_integer(maps, "maps")
    if (any(index < 1L | index > n)) {
      .ngeo_abort(
        sprintf("Map positions must be between 1 and %d.", n),
        "ngeo_error_index"
      )
    }
  }
  if (anyDuplicated(index)) {
    .ngeo_abort(
      "Map selection must not contain duplicates.",
      "ngeo_error_index"
    )
  }
  as.integer(index)
}

.ngeo_subset_surface_domain <- function(domain, index) {
  old_n <- nrow(domain$elements)
  old_to_new <- integer(old_n)
  old_to_new[index] <- seq_along(index)
  faces <- domain$faces
  keep <- if (nrow(faces)) {
    rowSums(matrix(old_to_new[faces] > 0L, ncol = 3L)) == 3L
  } else {
    logical()
  }
  new_faces <- if (any(keep)) {
    matrix(old_to_new[faces[keep, , drop = FALSE]], ncol = 3L)
  } else {
    matrix(integer(), nrow = 0L, ncol = 3L)
  }

  domain$elements <- domain$elements[index, , drop = FALSE]
  rownames(domain$elements) <- NULL
  domain$coordinates <- lapply(
    domain$coordinates,
    function(value) value[index, , drop = FALSE]
  )
  domain$faces <- new_faces
  domain$mask <- domain$mask[index]
  domain
}

.ngeo_subset_volume_domain <- function(domain, index) {
  domain$elements <- domain$elements[index, , drop = FALSE]
  rownames(domain$elements) <- NULL
  domain$voxel_index <- domain$voxel_index[index, , drop = FALSE]
  domain$source_voxel_index <-
    domain$source_voxel_index[index, , drop = FALSE]

  linear <- 1L +
    (domain$voxel_index[, 1L] - 1L) +
    domain$dim[1L] * (domain$voxel_index[, 2L] - 1L) +
    domain$dim[1L] * domain$dim[2L] *
      (domain$voxel_index[, 3L] - 1L)
  domain$mask <- rep.int(FALSE, prod(domain$dim))
  domain$mask[linear] <- TRUE
  domain
}

.ngeo_subset_points_domain <- function(domain, index) {
  domain$elements <- domain$elements[index, , drop = FALSE]
  rownames(domain$elements) <- NULL
  domain$coordinates <- domain$coordinates[index, , drop = FALSE]
  if (!is.null(domain$uncertainty)) {
    domain$uncertainty <- domain$uncertainty[index]
  }
  domain
}

.ngeo_subset_grayordinates_domain <- function(domain, index) {
  components <- lapply(domain$components, function(component) {
    new_rows <- which(index %in% component$global_rows)
    if (!length(new_rows)) {
      return(NULL)
    }
    old_local <- match(index[new_rows], component$global_rows)
    if (identical(component$kind, "surface")) {
      component$vertex_index <- component$vertex_index[old_local]
      component$internal_vertex_index <-
        component$internal_vertex_index[old_local]
    } else {
      component$voxel_index <-
        component$voxel_index[old_local, , drop = FALSE]
    }
    component$global_rows <- new_rows
    component$n_element <- length(new_rows)
    component
  })
  components <- Filter(Negate(is.null), components)
  names(components) <- vapply(
    components,
    `[[`,
    character(1),
    "component_id"
  )

  domain$elements <- domain$elements[index, , drop = FALSE]
  rownames(domain$elements) <- NULL
  domain$elements$component_index <- ave(
    seq_len(nrow(domain$elements)),
    domain$elements$component_id,
    FUN = seq_along
  )
  domain$components <- components
  domain
}

.ngeo_subset_regions_domain <- function(domain, index) {
  old_elements <- domain$elements
  domain$elements <- old_elements[index, , drop = FALSE]
  rownames(domain$elements) <- NULL
  if (!is.null(domain$centroid)) {
    domain$centroid <- domain$centroid[index, , drop = FALSE]
  }
  domain$support_size <- domain$support_size[index]
  if (!is.null(domain$adjacency)) {
    domain$adjacency <- domain$adjacency[index, index, drop = FALSE]
  }
  if (!is.null(domain$membership)) {
    if (is.matrix(domain$membership) ||
        inherits(domain$membership, "Matrix")) {
      if (ncol(domain$membership) == nrow(old_elements)) {
        domain$membership <-
          domain$membership[, index, drop = FALSE]
      }
    } else {
      selected_ids <- as.character(old_elements$region_id[index])
      member <- as.character(domain$membership)
      member[!is.na(member) & !member %in% selected_ids] <- NA_character_
      domain$membership <- member
    }
  }
  domain
}

.ngeo_subset_domain <- function(domain, index) {
  switch(
    domain$type,
    surface = .ngeo_subset_surface_domain(domain, index),
    volume = .ngeo_subset_volume_domain(domain, index),
    points = .ngeo_subset_points_domain(domain, index),
    grayordinates = .ngeo_subset_grayordinates_domain(domain, index),
    regions = .ngeo_subset_regions_domain(domain, index),
    .ngeo_abort(
      sprintf("Unsupported domain type `%s`.", domain$type),
      "ngeo_error_domain"
    )
  )
}

#' Subset or reorder an NGCS dataset
#'
#' Domain elements and value rows are always changed together. Surface faces,
#' volume masks, grayordinate component mappings, and region metadata are
#' rebuilt as required.
#'
#' @param x An `ngeo` object.
#' @param elements Element positions, stable element IDs, or a logical mask.
#' @param maps Map positions, map IDs/names, or a logical mask.
#'
#' @return An object of the same `ngeo` subclass.
#' @examples
#' points <- ngeo_points(
#'   matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(first = 1:4, second = 5:8)
#' )
#' subset <- ngeo_subset(points, elements = c(1, 3), maps = "second")
#' ngeo_elements(subset)
#' ngeo_values(subset)
#' @export
ngeo_subset <- function(x, elements = NULL, maps = NULL) {
  ngeo_validate(x, "basic")
  element_index <- .ngeo_element_selection(x, elements)
  map_index <- .ngeo_map_selection(x, maps)
  old_hash <- ngeo_domain_hash(x)

  domain <- .ngeo_subset_domain(x$domain, element_index)
  values <- if (is.null(x$values)) {
    NULL
  } else {
    x$values[element_index, map_index, drop = FALSE]
  }
  map_metadata <- x$maps[map_index, , drop = FALSE]
  measure_metadata <- x$measures[map_index, , drop = FALSE]
  rownames(map_metadata) <- NULL
  rownames(measure_metadata) <- NULL

  result <- base::structure(
    list(
      domain = domain,
      values = values,
      maps = map_metadata,
      measures = measure_metadata,
      labels = x$labels,
      provenance = x$provenance
    ),
    class = class(x)
  )
  result$provenance$operations <- c(
    result$provenance$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_subset",
      list(
        source_domain_hash = old_hash,
        result_domain_hash = ngeo_domain_hash(result),
        source_elements = nrow(x$domain$elements),
        result_elements = length(element_index),
        source_maps = nrow(x$maps),
        result_maps = length(map_index)
      )
    ))
  )
  ngeo_validate(result, "basic")
  result
}
