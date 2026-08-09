.ngeo_element_selection <- function(x, elements) {
  n <- nrow(x$base$elements)
  if (is.null(elements)) {
    return(seq_len(n))
  }
  if (is.character(elements)) {
    index <- match(elements, x$base$elements$element_id)
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
      "An `ngeo` base cannot be subset to zero elements.",
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

.ngeo_layer_selection <- function(x, layers) {
  n <- nrow(x$layers)
  if (is.null(layers)) {
    return(seq_len(n))
  }
  if (n == 0L) {
    .ngeo_abort(
      "Cannot select layers from a geometry-only object.",
      "ngeo_error_index"
    )
  }
  if (is.character(layers)) {
    id_match <- match(layers, x$layers$layer_id)
    name_match <- match(layers, x$layers$name)
    index <- ifelse(!is.na(id_match), id_match, name_match)
    if (anyNA(index)) {
      .ngeo_abort(
        sprintf(
          "Unknown layers: %s.",
          paste(unique(layers[is.na(index)]), collapse = ", ")
        ),
        "ngeo_error_index"
      )
    }
  } else if (is.logical(layers)) {
    if (length(layers) != n || anyNA(layers)) {
      .ngeo_abort(
        sprintf("Logical layer selection must have length %d.", n),
        "ngeo_error_index"
      )
    }
    index <- which(layers)
  } else {
    index <- .ngeo_as_integer(layers, "layers")
    if (any(index < 1L | index > n)) {
      .ngeo_abort(
        sprintf("Layer positions must be between 1 and %d.", n),
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

.ngeo_subset_surface_base <- function(base, index) {
  old_n <- nrow(base$elements)
  old_to_new <- integer(old_n)
  old_to_new[index] <- seq_along(index)
  faces <- base$geometry$faces
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

  base$elements <- base$elements[index, , drop = FALSE]
  rownames(base$elements) <- NULL
  base$geometry$coordinates <- lapply(
    base$geometry$coordinates,
    function(value) value[index, , drop = FALSE]
  )
  base$geometry$faces <- new_faces
  base$geometry$mask <- base$geometry$mask[index]
  base
}

.ngeo_subset_volume_base <- function(base, index) {
  base$elements <- base$elements[index, , drop = FALSE]
  rownames(base$elements) <- NULL
  base$geometry$voxel_index <- base$geometry$voxel_index[index, , drop = FALSE]
  base$geometry$source_voxel_index <-
    base$geometry$source_voxel_index[index, , drop = FALSE]

  linear <- 1L +
    (base$geometry$voxel_index[, 1L] - 1L) +
    base$geometry$dim[1L] * (base$geometry$voxel_index[, 2L] - 1L) +
    base$geometry$dim[1L] * base$geometry$dim[2L] *
      (base$geometry$voxel_index[, 3L] - 1L)
  base$geometry$mask <- rep.int(FALSE, prod(base$geometry$dim))
  base$geometry$mask[linear] <- TRUE
  base
}

.ngeo_subset_point_base <- function(base, index) {
  base$elements <- base$elements[index, , drop = FALSE]
  rownames(base$elements) <- NULL
  base$geometry$coordinates <- base$geometry$coordinates[index, , drop = FALSE]
  if (!is.null(base$geometry$uncertainty)) {
    base$geometry$uncertainty <- base$geometry$uncertainty[index]
  }
  base
}

.ngeo_subset_grayordinate_base <- function(base, index) {
  components <- lapply(base$geometry$components, function(component) {
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

  base$elements <- base$elements[index, , drop = FALSE]
  rownames(base$elements) <- NULL
  base$elements$component_index <- ave(
    seq_len(nrow(base$elements)),
    base$elements$component_id,
    FUN = seq_along
  )
  base$geometry$components <- components
  base
}

.ngeo_subset_parcellation_base <- function(base, index) {
  old_elements <- base$elements
  base$elements <- old_elements[index, , drop = FALSE]
  rownames(base$elements) <- NULL
  if (!is.null(base$geometry$centroid)) {
    base$geometry$centroid <- base$geometry$centroid[index, , drop = FALSE]
  }
  base$geometry$support_size <- base$geometry$support_size[index]
  if (!is.null(base$topology$adjacency)) {
    base$topology$adjacency <- base$topology$adjacency[index, index, drop = FALSE]
  }
  if (!is.null(base$geometry$membership)) {
    if (is.matrix(base$geometry$membership) ||
        inherits(base$geometry$membership, "Matrix")) {
      if (ncol(base$geometry$membership) == nrow(old_elements)) {
        base$geometry$membership <-
          base$geometry$membership[, index, drop = FALSE]
      }
    } else {
      selected_ids <- as.character(old_elements$region_id[index])
      member <- as.character(base$geometry$membership)
      member[!is.na(member) & !member %in% selected_ids] <- NA_character_
      base$geometry$membership <- member
    }
  }
  base
}

.ngeo_subset_base <- function(base, index) {
  switch(
    base$type,
    surface = .ngeo_subset_surface_base(base, index),
    volume = .ngeo_subset_volume_base(base, index),
    point = .ngeo_subset_point_base(base, index),
    grayordinate = .ngeo_subset_grayordinate_base(base, index),
    parcellation = .ngeo_subset_parcellation_base(base, index),
    .ngeo_abort(
      sprintf("Unsupported base type `%s`.", base$type),
      "ngeo_error_base"
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
#' @param layers Layer positions, IDs/names, or a logical mask.
#'
#' @return An object of the same `ngeo` subclass.
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(first = 1:4, second = 5:8)
#' )
#' subset <- ngeo_subset(point, elements = c(1, 3), layers = "second")
#' base_elements(subset)
#' values(subset)
#' @export
ngeo_subset <- function(x, elements = NULL, layers = NULL) {
  ngeo_validate(x, "basic")
  element_index <- .ngeo_element_selection(x, elements)
  layer_index <- .ngeo_layer_selection(x, layers)
  old_hash <- base_hash(x)

  base <- .ngeo_subset_base(x$base, element_index)
  values <- if (is.null(x$values)) {
    NULL
  } else {
    x$values[element_index, layer_index, drop = FALSE]
  }
  layer_metadata <- x$layers[layer_index, , drop = FALSE]
  measure_ids <- unique(layer_metadata$measure_id)
  measure_metadata <- x$measures[
    match(measure_ids, x$measures$measure_id),
    ,
    drop = FALSE
  ]
  rownames(layer_metadata) <- NULL
  rownames(measure_metadata) <- NULL
  base$labels <- .ngeo_subset_labels(
    x$base$labels %||% list(),
    element_index,
    nrow(x$base$elements),
    as.character(layer_metadata$layer_id)
  )

  result <- base::structure(
    list(
      base = base,
      values = values,
      layers = layer_metadata,
      measures = measure_metadata,
      history = x$history
    ),
    class = class(x)
  )
  result$history$operations <- c(
    result$history$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_subset",
      list(
        source_base_hash = old_hash,
        result_base_hash = base_hash(result),
        source_elements = nrow(x$base$elements),
        result_elements = length(element_index),
        source_layers = nrow(x$layers),
        result_layers = length(layer_index)
      )
    ))
  )
  ngeo_validate(result, "basic")
  result
}
