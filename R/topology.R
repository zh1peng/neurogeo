.ngeo_sparse_edges <- function(n, edges, spatial_weights = NULL) {
  if (is.null(edges) || !nrow(edges)) {
    return(Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      dims = c(n, n),
      giveCsparse = TRUE
    ))
  }
  edges <- as.matrix(edges)
  if (is.null(spatial_weights)) {
    spatial_weights <- rep.int(1, nrow(edges))
  }
  Matrix::sparseMatrix(
    i = c(edges[, 1L], edges[, 2L]),
    j = c(edges[, 2L], edges[, 1L]),
    x = c(spatial_weights, spatial_weights),
    dims = c(n, n),
    giveCsparse = TRUE
  )
}

.ngeo_surface_edges <- function(faces) {
  if (!nrow(faces)) {
    return(matrix(integer(), nrow = 0L, ncol = 2L))
  }
  edges <- rbind(
    faces[, c(1L, 2L), drop = FALSE],
    faces[, c(2L, 3L), drop = FALSE],
    faces[, c(3L, 1L), drop = FALSE]
  )
  edges <- t(apply(edges, 1L, sort))
  unique(edges)
}

.ngeo_voxel_offsets <- function(connectivity) {
  connectivity <- as.integer(connectivity)
  if (!connectivity %in% c(6L, 18L, 26L)) {
    .ngeo_abort(
      "`connectivity` must be 6, 18, or 26.",
      "ngeo_error_argument"
    )
  }
  offsets <- as.matrix(expand.grid(
    i = -1:1,
    j = -1:1,
    k = -1:1
  ))
  nonzero <- rowSums(abs(offsets)) > 0L
  manhattan <- rowSums(abs(offsets))
  keep <- nonzero & switch(
    as.character(connectivity),
    `6` = manhattan == 1L,
    `18` = manhattan <= 2L,
    `26` = TRUE
  )
  offsets <- offsets[keep, , drop = FALSE]
  half <- offsets[, 1L] > 0L |
    (offsets[, 1L] == 0L & offsets[, 2L] > 0L) |
    (offsets[, 1L] == 0L & offsets[, 2L] == 0L &
      offsets[, 3L] > 0L)
  offsets[half, , drop = FALSE]
}

.ngeo_voxel_adjacency_index <- function(voxel_index, connectivity) {
  n <- nrow(voxel_index)
  if (!n) {
    return(.ngeo_sparse_edges(0L, NULL))
  }
  keys <- paste(
    voxel_index[, 1L],
    voxel_index[, 2L],
    voxel_index[, 3L],
    sep = ","
  )
  offsets <- .ngeo_voxel_offsets(connectivity)
  edge_parts <- vector("list", nrow(offsets))
  for (i in seq_len(nrow(offsets))) {
    target <- sweep(voxel_index, 2L, offsets[i, ], "+")
    target_keys <- paste(target[, 1L], target[, 2L], target[, 3L], sep = ",")
    match_index <- match(target_keys, keys)
    from <- which(!is.na(match_index))
    edge_parts[[i]] <- cbind(from, match_index[from])
  }
  edges <- do.call(rbind, edge_parts)
  .ngeo_sparse_edges(n, edges)
}

.ngeo_surface_adjacency <- function(x, include_masked) {
  edges <- .ngeo_surface_edges(x$base$geometry$faces)
  if (!isTRUE(include_masked) && nrow(edges)) {
    include <- x$base$geometry$mask
    edges <- edges[
      include[edges[, 1L]] & include[edges[, 2L]],
      ,
      drop = FALSE
    ]
  }
  .ngeo_sparse_edges(nrow(x$base$elements), edges)
}

.ngeo_grayordinate_adjacency <- function(x, connectivity) {
  n <- nrow(x$base$elements)
  i_values <- integer()
  j_values <- integer()
  x_values <- numeric()

  for (component in x$base$geometry$components) {
    adjacency <- if (identical(component$kind, "surface")) {
      if (!inherits(component$geometry, "ngeo_surface")) {
        .ngeo_abort(
          sprintf(
            "Grayordinate component `%s` lacks surface geometry.",
            component$component_id
          ),
          "ngeo_error_capability"
        )
      }
      full <- .ngeo_surface_adjacency(
        component$geometry,
        include_masked = FALSE
      )
      full[
        component$internal_vertex_index,
        component$internal_vertex_index,
        drop = FALSE
      ]
    } else {
      .ngeo_voxel_adjacency_index(
        component$voxel_index,
        connectivity
      )
    }
    entries <- Matrix::summary(adjacency)
    if (nrow(entries)) {
      i_values <- c(i_values, component$global_rows[entries$i])
      j_values <- c(j_values, component$global_rows[entries$j])
      x_values <- c(x_values, entries$x)
    }
  }
  Matrix::sparseMatrix(
    i = i_values,
    j = j_values,
    x = x_values,
    dims = c(n, n),
    giveCsparse = TRUE
  )
}

#' Construct sparse base adjacency
#'
#' @param x An `ngeo` object.
#' @param method Automatic or base-specific topology.
#' @param connectivity Voxel 6, 18, or 26 connectivity.
#' @param include_masked Whether masked surface vertices retain edges.
#'
#' @return A sparse symmetric `Matrix`.
#' @export
ngeo_adjacency <- function(x,
                           method = c(
                             "auto", "mesh", "voxel", "region", "component"
                           ),
                           connectivity = 6L,
                           include_masked = FALSE) {
  ngeo_validate(x, "basic")
  method <- match.arg(method)
  type <- x$base$type
  if (identical(method, "auto")) {
    method <- switch(
      type,
      surface = "mesh",
      volume = "voxel",
      grayordinate = "component",
      parcellation = "region",
      .ngeo_abort(
        sprintf("Domain `%s` has no implicit topology.", type),
        "ngeo_error_capability"
      )
    )
  }

  adjacency <- switch(
    method,
    mesh = {
      if (!identical(type, "surface")) {
        .ngeo_abort("Mesh adjacency requires a surface.", "ngeo_error_capability")
      }
      .ngeo_surface_adjacency(x, include_masked)
    },
    voxel = {
      if (!identical(type, "volume")) {
        .ngeo_abort("Voxel adjacency requires a volume.", "ngeo_error_capability")
      }
      .ngeo_voxel_adjacency_index(x$base$geometry$voxel_index, connectivity)
    },
    component = {
      if (!identical(type, "grayordinate")) {
        .ngeo_abort(
          "Component adjacency requires grayordinate.",
          "ngeo_error_capability"
        )
      }
      .ngeo_grayordinate_adjacency(x, connectivity)
    },
    region = {
      if (!identical(type, "parcellation") || is.null(x$base$topology$adjacency)) {
        .ngeo_abort(
          "Region adjacency is unavailable.",
          "ngeo_error_capability"
        )
      }
      Matrix::Matrix(x$base$topology$adjacency, sparse = TRUE)
    }
  )
  diag(adjacency) <- 0
  .ngeo_as_dgCMatrix(adjacency)
}

#' Find connected components
#'
#' @param x An `ngeo` object or a sparse adjacency matrix.
#' @param adjacency Optional precomputed adjacency.
#'
#' @return An integer component identifier per element.
#' @export
ngeo_components <- function(x, adjacency = NULL) {
  if (inherits(x, "ngeo")) {
    adjacency <- adjacency %||% ngeo_adjacency(x)
    n <- nrow(x$base$elements)
  } else {
    adjacency <- x
    n <- nrow(adjacency)
  }
  adjacency <- Matrix::Matrix(adjacency, sparse = TRUE)
  entries <- Matrix::summary(adjacency)
  neighbors <- split(entries$j, factor(entries$i, levels = seq_len(n)))

  component <- integer(n)
  component_id <- 0L
  for (start in seq_len(n)) {
    if (component[[start]] != 0L) {
      next
    }
    component_id <- component_id + 1L
    queue <- start
    component[[start]] <- component_id
    head_index <- 1L
    while (head_index <= length(queue)) {
      vertex <- queue[[head_index]]
      head_index <- head_index + 1L
      candidate <- neighbors[[vertex]]
      candidate <- candidate[component[candidate] == 0L]
      if (length(candidate)) {
        candidate <- unique(candidate)
        component[candidate] <- component_id
        queue <- c(queue, candidate)
      }
    }
  }
  component
}

#' Return element support sizes
#'
#' @param x An `ngeo` object.
#' @return A numeric vector aligned with base elements.
#' @templateVar example_call ngeo_support_size(parcellation_data)
#' @template stable-neuroimaging-method
#' @export
ngeo_support_size <- function(x) {
  ngeo_validate(x, "basic")
  switch(
    x$base$type,
    surface = ngeo_vertex_area(x),
    volume = rep.int(ngeo_voxel_volume(x), nrow(x$base$elements)),
    point = rep.int(NA_real_, nrow(x$base$elements)),
    parcellation = x$base$geometry$support_size,
    grayordinate = {
      result <- rep.int(NA_real_, nrow(x$base$elements))
      for (component in x$base$geometry$components) {
        if (identical(component$kind, "surface")) {
          if (!inherits(component$geometry, "ngeo_surface")) {
            next
          }
          full_area <- ngeo_vertex_area(component$geometry)
          result[component$global_rows] <-
            full_area[component$internal_vertex_index]
        } else {
          volume <- abs(det(component$affine[1:3, 1:3, drop = FALSE]))
          result[component$global_rows] <- volume
        }
      }
      result
    }
  )
}
