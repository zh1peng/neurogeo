#' Define an explicit distance metric
#'
#' @param name Metric name.
#' @param ... Metric parameters.
#' @return An `ngeo_metric` object.
#' @export
ngeo_metric <- function(name = c(
                          "euclidean", "world_euclidean",
                          "edge_geodesic", "hops", "region_centroid"
                        ),
                        ...) {
  name <- match.arg(name)
  base::structure(
    list(name = name, parameters = list(...)),
    class = "ngeo_metric"
  )
}

.ngeo_metric_name <- function(metric) {
  if (inherits(metric, "ngeo_metric")) {
    metric$name
  } else {
    match.arg(
      metric,
      c(
        "euclidean", "world_euclidean",
        "edge_geodesic", "hops", "region_centroid"
      )
    )
  }
}

.ngeo_world_coordinates <- function(index, affine) {
  homogeneous <- cbind(index, 1)
  world <- t(affine %*% t(homogeneous))
  world[, 1:3, drop = FALSE]
}

.ngeo_element_coordinates <- function(x) {
  type <- x$domain$type
  coordinates <- switch(
    type,
    surface = x$domain$coordinates[[x$domain$active_coordinates]],
    points = x$domain$coordinates,
    volume = .ngeo_world_coordinates(
      x$domain$source_voxel_index,
      x$domain$affine
    ),
    regions = x$domain$centroid,
    grayordinates = {
      result <- matrix(
        NA_real_,
        nrow = nrow(x$domain$elements),
        ncol = 3L
      )
      for (component in x$domain$components) {
        if (identical(component$kind, "surface")) {
          if (!inherits(component$geometry, "ngeo_surface")) {
            next
          }
          full <- component$geometry$domain$coordinates[[
            component$geometry$domain$active_coordinates
          ]]
          if (ncol(full) == 2L) {
            full <- cbind(full, 0)
          }
          result[component$global_rows, ] <-
            full[component$internal_vertex_index, , drop = FALSE]
        } else {
          result[component$global_rows, ] <- .ngeo_world_coordinates(
            component$voxel_index,
            component$affine
          )
        }
      }
      result
    }
  )
  if (is.null(coordinates)) {
    .ngeo_abort(
      sprintf("Domain `%s` has no coordinates for this metric.", type),
      "ngeo_error_capability"
    )
  }
  if (ncol(coordinates) == 2L) {
    coordinates <- cbind(coordinates, 0)
  }
  coordinates
}

.ngeo_edge_weight_matrix <- function(x, adjacency) {
  coordinates <- .ngeo_element_coordinates(x)
  if (anyNA(coordinates)) {
    .ngeo_abort(
      "Metric coordinates are incomplete.",
      "ngeo_error_capability"
    )
  }
  entries <- Matrix::summary(adjacency)
  keep <- entries$i < entries$j
  entries <- entries[keep, , drop = FALSE]
  if (!nrow(entries)) {
    return(.ngeo_sparse_edges(nrow(adjacency), NULL))
  }
  difference <- coordinates[entries$i, , drop = FALSE] -
    coordinates[entries$j, , drop = FALSE]
  weights <- sqrt(rowSums(difference^2))
  .ngeo_sparse_edges(
    nrow(adjacency),
    cbind(entries$i, entries$j),
    weights
  )
}

.ngeo_dijkstra_one <- function(adjacency,
                               edge_weights,
                               source,
                               targets,
                               max_distance) {
  n <- nrow(adjacency)
  entries <- Matrix::summary(edge_weights)
  neighbors <- split(entries$j, factor(entries$i, levels = seq_len(n)))
  weights <- split(entries$x, factor(entries$i, levels = seq_len(n)))

  distance <- rep.int(Inf, n)
  visited <- rep.int(FALSE, n)
  distance[[source]] <- 0
  heap_vertex <- source
  heap_distance <- 0
  target_pending <- rep.int(FALSE, n)
  target_pending[targets] <- TRUE
  pending_count <- sum(target_pending)

  heap_push <- function(vertex, value) {
    position <- length(heap_vertex) + 1L
    heap_vertex[[position]] <<- vertex
    heap_distance[[position]] <<- value
    while (position > 1L) {
      parent <- position %/% 2L
      if (heap_distance[[parent]] <= value) {
        break
      }
      heap_vertex[[position]] <<- heap_vertex[[parent]]
      heap_distance[[position]] <<- heap_distance[[parent]]
      position <- parent
    }
    heap_vertex[[position]] <<- vertex
    heap_distance[[position]] <<- value
  }

  heap_pop <- function() {
    vertex <- heap_vertex[[1L]]
    value <- heap_distance[[1L]]
    last_vertex <- heap_vertex[[length(heap_vertex)]]
    last_distance <- heap_distance[[length(heap_distance)]]
    length(heap_vertex) <<- length(heap_vertex) - 1L
    length(heap_distance) <<- length(heap_distance) - 1L
    if (length(heap_vertex)) {
      position <- 1L
      while (TRUE) {
        left <- position * 2L
        if (left > length(heap_vertex)) {
          break
        }
        right <- left + 1L
        child <- if (right <= length(heap_vertex) &&
            heap_distance[[right]] < heap_distance[[left]]) {
          right
        } else {
          left
        }
        if (heap_distance[[child]] >= last_distance) {
          break
        }
        heap_vertex[[position]] <<- heap_vertex[[child]]
        heap_distance[[position]] <<- heap_distance[[child]]
        position <- child
      }
      heap_vertex[[position]] <<- last_vertex
      heap_distance[[position]] <<- last_distance
    }
    c(vertex = vertex, distance = value)
  }

  while (length(heap_vertex) && pending_count > 0L) {
    current <- heap_pop()
    vertex <- as.integer(current[["vertex"]])
    current_distance <- current[["distance"]]
    if (visited[[vertex]] || current_distance > distance[[vertex]]) {
      next
    }
    if (current_distance > max_distance) {
      break
    }
    visited[[vertex]] <- TRUE
    if (target_pending[[vertex]]) {
      target_pending[[vertex]] <- FALSE
      pending_count <- pending_count - 1L
    }

    vertex_neighbors <- neighbors[[vertex]]
    vertex_weights <- weights[[vertex]]
    for (i in seq_along(vertex_neighbors)) {
      neighbor <- vertex_neighbors[[i]]
      candidate <- current_distance + vertex_weights[[i]]
      if (!visited[[neighbor]] &&
          candidate < distance[[neighbor]] &&
          candidate <= max_distance) {
        distance[[neighbor]] <- candidate
        heap_push(neighbor, candidate)
      }
    }
  }
  distance[targets]
}

.ngeo_distance_selection <- function(x, selection, default_all = FALSE) {
  if (is.null(selection) && isTRUE(default_all)) {
    return(seq_len(nrow(x$domain$elements)))
  }
  if (is.null(selection)) {
    .ngeo_abort(
      "`from` must identify at least one source element.",
      "ngeo_error_argument"
    )
  }
  .ngeo_element_selection(x, selection)
}

#' Compute explicit source-target distances
#'
#' @param x An `ngeo` object.
#' @param from Source positions or stable element IDs.
#' @param to Target positions or IDs. `NULL` means all elements.
#' @param metric A metric name or `ngeo_metric`; `NULL` selects the
#'   domain-specific default.
#' @param max_distance Distances beyond this limit remain infinite.
#' @param connectivity Voxel connectivity for graph metrics.
#'
#' @return A source by target numeric matrix.
#' @export
ngeo_distance <- function(x,
                          from,
                          to = NULL,
                          metric = NULL,
                          max_distance = Inf,
                          connectivity = 6L) {
  ngeo_validate(x, "basic")
  metric <- metric %||% switch(
    x$domain$type,
    surface = "edge_geodesic",
    volume = "world_euclidean",
    points = "euclidean",
    regions = "region_centroid",
    grayordinates = "edge_geodesic"
  )
  metric <- .ngeo_metric_name(metric)
  from <- .ngeo_distance_selection(x, from)
  to <- .ngeo_distance_selection(x, to, default_all = TRUE)
  pair_count <- length(from) * length(to)
  maximum <- getOption("neurogeo.max_distance_pairs", 1e6)
  if (pair_count > maximum) {
    .ngeo_abort(
      sprintf(
        "Requested %s distances exceeds the configured limit of %s.",
        format(pair_count, big.mark = ","),
        format(maximum, big.mark = ",")
      ),
      "ngeo_error_dense_distance"
    )
  }
  if (!is.numeric(max_distance) || length(max_distance) != 1L ||
      is.na(max_distance) || max_distance < 0) {
    .ngeo_abort(
      "`max_distance` must be one non-negative number.",
      "ngeo_error_argument"
    )
  }

  result <- if (metric %in% c(
    "euclidean", "world_euclidean", "region_centroid"
  )) {
    coordinates <- .ngeo_element_coordinates(x)
    if (anyNA(coordinates[c(from, to), , drop = FALSE])) {
      .ngeo_abort(
        "Requested elements lack metric coordinates.",
        "ngeo_error_capability"
      )
    }
    output <- matrix(Inf, nrow = length(from), ncol = length(to))
    for (i in seq_along(from)) {
      difference <- sweep(
        coordinates[to, , drop = FALSE],
        2L,
        coordinates[from[[i]], ],
        "-"
      )
      output[i, ] <- sqrt(rowSums(difference^2))
    }
    output[output > max_distance] <- Inf
    output
  } else {
    adjacency <- ngeo_adjacency(x, connectivity = connectivity)
    weights <- if (identical(metric, "hops")) {
      adjacency
    } else {
      .ngeo_edge_weight_matrix(x, adjacency)
    }
    output <- matrix(Inf, nrow = length(from), ncol = length(to))
    for (i in seq_along(from)) {
      output[i, ] <- .ngeo_dijkstra_one(
        adjacency,
        weights,
        from[[i]],
        to,
        max_distance
      )
    }
    output
  }

  rownames(result) <- x$domain$elements$element_id[from]
  colnames(result) <- x$domain$elements$element_id[to]
  result
}
