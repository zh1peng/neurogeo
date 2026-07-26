.ngeo_sparse_directed <- function(n, from, to, values = NULL) {
  if (!length(from)) {
    return(.ngeo_sparse_edges(n, NULL))
  }
  values <- values %||% rep.int(1, length(from))
  Matrix::sparseMatrix(
    i = from,
    j = to,
    x = values,
    dims = c(n, n),
    giveCsparse = TRUE
  )
}

.ngeo_coordinate_neighbors <- function(x,
                                       method,
                                       k,
                                       threshold,
                                       symmetry) {
  coordinates <- .ngeo_element_coordinates(x)
  if (anyNA(coordinates)) {
    .ngeo_abort(
      "All elements need coordinates for coordinate neighbours.",
      "ngeo_error_capability"
    )
  }
  n <- nrow(coordinates)
  limit <- getOption("neurogeo.max_exact_neighbors", 5000L)
  if (n > limit) {
    return(.ngeo_coordinate_neighbors_tree(
      coordinates,
      method,
      k,
      threshold,
      symmetry
    ))
  }
  .ngeo_coordinate_neighbors_exact(
    coordinates,
    method,
    k,
    threshold,
    symmetry
  )
}

.ngeo_graph_neighbors <- function(x,
                                  method,
                                  k,
                                  threshold,
                                  symmetry,
                                  metric) {
  n <- nrow(x$domain$elements)
  limit <- getOption("neurogeo.max_exact_neighbors", 5000L)
  if (n > limit) {
    .ngeo_abort(
      sprintf(
        "Graph-metric neighbour queries are limited to %s elements.",
        format(limit, big.mark = ",")
      ),
      "ngeo_error_resource"
    )
  }
  .ngeo_neighbor_parameters(n, method, k, threshold)
  from <- integer()
  to <- integer()
  distance <- numeric()
  for (i in seq_len(n)) {
    current <- as.numeric(ngeo_distance(
      x,
      from = i,
      metric = metric,
      max_distance = if (identical(method, "knn")) Inf else threshold
    ))
    current[[i]] <- Inf
    if (identical(method, "knn")) {
      finite <- which(is.finite(current))
      if (length(finite) < k) {
        .ngeo_abort(
          "A graph component has fewer reachable elements than requested `k`.",
          "ngeo_error_zero_policy"
        )
      }
      selected <- finite[order(current[finite], finite)][seq_len(k)]
    } else {
      selected <- which(is.finite(current) & current <= threshold)
      selected <- selected[selected > i]
    }
    if (length(selected)) {
      from <- c(from, rep.int(i, length(selected)))
      to <- c(to, selected)
      distance <- c(distance, current[selected])
    }
  }
  maximum <- getOption("neurogeo.max_neighbor_edges", 10000000L)
  if (length(from) > maximum) {
    .ngeo_abort(
      sprintf(
        "Neighbour query returned more than %s directed edges.",
        format(maximum, big.mark = ",")
      ),
      "ngeo_error_resource"
    )
  }
  matrix <- .ngeo_sparse_directed(n, from, to, distance)
  if (identical(method, "distance_band")) {
    matrix <- matrix + Matrix::t(matrix)
  } else if (identical(symmetry, "union")) {
    reverse <- Matrix::t(matrix)
    shared <- matrix * (reverse != 0)
    matrix <- methods::as(matrix + reverse - shared, "dgCMatrix")
  } else if (identical(symmetry, "mutual")) {
    reverse <- Matrix::t(matrix)
    matrix <- matrix * (reverse != 0)
  }
  diag(matrix) <- 0
  methods::as(matrix, "dgCMatrix")
}

.ngeo_metric_neighbors <- function(x,
                                   method,
                                   k,
                                   threshold,
                                   symmetry,
                                   metric) {
  if (metric %in% c(
    "euclidean", "world_euclidean", "region_centroid"
  )) {
    return(.ngeo_coordinate_neighbors(
      x, method, k, threshold, symmetry
    ))
  }
  .ngeo_graph_neighbors(
    x, method, k, threshold, symmetry, metric
  )
}

.ngeo_neighbor_parameters <- function(n, method, k, threshold) {
  if (identical(method, "knn")) {
    if (is.null(k) || length(k) != 1L || is.na(k) ||
        k < 1L || k >= n || k != floor(k)) {
      .ngeo_abort(
        "`k` must be one integer between 1 and n - 1.",
        "ngeo_error_argument"
      )
    }
  } else if (is.null(threshold) || length(threshold) != 1L ||
      is.na(threshold) || !is.finite(threshold) || threshold <= 0) {
    .ngeo_abort(
      "`threshold` must be one positive finite distance.",
      "ngeo_error_argument"
    )
  }
  invisible(TRUE)
}

.ngeo_coordinate_neighbors_exact <- function(coordinates,
                                             method,
                                             k,
                                             threshold,
                                             symmetry) {
  n <- nrow(coordinates)
  .ngeo_neighbor_parameters(n, method, k, threshold)

  from <- integer()
  to <- integer()
  distance <- numeric()
  if (identical(method, "knn")) {
    k <- as.integer(k)
    for (i in seq_len(n)) {
      difference <- sweep(coordinates, 2L, coordinates[i, ], "-")
      current <- sqrt(rowSums(difference^2))
      current[[i]] <- Inf
      selected <- order(current)[seq_len(k)]
      from <- c(from, rep.int(i, k))
      to <- c(to, selected)
      distance <- c(distance, current[selected])
    }
  } else {
    for (i in seq_len(n - 1L)) {
      candidates <- seq.int(i + 1L, n)
      difference <- sweep(
        coordinates[candidates, , drop = FALSE],
        2L,
        coordinates[i, ],
        "-"
      )
      current <- sqrt(rowSums(difference^2))
      selected <- which(current <= threshold)
      if (length(selected)) {
        from <- c(from, rep.int(i, length(selected)))
        to <- c(to, candidates[selected])
        distance <- c(distance, current[selected])
      }
    }
  }

  matrix <- .ngeo_sparse_directed(n, from, to, distance)
  if (identical(method, "distance_band")) {
    matrix <- matrix + Matrix::t(matrix)
  } else if (identical(symmetry, "union")) {
    matrix <- methods::as(pmax(matrix, Matrix::t(matrix)), "dgCMatrix")
  } else if (identical(symmetry, "mutual")) {
    reverse <- Matrix::t(matrix)
    matrix <- matrix * (reverse != 0)
  }
  diag(matrix) <- 0
  matrix
}

.ngeo_coordinate_neighbors_tree <- function(coordinates,
                                            method,
                                            k,
                                            threshold,
                                            symmetry) {
  .ngeo_require("dbscan", "scalable coordinate neighbour queries")
  n <- nrow(coordinates)
  .ngeo_neighbor_parameters(n, method, k, threshold)
  if (identical(method, "knn")) {
    neighbors <- dbscan::kNN(coordinates, k = as.integer(k), sort = TRUE)
    from <- rep(seq_len(n), each = as.integer(k))
    to <- as.vector(t(neighbors$id))
    distance <- as.vector(t(neighbors$dist))
  } else {
    neighbors <- dbscan::frNN(
      coordinates,
      eps = threshold,
      sort = TRUE
    )
    counts <- lengths(neighbors$id)
    from <- rep.int(seq_len(n), counts)
    to <- unlist(neighbors$id, use.names = FALSE)
    distance <- unlist(neighbors$dist, use.names = FALSE)
  }
  maximum <- getOption("neurogeo.max_neighbor_edges", 10000000L)
  if (length(from) > maximum) {
    .ngeo_abort(
      sprintf(
        "Neighbour query returned more than %s directed edges.",
        format(maximum, big.mark = ",")
      ),
      "ngeo_error_resource"
    )
  }
  matrix <- .ngeo_sparse_directed(n, from, to, distance)
  if (identical(method, "knn")) {
    if (identical(symmetry, "union")) {
      reverse <- Matrix::t(matrix)
      shared <- matrix * (reverse != 0)
      matrix <- methods::as(
        matrix + reverse - shared,
        "dgCMatrix"
      )
    } else if (identical(symmetry, "mutual")) {
      reverse <- Matrix::t(matrix)
      matrix <- matrix * (reverse != 0)
    }
  }
  diag(matrix) <- 0
  methods::as(matrix, "dgCMatrix")
}

.ngeo_binary <- function(matrix) {
  result <- methods::as(matrix, "dgCMatrix")
  if (length(result@x)) {
    result@x[] <- 1
  }
  result
}

.ngeo_row_standardize <- function(matrix) {
  totals <- Matrix::rowSums(matrix)
  inverse <- numeric(length(totals))
  nonzero <- totals != 0
  inverse[nonzero] <- 1 / totals[nonzero]
  methods::as(Matrix::Diagonal(x = inverse) %*% matrix, "dgCMatrix")
}

.ngeo_weight_diagnostics <- function(matrix) {
  row_totals <- Matrix::rowSums(matrix)
  binary <- .ngeo_binary(matrix)
  component <- ngeo_components(binary)
  list(
    isolated = which(row_totals == 0),
    n_isolated = sum(row_totals == 0),
    component = component,
    n_component = max(component, 0L)
  )
}

#' Construct sparse spatial weights
#'
#' @param x An `ngeo` object.
#' @param method Contiguity, KNN, radius/distance band, or a distance kernel.
#' @param style `"W"` row standardisation, `"B"` binary, or `"none"`.
#' @param connectivity Voxel connectivity.
#' @param k K for nearest neighbours.
#' @param threshold Distance-band threshold.
#' @param metric Coordinate metric recorded in the result.
#' @param bandwidth Gaussian bandwidth.
#' @param symmetry KNN symmetry policy.
#'
#' @return An `ngeo_weights` object.
#' @export
ngeo_weights <- function(x,
                         method = c(
                           "mesh_contiguity", "voxel_contiguity",
                           "component_contiguity", "region_contiguity",
                           "knn", "distance_band", "radius",
                           "inverse_distance", "gaussian"
                         ),
                         style = c("W", "B", "none"),
                         connectivity = 6L,
                         k = NULL,
                         threshold = NULL,
                         metric = NULL,
                         bandwidth = NULL,
                         symmetry = c("union", "directed", "mutual")) {
  ngeo_validate(x, "basic")
  method <- match.arg(method)
  style <- match.arg(style)
  symmetry <- match.arg(symmetry)
  contiguity_method <- grepl("_contiguity$", method)
  metric <- metric %||% if (contiguity_method) {
    "hops"
  } else {
    switch(
      x$domain$type,
      volume = "world_euclidean",
      regions = "region_centroid",
      "euclidean"
    )
  }
  metric_name <- .ngeo_metric_name(metric)

  raw <- switch(
    method,
    mesh_contiguity = ngeo_adjacency(x, method = "mesh"),
    voxel_contiguity = ngeo_adjacency(
      x,
      method = "voxel",
      connectivity = connectivity
    ),
    component_contiguity = ngeo_adjacency(
      x,
      method = "component",
      connectivity = connectivity
    ),
    region_contiguity = ngeo_adjacency(x, method = "region"),
    knn = .ngeo_metric_neighbors(
      x, "knn", k, threshold, symmetry, metric_name
    ),
    distance_band = .ngeo_metric_neighbors(
      x, "distance_band", k, threshold, symmetry, metric_name
    ),
    radius = .ngeo_metric_neighbors(
      x, "distance_band", k, threshold, symmetry, metric_name
    ),
    inverse_distance = {
      distances <- .ngeo_metric_neighbors(
        x,
        if (!is.null(k)) "knn" else "distance_band",
        k,
        threshold,
        symmetry,
        metric_name
      )
      result <- distances
      if (length(result@x)) {
        result@x <- 1 / result@x
      }
      result
    },
    gaussian = {
      if (is.null(bandwidth) || length(bandwidth) != 1L ||
          is.na(bandwidth) || bandwidth <= 0) {
        .ngeo_abort(
          "Gaussian weights require a positive `bandwidth`.",
          "ngeo_error_argument"
        )
      }
      distances <- .ngeo_metric_neighbors(
        x,
        if (!is.null(k)) "knn" else "distance_band",
        k,
        threshold,
        symmetry,
        metric_name
      )
      result <- distances
      if (length(result@x)) {
        result@x <- exp(-0.5 * (result@x / bandwidth)^2)
      }
      result
    }
  )
  diag(raw) <- 0
  raw <- methods::as(raw, "dgCMatrix")
  matrix <- switch(
    style,
    W = .ngeo_row_standardize(raw),
    B = .ngeo_binary(raw),
    none = raw
  )

  base::structure(
    list(
      matrix = matrix,
      raw_matrix = raw,
      domain_hash = ngeo_domain_hash(x),
      method = method,
      metric = metric_name,
      normalization = style,
      parameters = list(
        connectivity = connectivity,
        k = k,
        threshold = threshold,
        bandwidth = bandwidth
      ),
      symmetry = if (method == "knn") symmetry else "symmetric",
      diagonal = "zero",
      diagnostics = .ngeo_weight_diagnostics(raw)
    ),
    class = "ngeo_weights"
  )
}

#' Construct neighbours through the weights API
#'
#' @param x An `ngeo` object.
#' @param method Contiguity, KNN, or distance band.
#' @param k K for nearest neighbours.
#' @param threshold Distance threshold.
#' @param metric Explicit metric.
#' @param ... Additional `ngeo_weights()` arguments.
#'
#' @return An `ngeo_weights` object.
#' @export
ngeo_neighbors <- function(x,
                           method = c("contiguity", "knn", "distance_band"),
                           k = NULL,
                           threshold = NULL,
                           metric = NULL,
                           ...) {
  method <- match.arg(method)
  weight_method <- if (identical(method, "contiguity")) {
    switch(
      x$domain$type,
      surface = "mesh_contiguity",
      volume = "voxel_contiguity",
      grayordinates = "component_contiguity",
      regions = "region_contiguity",
      .ngeo_abort(
        sprintf("Domain `%s` has no contiguity.", x$domain$type),
        "ngeo_error_capability"
      )
    )
  } else {
    method
  }
  ngeo_weights(
    x,
    method = weight_method,
    style = "B",
    k = k,
    threshold = threshold,
    metric = metric,
    ...
  )
}

#' @export
print.ngeo_weights <- function(x, ...) {
  cat(
    "<ngeo_weights>\n",
    "  method: ", x$method, "\n",
    "  elements: ", nrow(x$matrix), "\n",
    "  nonzero: ", length(x$matrix@x), "\n",
    "  normalization: ", x$normalization, "\n",
    "  components: ", x$diagnostics$n_component, "\n",
    sep = ""
  )
  invisible(x)
}

#' Convert weights to an igraph graph
#'
#' @param x An `ngeo_weights`.
#' @return An igraph object.
#' @export
as_igraph <- function(x) {
  .ngeo_require("igraph", "igraph conversion")
  if (!inherits(x, "ngeo_weights")) {
    .ngeo_abort("`x` must be `ngeo_weights`.", "ngeo_error_argument")
  }
  igraph::graph_from_adjacency_matrix(
    x$matrix,
    mode = if (identical(x$symmetry, "directed")) "directed" else "undirected",
    weighted = TRUE,
    diag = FALSE
  )
}

#' Convert weights to an spdep neighbour list
#'
#' @param x An `ngeo_weights`.
#' @return An `spdep` `nb` object.
#' @export
as_spdep_nb <- function(x) {
  .ngeo_require("spdep", "spdep conversion")
  if (!inherits(x, "ngeo_weights")) {
    .ngeo_abort("`x` must be `ngeo_weights`.", "ngeo_error_argument")
  }
  n <- nrow(x$matrix)
  entries <- Matrix::summary(x$matrix)
  neighbors <- split(entries$j, factor(entries$i, levels = seq_len(n)))
  neighbors <- lapply(neighbors, as.integer)
  attributes(neighbors) <- c(
    attributes(neighbors),
    list(
      region.id = as.character(seq_len(n)),
      call = match.call(),
      type = x$method,
      sym = !identical(x$symmetry, "directed"),
      class = "nb"
    )
  )
  neighbors
}

#' Convert weights to an spdep listw object
#'
#' @param x An `ngeo_weights`.
#' @return An `spdep` `listw` object.
#' @export
as_spdep_listw <- function(x) {
  .ngeo_require("spdep", "spdep conversion")
  nb <- as_spdep_nb(x)
  n <- nrow(x$matrix)
  entries <- Matrix::summary(x$matrix)
  weights <- split(entries$x, factor(entries$i, levels = seq_len(n)))
  spdep::nb2listw(
    nb,
    glist = weights,
    style = "B",
    zero.policy = TRUE
  )
}
