.ngeo_landscape_edges <- function(spatial_weights) {
  adjacency <- .ngeo_undirected_adjacency(spatial_weights)
  entries <- Matrix::summary(adjacency)
  keep <- entries$i < entries$j & entries$x != 0
  list(
    adjacency = adjacency,
    from = entries$i[keep],
    to = entries$j[keep]
  )
}

.ngeo_landscape_threshold <- function(
    values, support, threshold, threshold_type, tail) {
  cutoff <- if (identical(threshold_type, "quantile")) {
    if (!is.numeric(threshold) || length(threshold) != 1L ||
        is.na(threshold) || threshold < 0 || threshold > 1) {
      .ngeo_abort(
        "A quantile threshold must lie between zero and one.",
        "ngeo_error_argument"
      )
    }
    .ngeo_weighted_quantile(
      values, support,
      if (identical(tail, "high")) threshold else 1 - threshold
    )
  } else {
    if (!is.numeric(threshold) || length(threshold) != 1L ||
        is.na(threshold) || !is.finite(threshold)) {
      .ngeo_abort(
        "A value threshold must be one finite number.",
        "ngeo_error_argument"
      )
    }
    threshold
  }
  list(
    cutoff = cutoff,
    active = if (identical(tail, "high")) values >= cutoff else values <= cutoff
  )
}

.ngeo_landscape_layer <- function(
    x, layer, edge, support, threshold, threshold_type, tail,
    boundary_quantile) {
  values <- as.numeric(x$values[, layer])
  layer_id <- x$layers$layer_id[[layer]]
  layer_name <- x$layers$name[[layer]]
  standardized <- .ngeo_weighted_standardize(values, support)
  if (any(!is.finite(values)) || any(!is.finite(standardized$values))) {
    .ngeo_abort(
      "Landscape layers must contain finite, variable values.",
      "ngeo_error_measure"
    )
  }
  threshold_result <- .ngeo_landscape_threshold(
    values, support, threshold, threshold_type, tail
  )
  active <- threshold_result$active
  component <- rep.int(NA_integer_, length(values))
  if (any(active)) {
    component[active] <- ngeo_components(
      edge$adjacency[active, active, drop = FALSE]
    )
  }
  difference <- abs(values[edge$from] - values[edge$to])
  if (any(!is.finite(difference))) {
    .ngeo_abort(
      paste(
        "Landscape edge differences exceed the finite numeric range;",
        "rescale the layer before analysis."
      ),
      "ngeo_error_measure"
    )
  }
  boundary_cutoff <- stats::quantile(
    difference, boundary_quantile, names = FALSE
  )
  boundary_edge <- difference >= boundary_cutoff & difference > 0
  boundary_node <- rep.int(FALSE, length(values))
  boundary_node[unique(c(
    edge$from[boundary_edge], edge$to[boundary_edge]
  ))] <- TRUE
  degree <- Matrix::rowSums(edge$adjacency)
  difference_scale <- max(difference)
  gradient <- if (difference_scale > 0) {
    scaled_difference <- difference / difference_scale
    contribution <- scaled_difference^2
    gradient_squared <- numeric(length(values))
    from_contribution <- rowsum(contribution, edge$from, reorder = FALSE)
    gradient_squared[as.integer(rownames(from_contribution))] <-
      from_contribution[, 1L]
    to_contribution <- rowsum(contribution, edge$to, reorder = FALSE)
    to_rows <- as.integer(rownames(to_contribution))
    gradient_squared[to_rows] <- gradient_squared[to_rows] +
      to_contribution[, 1L]
    difference_scale * sqrt(
      pmax(0, gradient_squared / pmax(1, degree))
    )
  } else {
    numeric(length(values))
  }
  if (any(!is.finite(gradient))) {
    .ngeo_abort(
      "Landscape gradients exceed the finite numeric range; rescale the layer.",
      "ngeo_error_measure"
    )
  }
  patch_ids <- sort(unique(component[!is.na(component)]))
  patch <- lapply(patch_ids, function(id) {
    rows <- which(component == id)
    in_patch_from <- component[edge$from] == id
    in_patch_to <- component[edge$to] == id
    crossing <- xor(in_patch_from %in% TRUE, in_patch_to %in% TRUE)
    internal <- in_patch_from %in% TRUE & in_patch_to %in% TRUE
    perimeter <- sum(crossing)
    area <- sum(support[rows])
    data.frame(
      patch_id = paste0(layer_id, "::", id),
      layer_id = layer_id,
      layer = layer_name,
      elements = length(rows),
      support = area,
      mean_value = stats::weighted.mean(values[rows], support[rows]),
      maximum_gradient = max(gradient[rows]),
      internal_edges = sum(internal),
      boundary_edges = perimeter,
      support_per_boundary_edge_squared = if (perimeter > 0) {
        area / perimeter^2
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })
  patch <- if (length(patch)) do.call(rbind, patch) else data.frame(
    patch_id = character(), layer_id = character(), layer = character(),
    elements = integer(),
    support = numeric(), mean_value = numeric(), maximum_gradient = numeric(),
    internal_edges = integer(), boundary_edges = integer(),
    support_per_boundary_edge_squared = numeric(), stringsAsFactors = FALSE
  )
  list(
    nodes = data.frame(
      layer_id = layer_id,
      layer = layer_name,
      element_id = x$base$elements$element_id,
      value = values,
      gradient = gradient,
      active = active,
      patch_id = ifelse(
        is.na(component), NA_character_,
        paste0(layer_id, "::", component)
      ),
      boundary_node = boundary_node,
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      layer_id = layer_id,
      layer = layer_name,
      from = x$base$elements$element_id[edge$from],
      to = x$base$elements$element_id[edge$to],
      absolute_difference = difference,
      boundary = boundary_edge,
      stringsAsFactors = FALSE
    ),
    patch = patch,
    summary = data.frame(
      layer_id = layer_id,
      layer = layer_name,
      cutoff = threshold_result$cutoff,
      active_elements = sum(active),
      active_support_fraction = sum(support[active]) / sum(support),
      patches = length(patch_ids),
      boundary_cutoff = boundary_cutoff,
      boundary_nodes = sum(boundary_node),
      stringsAsFactors = FALSE
    )
  )
}

.ngeo_landscape_overlap <- function(nodes, edges, support, budget) {
  layer_id <- unique(nodes$layer_id)
  if (length(layer_id) < 2L) {
    return(data.frame(
      layer_id_x = character(), layer_id_y = character(),
      layer_x = character(), layer_y = character(),
      active_jaccard = numeric(),
      boundary_node_support_jaccard = numeric(),
      boundary_edge_jaccard = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  pairs <- utils::combn(layer_id, 2L)
  do.call(rbind, lapply(seq_len(ncol(pairs)), function(i) {
    .ngeo_budget_checkpoint(budget)
    first <- nodes[nodes$layer_id == pairs[1L, i], ]
    second <- nodes[nodes$layer_id == pairs[2L, i], ]
    first_edge <- edges[edges$layer_id == pairs[1L, i], , drop = FALSE]
    second_edge <- edges[edges$layer_id == pairs[2L, i], , drop = FALSE]
    active_union <- first$active | second$active
    boundary_union <- first$boundary_node | second$boundary_node
    data.frame(
      layer_id_x = pairs[1L, i],
      layer_id_y = pairs[2L, i],
      layer_x = first$layer[[1L]],
      layer_y = second$layer[[1L]],
      active_jaccard = if (any(active_union)) {
        sum(support[first$active & second$active]) /
          sum(support[active_union])
      } else {
        NA_real_
      },
      boundary_node_support_jaccard = if (any(boundary_union)) {
        sum(support[first$boundary_node & second$boundary_node]) /
          sum(support[boundary_union])
      } else {
        NA_real_
      },
      boundary_edge_jaccard = {
        edge_union <- first_edge$boundary | second_edge$boundary
        if (any(edge_union)) {
          sum(first_edge$boundary & second_edge$boundary) / sum(edge_union)
        } else {
          NA_real_
        }
      },
      stringsAsFactors = FALSE
    )
  }))
}

#' Describe thresholded brain landscapes and graph boundaries
#'
#' Converts selected continuous layers into explicitly thresholded connected
#' patches on the undirected union of a spatial graph. It reports support size,
#' graph-edge perimeter and a support-per-edge shape proxy, edge-contrast
#' summaries, high-contrast
#' boundary nodes, and support-weighted cross-layer overlap. Graph-edge counts
#' are not physical surface perimeter.
#'
#' @param x An `ngeo` object with finite numeric layers.
#' @param spatial_weights Matching spatial weights. Directed graphs are reduced
#'   to their undirected union.
#' @param layers One or more layer selectors.
#' @param threshold Scalar or layer-aligned threshold. Under `quantile`, a high
#'   tail uses this quantile and a low tail uses its complement.
#' @param threshold_type Interpret `threshold` as a quantile or observed value.
#' @param tail Analyze the high or low tail.
#' @param boundary_quantile Quantile of absolute edge differences used to
#'   identify graph-Wombling boundaries.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#'
#' @return An `ngeo_brain_landscape` with node, edge, patch, layer-summary, and
#'   cross-layer-overlap tables. Thresholds are analytic decisions; the result
#'   contains no calibrated p-values.
#' @templateVar example_call ngeo_brain_landscape(x, spatial_weights, layers = "signal", threshold = 0.9)
#' @template stable-statistical-method
#' @references
#' Fortin, M.-J. and Dale, M. R. T. (2005). *Spatial Analysis: A Guide for
#' Ecologists*. Cambridge University Press.
#' @examples
#' \dontrun{
#' ngeo_brain_landscape(
#'   x, spatial_weights, layers = "signal", threshold = 0.9
#' )
#' }
#' @export
ngeo_brain_landscape <- function(
    x,
    spatial_weights,
    layers,
    threshold = 0.9,
    threshold_type = c("quantile", "value"),
    tail = c("high", "low"),
    boundary_quantile = 0.9,
    budget = ngeo_resource_budget()) {
  ngeo_validate(x, "basic")
  context <- .ngeo_budget_context(budget)
  if (!inherits(spatial_weights, "ngeo_spatial_weights") ||
      !identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort(
      "`spatial_weights` must match the landscape base.",
      "ngeo_error_base_mismatch"
    )
  }
  threshold_type <- match.arg(threshold_type)
  tail <- match.arg(tail)
  if (!is.numeric(boundary_quantile) || length(boundary_quantile) != 1L ||
      is.na(boundary_quantile) || boundary_quantile < 0 ||
      boundary_quantile > 1) {
    .ngeo_abort(
      "`boundary_quantile` must lie between zero and one.",
      "ngeo_error_argument"
    )
  }
  selected <- .ngeo_layer_selection(x, layers)
  .ngeo_projection_measures(x, selected)
  if (length(threshold) == 1L) threshold <- rep(threshold, length(selected))
  if (length(threshold) != length(selected)) {
    .ngeo_abort(
      "`threshold` must be scalar or aligned with selected layers.",
      "ngeo_error_alignment"
    )
  }
  edge <- .ngeo_landscape_edges(spatial_weights)
  if (!length(edge$from)) {
    .ngeo_abort(
      "Landscape analysis requires at least one spatial edge.",
      "ngeo_error_topology"
    )
  }
  support_info <- .ngeo_support_weights(x, "auto")
  layer_count <- length(selected)
  node_count <- nrow(x$base$elements)
  edge_count <- length(edge$from)
  overlap_count <- layer_count * (layer_count - 1) / 2
  maximum_rows <- as.double(layer_count) * (2 * node_count + edge_count) +
    layer_count + overlap_count
  .ngeo_budget_assert(context, "blocks", layer_count + overlap_count)
  .ngeo_budget_assert(
    context, "materialized_elements", 12 * maximum_rows
  )
  .ngeo_budget_assert(context, "memory_bytes", 256 * maximum_rows)
  stage <- lapply(seq_along(selected), function(i) {
    .ngeo_budget_checkpoint(context)
    .ngeo_landscape_layer(
      x, selected[[i]], edge, support_info$values, threshold[[i]],
      threshold_type, tail, boundary_quantile
    )
  })
  nodes <- do.call(rbind, lapply(stage, `[[`, "nodes"))
  edges <- do.call(rbind, lapply(stage, `[[`, "edges"))
  patch <- do.call(rbind, lapply(stage, `[[`, "patch"))
  summary <- do.call(rbind, lapply(stage, `[[`, "summary"))
  rownames(nodes) <- rownames(edges) <- rownames(patch) <-
    rownames(summary) <- NULL
  result <- list(
    nodes = nodes,
    edges = edges,
    patches = patch,
    layer_summary = summary,
    base_hash = base_hash(x),
    value_hash = .ngeo_layer_digest(
      as.matrix(x$values[, selected, drop = FALSE])
    ),
    weights_hash = .ngeo_layer_digest(list(
      base_hash = spatial_weights$base_hash,
      method = spatial_weights$method,
      raw_matrix = spatial_weights$raw_matrix
    )),
    analysis_hash = .ngeo_layer_digest(list(
      base_hash = base_hash(x),
      layer_id = x$layers$layer_id[selected],
      values = as.matrix(x$values[, selected, drop = FALSE]),
      threshold = threshold, threshold_type = threshold_type, tail = tail,
      boundary_quantile = boundary_quantile,
      weights = spatial_weights$raw_matrix
    )),
    cross_layer_overlap = .ngeo_landscape_overlap(
      nodes, edges, support_info$values, context
    ),
    history = list(
      method = "graph_landscape_wombling",
      base_hash = base_hash(x),
      weights_method = spatial_weights$method,
      threshold_type = threshold_type,
      quantile_weighting = if (identical(threshold_type, "quantile")) {
        "base_support"
      } else {
        "not_applicable"
      },
      tail = tail,
      boundary_quantile = boundary_quantile,
      gradient_metric = "root_mean_squared_edge_difference",
      perimeter_metric = "crossing_graph_edge_count",
      physical_perimeter_claimed = FALSE,
      directed_weights_reduced_to_undirected_union = TRUE,
      inference = "descriptive; thresholds are caller-declared decisions",
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_brain_landscape")
}

.ngeo_resistance_pairs <- function(x, pairs, exact_limit) {
  n <- nrow(x$base$elements)
  ids <- x$base$elements$element_id
  if (is.null(pairs)) {
    if (n > exact_limit) {
      .ngeo_abort(
        "All-pairs resistance distance exceeds the configured limit.",
        "ngeo_error_resource"
      )
    }
    pair <- utils::combn(seq_len(n), 2L)
    return(data.frame(from = pair[1L, ], to = pair[2L, ]))
  }
  if (!is.data.frame(pairs) || !all(c("from", "to") %in% names(pairs)) ||
      !nrow(pairs)) {
    .ngeo_abort(
      "`pairs` must contain non-empty `from` and `to` columns.",
      "ngeo_error_argument"
    )
  }
  resolve <- function(value) {
    if (is.numeric(value)) {
      value <- .ngeo_as_integer(value, "pairs")
      if (any(value < 1L | value > n)) return(rep.int(NA_integer_, length(value)))
      return(value)
    }
    match(as.character(value), ids)
  }
  from <- resolve(pairs$from)
  to <- resolve(pairs$to)
  if (anyNA(from) || anyNA(to) || any(from == to)) {
    .ngeo_abort(
      "Resistance pairs must name distinct existing elements.",
      "ngeo_error_alignment"
    )
  }
  data.frame(from = from, to = to)
}

.ngeo_conductance_graph <- function(
    x, spatial_weights, layer, interpretation, beta, edge_cost) {
  selected <- .ngeo_layer_selection(x, layer)
  if (length(selected) != 1L) {
    .ngeo_abort(
      "`layer` must select one conductance or barrier field.",
      "ngeo_error_argument"
    )
  }
  .ngeo_projection_measures(x, selected)
  values <- as.numeric(x$values[, selected])
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "The resistance field must be finite.",
      "ngeo_error_missing"
    )
  }
  adjacency <- .ngeo_undirected_adjacency(spatial_weights)
  entries <- Matrix::summary(adjacency)
  keep <- entries$i < entries$j & entries$x != 0
  from <- entries$i[keep]
  to <- entries$j[keep]
  if (!length(from)) {
    .ngeo_abort(
      "Resistance distance requires at least one graph edge.",
      "ngeo_error_topology"
    )
  }
  baseline_cost <- if (identical(edge_cost, "unit")) {
    rep.int(1, length(from))
  } else {
    if (!identical(spatial_weights$method, "inverse_distance")) {
      .ngeo_abort(
        paste(
          "`edge_cost = \"spatial_weights\"` requires inverse-distance",
          "raw weights so reciprocal values have distance units."
        ),
        "ngeo_error_metric"
      )
    }
    raw <- .ngeo_as_dgCMatrix(spatial_weights$raw_matrix)
    forward <- as.numeric(raw[cbind(from, to)])
    reverse <- as.numeric(raw[cbind(to, from)])
    both <- forward > 0 & reverse > 0
    relative_difference <- abs(forward[both] - reverse[both]) /
      pmax(forward[both], reverse[both])
    if (any(relative_difference > 1e-10)) {
      .ngeo_abort(
        "Reciprocal inverse-distance weights disagree after symmetrization.",
        "ngeo_error_metric"
      )
    }
    reciprocal <- pmax(forward, reverse)
    if (any(!is.finite(reciprocal)) || any(reciprocal <= 0)) {
      .ngeo_abort(
        "Inverse-distance edges must have one positive reciprocal weight.",
        "ngeo_error_metric"
      )
    }
    1 / reciprocal
  }
  conductance <- if (identical(interpretation, "conductance")) {
    if (any(values <= 0)) {
      .ngeo_abort(
        "A conductance field must be strictly positive.",
        "ngeo_error_measure"
      )
    }
    low <- pmin(values[from], values[to])
    high <- pmax(values[from], values[to])
    (low / (0.5 + 0.5 * low / high)) / baseline_cost
  } else {
    support <- .ngeo_support_weights(x, "auto")$values
    z <- .ngeo_weighted_standardize(values, support)$values
    if (any(!is.finite(z))) {
      .ngeo_abort(
        "A barrier field must vary over the graph.",
        "ngeo_error_measure"
      )
    }
    exponent <- -beta * (z[from] + z[to]) / 2
    if (any(exponent < log(.Machine$double.xmin)) ||
        any(exponent > log(.Machine$double.xmax))) {
      .ngeo_abort(
        "`beta` makes barrier conductance numerically zero or infinite.",
        "ngeo_error_measure"
      )
    }
    exp(exponent) / baseline_cost
  }
  if (any(!is.finite(conductance)) || any(conductance <= 0)) {
    .ngeo_abort(
      "Edge conductances must be positive and finite.",
      "ngeo_error_measure"
    )
  }
  if (any(!is.finite(1 / conductance))) {
    .ngeo_abort(
      "Edge resistance is outside the finite numeric range; rescale the conductance field.",
      "ngeo_error_measure"
    )
  }
  matrix <- .ngeo_as_dgCMatrix(Matrix::sparseMatrix(
    i = c(from, to),
    j = c(to, from),
    x = c(conductance, conductance),
    dims = dim(adjacency)
  ))
  list(
    matrix = matrix,
    from = from,
    to = to,
    conductance = conductance,
    layer = selected,
    edge_cost = edge_cost
  )
}

.ngeo_least_cost_pairs <- function(conductance, pairs, budget) {
  reversed <- length(unique(pairs$to)) < length(unique(pairs$from))
  oriented <- if (reversed) {
    data.frame(from = pairs$to, to = pairs$from)
  } else {
    pairs
  }
  source <- unique(oriented$from)
  .ngeo_budget_assert(budget, "blocks", length(source))
  .ngeo_budget_assert(
    budget, "materialized_elements",
    length(conductance@x) + as.double(nrow(conductance)) * length(source)
  )
  .ngeo_budget_assert(
    budget, "memory_bytes",
    24 * length(conductance@x) + 40 * nrow(conductance)
  )
  resistance <- conductance
  resistance@x <- 1 / resistance@x
  distance <- lapply(source, function(current) {
    .ngeo_budget_checkpoint(budget)
    targets <- unique(oriented$to[oriented$from == current])
    values <- .ngeo_dijkstra_one(
      conductance, resistance, current, targets, Inf
    )
    stats::setNames(values, targets)
  })
  names(distance) <- as.character(source)
  vapply(seq_len(nrow(oriented)), function(i) {
    distance[[as.character(oriented$from[[i]])]][
      as.character(oriented$to[[i]])
    ]
  }, numeric(1))
}

.ngeo_effective_resistance_pairs <- function(
    conductance, pairs, budget, exact_limit) {
  n <- nrow(conductance)
  if (n > exact_limit) {
    .ngeo_abort(
      "Exact effective resistance exceeds the configured dense limit.",
      "ngeo_error_resource"
    )
  }
  if (length(conductance@x) / 2L == n - 1L) {
    return(.ngeo_least_cost_pairs(conductance, pairs, budget))
  }
  cells <- as.double(n) * n
  .ngeo_budget_assert(budget, "materialized_elements", 4 * cells)
  .ngeo_budget_assert(budget, "memory_bytes", 32 * cells)
  conductance_scale <- max(conductance@x)
  scaled_conductance <- conductance / conductance_scale
  if (any(conductance@x > 0 & scaled_conductance@x == 0)) {
    .ngeo_abort(
      "Conductance dynamic range cannot be represented in the dense effective-resistance solve.",
      "ngeo_error_convergence"
    )
  }
  laplacian <- diag(Matrix::rowSums(scaled_conductance)) -
    as.matrix(scaled_conductance)
  reference <- n
  grounded <- laplacian[-reference, -reference, drop = FALSE]
  reciprocal_condition <- rcond(grounded)
  if (!is.finite(reciprocal_condition) || reciprocal_condition < 1e-12) {
    .ngeo_abort(
      "The grounded conductance Laplacian is too ill-conditioned for a reliable dense solve.",
      "ngeo_error_convergence"
    )
  }
  inverse <- tryCatch(
    solve(grounded),
    error = function(error) NULL
  )
  if (is.null(inverse) || any(!is.finite(inverse))) {
    .ngeo_abort(
      "The grounded conductance Laplacian could not be solved reliably.",
      "ngeo_error_convergence"
    )
  }
  scaled_value <- vapply(seq_len(nrow(pairs)), function(i) {
    first <- pairs$from[[i]]
    second <- pairs$to[[i]]
    if (first == reference) return(inverse[second, second])
    if (second == reference) return(inverse[first, first])
    inverse[first, first] + inverse[second, second] - 2 * inverse[first, second]
  }, numeric(1))
  tolerance <- 100 * .Machine$double.eps * pmax(1, abs(scaled_value))
  if (any(scaled_value < -tolerance)) {
    .ngeo_abort(
      "Effective resistance produced a negative value beyond numerical tolerance.",
      "ngeo_error_convergence"
    )
  }
  value <- pmax(0, scaled_value) / conductance_scale
  if (any(!is.finite(value))) {
    .ngeo_abort(
      "Effective resistance is outside the finite numeric range.",
      "ngeo_error_convergence"
    )
  }
  value
}

.ngeo_diffusion_distance_pairs <- function(
    conductance, pairs, time, budget, exact_limit) {
  n <- nrow(conductance)
  if (n > exact_limit) {
    .ngeo_abort(
      "Exact diffusion distance exceeds the configured dense limit.",
      "ngeo_error_resource"
    )
  }
  cells <- as.double(n) * n
  .ngeo_budget_assert(budget, "materialized_elements", 4 * cells)
  .ngeo_budget_assert(budget, "memory_bytes", 32 * cells)
  conductance_scale <- max(conductance@x)
  scaled_conductance <- conductance / conductance_scale
  if (any(conductance@x > 0 & scaled_conductance@x == 0)) {
    .ngeo_abort(
      "Conductance dynamic range cannot be represented in the dense diffusion solve.",
      "ngeo_error_convergence"
    )
  }
  laplacian <- diag(Matrix::rowSums(scaled_conductance)) -
    as.matrix(scaled_conductance)
  decomposition <- eigen(laplacian, symmetric = TRUE)
  scaled_time <- time * conductance_scale
  exponent <- rep.int(0, length(decomposition$values))
  spectral_scale <- max(abs(decomposition$values))
  exact_null <- rep.int(1 / sqrt(n), n)
  null_overlap <- abs(as.numeric(crossprod(
    decomposition$vectors, exact_null
  )))
  null_index <- which.max(null_overlap)
  resolution <- 10 * .Machine$double.eps * spectral_scale * n
  candidate <- setdiff(seq_along(decomposition$values), null_index)
  if (null_overlap[[null_index]] < 0.9 ||
      abs(decomposition$values[[null_index]]) > resolution) {
    .ngeo_abort(
      "The diffusion eigensystem did not recover the constant null mode reliably.",
      "ngeo_error_convergence"
    )
  }
  if (any(decomposition$values[candidate] < -resolution)) {
    .ngeo_abort(
      "The conductance Laplacian produced a negative diffusion mode.",
      "ngeo_error_convergence"
    )
  }
  if (any(decomposition$values[candidate] <= resolution)) {
    .ngeo_abort(
      paste(
        "The diffusion eigensystem cannot separate a slow mode from the",
        "constant null at the available numeric precision."
      ),
      "ngeo_error_convergence"
    )
  }
  positive <- seq_along(decomposition$values) != null_index
  exponent[positive] <- scaled_time * decomposition$values[positive]
  embedding <- sweep(
    decomposition$vectors,
    2L,
    exp(-exponent),
    "*"
  )
  vapply(seq_len(nrow(pairs)), function(i) {
    difference <- embedding[pairs$from[[i]], ] -
      embedding[pairs$to[[i]], ]
    sqrt(sum(difference^2))
  }, numeric(1))
}

#' Compute anatomy-conditioned graph distances
#'
#' Converts a declared positive conductance field, or a standardized barrier
#' field, into symmetric edge conductances. It then computes selected-pair
#' least-cost, effective-resistance, or heat-kernel diffusion distances.
#' Effective resistance and diffusion distance are exact dense small-graph
#' references and are capped; least-cost distance is sparse but currently uses
#' the package's sparse heap-based Dijkstra implementation.
#'
#' @param x An `ngeo` object containing the conditioning layer.
#' @param spatial_weights Matching spatial weights. Direction is reduced to the
#'   undirected union because resistance networks require reciprocal edges.
#' @param layer Single conductance or barrier layer selector.
#' @param pairs Optional non-empty data frame with `from` and `to` element IDs
#'   or one-based indices. `NULL` requests all unordered pairs within the
#'   configured safety limit.
#' @param interpretation Whether larger layer values increase conductance or
#'   increase standardized barrier cost.
#' @param method Least-cost, effective-resistance, or diffusion distance.
#' @param beta Positive barrier contrast. It is unused for direct conductance.
#' @param diffusion_time Positive heat-kernel time, used only for diffusion
#'   distance.
#' @param edge_cost Use unit graph-edge costs, or reciprocal raw weights from
#'   an `inverse_distance` spatial-weights object. The default therefore yields
#'   a graph-topological resistance metric, not millimetres.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#' @param exact_limit Hard element cap for all-pairs requests and dense exact
#'   effective-resistance or diffusion calculations.
#'
#' @return An `ngeo_resistance_distance` with requested distances, the sparse
#'   conductance graph, edge conductance/resistance, and an explicit metric
#'   contract. No causal propagation is claimed.
#' @templateVar example_call ngeo_resistance_distance(x, spatial_weights, layer = "barrier", pairs = pairs)
#' @template stable-spatial-relation-core
#' @references
#' Doyle, P. G. and Snell, J. L. (1984). *Random Walks and Electric
#' Networks*. Mathematical Association of America.
#' @export
ngeo_resistance_distance <- function(
    x,
    spatial_weights,
    layer,
    pairs = NULL,
    interpretation = c("barrier", "conductance"),
    method = c("least_cost", "effective_resistance", "diffusion_distance"),
    beta = 1,
    diffusion_time = 1,
    edge_cost = c("unit", "spatial_weights"),
    exact_limit = 500L,
    budget = ngeo_resource_budget()) {
  ngeo_validate(x, "basic")
  if (!inherits(spatial_weights, "ngeo_spatial_weights") ||
      !identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort(
      "`spatial_weights` must match the resistance base.",
      "ngeo_error_base_mismatch"
    )
  }
  interpretation <- match.arg(interpretation)
  method <- match.arg(method)
  edge_cost <- match.arg(edge_cost)
  exact_limit <- .ngeo_as_integer(exact_limit, "exact_limit")
  if (length(exact_limit) != 1L || exact_limit < 2L) {
    .ngeo_abort("`exact_limit` must be one integer of at least two.",
                "ngeo_error_argument")
  }
  invalid_beta <- identical(interpretation, "barrier") &&
    (!is.numeric(beta) || length(beta) != 1L || is.na(beta) ||
       !is.finite(beta) || beta <= 0)
  invalid_time <- identical(method, "diffusion_distance") &&
    (!is.numeric(diffusion_time) || length(diffusion_time) != 1L ||
       is.na(diffusion_time) || !is.finite(diffusion_time) ||
       diffusion_time <= 0)
  if (invalid_beta || invalid_time) {
    .ngeo_abort(
      "`beta` and `diffusion_time` must be positive finite numbers.",
      "ngeo_error_argument"
    )
  }
  graph <- .ngeo_conductance_graph(
    x, spatial_weights, layer, interpretation, beta, edge_cost
  )
  component <- ngeo_components(graph$matrix)
  if (length(unique(component)) != 1L) {
    .ngeo_abort(
      "Resistance distance currently requires a connected conductance graph.",
      "ngeo_error_topology"
    )
  }
  pair_index <- .ngeo_resistance_pairs(x, pairs, exact_limit)
  if (identical(method, "diffusion_distance")) {
    support <- .ngeo_support_weights(x, "auto")$values
    support_scale <- max(abs(support))
    if (!is.finite(support_scale) || support_scale <= 0 ||
        any(abs(support / support_scale - support[[1L]] / support_scale) >
            sqrt(.Machine$double.eps))) {
      .ngeo_abort(
        paste(
          "Graph diffusion currently uses a unit-node combinatorial",
          "Laplacian and therefore requires equal element support."
        ),
        "ngeo_error_support",
        hint = paste(
          "Aggregate to equal-support elements or use least-cost/effective",
          "resistance; a mass-matrix diffusion model is not implied."
        )
      )
    }
  }
  budget_context <- .ngeo_budget_context(budget)
  .ngeo_budget_assert(
    budget_context, "materialized_elements", 6 * nrow(pair_index)
  )
  distance <- switch(
    method,
    least_cost = .ngeo_least_cost_pairs(
      graph$matrix, pair_index, budget_context
    ),
    effective_resistance = .ngeo_effective_resistance_pairs(
      graph$matrix, pair_index, budget_context, exact_limit
    ),
    diffusion_distance = .ngeo_diffusion_distance_pairs(
      graph$matrix, pair_index, diffusion_time, budget_context, exact_limit
    )
  )
  if (any(!is.finite(distance)) || any(distance < 0)) {
    .ngeo_abort(
      paste(
        "Requested resistance distances exceed the finite numeric range or",
        "are otherwise invalid; rescale the conductance field."
      ),
      "ngeo_error_measure"
    )
  }
  field_measure <- .ngeo_measures_for_layers(x, graph$layer)
  baseline_unit <- if (identical(edge_cost, "unit")) {
    "unit_edge"
  } else {
    x$base$coordinate_space$unit
  }
  resistance_source <- if (identical(interpretation, "barrier")) {
    paste0("standardized_barrier_and_", baseline_unit)
  } else {
    paste0(field_measure$unit[[1L]], "_conductivity_and_", baseline_unit)
  }
  distance_unit <- switch(
    method,
    diffusion_distance = "dimensionless_heat_kernel_row_norm",
    least_cost = paste0("derived_resistance_from_", resistance_source),
    effective_resistance = paste0(
      "derived_effective_resistance_from_", resistance_source
    )
  )
  result <- list(
    distances = data.frame(
      from = x$base$elements$element_id[pair_index$from],
      to = x$base$elements$element_id[pair_index$to],
      distance = distance,
      stringsAsFactors = FALSE
    ),
    conductance = graph$matrix,
    base_hash = base_hash(x),
    layer_id = x$layers$layer_id[[graph$layer]],
    value_hash = .ngeo_layer_digest(
      as.numeric(x$values[, graph$layer])
    ),
    weights_hash = .ngeo_layer_digest(list(
      base_hash = spatial_weights$base_hash,
      method = spatial_weights$method,
      raw_matrix = spatial_weights$raw_matrix
    )),
    analysis_hash = .ngeo_layer_digest(list(
      base_hash = base_hash(x), layer_id = x$layers$layer_id[[graph$layer]],
      pair = pair_index, interpretation = interpretation, method = method,
      beta = if (identical(interpretation, "barrier")) beta else NULL,
      diffusion_time = if (identical(method, "diffusion_distance")) {
        diffusion_time
      } else NULL,
      edge_cost = edge_cost, conductance = graph$matrix
    )),
    edge = data.frame(
      from = x$base$elements$element_id[graph$from],
      to = x$base$elements$element_id[graph$to],
      conductance = graph$conductance,
      resistance = 1 / graph$conductance,
      stringsAsFactors = FALSE
    ),
    history = list(
      method = method,
      interpretation = interpretation,
      beta = if (identical(interpretation, "barrier")) beta else NA_real_,
      diffusion_time = if (identical(method, "diffusion_distance")) {
        diffusion_time
      } else {
        NA_real_
      },
      diffusion_kernel = if (identical(method, "diffusion_distance")) {
        "equal_support_combinatorial_laplacian_heat_kernel"
      } else {
        "not_applicable"
      },
      field_layer_id = x$layers$layer_id[[graph$layer]],
      field_layer = x$layers$name[[graph$layer]],
      base_hash = base_hash(x),
      weights_method = spatial_weights$method,
      baseline_edge_cost = edge_cost,
      distance_unit = distance_unit,
      diffusion_time_unit = if (identical(method, "diffusion_distance")) {
        "inverse_conductance_laplacian_eigenvalue"
      } else {
        "not_applicable"
      },
      directed_weights_reduced_to_undirected_union = TRUE,
      causal_propagation_claimed = FALSE,
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_resistance_distance")
}
