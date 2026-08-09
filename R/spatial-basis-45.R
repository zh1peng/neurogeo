.ngeo_support_weights <- function(x, support = c("auto", "identity")) {
  support <- match.arg(support)
  n <- nrow(x$base$elements)
  if (identical(support, "identity") || identical(x$base$type, "point")) {
    values <- rep.int(1, n)
    type <- "identity"
  } else {
    values <- as.numeric(ngeo_support_size(x))
    type <- "base"
  }
  if (length(values) != n || anyNA(values) || any(!is.finite(values)) ||
      any(values <= 0)) {
    .ngeo_abort(
      "Spatial basis support must be finite, positive, and element-aligned.",
      "ngeo_error_support"
    )
  }
  list(
    values = values,
    type = type,
    hash = .ngeo_layer_digest(list(
      base_hash = base_hash(x),
      type = type,
      values = values
    ))
  )
}

.ngeo_weighted_center <- function(values, support) {
  mean <- sum(support * values) / sum(support)
  list(values = values - mean, mean = mean)
}

.ngeo_weighted_inner <- function(x, y, support) {
  sum(support * x * y)
}

.ngeo_weighted_norm <- function(x, support) {
  sqrt(.ngeo_weighted_inner(x, x, support))
}

.ngeo_weighted_correlation <- function(x, y, support, tolerance = 1e-12) {
  x <- .ngeo_weighted_center(x, support)$values
  y <- .ngeo_weighted_center(y, support)$values
  denominator <- .ngeo_weighted_norm(x, support) *
    .ngeo_weighted_norm(y, support)
  if (!is.finite(denominator) || denominator <= tolerance) {
    .ngeo_abort("Weighted correlation is undefined for a constant map.",
                "ngeo_error_measure")
  }
  .ngeo_weighted_inner(x, y, support) / denominator
}

.ngeo_graph_stiffness <- function(x, spatial_weights, symmetrize, tolerance) {
  if (!inherits(spatial_weights, "ngeo_spatial_weights")) {
    .ngeo_abort("`spatial_weights` must be an `ngeo_spatial_weights` object.",
                "ngeo_error_argument")
  }
  if (!identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort("Spatial spatial_weights do not match the basis base.",
                "ngeo_error_base_mismatch")
  }
  raw <- .ngeo_as_dgCMatrix(spatial_weights$raw_matrix)
  if (any(!is.finite(raw@x)) || any(raw@x < -tolerance)) {
    .ngeo_abort("Graph Laplacian spatial_weights must be finite and non-negative.",
                "ngeo_error_operator")
  }
  if (length(raw@x)) raw@x[raw@x < 0] <- 0
  symmetric <- isTRUE(Matrix::isSymmetric(raw, tol = tolerance))
  if (!symmetric && identical(symmetrize, "error")) {
    .ngeo_abort(
      "Graph Laplacian construction requires symmetric raw spatial_weights.",
      "ngeo_error_operator"
    )
  }
  if (!symmetric) raw <- (raw + Matrix::t(raw)) / 2
  diag(raw) <- 0
  raw <- .ngeo_as_dgCMatrix(raw)
  degree <- Matrix::rowSums(raw)
  stiffness <- .ngeo_as_dgCMatrix(Matrix::Diagonal(x = degree) - raw)
  list(
    adjacency = raw,
    stiffness = stiffness,
    component = ngeo_components(.ngeo_binary(raw)),
    symmetrized = !symmetric
  )
}

.ngeo_degenerate_clusters <- function(eigenvalues, tolerance) {
  if (!length(eigenvalues)) return(integer())
  cluster <- integer(length(eigenvalues))
  cluster[[1L]] <- 1L
  threshold <- max(100 * tolerance, sqrt(.Machine$double.eps))
  if (length(eigenvalues) > 1L) {
    for (i in 2:length(eigenvalues)) {
      gap <- abs(eigenvalues[[i]] - eigenvalues[[i - 1L]]) /
        max(1, abs(eigenvalues[[i]]), abs(eigenvalues[[i - 1L]]))
      cluster[[i]] <- cluster[[i - 1L]] + as.integer(gap > threshold)
    }
  }
  cluster
}

.ngeo_partial_eigen <- function(operator, support, n_modes, tolerance, budget) {
  n <- nrow(operator)
  nonzero_requested <- min(n_modes, max(0L, n - 1L))
  if (!nonzero_requested) {
    return(list(
      values = numeric(), vectors = matrix(numeric(), n, 0L),
      zero_modes = 1L, method = "singleton"
    ))
  }
  inverse_root <- 1 / sqrt(support)
  transformed <- Matrix::Diagonal(x = inverse_root) %*%
    operator %*% Matrix::Diagonal(x = inverse_root)
  transformed <- .ngeo_as_dgCMatrix((transformed + Matrix::t(transformed)) / 2)
  requested <- nonzero_requested + 1L
  dense_threshold <- getOption("neurogeo.max_dense_basis_elements", 512L)
  if (requested >= n && n > dense_threshold) {
    .ngeo_abort(
      "A full eigensystem is not permitted for this base size.",
      "ngeo_error_resource"
    )
  }
  decomposition <- if (n <= dense_threshold || requested >= n) {
    cells <- as.double(n) * n
    .ngeo_budget_assert(budget, "materialized_elements", cells)
    .ngeo_budget_assert(budget, "memory_bytes", cells * 8)
    current <- eigen(as.matrix(transformed), symmetric = TRUE)
    list(values = current$values, vectors = current$vectors,
         method = "base_eigen_reference")
  } else {
    if (!requireNamespace("RSpectra", quietly = TRUE)) {
      .ngeo_abort(
        "Large partial spatial bases require the `RSpectra` package.",
        "ngeo_error_dependency"
      )
    }
    current <- RSpectra::eigs_sym(
      transformed,
      k = requested,
      which = "SM",
      opts = list(tol = tolerance, maxitr = max(1000L, 20L * n))
    )
    list(values = current$values, vectors = current$vectors,
         method = "RSpectra_eigs_sym")
  }
  order <- order(decomposition$values)
  values <- as.numeric(decomposition$values[order])
  vectors <- decomposition$vectors[, order, drop = FALSE]
  zero_tolerance <- max(tolerance, tolerance * max(1, max(abs(values))))
  zero <- abs(values) <= zero_tolerance
  observed_zero <- sum(zero)
  nonzero <- which(!zero & values > -zero_tolerance)
  if (length(nonzero) < nonzero_requested) {
    .ngeo_abort(
      "The partial eigensolver did not recover the requested nonzero modes.",
      "ngeo_error_convergence"
    )
  }
  nonzero <- nonzero[seq_len(nonzero_requested)]
  values <- values[nonzero]
  vectors <- inverse_root * vectors[, nonzero, drop = FALSE]
  norms <- sqrt(colSums(support * vectors^2))
  vectors <- sweep(vectors, 2L, norms, "/")
  for (j in seq_len(ncol(vectors))) {
    anchor <- which.max(abs(vectors[, j]))
    if (vectors[anchor, j] < 0) vectors[, j] <- -vectors[, j]
  }
  list(
    values = values,
    vectors = vectors,
    zero_modes = observed_zero,
    method = decomposition$method
  )
}

.ngeo_basis_component_id <- function(x, rows, position) {
  if ("component_id" %in% names(x$base$elements)) {
    value <- unique(as.character(x$base$elements$component_id[rows]))
    if (length(value) == 1L && !is.na(value) && nzchar(value)) return(value)
  }
  sprintf("component_%03d", position)
}

.ngeo_basis_diagnostics <- function(stiffness, support, vectors,
                                    eigenvalues, tolerance) {
  if (!length(eigenvalues)) {
    return(list(residual = numeric(), orthogonality_error = 0))
  }
  residual <- vapply(seq_along(eigenvalues), function(j) {
    left <- as.numeric(stiffness %*% vectors[, j])
    right <- eigenvalues[[j]] * support * vectors[, j]
    sqrt(sum((left - right)^2)) /
      (sqrt(sum(left^2)) + abs(eigenvalues[[j]]) *
        sqrt(sum((support * vectors[, j])^2)) + tolerance)
  }, numeric(1))
  gram <- crossprod(vectors, support * vectors)
  list(
    residual = residual,
    orthogonality_error = max(abs(gram - diag(ncol(vectors))))
  )
}

#' Construct a fixed support-weighted spatial basis
#'
#' The stable graph-Laplacian path uses symmetric non-negative raw spatial_weights and
#' solves a component-local generalized eigensystem. Map values, outcomes, and
#' group labels are never used.
#'
#' @param x An `ngeo` object.
#' @param spatial_weights Matching symmetric `ngeo_spatial_weights`.
#' @param operator Spatial operator. The cotangent option is reserved for its
#'   validation gate and is not yet stable.
#' @param coordinates Coordinate set for a future cotangent operator.
#' @param support Domain support or identity mass.
#' @param n_modes Number of non-constant modes per connected component.
#' @param components Whether disconnected components are separated or rejected.
#' @param symmetrize Whether directed raw spatial_weights fail or are explicitly
#'   averaged with their transpose.
#' @param tolerance Numerical tolerance.
#' @param budget Hard execution resource limits.
#'
#' @return An `ngeo_spatial_basis` with component-local dense mode blocks.
#' @export
ngeo_spatial_basis <- function(
    x,
    spatial_weights = NULL,
    operator = c("graph_laplacian", "cotangent"),
    coordinates = "active",
    support = c("auto", "identity"),
    n_modes = 64L,
    components = c("separate", "error"),
    symmetrize = c("error", "mean"),
    tolerance = 1e-8,
    budget = ngeo_resource_budget()) {
  ngeo_validate(x, "basic")
  operator <- match.arg(operator)
  support <- match.arg(support)
  components <- match.arg(components)
  symmetrize <- match.arg(symmetrize)
  n_modes <- .ngeo_as_integer(n_modes, "n_modes")
  if (length(n_modes) != 1L || n_modes < 1L) {
    .ngeo_abort("`n_modes` must be one positive integer.",
                "ngeo_error_argument")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0) {
    .ngeo_abort("`tolerance` must be one positive finite number.",
                "ngeo_error_argument")
  }
  if (!identical(operator, "graph_laplacian")) {
    .ngeo_abort(
      "The cotangent basis is experimental and requires explicit opt-in.",
      "ngeo_error_capability"
    )
  }
  if (is.null(spatial_weights)) {
    .ngeo_abort("A graph-Laplacian basis requires explicit spatial weights.",
                "ngeo_error_argument")
  }

  mass <- .ngeo_support_weights(x, support)
  graph <- .ngeo_graph_stiffness(x, spatial_weights, symmetrize, tolerance)
  component_ids <- sort(unique(graph$component))
  if (identical(components, "error") && length(component_ids) > 1L) {
    .ngeo_abort(
      "The spatial graph is disconnected; use `components = \"separate\"`.",
      "ngeo_error_topology"
    )
  }
  output_cells <- sum(vapply(component_ids, function(id) {
    rows <- which(graph$component == id)
    as.double(length(rows)) * min(n_modes, max(0L, length(rows) - 1L))
  }, numeric(1)))
  estimated_bytes <- 8 * output_cells +
    8 * nrow(graph$adjacency) * (3 * n_modes + 10) +
    16 * length(graph$adjacency@x)
  .ngeo_budget_assert(budget, "materialized_elements", output_cells)
  .ngeo_budget_assert(budget, "memory_bytes", estimated_bytes)

  basis_components <- vector("list", length(component_ids))
  observed_zero <- integer(length(component_ids))
  for (position in seq_along(component_ids)) {
    id <- component_ids[[position]]
    rows <- which(graph$component == id)
    local_stiffness <- graph$stiffness[rows, rows, drop = FALSE]
    local_support <- mass$values[rows]
    decomposition <- .ngeo_partial_eigen(
      local_stiffness, local_support, n_modes, tolerance, budget
    )
    if (any(decomposition$values < -tolerance)) {
      .ngeo_abort("The graph basis contains a negative eigenvalue.",
                  "ngeo_error_operator")
    }
    diagnostics <- .ngeo_basis_diagnostics(
      local_stiffness,
      local_support,
      decomposition$vectors,
      decomposition$values,
      tolerance
    )
    observed_zero[[position]] <- decomposition$zero_modes
    basis_components[[position]] <- list(
      component_id = .ngeo_basis_component_id(x, rows, position),
      rows = rows,
      element_id = x$base$elements$element_id[rows],
      support = local_support,
      eigenvalues = decomposition$values,
      vectors = decomposition$vectors,
      mode_within_component = seq_along(decomposition$values),
      degenerate_cluster = .ngeo_degenerate_clusters(
        decomposition$values, tolerance
      ),
      diagnostics = diagnostics,
      solver = decomposition$method
    )
  }
  names(basis_components) <- vapply(
    basis_components, `[[`, character(1), "component_id"
  )
  max_residual <- max(c(0, unlist(lapply(
    basis_components, function(z) z$diagnostics$residual
  ))))
  max_orthogonality <- max(c(0, vapply(
    basis_components,
    function(z) z$diagnostics$orthogonality_error,
    numeric(1)
  )))
  operator_hash <- .ngeo_layer_digest(list(
    base_hash = base_hash(x),
    adjacency = graph$adjacency,
    support_hash = mass$hash,
    symmetrized = graph$symmetrized
  ))
  identity <- list(
    base_hash = base_hash(x),
    operator_hash = operator_hash,
    support_hash = mass$hash,
    n_modes = n_modes,
    components = lapply(basis_components, function(z) list(
      component_id = z$component_id,
      rows = z$rows,
      eigenvalues = z$eigenvalues,
      vectors = z$vectors,
      degenerate_cluster = z$degenerate_cluster
    )),
    tolerance = tolerance
  )
  result <- list(
    operator = operator,
    coordinates = coordinates,
    base_hash = base_hash(x),
    space_hash = ngeo_coordinate_space_hash(x$base$coordinate_space),
    operator_hash = operator_hash,
    basis_hash = .ngeo_layer_digest(identity),
    support = mass,
    components = basis_components,
    n_modes_requested = n_modes,
    symmetrized = graph$symmetrized,
    tolerance = tolerance,
    diagnostics = list(
      expected_zero_modes = length(component_ids),
      observed_zero_modes = sum(observed_zero),
      zero_modes_by_component = observed_zero,
      max_residual = max_residual,
      max_orthogonality_error = max_orthogonality,
      dense_full_base_matrix = FALSE,
      estimated_memory_bytes = estimated_bytes
    ),
    history = list(
      method = "fixed_support_weighted_graph_laplacian",
      uses_map_values = FALSE,
      uses_group_labels = FALSE,
      weights_method = spatial_weights$method,
      weights_normalization_ignored = spatial_weights$normalization,
      raw_weights_used = TRUE,
      components = components,
      implicit_resampling = FALSE
    )
  )
  class(result) <- "ngeo_spatial_basis"
  result
}

#' @export
print.ngeo_spatial_basis <- function(x, ...) {
  cat(
    "<ngeo_spatial_basis>\n",
    "  operator: ", x$operator, "\n",
    "  components: ", length(x$components), "\n",
    "  modes: ", paste(vapply(
      x$components, function(z) ncol(z$vectors), integer(1)
    ), collapse = ", "), "\n",
    "  max residual: ", format(x$diagnostics$max_residual, digits = 4L), "\n",
    "  basis hash: ", x$basis_hash, "\n",
    sep = ""
  )
  invisible(x)
}
