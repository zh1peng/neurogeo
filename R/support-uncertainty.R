.ngeo_covariance_base <- function(x) {
  if (!inherits(x, "ngeo")) {
    .ngeo_abort(
      "`x` must be an `ngeo` dataset.",
      "ngeo_error_argument"
    )
  }
  ngeo_validate(x, "strict")
  list(
    hash = base_hash(x),
    element_id = x$base$elements$element_id,
    n = nrow(x$base$elements)
  )
}

#' Construct base-bound value covariance
#'
#' Exactly one of a full/sparse `covariance` matrix or a low-rank `factor`
#' may be supplied. A non-negative `variance` vector represents diagonal
#' covariance by itself or the residual diagonal of a low-rank model.
#'
#' @param x Domain-defining `ngeo` dataset.
#' @param variance Optional non-negative aligned diagonal variance.
#' @param covariance Optional symmetric positive-semidefinite matrix.
#' @param factor Optional aligned low-rank factor whose covariance is its
#'   matrix product with its transpose.
#' @param tolerance Symmetry and positive-semidefinite tolerance.
#' @param history Optional covariance history.
#'
#' @return An `ngeo_support_covariance`.
#' @export
ngeo_support_covariance <- function(
    x,
    variance = NULL,
    covariance = NULL,
    factor = NULL,
    tolerance = 1e-10,
    history = list()) {
  base <- .ngeo_covariance_base(x)
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance < 0) {
    .ngeo_abort(
      "`tolerance` must be finite and non-negative.",
      "ngeo_error_argument"
    )
  }
  if (!is.null(covariance) && !is.null(factor)) {
    .ngeo_abort(
      "Supply a covariance matrix or a low-rank factor, not both.",
      "ngeo_error_uncertainty"
    )
  }
  if (is.null(covariance) && is.null(factor) && is.null(variance)) {
    .ngeo_abort(
      "Supply `variance`, `covariance`, or `factor`.",
      "ngeo_error_uncertainty"
    )
  }
  if (is.null(variance)) {
    variance <- rep.int(0, base$n)
  }
  if (!is.numeric(variance) || length(variance) != base$n ||
      anyNA(variance) || any(!is.finite(variance)) ||
      any(variance < 0)) {
    .ngeo_abort(
      "`variance` must be non-negative and base-aligned.",
      "ngeo_error_uncertainty"
    )
  }
  representation <- "diagonal"
  matrix_value <- NULL
  factor_value <- NULL
  if (!is.null(covariance)) {
    if (!(is.matrix(covariance) || inherits(covariance, "Matrix")) ||
        !identical(dim(covariance), c(base$n, base$n))) {
      .ngeo_abort(
        "`covariance` must be an n-element square matrix.",
        "ngeo_error_alignment"
      )
    }
    matrix_value <- methods::as(covariance, "dMatrix")
    difference <- matrix_value - Matrix::t(matrix_value)
    if (length(difference@x) &&
        max(abs(difference@x)) > tolerance) {
      .ngeo_abort(
        "`covariance` must be symmetric.",
        "ngeo_error_uncertainty"
      )
    }
    diagonal <- Matrix::diag(matrix_value)
    if (any(!is.finite(matrix_value@x)) ||
        any(diagonal < -tolerance)) {
      .ngeo_abort(
        "`covariance` must be finite with non-negative diagonal.",
        "ngeo_error_uncertainty"
      )
    }
    check_limit <- getOption(
      "neurogeo.max_covariance_psd_check", 2000L
    )
    if (base$n <= check_limit) {
      eigenvalue <- eigen(
        as.matrix(matrix_value),
        symmetric = TRUE,
        only.values = TRUE
      )$values
      if (min(eigenvalue) < -tolerance *
          max(1, max(abs(eigenvalue)))) {
        .ngeo_abort(
          "`covariance` is not positive semidefinite.",
          "ngeo_error_uncertainty"
        )
      }
    }
    representation <- "matrix"
  } else if (!is.null(factor)) {
    if (!is.matrix(factor) || !is.numeric(factor) ||
        nrow(factor) != base$n || !ncol(factor) ||
        anyNA(factor) || any(!is.finite(factor))) {
      .ngeo_abort(
        "`factor` must be a finite n-element by rank matrix.",
        "ngeo_error_uncertainty"
      )
    }
    factor_value <- factor
    representation <- "low_rank"
  }
  history$operations <- c(
    history$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_support_covariance",
      list(
        representation = representation,
        rank = if (is.null(factor_value)) 0L else ncol(factor_value),
        tolerance = tolerance
      )
    ))
  )
  result <- structure(
    list(
      representation = representation,
      variance = as.numeric(variance),
      covariance = matrix_value,
      factor = factor_value,
      base_hash = base$hash,
      element_id = base$element_id,
      dimension = base$n,
      tolerance = tolerance,
      history = history,
      spec_version = "2.2"
    ),
    class = "ngeo_support_covariance"
  )
  ngeo_validate_support_covariance(result)
  result
}

#' Validate base-bound covariance
#'
#' @param x An `ngeo_support_covariance`.
#'
#' @return `x`, invisibly.
#' @export
ngeo_validate_support_covariance <- function(x) {
  if (!inherits(x, "ngeo_support_covariance") ||
      !x$representation %in% c("diagonal", "matrix", "low_rank") ||
      !is.numeric(x$variance) ||
      length(x$variance) != x$dimension ||
      anyNA(x$variance) || any(!is.finite(x$variance)) ||
      any(x$variance < 0) ||
      !is.character(x$element_id) ||
      length(x$element_id) != x$dimension ||
      anyNA(x$element_id) || anyDuplicated(x$element_id) ||
      !is.character(x$base_hash) ||
      length(x$base_hash) != 1L ||
      is.na(x$base_hash) || !nzchar(x$base_hash)) {
    .ngeo_abort(
      "Support covariance structure or base identity is invalid.",
      "ngeo_error_uncertainty"
    )
  }
  if (identical(x$representation, "matrix") &&
      (!inherits(x$covariance, "dMatrix") ||
        !identical(dim(x$covariance), c(x$dimension, x$dimension)))) {
    .ngeo_abort(
      "Stored covariance matrix is invalid.",
      "ngeo_error_uncertainty"
    )
  }
  if (identical(x$representation, "low_rank") &&
      (!is.matrix(x$factor) || nrow(x$factor) != x$dimension ||
        !ncol(x$factor))) {
    .ngeo_abort(
      "Stored low-rank covariance factor is invalid.",
      "ngeo_error_uncertainty"
    )
  }
  invisible(x)
}

.ngeo_validate_covariance_base <- function(covariance, x) {
  ngeo_validate_support_covariance(covariance)
  if (!identical(covariance$base_hash, base_hash(x)) ||
      !identical(
        covariance$element_id,
        x$base$elements$element_id
      )) {
    .ngeo_abort(
      "Covariance does not match the ordered source base.",
      "ngeo_error_base_mismatch"
    )
  }
  invisible(TRUE)
}

.ngeo_covariance_diagonal_transform <- function(linear, covariance) {
  base <- as.numeric((linear^2) %*% covariance$variance)
  if (identical(covariance$representation, "diagonal")) {
    return(base)
  }
  if (identical(covariance$representation, "low_rank")) {
    projected <- as.matrix(linear %*% covariance$factor)
    return(base + rowSums(projected^2))
  }
  product <- linear %*% covariance$covariance
  base + Matrix::rowSums(product * linear)
}

.ngeo_covariance_full_transform <- function(linear, covariance) {
  maximum <- getOption("neurogeo.max_full_covariance_targets", 2000L)
  if (nrow(linear) > maximum) {
    .ngeo_abort(
      "Full target covariance exceeds the configured target limit.",
      "ngeo_error_resource"
    )
  }
  diagonal_part <- linear %*%
    Matrix::Diagonal(x = covariance$variance) %*%
    Matrix::t(linear)
  if (identical(covariance$representation, "diagonal")) {
    return(methods::as(diagonal_part, "dMatrix"))
  }
  if (identical(covariance$representation, "low_rank")) {
    projected <- as.matrix(linear %*% covariance$factor)
    return(methods::as(
      diagonal_part + tcrossprod(projected),
      "dMatrix"
    ))
  }
  methods::as(
    diagonal_part +
      linear %*% covariance$covariance %*% Matrix::t(linear),
    "dMatrix"
  )
}

.ngeo_operator_uncertainty_variance <- function(
    x,
    support_map,
    layer_index,
    semantics,
    linear,
    denominator) {
  operator_variance <- support_map$weight_variance
  if (is.null(operator_variance)) {
    return(rep.int(0, nrow(support_map$operator)))
  }
  values <- as.numeric(x$values[, layer_index])
  if (identical(semantics, "intensive")) {
    estimate <- as.numeric(
      support_map$operator %*%
        (support_map$source_support * values) /
        denominator
    )
    entries <- Matrix::summary(operator_variance)
    contribution <- entries$x *
      (
        support_map$source_support[entries$j] *
          (values[entries$j] - estimate[entries$i]) /
          denominator[entries$i]
      )^2
    result <- numeric(nrow(support_map$operator))
    if (length(contribution)) {
      grouped <- rowsum(
        contribution,
        entries$i,
        reorder = FALSE
      )
      result[as.integer(rownames(grouped))] <- grouped[, 1L]
    }
    return(result)
  }
  as.numeric(operator_variance %*% (values^2))
}

.ngeo_covariance_for_maps <- function(value_covariance, count) {
  if (inherits(value_covariance, "ngeo_support_covariance")) {
    return(rep(list(value_covariance), count))
  }
  if (!is.list(value_covariance) ||
      length(value_covariance) != count ||
      !all(vapply(
        value_covariance,
        inherits,
        logical(1),
        what = "ngeo_support_covariance"
      ))) {
    .ngeo_abort(
      "`value_covariance` must be shared or align with selected layers.",
      "ngeo_error_uncertainty"
    )
  }
  value_covariance
}

.ngeo_support_linear_operator <- function(
    support_map,
    semantics,
    allocation,
    unmapped) {
  source_support <- support_map$source_support
  if (identical(semantics, "intensive")) {
    denominator <- as.numeric(
      support_map$operator %*% source_support
    )
    inverse <- numeric(length(denominator))
    inverse[denominator > 0] <- 1 / denominator[denominator > 0]
    linear <- Matrix::Diagonal(x = inverse) %*%
      support_map$operator %*%
      Matrix::Diagonal(x = source_support)
    return(list(
      linear = .ngeo_as_dgCMatrix(linear),
      denominator = denominator
    ))
  }
  list(
    linear = .ngeo_allocation_operator(
      support_map, allocation, unmapped
    ),
    denominator = NULL
  )
}

.ngeo_draw_covariance <- function(covariance, nsim) {
  n <- covariance$dimension
  residual <- matrix(
    stats::rnorm(n * nsim),
    nrow = n,
    ncol = nsim
  ) * sqrt(covariance$variance)
  if (identical(covariance$representation, "diagonal")) {
    return(residual)
  }
  if (identical(covariance$representation, "low_rank")) {
    latent <- matrix(
      stats::rnorm(ncol(covariance$factor) * nsim),
      nrow = ncol(covariance$factor),
      ncol = nsim
    )
    return(residual + covariance$factor %*% latent)
  }
  maximum <- getOption("neurogeo.max_covariance_draw_dimension", 5000L)
  if (n > maximum) {
    .ngeo_abort(
      "Matrix covariance draw exceeds the configured dimension limit.",
      "ngeo_error_resource"
    )
  }
  dense <- as.matrix(covariance$covariance)
  decomposition <- eigen(dense, symmetric = TRUE)
  keep <- decomposition$values > covariance$tolerance
  if (!any(keep)) {
    return(residual)
  }
  latent <- matrix(
    stats::rnorm(sum(keep) * nsim),
    nrow = sum(keep),
    ncol = nsim
  )
  residual + decomposition$vectors[, keep, drop = FALSE] %*%
    (sqrt(decomposition$values[keep]) * latent)
}

#' Propagate base-bound value and operator uncertainty
#'
#' Analytic propagation applies the exact linear covariance transform for
#' extensive/count values and the first-order normalized transform for
#' intensive values. Monte Carlo propagation can also use an alternative
#' operator ensemble.
#'
#' @inheritParams aggregate_to
#' @param value_covariance One covariance object shared across selected layers,
#'   or an aligned list.
#' @param method Analytic or Monte Carlo propagation.
#' @param output Return diagonal variance only or full bounded covariance.
#' @param operator_ensemble Optional common-base support-map ensemble.
#' @param nsim Monte Carlo draws.
#' @param seed Reproducible seed.
#'
#' @return An `ngeo_support_uncertainty`.
#' @export
ngeo_support_uncertainty <- function(
    x,
    target,
    support_map,
    value_covariance,
    layers = NULL,
    method = c("analytic", "monte_carlo"),
    output = c("diagonal", "covariance"),
    operator_ensemble = NULL,
    nsim = 999L,
    seed = NULL,
    allocation = c("error", "normalize"),
    unmapped = c("error", "drop"),
    unknown = c("error", "intensive", "extensive")) {
  .ngeo_validate_support_bases(x, target, support_map)
  method <- match.arg(method)
  output <- match.arg(output)
  allocation <- match.arg(allocation)
  unmapped <- match.arg(unmapped)
  unknown <- match.arg(unknown)
  layer_index <- .ngeo_layer_selection(x, layers)
  covariance <- .ngeo_covariance_for_maps(
    value_covariance,
    length(layer_index)
  )
  lapply(covariance, .ngeo_validate_covariance_base, x = x)
  if (is.null(support_map$source_support)) {
    support_map$source_support <- .ngeo_support_vector(x)
  }
  estimate <- aggregate_to(
    x,
    target,
    support_map,
    layers = layer_index,
    allocation = allocation,
    unmapped = unmapped,
    unknown = unknown
  )$values
  if (identical(method, "analytic")) {
    variance <- matrix(
      NA_real_,
      nrow = nrow(support_map$operator),
      ncol = length(layer_index)
    )
    covariance_out <- if (identical(output, "covariance")) {
      vector("list", length(layer_index))
    } else {
      NULL
    }
    for (i in seq_along(layer_index)) {
      semantics <- .ngeo_measures_for_layers(
        x,
        layer_index[[i]]
      )$support_behavior[[1L]]
      if (identical(semantics, "unknown")) {
        if (identical(unknown, "error")) {
          .ngeo_abort(
            "Declare unknown semantics before uncertainty propagation.",
            "ngeo_error_measure"
          )
        }
        semantics <- unknown
      }
      if (identical(semantics, "categorical")) {
        .ngeo_abort(
          "Categorical covariance requires a declared probability model.",
          "ngeo_error_uncertainty"
        )
      }
      transform <- .ngeo_support_linear_operator(
        support_map, semantics, allocation, unmapped
      )
      variance[, i] <- .ngeo_covariance_diagonal_transform(
        transform$linear,
        covariance[[i]]
      ) + .ngeo_operator_uncertainty_variance(
        x,
        support_map,
        layer_index[[i]],
        semantics,
        transform$linear,
        transform$denominator
      )
      if (identical(output, "covariance")) {
        covariance_out[[i]] <- .ngeo_covariance_full_transform(
          transform$linear,
          covariance[[i]]
        )
        Matrix::diag(covariance_out[[i]]) <-
          variance[, i]
      }
    }
    colnames(variance) <- x$layers$name[layer_index]
    if (!is.null(covariance_out)) {
      names(covariance_out) <- colnames(variance)
    }
    result <- list(
      estimate = estimate,
      variance = variance,
      standard_error = sqrt(variance),
      covariance = covariance_out,
      method = method,
      assumptions = paste(
        "exact linear value covariance;",
        "first-order normalized intensive/operator covariance;",
        "independent operator entries"
      ),
      support_map_hash = ngeo_support_map_hash(support_map),
      target_base_hash = base_hash(target),
      layers = colnames(variance),
      nsim = NULL,
      seed = NULL
    )
  } else {
    nsim <- .ngeo_nsim(nsim)
    if (!is.null(operator_ensemble)) {
      ngeo_validate_support_ensemble(operator_ensemble)
      if (!identical(
        operator_ensemble$source_base_hash,
        support_map$source_base_hash
      ) || !identical(
        operator_ensemble$target_base_hash,
        support_map$target_base_hash
      )) {
        .ngeo_abort(
          "Operator ensemble bases do not match the support map.",
          "ngeo_error_base_mismatch"
        )
      }
      operator_estimates <- lapply(operator_ensemble$layers, function(map) {
        aggregate_to(
          x,
          target,
          map,
          layers = layer_index,
          allocation = allocation,
          unmapped = unmapped,
          unknown = unknown
        )$values
      })
      estimate <- Reduce(`+`, Map(
        function(value, weight) value * weight,
        operator_estimates,
        operator_ensemble$spatial_weights
      ))
    }
    simulation_result <- .ngeo_with_seed(seed, function() {
      operator_draw <- if (is.null(operator_ensemble)) {
        NULL
      } else {
        sample.int(
          length(operator_ensemble$layers),
          size = nsim,
          replace = TRUE,
          prob = operator_ensemble$spatial_weights
        )
      }
      value_draws <- lapply(
        covariance,
        .ngeo_draw_covariance,
        nsim = nsim
      )
      array_out <- array(
        NA_real_,
        dim = c(
          nrow(support_map$operator),
          length(layer_index),
          nsim
        )
      )
      for (simulation in seq_len(nsim)) {
        current <- x
        for (i in seq_along(layer_index)) {
          current$values[, layer_index[[i]]] <-
            x$values[, layer_index[[i]]] + value_draws[[i]][, simulation]
        }
        current_map <- if (is.null(operator_ensemble)) {
          support_map
        } else {
          operator_ensemble$layers[[operator_draw[[simulation]]]]
        }
        array_out[, , simulation] <- aggregate_to(
          current,
          target,
          current_map,
          layers = layer_index,
          allocation = allocation,
          unmapped = unmapped,
          unknown = unknown
        )$values
      }
      list(simulations = array_out, operator_draw = operator_draw)
    })
    simulated <- simulation_result$simulations
    variance <- apply(simulated, c(1L, 2L), stats::var)
    if (length(layer_index) == 1L) {
      variance <- matrix(variance, ncol = 1L)
    }
    colnames(variance) <- x$layers$name[layer_index]
    covariance_out <- NULL
    if (identical(output, "covariance")) {
      maximum <- getOption(
        "neurogeo.max_full_covariance_targets", 2000L
      )
      if (nrow(support_map$operator) > maximum) {
        .ngeo_abort(
          "Full Monte Carlo covariance exceeds the target limit.",
          "ngeo_error_resource"
        )
      }
      covariance_out <- lapply(seq_along(layer_index), function(i) {
        stats::cov(t(simulated[, i, , drop = FALSE][, 1L, ]))
      })
      names(covariance_out) <- colnames(variance)
    }
    result <- list(
      estimate = estimate,
      variance = variance,
      standard_error = sqrt(variance),
      covariance = covariance_out,
      simulations = simulated,
      method = method,
      assumptions = paste(
        "Gaussian value covariance draws;",
        if (is.null(operator_ensemble)) {
          "fixed operator"
        } else {
          paste0(
            operator_ensemble$kind,
            " ensemble sampled using declared weights"
          )
        }
      ),
      support_map_hash = ngeo_support_map_hash(support_map),
      operator_ensemble_hash = if (is.null(operator_ensemble)) {
        NULL
      } else {
        operator_ensemble$ensemble_hash
      },
      target_base_hash = base_hash(target),
      layers = colnames(variance),
      nsim = nsim,
      seed = .ngeo_seed(seed),
      operator_draw = simulation_result$operator_draw
    )
  }
  class(result) <- "ngeo_support_uncertainty"
  result
}

#' @export
print.ngeo_support_covariance <- function(x, ...) {
  cat(
    "<ngeo_support_covariance>\n",
    "  representation: ", x$representation, "\n",
    "  dimension: ", x$dimension, "\n",
    "  rank: ", if (is.null(x$factor)) 0L else ncol(x$factor), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_support_uncertainty <- function(x, ...) {
  cat(
    "<ngeo_support_uncertainty>\n",
    "  method: ", x$method, "\n",
    "  targets: ", nrow(x$variance), "\n",
    "  layers: ", ncol(x$variance), "\n",
    if (is.null(x$nsim)) "" else paste0("  simulations: ", x$nsim, "\n"),
    sep = ""
  )
  invisible(x)
}

.ngeo_power_norm <- function(operator, iterations = 30L) {
  if (!nrow(operator) || !ncol(operator) || !length(operator@x)) {
    return(0)
  }
  vector <- rep.int(1 / sqrt(ncol(operator)), ncol(operator))
  for (iteration in seq_len(iterations)) {
    next_vector <- as.numeric(
      Matrix::t(operator) %*% (operator %*% vector)
    )
    norm <- sqrt(sum(next_vector^2))
    if (!is.finite(norm) || norm == 0) {
      return(0)
    }
    vector <- next_vector / norm
  }
  sqrt(sum((operator %*% vector)^2))
}

#' Diagnose sparse support-operator conditioning
#'
#' @param x An `ngeo_support_map`.
#' @param tolerance Weak-support and numerical-rank tolerance.
#' @param iterations Sparse power iterations for the largest singular value.
#'
#' @return An `ngeo_support_condition`.
#' @export
ngeo_support_condition <- function(
    x,
    tolerance = 1e-10,
    iterations = 30L) {
  ngeo_validate_support_map(x, tolerance)
  iterations <- .ngeo_as_integer(iterations, "iterations")
  if (length(iterations) != 1L || iterations < 1L) {
    .ngeo_abort(
      "`iterations` must be one positive integer.",
      "ngeo_error_argument"
    )
  }
  column_sum <- Matrix::colSums(x$operator)
  row_sum <- Matrix::rowSums(x$operator)
  column_norm <- sqrt(Matrix::colSums(x$operator^2))
  row_norm <- sqrt(Matrix::rowSums(x$operator^2))
  sigma_max <- .ngeo_power_norm(x$operator, iterations)
  frobenius_squared <- sum(x$operator@x^2)
  stable_rank <- if (sigma_max > 0) {
    frobenius_squared / sigma_max^2
  } else {
    0
  }
  exact_limit <- getOption("neurogeo.max_exact_support_rank", 500L)
  singular_values <- NULL
  numerical_rank <- NA_integer_
  condition_number <- NA_real_
  if (min(dim(x$operator)) <= exact_limit) {
    singular_values <- svd(
      as.matrix(x$operator),
      nu = 0L,
      nv = 0L
    )$d
    threshold <- tolerance * max(1, singular_values[[1L]])
    positive <- singular_values[singular_values > threshold]
    numerical_rank <- length(positive)
    condition_number <- if (
      length(positive) == min(dim(x$operator))
    ) {
      max(positive) / min(positive)
    } else {
      Inf
    }
  }
  result <- list(
    source = data.frame(
      element_id = x$source_element_id,
      sum = as.numeric(column_sum),
      norm = as.numeric(column_norm),
      isolate = column_sum <= tolerance,
      weak = column_norm <= tolerance,
      stringsAsFactors = FALSE
    ),
    target = data.frame(
      element_id = x$target_element_id,
      sum = as.numeric(row_sum),
      norm = as.numeric(row_norm),
      isolate = row_sum <= tolerance,
      weak = row_norm <= tolerance,
      stringsAsFactors = FALSE
    ),
    sigma_max = sigma_max,
    stable_rank = stable_rank,
    numerical_rank = numerical_rank,
    condition_number = condition_number,
    singular_values = singular_values,
    tolerance = tolerance,
    support_map_hash = ngeo_support_map_hash(x),
    exact_rank_computed = !is.null(singular_values)
  )
  class(result) <- "ngeo_support_condition"
  result
}

#' @export
print.ngeo_support_condition <- function(x, ...) {
  cat(
    "<ngeo_support_condition>\n",
    "  stable rank: ", format(x$stable_rank, digits = 5L), "\n",
    "  numerical rank: ", if (is.na(x$numerical_rank)) {
      "not computed"
    } else {
      x$numerical_rank
    }, "\n",
    "  source isolates: ", sum(x$source$isolate), "\n",
    "  target isolates: ", sum(x$target$isolate), "\n",
    sep = ""
  )
  invisible(x)
}

.ngeo_support_ensemble_hash <- function(layers, kind, spatial_weights) {
  digest::digest(
    list(
      kind = kind,
      layers = vapply(layers, ngeo_support_map_hash, character(1)),
      spatial_weights = spatial_weights
    ),
    algo = "xxhash64",
    serialize = TRUE
  )
}

#' Construct a validated support-map ensemble
#'
#' @param layers Two or more layers with identical ordered source and target
#'   bases.
#' @param kind Operator, registration, or segmentation alternatives.
#' @param spatial_weights Optional non-negative ensemble spatial_weights.
#' @param history Optional ensemble history.
#'
#' @return An `ngeo_support_ensemble`.
#' @export
ngeo_support_ensemble <- function(
    layers,
    kind = c("operator", "registration", "segmentation"),
    spatial_weights = NULL,
    history = list()) {
  kind <- match.arg(kind)
  if (!is.list(layers) || length(layers) < 2L ||
      !all(vapply(
        layers,
        inherits,
        logical(1),
        what = "ngeo_support_map"
      ))) {
    .ngeo_abort(
      "`layers` must contain at least two support layers.",
      "ngeo_error_uncertainty"
    )
  }
  lapply(layers, ngeo_validate_support_map)
  if (is.null(spatial_weights)) {
    spatial_weights <- rep.int(1 / length(layers), length(layers))
  }
  if (!is.numeric(spatial_weights) || length(spatial_weights) != length(layers) ||
      anyNA(spatial_weights) || any(!is.finite(spatial_weights)) ||
      any(spatial_weights < 0) || sum(spatial_weights) <= 0) {
    .ngeo_abort(
      "`spatial_weights` must be finite, non-negative, and ensemble-aligned.",
      "ngeo_error_uncertainty"
    )
  }
  spatial_weights <- spatial_weights / sum(spatial_weights)
  history$operations <- c(
    history$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_support_ensemble",
      list(kind = kind, size = length(layers))
    ))
  )
  result <- structure(
    list(
      layers = layers,
      samples = layers,
      kind = kind,
      spatial_weights = spatial_weights,
      source_base_hash = layers[[1L]]$source_base_hash,
      target_base_hash = layers[[1L]]$target_base_hash,
      source_element_id = layers[[1L]]$source_element_id,
      target_element_id = layers[[1L]]$target_element_id,
      map_hashes = vapply(
        layers, ngeo_support_map_hash, character(1)
      ),
      history = history,
      spec_version = "2.2"
    ),
    class = "ngeo_support_ensemble"
  )
  result$ensemble_hash <- .ngeo_support_ensemble_hash(
    layers, kind, spatial_weights
  )
  ngeo_validate_support_ensemble(result)
  result
}

#' Validate a support-map ensemble
#'
#' @param x An `ngeo_support_ensemble`.
#'
#' @return `x`, invisibly.
#' @export
ngeo_validate_support_ensemble <- function(x) {
  layers <- x$layers %||% x$samples
  if (!inherits(x, "ngeo_support_ensemble") ||
      !is.list(layers) || length(layers) < 1L ||
      !x$kind %in% c("operator", "registration", "segmentation") ||
      !is.numeric(x$spatial_weights) ||
      length(x$spatial_weights) != length(layers) ||
      anyNA(x$spatial_weights) || any(x$spatial_weights < 0) ||
      abs(sum(x$spatial_weights) - 1) > 1e-10) {
    .ngeo_abort(
      "Support ensemble structure or spatial_weights are invalid.",
      "ngeo_error_uncertainty"
    )
  }
  lapply(layers, ngeo_validate_support_map)
  common <- vapply(layers, function(map) {
    identical(map$source_base_hash, layers[[1L]]$source_base_hash) &&
      identical(map$target_base_hash, layers[[1L]]$target_base_hash) &&
      identical(map$source_element_id, layers[[1L]]$source_element_id) &&
      identical(map$target_element_id, layers[[1L]]$target_element_id)
  }, logical(1))
  if (!all(common)) {
    .ngeo_abort(
      "Ensemble layers must share ordered source and target bases.",
      "ngeo_error_base_mismatch"
    )
  }
  expected_hash <- .ngeo_support_ensemble_hash(
    layers, x$kind, x$spatial_weights
  )
  if (!is.null(x$ensemble_hash) &&
      !identical(x$ensemble_hash, expected_hash)) {
    .ngeo_abort(
      "Support ensemble hash verification failed.",
      "ngeo_error_uncertainty"
    )
  }
  invisible(x)
}

#' Construct a known-registration operator ensemble
#'
#' @inheritParams ngeo_support_ensemble
#' @return An `ngeo_support_ensemble`.
#' @export
ngeo_registration_ensemble <- function(
    layers,
    spatial_weights = NULL,
    history = list()) {
  ngeo_support_ensemble(
    layers,
    kind = "registration",
    spatial_weights = spatial_weights,
    history = history
  )
}

#' Construct a segmentation operator ensemble
#'
#' @inheritParams ngeo_support_ensemble
#' @return An `ngeo_support_ensemble`.
#' @export
ngeo_segmentation_ensemble <- function(
    layers,
    spatial_weights = NULL,
    history = list()) {
  ngeo_support_ensemble(
    layers,
    kind = "segmentation",
    spatial_weights = spatial_weights,
    history = history
  )
}

#' @export
print.ngeo_support_ensemble <- function(x, ...) {
  layers <- x$layers %||% x$samples
  cat(
    "<ngeo_support_ensemble>\n",
    "  kind: ", x$kind %||% "operator", "\n",
    "  layers: ", length(layers), "\n",
    "  source: ", x$source_base_hash, "\n",
    "  target: ", x$target_base_hash, "\n",
    sep = ""
  )
  invisible(x)
}
