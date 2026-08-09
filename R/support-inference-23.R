#' Adjust a family of support-aware tests
#'
#' @param p_value Raw p-values for BH, BY, or Holm adjustment.
#' @param method BH, BY, Holm, or common-simulation max-T.
#' @param observed Observed statistics for max-T.
#' @param simulated Common simulation matrix with simulations in rows.
#' @param alternative Two-sided, greater, or less max-T family.
#'
#' @return An `ngeo_support_adjustment`.
#' @templateVar example_call ngeo_support_adjust(p_values, support_family)
#' @template stable-neuroimaging-method
#' @export
ngeo_support_adjust <- function(
    p_value = NULL,
    method = c("BH", "BY", "holm", "maxT"),
    observed = NULL,
    simulated = NULL,
    alternative = c("two.sided", "greater", "less")) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  if (!identical(method, "maxT")) {
    if (!is.numeric(p_value) || !length(p_value) ||
        anyNA(p_value) || any(!is.finite(p_value)) ||
        any(p_value < 0 | p_value > 1)) {
      .ngeo_abort(
        "`p_value` must contain finite probabilities.",
        "ngeo_error_inference"
      )
    }
    adjusted <- stats::p.adjust(p_value, method = method)
    raw <- p_value
    simulations <- NULL
  } else {
    if (!is.numeric(observed) || !length(observed) ||
        anyNA(observed) || any(!is.finite(observed)) ||
        !is.matrix(simulated) || !is.numeric(simulated) ||
        ncol(simulated) != length(observed) ||
        anyNA(simulated) || any(!is.finite(simulated))) {
      .ngeo_abort(
        "max-T requires finite observed statistics and a common simulation matrix.",
        "ngeo_error_inference"
      )
    }
    transformed_observed <- switch(
      alternative,
      two.sided = abs(observed),
      greater = observed,
      less = -observed
    )
    transformed_simulated <- switch(
      alternative,
      two.sided = abs(simulated),
      greater = simulated,
      less = -simulated
    )
    maximum <- apply(transformed_simulated, 1L, max)
    adjusted <- vapply(transformed_observed, function(value) {
      (1 + sum(maximum >= value)) / (nrow(simulated) + 1)
    }, numeric(1))
    raw <- vapply(seq_along(observed), function(i) {
      (1 + sum(
        transformed_simulated[, i] >= transformed_observed[[i]]
      )) / (nrow(simulated) + 1)
    }, numeric(1))
    simulations <- nrow(simulated)
  }
  result <- list(
    raw = raw,
    adjusted = adjusted,
    method = method,
    alternative = alternative,
    simulations = simulations
  )
  class(result) <- "ngeo_support_adjustment"
  result
}

#' @export
print.ngeo_support_adjustment <- function(x, ...) {
  cat(
    "<ngeo_support_adjustment>\n",
    "  method: ", x$method, "\n",
    "  tests: ", length(x$adjusted), "\n",
    "  minimum adjusted p: ",
    format(min(x$adjusted), digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}

.ngeo_consensus_inputs <- function(x, standard_error, labels) {
  if (inherits(x, "ngeo_atlas_robust_effect")) {
    labels <- x$estimates$atlas
    standard_error <- x$estimates$standard_error
    hashes <- x$estimates$support_map_hash
    estimate <- x$estimates$estimate
  } else if (is.data.frame(x) &&
      all(c("estimate", "standard_error") %in% names(x))) {
    estimate <- x$estimate
    standard_error <- x$standard_error
    labels <- labels %||% x$atlas %||% rownames(x)
    hashes <- x$support_map_hash %||%
      rep.int(NA_character_, length(estimate))
  } else {
    estimate <- x
    hashes <- rep.int(NA_character_, length(estimate))
  }
  if (!is.numeric(estimate) || length(estimate) < 2L ||
      anyNA(estimate) || any(!is.finite(estimate)) ||
      !is.numeric(standard_error) ||
      length(standard_error) != length(estimate) ||
      anyNA(standard_error) || any(!is.finite(standard_error)) ||
      any(standard_error <= 0)) {
    .ngeo_abort(
      "Consensus requires at least two finite effects with positive standard errors.",
      "ngeo_error_inference"
    )
  }
  if (is.null(labels)) {
    labels <- paste0("atlas_", seq_along(estimate))
  }
  if (length(labels) != length(estimate) ||
      anyNA(labels) || any(!nzchar(as.character(labels)))) {
    .ngeo_abort(
      "`labels` must identify every atlas effect.",
      "ngeo_error_alignment"
    )
  }
  list(
    estimate = as.numeric(estimate),
    standard_error = as.numeric(standard_error),
    labels = as.character(labels),
    hashes = hashes
  )
}

.ngeo_meta_estimate <- function(estimate, standard_error, method) {
  if (length(estimate) == 1L) {
    statistic <- estimate / standard_error
    return(list(
      estimate = unname(estimate),
      standard_error = unname(standard_error),
      statistic = unname(statistic),
      p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      q = 0,
      q_p_value = NA_real_,
      i_squared = 0,
      tau_squared = 0,
      spatial_weights = 1,
      df = 0L
    ))
  }

  variance <- standard_error^2
  fixed_weight <- 1 / variance
  fixed <- sum(fixed_weight * estimate) / sum(fixed_weight)
  q <- sum(fixed_weight * (estimate - fixed)^2)
  df <- length(estimate) - 1L
  c_value <- sum(fixed_weight) -
    sum(fixed_weight^2) / sum(fixed_weight)
  tau_squared <- if (c_value > 0) {
    max(0, (q - df) / c_value)
  } else {
    0
  }
  weight <- if (identical(method, "fixed")) {
    fixed_weight
  } else {
    1 / (variance + tau_squared)
  }
  pooled <- sum(weight * estimate) / sum(weight)
  pooled_se <- sqrt(1 / sum(weight))
  list(
    estimate = pooled,
    standard_error = pooled_se,
    statistic = pooled / pooled_se,
    p_value = 2 * stats::pnorm(-abs(pooled / pooled_se)),
    q = q,
    q_p_value = stats::pchisq(q, df = df, lower.tail = FALSE),
    i_squared = if (q > 0) max(0, (q - df) / q) else 0,
    tau_squared = tau_squared,
    spatial_weights = weight / sum(weight),
    df = df
  )
}

.ngeo_descriptive_consensus <- function(estimate, standard_error) {
  weight <- 1 / standard_error^2
  list(
    estimate = sum(weight * estimate) / sum(weight),
    standard_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    q = NA_real_,
    q_p_value = NA_real_,
    i_squared = NA_real_,
    tau_squared = NA_real_,
    spatial_weights = weight / sum(weight),
    df = length(estimate) - 1L
  )
}

.ngeo_consensus_covariance <- function(covariance, standard_error, labels) {
  n <- length(standard_error)
  if (!is.matrix(covariance) || !is.numeric(covariance) ||
      !identical(dim(covariance), c(n, n)) || anyNA(covariance) ||
      any(!is.finite(covariance))) {
    .ngeo_abort(
      "`covariance` must be a finite square matrix aligned to the atlas effects.",
      "ngeo_error_inference"
    )
  }
  scale <- max(1, max(abs(covariance)))
  if (max(abs(covariance - t(covariance))) > 1e-10 * scale) {
    .ngeo_abort(
      "`covariance` must be symmetric.",
      "ngeo_error_inference"
    )
  }
  covariance_names <- dimnames(covariance)
  if (!is.null(covariance_names) &&
      (!identical(covariance_names[[1L]], labels) ||
       !identical(covariance_names[[2L]], labels))) {
    .ngeo_abort(
      "Covariance row and column names must match the ordered atlas labels.",
      "ngeo_error_alignment"
    )
  }
  if (max(abs(diag(covariance) - standard_error^2)) > 1e-8 * scale) {
    .ngeo_abort(
      "The covariance diagonal must equal `standard_error^2`.",
      "ngeo_error_inference"
    )
  }
  factor <- tryCatch(chol(covariance), error = function(error) NULL)
  if (is.null(factor)) {
    .ngeo_abort(
      "`covariance` must be positive definite.",
      "ngeo_error_inference"
    )
  }
  covariance
}

.ngeo_gls_consensus <- function(estimate, covariance) {
  precision <- chol2inv(chol(covariance))
  one <- rep.int(1, length(estimate))
  denominator <- as.numeric(crossprod(one, precision %*% one))
  weight <- as.numeric(precision %*% one) / denominator
  pooled <- sum(weight * estimate)
  pooled_se <- sqrt(1 / denominator)
  residual <- estimate - pooled
  q <- as.numeric(crossprod(residual, precision %*% residual))
  df <- length(estimate) - 1L
  list(
    estimate = pooled,
    standard_error = pooled_se,
    statistic = pooled / pooled_se,
    p_value = 2 * stats::pnorm(abs(pooled / pooled_se), lower.tail = FALSE),
    q = q,
    q_p_value = if (df > 0L) {
      stats::pchisq(q, df = df, lower.tail = FALSE)
    } else {
      NA_real_
    },
    i_squared = if (q > 0) max(0, (q - df) / q) else 0,
    tau_squared = 0,
    spatial_weights = weight,
    df = df
  )
}

.ngeo_cross_atlas_setup <- function(
    x,
    standard_error,
    labels,
    confidence,
    covariance,
    independence,
    method,
    method_missing) {
  if (!is.logical(independence) || length(independence) != 1L ||
      is.na(independence)) {
    .ngeo_abort(
      "`independence` must be one non-missing logical value.",
      "ngeo_error_argument"
    )
  }
  if (!is.null(covariance) && isTRUE(independence)) {
    .ngeo_abort(
      "Supply `covariance` or authorize `independence`, not both.",
      "ngeo_error_inference"
    )
  }
  if (!is.null(covariance) && is.null(standard_error) && is.numeric(x)) {
    if (!is.matrix(covariance) || !is.numeric(covariance) ||
        nrow(covariance) != ncol(covariance)) {
      .ngeo_abort(
        "`covariance` must be a finite square matrix aligned to the atlas effects.",
        "ngeo_error_inference"
      )
    }
    covariance_diagonal <- diag(covariance)
    if (anyNA(covariance_diagonal) ||
        any(!is.finite(covariance_diagonal)) ||
        any(covariance_diagonal <= 0)) {
      .ngeo_abort(
        "The covariance diagonal must contain finite positive variances.",
        "ngeo_error_inference"
      )
    }
    standard_error <- sqrt(covariance_diagonal)
  }
  input <- .ngeo_consensus_inputs(x, standard_error, labels)
  if (!is.numeric(confidence) || length(confidence) != 1L ||
      is.na(confidence) || confidence <= 0 || confidence >= 1) {
    .ngeo_abort(
      "`confidence` must lie strictly between zero and one.",
      "ngeo_error_argument"
    )
  }
  covariance <- if (is.null(covariance)) NULL else
    .ngeo_consensus_covariance(
      covariance, input$standard_error, input$labels
    )
  if (!is.null(covariance) && identical(method, "random") && !method_missing) {
    .ngeo_abort(
      "Correlated random-effects consensus is not implemented; use `method = \"fixed\"`.",
      "ngeo_error_inference"
    )
  }
  if (!is.null(covariance)) method <- "fixed"
  inference_mode <- if (!is.null(covariance)) {
    "covariance-aware"
  } else if (isTRUE(independence)) {
    "independence"
  } else {
    "descriptive"
  }
  list(
    input = input, covariance = covariance, method = method,
    inference_mode = inference_mode
  )
}

.ngeo_fit_cross_atlas_consensus <- function(
    estimate, standard_error, covariance, inference_mode, method) {
  if (identical(inference_mode, "covariance-aware")) {
    .ngeo_gls_consensus(estimate, covariance)
  } else if (identical(inference_mode, "independence")) {
    .ngeo_meta_estimate(estimate, standard_error, method)
  } else {
    .ngeo_descriptive_consensus(estimate, standard_error)
  }
}

.ngeo_cross_atlas_leave_one_out <- function(
    input, covariance, inference_mode, method, estimate) {
  do.call(rbind, lapply(seq_along(input$estimate), function(i) {
    current <- .ngeo_fit_cross_atlas_consensus(
      input$estimate[-i], input$standard_error[-i],
      if (is.null(covariance)) NULL else covariance[-i, -i, drop = FALSE],
      inference_mode, method
    )
    data.frame(
      omitted = input$labels[[i]], estimate = current$estimate,
      standard_error = current$standard_error,
      change = current$estimate - estimate, stringsAsFactors = FALSE
    )
  }))
}

#' Summarize cross-atlas effects with explicit dependence assumptions
#'
#' Consensus summarizes declared atlas-specific effects. By default it is
#' descriptive: the function reports no interval or p-value until the caller
#' supplies an atlas covariance matrix or explicitly authorizes independence.
#' This is not a claim of local parcellation invariance.
#'
#' @param x Effect vector, estimates data frame, or
#'   `ngeo_atlas_robust_effect`.
#' @param standard_error Positive effect standard errors.
#' @param method Fixed or DerSimonian-Laird random effects. Random effects are
#'   available only for the explicit independence analysis.
#' @param labels Optional atlas labels.
#' @param confidence Confidence level.
#' @param covariance Optional positive-definite covariance matrix for the
#'   aligned atlas estimates. Its diagonal must equal `standard_error^2`; any
#'   row and column names must match the ordered atlas labels.
#' @param independence Explicit authorization for the conventional
#'   independence analysis when `covariance` is not supplied. The safe default
#'   returns a descriptive consensus without an interval or p-value.
#'
#' @return An `ngeo_cross_atlas_consensus` with `inference_mode`, the pooled
#'   estimate, atlas weights, dependence assumptions, and inferential fields
#'   only when their assumptions were explicitly supplied.
#' @templateVar example_call ngeo_cross_atlas_consensus(atlas_results)
#' @template stable-neuroimaging-method
#' @export
ngeo_cross_atlas_consensus <- function(
    x,
    standard_error = NULL,
    method = c("random", "fixed"),
    labels = NULL,
    confidence = 0.95,
    covariance = NULL,
    independence = FALSE) {
  method_missing <- missing(method)
  method <- match.arg(method)
  setup <- .ngeo_cross_atlas_setup(
    x, standard_error, labels, confidence, covariance, independence,
    method, method_missing
  )
  input <- setup$input
  covariance <- setup$covariance
  method <- setup$method
  inference_mode <- setup$inference_mode
  fit <- .ngeo_fit_cross_atlas_consensus(
    input$estimate, input$standard_error, covariance, inference_mode, method
  )
  critical <- stats::qnorm(1 - (1 - confidence) / 2)
  leave_one_out <- .ngeo_cross_atlas_leave_one_out(
    input, covariance, inference_mode, method, fit$estimate
  )
  result <- list(
    estimate = fit$estimate,
    standard_error = fit$standard_error,
    confidence_interval = if (is.finite(fit$standard_error)) {
      c(
        fit$estimate - critical * fit$standard_error,
        fit$estimate + critical * fit$standard_error
      )
    } else {
      c(NA_real_, NA_real_)
    },
    statistic = fit$statistic,
    p_value = fit$p_value,
    method = if (identical(inference_mode, "descriptive")) {
      "descriptive"
    } else {
      method
    },
    inference_mode = inference_mode,
    covariance = covariance,
    null_model = if (identical(inference_mode, "covariance-aware")) {
      "normal GLS with supplied atlas covariance"
    } else if (identical(inference_mode, "independence")) {
      "normal independent-estimate meta-analysis approximation"
    } else {
      "not applicable for descriptive consensus"
    },
    uncertainty_target = if (identical(inference_mode, "covariance-aware")) {
      "pooled effect under supplied atlas covariance"
    } else if (identical(inference_mode, "independence")) {
      "approximate meta-analytic standard error under explicit independence"
    } else {
      "not applicable for descriptive consensus"
    },
    assumptions = if (identical(inference_mode, "covariance-aware")) {
      "The supplied covariance captures dependence among aligned atlas estimates."
    } else if (identical(inference_mode, "independence")) {
      "Atlas estimates are explicitly assumed independent."
    } else {
      "Descriptive marginal-precision weighting; no inferential independence claim."
    },
    confidence = confidence,
    heterogeneity = list(
      q = fit$q,
      df = fit$df,
      p_value = fit$q_p_value,
      i_squared = fit$i_squared,
      tau_squared = fit$tau_squared
    ),
    atlas = data.frame(
      atlas = input$labels,
      estimate = input$estimate,
      standard_error = input$standard_error,
      weight = fit$spatial_weights,
      support_map_hash = input$hashes,
      stringsAsFactors = FALSE
    ),
    leave_one_out = leave_one_out,
    claim = paste0(
      "cross-atlas consensus; ", inference_mode,
      "; not parcellation invariance"
    )
  )
  class(result) <- "ngeo_cross_atlas_consensus"
  result
}

#' @export
print.ngeo_cross_atlas_consensus <- function(x, ...) {
  cat(
    "<ngeo_cross_atlas_consensus>\n",
    "  method: ", x$method, "\n",
    "  inference: ", x$inference_mode, "\n",
    "  atlases: ", nrow(x$atlas), "\n",
    "  estimate: ", format(x$estimate, digits = 5L), "\n",
    "  I-squared: ",
    if (is.finite(x$heterogeneity$i_squared)) {
      paste0(format(100 * x$heterogeneity$i_squared, digits = 4L), "%")
    } else {
      "not estimated"
    }, "\n",
    sep = ""
  )
  invisible(x)
}

.ngeo_common_support_statistic <- function(
    x,
    support_maps,
    targets,
    selected,
    statistic) {
  vapply(seq_along(support_maps), function(i) {
    changed <- aggregate_to(
      x,
      targets[[i]],
      support_maps[[i]],
      layers = unname(selected)
    )
    if (identical(statistic, "correlation")) {
      value <- stats::cor(changed$values[, 1L], changed$values[, 2L])
      if (!is.finite(value)) {
        .ngeo_abort(
          "A target-level correlation is undefined.",
          "ngeo_error_inference"
        )
      }
      value
    } else {
      support <- support_maps[[i]]$target_support %||%
        as.numeric(
          support_maps[[i]]$operator %*%
            (support_maps[[i]]$source_support %||%
              rep.int(1, ncol(support_maps[[i]]$operator)))
        )
      .ngeo_fit_support_effect(
        changed$values,
        support
      )[["estimate"]]
    }
  }, numeric(1))
}

#' Test effects under one common source-base null
#'
#' @inheritParams ngeo_support_test
#' @param null Permutation, Moran spectral, or surface-spin null.
#' @param spatial_weights Matching spatial_weights for a Moran spectral null.
#' @param coordinates Registration coordinate set for a spin null.
#' @param strata Optional spin strata.
#' @param workers Simulation workers.
#' @param adjustment BH, BY, Holm, or max-T.
#'
#' @return An `ngeo_common_support_test`.
#' @param experimental_null Must be `TRUE` when `null` is `"moran"` or
#'   `"spin"`. These spatial surrogates remain uncalibrated.
#' @templateVar example_call ngeo_common_support_test(subject_features, support_maps)
#' @template stable-neuroimaging-method
#' @export
ngeo_common_support_test <- function(
    x,
    support_maps,
    targets,
    outcome,
    predictor,
    statistic = c("correlation", "slope"),
    null = c("permutation", "moran", "spin"),
    spatial_weights = NULL,
    coordinates = NULL,
    strata = NULL,
    nsim = 999L,
    seed = NULL,
    workers = 1L,
    experimental_null = FALSE,
    adjustment = c("maxT", "BH", "BY", "holm")) {
  ngeo_validate(x, "strict")
  statistic <- match.arg(statistic)
  null <- match.arg(null)
  adjustment <- match.arg(adjustment)
  selected <- .ngeo_support_model_maps(x, outcome, predictor)
  nsim <- .ngeo_nsim(nsim)
  workers <- .ngeo_workers(workers)
  if (inherits(targets, "ngeo")) {
    targets <- rep(list(targets), length(support_maps))
  }
  if (!is.list(support_maps) || !length(support_maps) ||
      !is.list(targets) ||
      length(targets) != length(support_maps)) {
    .ngeo_abort(
      "`support_maps` and `targets` must be aligned non-empty lists.",
      "ngeo_error_alignment"
    )
  }
  for (i in seq_along(support_maps)) {
    .ngeo_validate_support_bases(
      x, targets[[i]], support_maps[[i]]
    )
    if (!identical(support_maps[[i]]$coverage, "complete")) {
      .ngeo_abort(
        "Common-support inference requires complete support layers.",
        "ngeo_error_coverage"
      )
    }
  }
  observed <- .ngeo_common_support_statistic(
    x, support_maps, targets, selected, statistic
  )
  predictor_values <- x$values[, selected[["predictor"]]]
  null_values <- switch(
    null,
    permutation = .ngeo_with_seed(seed, function() {
      replicate(nsim, sample(predictor_values))
    }),
    moran = {
      if (is.null(spatial_weights)) {
        .ngeo_abort(
          "A Moran null requires matching `spatial_weights`.",
          "ngeo_error_argument"
        )
      }
      ngeo_moran_null(
        x,
        spatial_weights,
        layer = selected[["predictor"]],
        nsim = nsim,
        seed = seed,
        workers = workers,
        experimental = experimental_null
      )$simulations
    },
    spin = ngeo_spin_null(
      x,
      layer = selected[["predictor"]],
      coordinates = coordinates,
      nsim = nsim,
      seed = seed,
      strata = strata,
      workers = workers,
      experimental = experimental_null
    )$simulations
  )
  if (!identical(dim(null_values), c(nrow(x$values), nsim))) {
    .ngeo_abort(
      "The common-source null does not align with source elements.",
      "ngeo_error_alignment"
    )
  }
  simulated <- matrix(
    NA_real_,
    nrow = nsim,
    ncol = length(support_maps)
  )
  for (simulation in seq_len(nsim)) {
    current <- x
    current$values[, selected[["predictor"]]] <-
      null_values[, simulation]
    simulated[simulation, ] <- .ngeo_common_support_statistic(
      current, support_maps, targets, selected, statistic
    )
  }
  raw <- vapply(seq_along(observed), function(i) {
    (1 + sum(abs(simulated[, i]) >= abs(observed[[i]]))) /
      (nsim + 1)
  }, numeric(1))
  adjusted <- if (identical(adjustment, "maxT")) {
    ngeo_support_adjust(
      method = "maxT",
      observed = observed,
      simulated = simulated
    )$adjusted
  } else {
    ngeo_support_adjust(raw, method = adjustment)$adjusted
  }
  atlas <- names(support_maps)
  if (is.null(atlas) || any(!nzchar(atlas))) {
    atlas <- paste0("atlas_", seq_along(support_maps))
  }
  estimates <- data.frame(
    atlas = atlas,
    statistic = observed,
    p_value = raw,
    adjusted_p_value = adjusted,
    support_map_hash = vapply(
      support_maps, ngeo_support_map_hash, character(1)
    ),
    stringsAsFactors = FALSE
  )
  result <- list(
    estimates = estimates,
    simulated = simulated,
    statistic = statistic,
    null = null,
    null_status = if (null %in% c("moran", "spin")) {
      "experimental_uncalibrated"
    } else {
      "stable"
    },
    preserves_spatial_autocorrelation = FALSE,
    adjustment = adjustment,
    nsim = nsim,
    seed = .ngeo_seed(seed),
    workers = workers,
    source_base_hash = base_hash(x),
    support_map_hashes = estimates$support_map_hash,
    claim = paste(
      "common-source family inference;",
      "not local parcellation invariance"
    )
  )
  class(result) <- c(
    "ngeo_common_support_test",
    "ngeo_support_test"
  )
  result
}

#' @export
print.ngeo_common_support_test <- function(x, ...) {
  cat(
    "<ngeo_common_support_test>\n",
    "  null: ", x$null, "\n",
    "  statistic: ", x$statistic, "\n",
    "  atlases: ", nrow(x$estimates), "\n",
    "  adjustment: ", x$adjustment, "\n",
    "  preserves spatial autocorrelation: ",
    x$preserves_spatial_autocorrelation, "\n",
    sep = ""
  )
  invisible(x)
}

#' Inference over a declared support scale hierarchy
#'
#' @inheritParams ngeo_common_support_test
#' @param scales Unique ordered scale labels aligned with support layers.
#'
#' @return An `ngeo_multiscale_inference`.
#' @templateVar example_call ngeo_multiscale_inference(scale_results, family = "support")
#' @template stable-inference-core
#' @export
ngeo_multiscale_inference <- function(
    x,
    support_maps,
    targets,
    scales,
    outcome,
    predictor,
    statistic = c("correlation", "slope"),
    null = c("permutation", "moran", "spin"),
    spatial_weights = NULL,
    coordinates = NULL,
    strata = NULL,
    nsim = 999L,
    seed = NULL,
    workers = 1L,
    experimental_null = FALSE,
    adjustment = c("maxT", "BH", "BY", "holm")) {
  if (length(scales) != length(support_maps) ||
      anyNA(scales) || anyDuplicated(scales)) {
    .ngeo_abort(
      "`scales` must uniquely label the declared support hierarchy.",
      "ngeo_error_alignment"
    )
  }
  test <- ngeo_common_support_test(
    x,
    support_maps,
    targets,
    outcome,
    predictor,
    statistic = match.arg(statistic),
    null = match.arg(null),
    spatial_weights = spatial_weights,
    coordinates = coordinates,
    strata = strata,
    nsim = nsim,
    seed = seed,
    workers = workers,
    experimental_null = experimental_null,
    adjustment = match.arg(adjustment)
  )
  estimates <- test$estimates
  estimates$scale <- scales
  estimates$scale_order <- seq_along(scales)
  adjacent_change <- if (nrow(estimates) > 1L) {
    data.frame(
      from = scales[-length(scales)],
      to = scales[-1L],
      change = diff(estimates$statistic),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      from = scales[FALSE],
      to = scales[FALSE],
      change = numeric()
    )
  }
  result <- list(
    estimates = estimates,
    adjacent_change = adjacent_change,
    stability = c(
      range = diff(range(estimates$statistic)),
      standard_deviation = stats::sd(estimates$statistic),
      maximum_adjacent_change = if (nrow(adjacent_change)) {
        max(abs(adjacent_change$change))
      } else {
        0
      }
    ),
    common_test = test,
    hierarchy = "caller-declared ordered scales",
    claim = "multiscale support sensitivity; not scale invariance"
  )
  class(result) <- "ngeo_multiscale_inference"
  result
}

#' @export
print.ngeo_multiscale_inference <- function(x, ...) {
  cat(
    "<ngeo_multiscale_inference>\n",
    "  scales: ", nrow(x$estimates), "\n",
    "  statistic range: ",
    format(x$stability[["range"]], digits = 5L), "\n",
    "  hierarchy: ", x$hierarchy, "\n",
    sep = ""
  )
  invisible(x)
}

#' Test effect dispersion across alternative boundaries
#'
#' @param x Source `ngeo` dataset.
#' @param ensemble Segmentation or operator ensemble with a common target.
#' @param target Common target template.
#' @param outcome Outcome map.
#' @param predictor Predictor map.
#' @param nsim Common-source permutations.
#' @param seed Reproducible seed.
#'
#' @return An `ngeo_boundary_test`.
#' @templateVar example_call ngeo_boundary_test(effect_map, boundary_family)
#' @template stable-inference-core
#' @export
ngeo_boundary_test <- function(
    x,
    ensemble,
    target,
    outcome,
    predictor,
    nsim = 999L,
    seed = NULL) {
  ngeo_validate_support_ensemble(ensemble)
  selected <- .ngeo_support_model_maps(x, outcome, predictor)
  layers <- ensemble$layers
  targets <- rep(list(target), length(layers))
  observed_effect <- .ngeo_common_support_statistic(
    x, layers, targets, selected, "slope"
  )
  observed_dispersion <- diff(range(observed_effect))
  simulated <- .ngeo_with_seed(seed, function() {
    vapply(seq_len(.ngeo_nsim(nsim)), function(...) {
      current <- x
      current$values[, selected[["predictor"]]] <-
        sample(x$values[, selected[["predictor"]]])
      effect <- .ngeo_common_support_statistic(
        current, layers, targets, selected, "slope"
      )
      diff(range(effect))
    }, numeric(1))
  })
  boundary <- ngeo_boundary_sensitivity(layers)
  result <- list(
    effects = data.frame(
      support_map_hash = ensemble$map_hashes,
      estimate = observed_effect,
      weight = ensemble$spatial_weights,
      stringsAsFactors = FALSE
    ),
    observed_dispersion = observed_dispersion,
    simulated_dispersion = simulated,
    p_value = (
      1 + sum(simulated >= observed_dispersion)
    ) / (length(simulated) + 1),
    boundary_sensitivity = boundary,
    ensemble_hash = ensemble$ensemble_hash,
    nsim = .ngeo_nsim(nsim),
    seed = .ngeo_seed(seed),
    null = "common-source unconstrained permutation",
    claim = "boundary sensitivity; not boundary-invariant inference"
  )
  class(result) <- "ngeo_boundary_test"
  result
}

#' @export
print.ngeo_boundary_test <- function(x, ...) {
  cat(
    "<ngeo_boundary_test>\n",
    "  alternatives: ", nrow(x$effects), "\n",
    "  effect dispersion: ",
    format(x$observed_dispersion, digits = 5L), "\n",
    "  p-value: ", format(x$p_value, digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}
