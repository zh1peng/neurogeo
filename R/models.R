.ngeo_model_maps <- function(x, response, predictors) {
  ngeo_validate(x, "strict")
  if (is.null(x$values)) {
    .ngeo_abort(
      "Spatial models require loaded values.",
      "ngeo_error_values"
    )
  }
  response_index <- .ngeo_layer_selection(x, response)
  predictor_index <- if (length(predictors)) {
    .ngeo_layer_selection(x, predictors)
  } else {
    integer()
  }
  if (length(response_index) != 1L ||
      anyDuplicated(c(response_index, predictor_index))) {
    .ngeo_abort(
      "Select one response and distinct predictor layers.",
      "ngeo_error_argument"
    )
  }
  selected <- c(response_index, predictor_index)
  if (any(
    .ngeo_measures_for_layers(x, selected)$support_behavior == "categorical"
  )) {
    .ngeo_abort(
      "Categorical layers cannot enter a numeric spatial model.",
      "ngeo_error_measure"
    )
  }
  list(
    response = response_index,
    predictors = predictor_index,
    response_name = x$layers$name[[response_index]],
    predictor_names = x$layers$name[predictor_index]
  )
}

.ngeo_model_weights <- function(x, spatial_weights, index, zero_policy) {
  if (is.null(spatial_weights)) {
    return(NULL)
  }
  if (!inherits(spatial_weights, "ngeo_spatial_weights")) {
    .ngeo_abort(
      "`spatial_weights` must be an `ngeo_spatial_weights` object.",
      "ngeo_error_argument"
    )
  }
  if (!identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort(
      "Weights base hash does not match the dataset.",
      "ngeo_error_base_mismatch"
    )
  }
  raw <- .ngeo_as_dgCMatrix(
    spatial_weights$raw_matrix[index, index, drop = FALSE]
  )
  matrix <- switch(
    spatial_weights$normalization,
    W = .ngeo_row_standardize(raw),
    B = .ngeo_binary(raw),
    none = raw
  )
  isolated <- Matrix::rowSums(abs(matrix)) == 0
  if (any(isolated) && !isTRUE(zero_policy)) {
    .ngeo_abort(
      "Model spatial_weights contain isolates; set `zero_policy = TRUE` to retain them.",
      "ngeo_error_zero_policy"
    )
  }
  matrix
}

#' Fit an aligned OLS or spatial-lag-of-X model
#'
#' This foundational adapter fits OLS or SLX (spatial lags of predictors).
#' It does not estimate SAR lag/error parameters. Residual Moran's I is
#' reported when spatial_weights are supplied.
#'
#' @param x An `ngeo` dataset.
#' @param response One response map.
#' @param predictors Predictor layers.
#' @param spatial_weights Optional matching `ngeo_spatial_weights`.
#' @param model `"ols"` or spatial-lag-of-X `"slx"`.
#' @param na_action Whether to fail or omit incomplete rows.
#' @param zero_policy Whether to retain isolates.
#'
#' @return An `ngeo_spatial_lm` object.
#' @examples
#' coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
#' predictor <- coordinates[, 1] + coordinates[, 2]
#' data <- ngeo_point(
#'   coordinates,
#'   values = cbind(
#'     response = 1 + 2 * predictor + rep(c(-0.1, 0, 0.1), 3),
#'     predictor = predictor
#'   )
#' )
#' spatial_weights <- ngeo_spatial_weights(
#'   data, method = "knn", k = 4, symmetry = "union"
#' )
#' ngeo_spatial_lm(
#'   data, "response", "predictor", spatial_weights, model = "slx"
#' )
#' @template stable-statistical-method
#' @export
ngeo_spatial_lm <- function(
    x,
    response,
    predictors = character(),
    spatial_weights = NULL,
    model = c("ols", "slx"),
    na_action = c("fail", "omit"),
    zero_policy = FALSE) {
  model <- match.arg(model)
  na_action <- match.arg(na_action)
  layers <- .ngeo_model_maps(x, response, predictors)
  response_values <- as.numeric(x$values[, layers$response])
  predictor_values <- if (length(layers$predictors)) {
    x$values[, layers$predictors, drop = FALSE]
  } else {
    matrix(numeric(), nrow = nrow(x$base$elements), ncol = 0L)
  }
  finite <- is.finite(response_values)
  if (ncol(predictor_values)) {
    finite <- finite & apply(is.finite(predictor_values), 1L, all)
  }
  if (identical(na_action, "fail") && !all(finite)) {
    .ngeo_abort(
      "Model layers contain missing or non-finite values.",
      "ngeo_error_missing"
    )
  }
  index <- which(finite)
  if (length(index) < 3L || stats::var(response_values[index]) == 0) {
    .ngeo_abort(
      "The response requires at least three finite, variable observations.",
      "ngeo_error_model"
    )
  }
  matrix <- .ngeo_model_weights(x, spatial_weights, index, zero_policy)
  if (identical(model, "slx") && is.null(matrix)) {
    .ngeo_abort(
      "An SLX model requires matching spatial weights.",
      "ngeo_error_weights"
    )
  }
  design <- cbind(
    `(Intercept)` = 1,
    predictor_values[index, , drop = FALSE]
  )
  colnames(design) <- c("(Intercept)", layers$predictor_names)
  if (identical(model, "slx") && length(layers$predictors)) {
    lagged <- as.matrix(matrix %*% predictor_values[index, , drop = FALSE])
    colnames(lagged) <- paste0("lag_", layers$predictor_names)
    design <- cbind(design, lagged)
  }
  if (nrow(design) <= ncol(design)) {
    .ngeo_abort(
      "The model needs more complete observations than coefficients.",
      "ngeo_error_model"
    )
  }
  fit <- stats::lm.fit(design, response_values[index])
  if (fit$rank < ncol(design)) {
    .ngeo_abort(
      "The spatial model design is rank deficient.",
      "ngeo_error_model"
    )
  }
  residual_df <- nrow(design) - fit$rank
  sigma2 <- sum(fit$residuals^2) / residual_df
  unscaled <- chol2inv(qr.R(fit$qr))
  covariance <- matrix(NA_real_, ncol(design), ncol(design))
  covariance[fit$qr$pivot, fit$qr$pivot] <- unscaled
  dimnames(covariance) <- list(colnames(design), colnames(design))
  covariance <- sigma2 * covariance
  standard_error <- sqrt(diag(covariance))
  statistic <- fit$coefficients / standard_error
  p_value <- 2 * stats::pt(
    abs(statistic),
    df = residual_df,
    lower.tail = FALSE
  )
  total <- sum(
    (response_values[index] - mean(response_values[index]))^2
  )
  residual_moran <- if (is.null(matrix) ||
      stats::var(fit$residuals) == 0 ||
      sum(matrix) == 0) {
    NA_real_
  } else {
    .ngeo_moran_value(fit$residuals, matrix)
  }
  coefficients <- data.frame(
    term = colnames(design),
    estimate = unname(fit$coefficients),
    std.error = standard_error,
    statistic = unname(statistic),
    p.value = unname(p_value),
    stringsAsFactors = FALSE
  )
  result <- list(
    model = model,
    coefficients = coefficients,
    fitted = as.numeric(fit$fitted.values),
    residuals = as.numeric(fit$residuals),
    element_id = x$base$elements$element_id[index],
    complete_index = index,
    response = layers$response_name,
    predictors = layers$predictor_names,
    r.squared = 1 - sum(fit$residuals^2) / total,
    sigma = sqrt(sigma2),
    df.residual = residual_df,
    residual_moran = residual_moran,
    base_hash = base_hash(x),
    weights_method = if (is.null(spatial_weights)) NULL else spatial_weights$method,
    zero_policy = isTRUE(zero_policy),
    omitted = length(finite) - length(index),
    covariance = covariance
  )
  class(result) <- "ngeo_spatial_lm"
  result
}

.ngeo_kernel_weights <- function(distance, bandwidth, kernel, cutoff) {
  scaled <- distance / bandwidth
  switch(
    kernel,
    gaussian = ifelse(
      is.finite(scaled) & scaled <= cutoff,
      exp(-0.5 * scaled^2),
      0
    ),
    bisquare = ifelse(
      is.finite(scaled) & scaled < 1,
      (1 - scaled^2)^2,
      0
    )
  )
}

#' Fit explicit-bandwidth spatial kernel regressions
#'
#' Local weighted least squares uses the selected NGCS distance_method. Surface
#' defaults to edge geodesic distance. Gaussian kernels are explicitly
#' truncated at `cutoff * bandwidth`; bisquare support is one bandwidth.
#'
#' @param x An `ngeo` dataset.
#' @param response One response map.
#' @param predictors Predictor layers.
#' @param bandwidth Positive distance bandwidth.
#' @param distance_method Explicit NGCS distance_method.
#' @param kernel Gaussian or compact bisquare kernel.
#' @param targets Optional target elements.
#' @param support Multiply kernel spatial_weights by explicit element support.
#' @param cutoff Gaussian truncation multiplier.
#' @param na_action Whether to fail or omit incomplete training rows.
#' @param singular Whether singular local designs return `NA` or fail.
#'
#' @return An `ngeo_kernel_regression` data frame.
#' @export
ngeo_kernel_regression <- function(
    x,
    response,
    predictors = character(),
    bandwidth,
    distance_method = NULL,
    kernel = c("gaussian", "bisquare"),
    targets = NULL,
    support = c("none", "base"),
    cutoff = 3,
    na_action = c("fail", "omit"),
    singular = c("na", "error")) {
  kernel <- match.arg(kernel)
  support <- match.arg(support)
  na_action <- match.arg(na_action)
  singular <- match.arg(singular)
  if (!is.numeric(bandwidth) || length(bandwidth) != 1L ||
      is.na(bandwidth) || !is.finite(bandwidth) || bandwidth <= 0) {
    .ngeo_abort(
      "`bandwidth` must be one positive finite distance.",
      "ngeo_error_argument"
    )
  }
  if (!is.numeric(cutoff) || length(cutoff) != 1L ||
      is.na(cutoff) || !is.finite(cutoff) || cutoff <= 0) {
    .ngeo_abort(
      "`cutoff` must be one positive finite multiplier.",
      "ngeo_error_argument"
    )
  }
  layers <- .ngeo_model_maps(x, response, predictors)
  response_values <- as.numeric(x$values[, layers$response])
  predictor_values <- if (length(layers$predictors)) {
    x$values[, layers$predictors, drop = FALSE]
  } else {
    matrix(numeric(), nrow = nrow(x$base$elements), ncol = 0L)
  }
  finite <- is.finite(response_values)
  if (ncol(predictor_values)) {
    finite <- finite & apply(is.finite(predictor_values), 1L, all)
  }
  if (identical(na_action, "fail") && !all(finite)) {
    .ngeo_abort(
      "Model layers contain missing or non-finite values.",
      "ngeo_error_missing"
    )
  }
  training <- which(finite)
  targets <- if (is.null(targets)) {
    seq_len(nrow(x$base$elements))
  } else {
    .ngeo_element_selection(x, targets)
  }
  maximum <- getOption("neurogeo.max_kernel_targets", 2000L)
  if (length(targets) > maximum) {
    .ngeo_abort(
      sprintf("Kernel regression is limited to %d targets.", maximum),
      "ngeo_error_resource"
    )
  }
  target_predictors <- predictor_values[targets, , drop = FALSE]
  if (ncol(target_predictors) &&
      any(!is.finite(target_predictors))) {
    .ngeo_abort(
      "Target predictor values must be finite.",
      "ngeo_error_missing"
    )
  }
  support_weight <- rep.int(1, length(training))
  if (identical(support, "base")) {
    all_support <- ngeo_support_size(x)
    support_weight <- all_support[training]
    if (any(!is.finite(support_weight)) ||
        any(support_weight <= 0)) {
      .ngeo_abort(
        "Domain support weighting requires positive finite support sizes.",
        "ngeo_error_support"
      )
    }
  }
  design <- cbind(
    `(Intercept)` = 1,
    predictor_values[training, , drop = FALSE]
  )
  colnames(design) <- c("(Intercept)", layers$predictor_names)
  coefficient <- matrix(
    NA_real_,
    nrow = length(targets),
    ncol = ncol(design),
    dimnames = list(NULL, colnames(design))
  )
  fitted <- effective_n <- condition_number <-
    rep.int(NA_real_, length(targets))
  maximum_distance <- bandwidth * if (kernel == "gaussian") cutoff else 1
  for (i in seq_along(targets)) {
    distance <- as.numeric(ngeo_distance(
      x,
      from = targets[[i]],
      to = training,
      distance_method = distance_method,
      max_distance = maximum_distance
    ))
    local_weight <- .ngeo_kernel_weights(
      distance,
      bandwidth,
      kernel,
      cutoff
    ) * support_weight
    selected <- local_weight > 0
    effective_n[[i]] <- if (any(selected)) {
      sum(local_weight)^2 / sum(local_weight^2)
    } else {
      NA_real_
    }
    if (sum(selected) < ncol(design)) {
      if (identical(singular, "error")) {
        .ngeo_abort(
          sprintf(
            "Target `%s` has insufficient local observations.",
            x$base$elements$element_id[targets[[i]]]
          ),
          "ngeo_error_model"
        )
      }
      next
    }
    weighted_design <- design[selected, , drop = FALSE] *
      sqrt(local_weight[selected])
    weighted_response <- response_values[training[selected]] *
      sqrt(local_weight[selected])
    fit <- stats::lm.fit(weighted_design, weighted_response)
    if (fit$rank < ncol(design)) {
      if (identical(singular, "error")) {
        .ngeo_abort(
          "A local kernel design is rank deficient.",
          "ngeo_error_model"
        )
      }
      next
    }
    condition_number[[i]] <- kappa(weighted_design, exact = TRUE)
    coefficient[i, ] <- fit$coefficients
    target_design <- c(1, target_predictors[i, ])
    fitted[[i]] <- sum(target_design * fit$coefficients)
  }
  result <- data.frame(
    element_id = x$base$elements$element_id[targets],
    target_index = targets,
    fitted = fitted,
    effective_n = effective_n,
    condition_number = condition_number,
    coefficient,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(result, "response") <- layers$response_name
  attr(result, "predictors") <- layers$predictor_names
  attr(result, "bandwidth") <- bandwidth
  attr(result, "distance_method") <- .ngeo_metric_name(distance_method %||% switch(
    x$base$type,
    surface = "edge_geodesic",
    volume = "world_euclidean",
    point = "euclidean",
    parcellation = "region_centroid",
    grayordinate = "edge_geodesic"
  ))
  attr(result, "kernel") <- kernel
  attr(result, "cutoff") <- cutoff
  attr(result, "support") <- support
  attr(result, "base_hash") <- base_hash(x)
  class(result) <- c("ngeo_kernel_regression", "data.frame")
  result
}

#' @export
print.ngeo_spatial_lm <- function(x, ...) {
  cat(
    "<ngeo_spatial_lm>\n",
    "  model: ", x$model, "\n",
    "  response: ", x$response, "\n",
    "  observations: ", length(x$fitted), "\n",
    "  R-squared: ", format(x$r.squared, digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_kernel_regression <- function(x, ...) {
  cat(
    "<ngeo_kernel_regression>\n",
    "  targets: ", nrow(x), "\n",
    "  response: ", attr(x, "response"), "\n",
    "  distance_method: ", attr(x, "distance_method"), "\n",
    "  bandwidth: ", attr(x, "bandwidth"), "\n",
    sep = ""
  )
  invisible(x)
}
