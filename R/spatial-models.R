# Spatial prediction and regression models.
.ngeo_variogram_curve <- function(distance, model, nugget, partial_sill, range) {
  ratio <- distance / range
  structure <- switch(
    model,
    spherical = ifelse(
      ratio < 1,
      1.5 * ratio - 0.5 * ratio^3,
      1
    ),
    exponential = 1 - exp(-3 * ratio),
    gaussian = 1 - exp(-3 * ratio^2)
  )
  nugget * (distance > 0) + partial_sill * structure
}

#' Fit a bounded weighted least-squares variogram model
#'
#' @param x An empirical `ngeo_variogram` or an `ngeo` dataset.
#' @param map,metric,breaks,max_distance Passed to `ngeo_variogram()`.
#' @param model Spherical, exponential, or Gaussian model.
#' @param start Optional named nugget, partial-sill, and range start.
#'
#' @return An `ngeo_variogram_fit`.
#' @examples
#' coordinates <- as.matrix(expand.grid(x = 0:4, y = 0:4))
#' signal <- sin(coordinates[, 1] / 2) + cos(coordinates[, 2] / 2)
#' points <- ngeo_points(coordinates, values = cbind(signal = signal))
#' fit <- ngeo_fit_variogram(
#'   points, "signal", breaks = c(0, 1.1, 2.1, 3.1, 4.5, 6),
#'   model = "spherical"
#' )
#' fit
#' @export
ngeo_fit_variogram <- function(
    x,
    map = 1L,
    metric = NULL,
    breaks = 10L,
    max_distance = Inf,
    model = c("spherical", "exponential", "gaussian"),
    start = NULL) {
  model <- match.arg(model)
  empirical <- if (inherits(x, "ngeo_variogram")) {
    x
  } else {
    ngeo_variogram(
      x, map = map, metric = metric, breaks = breaks,
      max_distance = max_distance
    )
  }
  keep <- is.finite(empirical$distance) &
    is.finite(empirical$semivariance) & empirical$n_pairs > 0
  if (sum(keep) < 3L) {
    .ngeo_abort(
      "Variogram fitting requires at least three finite bins.",
      "ngeo_error_model"
    )
  }
  distance <- empirical$distance[keep]
  observed <- empirical$semivariance[keep]
  weight <- empirical$n_pairs[keep] / pmax(observed^2, 1e-12)
  default <- c(
    nugget = max(0, min(observed)),
    partial_sill = max(max(observed) - min(observed), 1e-8),
    range = max(stats::median(distance), 1e-8)
  )
  if (!is.null(start)) {
    if (!is.numeric(start) ||
        !all(c("nugget", "partial_sill", "range") %in% names(start))) {
      .ngeo_abort(
        "`start` must name nugget, partial_sill, and range.",
        "ngeo_error_argument"
      )
    }
    default <- start[c("nugget", "partial_sill", "range")]
  }
  upper <- c(
    max(max(observed) * 5, 1),
    max(max(observed) * 10, 1),
    max(max(distance) * 10, 1e-6)
  )
  fit <- stats::optim(
    default,
    function(parameter) {
      fitted <- .ngeo_variogram_curve(
        distance, model, parameter[[1L]],
        parameter[[2L]], parameter[[3L]]
      )
      sum(weight * (observed - fitted)^2)
    },
    method = "L-BFGS-B",
    lower = c(0, 0, max(min(distance) * 1e-6, 1e-10)),
    upper = upper
  )
  if (fit$convergence != 0L) {
    .ngeo_abort("Variogram optimization did not converge.", "ngeo_error_model")
  }
  names(fit$par) <- c("nugget", "partial_sill", "range")
  result <- list(
    model = model,
    parameters = fit$par,
    objective = fit$value,
    empirical = empirical,
    fitted = .ngeo_variogram_curve(
      empirical$distance, model, fit$par[[1L]],
      fit$par[[2L]], fit$par[[3L]]
    ),
    weights = "n_pairs / semivariance^2",
    convergence = fit$convergence,
    metric = attr(empirical, "metric"),
    domain_hash = attr(empirical, "domain_hash")
  )
  class(result) <- "ngeo_variogram_fit"
  result
}

#' @export
print.ngeo_variogram_fit <- function(x, ...) {
  cat(
    "<ngeo_variogram_fit>\n",
    "  model: ", x$model, "\n",
    "  nugget: ", format(x$parameters[["nugget"]], digits = 5L), "\n",
    "  partial sill: ",
    format(x$parameters[["partial_sill"]], digits = 5L), "\n",
    "  range: ", format(x$parameters[["range"]], digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}

.ngeo_euclidean_matrix <- function(a, b) {
  sqrt(pmax(outer(
    rowSums(a^2), rowSums(b^2), "+"
  ) - 2 * tcrossprod(a, b), 0))
}

.ngeo_variogram_covariance <- function(distance, fit, diagonal = FALSE) {
  parameter <- fit$parameters
  sill <- parameter[["partial_sill"]]
  covariance <- sill - (
    .ngeo_variogram_curve(
      distance, fit$model, 0, sill, parameter[["range"]]
    )
  )
  if (diagonal) {
    diag(covariance) <- sill + parameter[["nugget"]]
  }
  covariance
}

#' Bounded local ordinary or universal kriging
#'
#' @param x An `ngeo` dataset.
#' @param map Response map.
#' @param variogram A fitted `ngeo_variogram_fit`.
#' @param targets Target element selection or coordinate matrix.
#' @param predictors Optional trend predictor maps for universal kriging.
#' @param target_predictors Predictor matrix for external target coordinates.
#' @param neighbors Maximum local observations.
#' @param metric Declared metric; current prediction coordinates must be
#'   Euclidean-eligible.
#'
#' @return An `ngeo_kriging` data frame.
#' @examples
#' coordinates <- as.matrix(expand.grid(x = 0:4, y = 0:4))
#' signal <- sin(coordinates[, 1] / 2) + cos(coordinates[, 2] / 2)
#' points <- ngeo_points(coordinates, values = cbind(signal = signal))
#' fit <- ngeo_fit_variogram(
#'   points, "signal", breaks = c(0, 1.1, 2.1, 3.1, 4.5, 6)
#' )
#' ngeo_kriging(
#'   points, "signal", fit,
#'   targets = matrix(
#'     c(1.5, 1.5, 0, 2.5, 2.5, 0),
#'     ncol = 3, byrow = TRUE
#'   ),
#'   neighbors = 12
#' )
#' @export
ngeo_kriging <- function(
    x,
    map,
    variogram,
    targets = NULL,
    predictors = character(),
    target_predictors = NULL,
    neighbors = 50L,
    metric = NULL) {
  ngeo_validate(x, "strict")
  if (!inherits(variogram, "ngeo_variogram_fit")) {
    .ngeo_abort("`variogram` must be fitted.", "ngeo_error_argument")
  }
  metric <- metric %||% variogram$metric %||% switch(
    x$domain$type,
    surface = "edge_geodesic",
    volume = "world_euclidean",
    points = "euclidean",
    regions = "region_centroid",
    grayordinates = "edge_geodesic"
  )
  metric_name <- .ngeo_metric_name(metric)
  if (!is.null(variogram$metric) &&
      !identical(.ngeo_metric_name(variogram$metric), metric_name)) {
    .ngeo_abort(
      "Kriging metric must match the fitted variogram metric.",
      "ngeo_error_metric"
    )
  }
  neighbors <- .ngeo_as_integer(neighbors, "neighbors")
  if (length(neighbors) != 1L || neighbors < 2L) {
    .ngeo_abort("`neighbors` must be at least two.", "ngeo_error_argument")
  }
  maps <- .ngeo_model_maps(x, map, predictors)
  coordinates <- .ngeo_element_coordinates(x)
  values <- as.numeric(x$values[, maps$response])
  design <- cbind(
    `(Intercept)` = 1,
    x$values[, maps$predictors, drop = FALSE]
  )
  finite <- is.finite(values) & apply(is.finite(design), 1L, all)
  training <- which(finite)
  target_index <- NULL
  target_coordinates <- if (is.null(targets)) {
    target_index <- seq_len(nrow(coordinates))
    coordinates
  } else if (is.matrix(targets) && is.numeric(targets)) {
    if (!metric_name %in% c(
      "euclidean", "world_euclidean", "region_centroid"
    )) {
      .ngeo_abort(
        "External kriging targets require a coordinate metric.",
        "ngeo_error_metric"
      )
    }
    if (ncol(targets) != ncol(coordinates) || any(!is.finite(targets))) {
      .ngeo_abort("Target coordinates do not align.", "ngeo_error_alignment")
    }
    targets
  } else {
    target_index <- .ngeo_element_selection(x, targets)
    coordinates[target_index, , drop = FALSE]
  }
  target_design <- if (!is.null(target_index)) {
    design[target_index, , drop = FALSE]
  } else {
    if (ncol(design) == 1L && is.null(target_predictors)) {
      matrix(1, nrow(target_coordinates), 1L)
    } else {
      candidate <- as.matrix(target_predictors)
      if (nrow(candidate) == nrow(target_coordinates) &&
          ncol(candidate) == ncol(design) - 1L &&
          all(is.finite(candidate))) {
        cbind(1, candidate)
      } else {
        .ngeo_abort(
          "External universal targets require aligned `target_predictors`.",
          "ngeo_error_alignment"
        )
      }
    }
  }
  prediction <- variance <- numeric(nrow(target_coordinates))
  linear_weights <- Matrix::Matrix(
    0,
    nrow = nrow(target_coordinates),
    ncol = nrow(coordinates),
    sparse = TRUE
  )
  used <- integer(nrow(target_coordinates))
  for (i in seq_len(nrow(target_coordinates))) {
    distance <- if (!is.null(target_index)) {
      as.numeric(ngeo_distance(
        x,
        from = target_index[[i]],
        to = training,
        metric = metric_name
      ))
    } else {
      sqrt(rowSums(
        sweep(coordinates[training, , drop = FALSE], 2L,
              target_coordinates[i, ], "-")^2
      ))
    }
    reachable <- which(is.finite(distance))
    if (length(reachable) < 2L) {
      .ngeo_abort(
        "A kriging target has fewer than two reachable observations.",
        "ngeo_error_model"
      )
    }
    selected <- reachable[order(distance[reachable], training[reachable])]
    selected <- selected[seq_len(min(neighbors, length(selected)))]
    local <- training[selected]
    local_distance <- if (!is.null(target_index)) {
      unname(ngeo_distance(
        x,
        from = local,
        to = local,
        metric = metric_name
      ))
    } else {
      .ngeo_euclidean_matrix(
        coordinates[local, , drop = FALSE],
        coordinates[local, , drop = FALSE]
      )
    }
    covariance <- .ngeo_variogram_covariance(
      local_distance, variogram, diagonal = TRUE
    )
    cross_distance <- if (!is.null(target_index)) {
      unname(ngeo_distance(
        x,
        from = local,
        to = target_index[[i]],
        metric = metric_name
      ))
    } else {
      .ngeo_euclidean_matrix(
        coordinates[local, , drop = FALSE],
        target_coordinates[i, , drop = FALSE]
      )
    }
    cross <- .ngeo_variogram_covariance(
      cross_distance, variogram
    )[, 1L]
    local_design <- design[local, , drop = FALSE]
    system <- rbind(
      cbind(covariance, local_design),
      cbind(t(local_design), matrix(0, ncol(design), ncol(design)))
    )
    rhs <- c(cross, target_design[i, ])
    solution <- tryCatch(
      solve(system, rhs),
      error = function(...) NULL
    )
    if (is.null(solution)) {
      .ngeo_abort("A local kriging system is singular.", "ngeo_error_model")
    }
    lambda <- solution[seq_along(local)]
    linear_weights[i, local] <- lambda
    multiplier <- solution[-seq_along(local)]
    prediction[[i]] <- sum(lambda * values[local])
    variance[[i]] <- max(
      0,
        variogram$parameters[["partial_sill"]] +
        variogram$parameters[["nugget"]] -
        sum(lambda * cross) - sum(multiplier * target_design[i, ])
    )
    used[[i]] <- length(local)
  }
  result <- data.frame(
    target = if (is.null(target_index)) seq_len(nrow(target_coordinates)) else
      x$domain$elements$element_id[target_index],
    prediction = prediction,
    variance = variance,
    standard_error = sqrt(variance),
    neighbors = used,
    stringsAsFactors = FALSE
  )
  attr(result, "method") <- if (length(predictors)) "universal" else "ordinary"
  attr(result, "metric") <- metric_name
  attr(result, "domain_hash") <- ngeo_domain_hash(x)
  attr(result, "linear_weights") <- .ngeo_as_dgCMatrix(
    linear_weights
  )
  class(result) <- c("ngeo_kriging", "data.frame")
  result
}

#' Select a GWR bandwidth by leave-one-out or deterministic k-fold CV
#'
#' @param candidates Positive bandwidth candidates.
#' @param cv Leave-one-out or k-fold.
#' @param folds Number of deterministic folds.
#' @inheritParams ngeo_kernel_regression
#'
#' @return An `ngeo_gwr_bandwidth`.
#' @export
ngeo_gwr_bandwidth <- function(
    x,
    response,
    predictors,
    candidates,
    metric = NULL,
    kernel = c("gaussian", "bisquare"),
    cv = c("loo", "kfold"),
    folds = 5L) {
  kernel <- match.arg(kernel)
  cv <- match.arg(cv)
  if (!is.numeric(candidates) || !length(candidates) ||
      anyNA(candidates) || any(!is.finite(candidates)) ||
      any(candidates <= 0)) {
    .ngeo_abort("Bandwidth candidates must be positive.", "ngeo_error_argument")
  }
  maps <- .ngeo_model_maps(x, response, predictors)
  y <- as.numeric(x$values[, maps$response])
  design <- cbind(1, x$values[, maps$predictors, drop = FALSE])
  metric <- metric %||% switch(
    x$domain$type,
    surface = "edge_geodesic",
    volume = "world_euclidean",
    points = "euclidean",
    regions = "region_centroid",
    grayordinates = "edge_geodesic"
  )
  metric_name <- .ngeo_metric_name(metric)
  distances <- unname(ngeo_distance(
    x,
    from = seq_len(nrow(design)),
    to = seq_len(nrow(design)),
    metric = metric_name
  ))
  folds <- if (cv == "loo") seq_len(nrow(design)) else
    ((seq_len(nrow(design)) - 1L) %% .ngeo_as_integer(folds, "folds")) + 1L
  score <- vapply(candidates, function(bandwidth) {
    error <- numeric(nrow(design))
    for (i in seq_len(nrow(design))) {
      holdout <- folds == folds[[i]]
      distance <- distances[i, ]
      weight <- .ngeo_kernel_weights(
        distance, bandwidth, kernel, 3
      )
      weight[holdout] <- 0
      selected <- weight > 0
      if (sum(selected) < ncol(design)) {
        return(Inf)
      }
      weighted <- design[selected, , drop = FALSE] * sqrt(weight[selected])
      fit <- stats::lm.fit(weighted, y[selected] * sqrt(weight[selected]))
      if (fit$rank < ncol(design)) {
        return(Inf)
      }
      error[[i]] <- y[[i]] - sum(design[i, ] * fit$coefficients)
    }
    sqrt(mean(error^2))
  }, numeric(1))
  if (!any(is.finite(score))) {
    .ngeo_abort("Every GWR bandwidth produced singular fits.", "ngeo_error_model")
  }
  result <- list(
    bandwidth = candidates[[which.min(score)]],
    candidates = data.frame(bandwidth = candidates, rmse = score),
    cv = cv,
    folds = if (cv == "loo") nrow(design) else length(unique(folds)),
    metric = metric_name,
    kernel = kernel,
    domain_hash = ngeo_domain_hash(x)
  )
  class(result) <- "ngeo_gwr_bandwidth"
  result
}

#' Fit geographically weighted regression with conditioning diagnostics
#'
#' @inheritParams ngeo_kernel_regression
#' @param bandwidth Positive bandwidth or `ngeo_gwr_bandwidth`.
#'
#' @return An `ngeo_gwr` data frame.
#' @examples
#' coordinates <- as.matrix(expand.grid(x = 0:3, y = 0:3))
#' predictor <- coordinates[, 1] - coordinates[, 2]
#' points <- ngeo_points(
#'   coordinates,
#'   values = cbind(
#'     response = 1 + 2 * predictor + 0.05 * coordinates[, 1],
#'     predictor = predictor
#'   )
#' )
#' head(ngeo_gwr(
#'   points, "response", "predictor",
#'   bandwidth = 2.5, singular = "error"
#' ))
#' @export
ngeo_gwr <- function(
    x,
    response,
    predictors,
    bandwidth,
    metric = NULL,
    kernel = c("gaussian", "bisquare"),
    targets = NULL,
    support = c("none", "domain"),
    singular = c("na", "error")) {
  value <- if (inherits(bandwidth, "ngeo_gwr_bandwidth")) {
    bandwidth$bandwidth
  } else {
    bandwidth
  }
  result <- ngeo_kernel_regression(
    x, response, predictors, value, metric = metric,
    kernel = match.arg(kernel), targets = targets,
    support = match.arg(support), singular = match.arg(singular)
  )
  class(result) <- c("ngeo_gwr", class(result))
  attr(result, "bandwidth_selection") <- if (
    inherits(bandwidth, "ngeo_gwr_bandwidth")
  ) bandwidth else NULL
  result
}

#' @export
print.ngeo_gwr_bandwidth <- function(x, ...) {
  cat("<ngeo_gwr_bandwidth>\n  bandwidth: ", x$bandwidth,
      "\n  CV: ", x$cv, "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_gwr <- function(x, ...) {
  cat("<ngeo_gwr>\n  targets: ", nrow(x), "\n  bandwidth: ",
      attr(x, "bandwidth"), "\n", sep = "")
  invisible(x)
}

.ngeo_spatial_ml <- function(y, design, weight, model) {
  n <- length(y)
  limit <- getOption("neurogeo.max_exact_logdet", 2000L)
  if (n > limit) {
    .ngeo_abort(
      sprintf("Exact spatial likelihood is limited to %d observations.", limit),
      "ngeo_error_resource"
    )
  }
  dense_weight <- as.matrix(weight)
  identity <- diag(n)
  eigenvalue <- eigen(dense_weight, only.values = TRUE)$values
  symmetric <- isTRUE(all.equal(
    dense_weight,
    t(dense_weight),
    tolerance = 1e-10,
    check.attributes = FALSE
  ))
  parameter_interval <- if (symmetric) {
    eigenvalue <- Re(eigenvalue)
    lower <- if (min(eigenvalue) < 0) 1 / min(eigenvalue) else -Inf
    upper <- if (max(eigenvalue) > 0) 1 / max(eigenvalue) else Inf
    c(max(-0.98, lower), min(0.98, upper))
  } else {
    radius <- max(Mod(eigenvalue))
    c(max(-0.98, -1 / radius), min(0.98, 1 / radius))
  }
  margin <- max(
    1e-8,
    sqrt(.Machine$double.eps) *
      max(1, abs(parameter_interval[is.finite(parameter_interval)]))
  )
  parameter_interval <- c(
    parameter_interval[[1L]] + margin,
    parameter_interval[[2L]] - margin
  )
  if (parameter_interval[[1L]] >= parameter_interval[[2L]]) {
    .ngeo_abort(
      "Spatial weights have no usable autoregressive parameter interval.",
      "ngeo_error_model"
    )
  }
  objective <- function(parameter, details = FALSE) {
    transform <- identity - parameter * dense_weight
    if (model == "sar") {
      transformed_y <- transform %*% y
      transformed_design <- design
    } else {
      transformed_y <- transform %*% y
      transformed_design <- transform %*% design
    }
    fit <- stats::lm.fit(transformed_design, transformed_y)
    if (fit$rank < ncol(design)) {
      return(if (details) NULL else Inf)
    }
    sigma2 <- sum(fit$residuals^2) / n
    determinant <- determinant(transform, logarithm = TRUE)
    if (determinant$sign <= 0 || !is.finite(sigma2) || sigma2 <= 0) {
      return(if (details) NULL else Inf)
    }
    negative_log_likelihood <- n / 2 * (
      log(2 * pi) + 1 + log(sigma2)
    ) - as.numeric(determinant$modulus)
    if (!details) {
      return(negative_log_likelihood)
    }
    fitted <- if (model == "sar") {
      solve(transform, design %*% fit$coefficients)
    } else {
      design %*% fit$coefficients
    }
    list(
      parameter = parameter,
      coefficients = as.numeric(fit$coefficients),
      sigma2 = sigma2,
      logLik = -negative_log_likelihood,
      fitted = as.numeric(fitted),
      residuals = as.numeric(y - fitted),
      log_determinant = as.numeric(determinant$modulus),
      parameter_interval = parameter_interval
    )
  }
  optimized <- stats::optimize(
    objective,
    interval = parameter_interval,
    tol = 1e-8
  )
  objective(optimized$minimum, details = TRUE)
}

#' Fit OLS, SLX, SAR, or SEM on one aligned domain
#'
#' SAR and SEM use a bounded exact dense log determinant. They are intended
#' for reference-sized problems; larger inputs fail rather than silently
#' changing approximation.
#'
#' @inheritParams ngeo_spatial_lm
#' @param model OLS, SLX, spatial lag (SAR), or spatial error (SEM).
#'
#' @return An `ngeo_spatial_regression`.
#' @examples
#' coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
#' predictor <- coordinates[, 1] + coordinates[, 2]
#' data <- ngeo_points(
#'   coordinates,
#'   values = cbind(
#'     response = 1 + 1.5 * predictor + rep(c(-0.2, 0, 0.2), 3),
#'     predictor = predictor
#'   )
#' )
#' weights <- ngeo_weights(
#'   data, method = "knn", k = 4, symmetry = "union"
#' )
#' ngeo_spatial_regression(
#'   data, "response", "predictor", weights, model = "sar"
#' )
#' @export
ngeo_spatial_regression <- function(
    x,
    response,
    predictors = character(),
    weights = NULL,
    model = c("ols", "slx", "sar", "sem"),
    na_action = c("fail", "omit"),
    zero_policy = FALSE) {
  model <- match.arg(model)
  if (model %in% c("ols", "slx")) {
    result <- ngeo_spatial_lm(
      x, response, predictors, weights,
      model = model, na_action = match.arg(na_action),
      zero_policy = zero_policy
    )
    class(result) <- c("ngeo_spatial_regression", class(result))
    result$log_determinant_method <- NULL
    return(result)
  }
  if (is.null(weights)) {
    .ngeo_abort("SAR and SEM require matching weights.", "ngeo_error_weights")
  }
  maps <- .ngeo_model_maps(x, response, predictors)
  y <- as.numeric(x$values[, maps$response])
  predictor <- x$values[, maps$predictors, drop = FALSE]
  finite <- is.finite(y)
  if (ncol(predictor)) {
    finite <- finite & apply(is.finite(predictor), 1L, all)
  }
  if (match.arg(na_action) == "fail" && !all(finite)) {
    .ngeo_abort("Model maps contain non-finite values.", "ngeo_error_missing")
  }
  index <- which(finite)
  design <- cbind(`(Intercept)` = 1, predictor[index, , drop = FALSE])
  colnames(design) <- c("(Intercept)", maps$predictor_names)
  if (nrow(design) <= ncol(design)) {
    .ngeo_abort("The spatial design is underpowered.", "ngeo_error_model")
  }
  weight <- .ngeo_model_weights(x, weights, index, zero_policy)
  fit <- .ngeo_spatial_ml(y[index], design, weight, model)
  coefficient <- data.frame(
    term = colnames(design),
    estimate = fit$coefficients,
    stringsAsFactors = FALSE
  )
  result <- list(
    model = model,
    coefficients = coefficient,
    spatial_parameter = fit$parameter,
    parameter_name = if (model == "sar") "rho" else "lambda",
    fitted = fit$fitted,
    residuals = fit$residuals,
    element_id = x$domain$elements$element_id[index],
    complete_index = index,
    response = maps$response_name,
    predictors = maps$predictor_names,
    sigma = sqrt(fit$sigma2),
    logLik = fit$logLik,
    residual_moran = if (stats::var(fit$residuals) > 0) {
      .ngeo_moran_value(fit$residuals, weight)
    } else {
      NA_real_
    },
    log_determinant = fit$log_determinant,
    log_determinant_method = "exact_dense",
    parameter_interval = fit$parameter_interval,
    tolerance = 1e-8,
    domain_hash = ngeo_domain_hash(x),
    weights_method = weights$method,
    zero_policy = isTRUE(zero_policy)
  )
  class(result) <- "ngeo_spatial_regression"
  result
}

#' @export
print.ngeo_spatial_regression <- function(x, ...) {
  cat(
    "<ngeo_spatial_regression>\n",
    "  model: ", x$model, "\n",
    "  response: ", x$response, "\n",
    if (!is.null(x$spatial_parameter)) paste0(
      "  ", x$parameter_name, ": ",
      format(x$spatial_parameter, digits = 5L), "\n"
    ) else "",
    sep = ""
  )
  invisible(x)
}

#' Fit a foundational Gaussian CAR smoother
#'
#' @param x An `ngeo` dataset.
#' @param response One numeric map.
#' @param weights Matching symmetric spatial weights.
#' @param type Proper or intrinsic CAR.
#' @param rho Proper-CAR dependence in `[0, 1)`.
#' @param precision Optional positive smoothing precision; when omitted it is
#'   selected by bounded leave-one-out-like generalized cross-validation.
#' @param zero_policy Whether isolates are retained.
#'
#' @return An `ngeo_car`.
#' @export
ngeo_car <- function(
    x,
    response,
    weights,
    type = c("proper", "intrinsic"),
    rho = 0.95,
    precision = NULL,
    zero_policy = FALSE) {
  type <- match.arg(type)
  maps <- .ngeo_model_maps(x, response, character())
  y <- as.numeric(x$values[, maps$response])
  if (any(!is.finite(y))) {
    .ngeo_abort("CAR response must be finite.", "ngeo_error_missing")
  }
  weight <- .ngeo_model_weights(
    x, weights, seq_along(y), zero_policy
  )
  weight <- (weight + Matrix::t(weight)) / 2
  degree <- Matrix::rowSums(abs(weight))
  if (any(degree == 0) && !isTRUE(zero_policy)) {
    .ngeo_abort("CAR weights contain isolates.", "ngeo_error_zero_policy")
  }
  if (!is.numeric(rho) || length(rho) != 1L ||
      !is.finite(rho) || rho < 0 || rho >= 1) {
    .ngeo_abort("`rho` must lie in [0, 1).", "ngeo_error_argument")
  }
  q <- Matrix::Diagonal(x = degree) - if (type == "proper") {
    rho * weight
  } else {
    weight
  }
  identity <- Matrix::Diagonal(n = length(y))
  score <- function(log_precision, details = FALSE) {
    current <- exp(log_precision)
    smoother <- solve(identity + current * q)
    fitted <- as.numeric(smoother %*% y)
    residual <- y - fitted
    denominator <- (1 - sum(Matrix::diag(smoother)) / length(y))^2
    gcv <- mean(residual^2) / max(denominator, 1e-12)
    if (details) list(
      precision = current, fitted = fitted,
      residuals = residual, gcv = gcv,
      effective_df = sum(Matrix::diag(smoother))
    ) else gcv
  }
  fit <- if (is.null(precision)) {
    selected <- stats::optimize(score, c(log(1e-6), log(1e6)))
    score(selected$minimum, details = TRUE)
  } else {
    if (!is.numeric(precision) || length(precision) != 1L ||
        !is.finite(precision) || precision <= 0) {
      .ngeo_abort("`precision` must be positive.", "ngeo_error_argument")
    }
    score(log(precision), details = TRUE)
  }
  if (type == "intrinsic") {
    fit$fitted <- fit$fitted - mean(fit$fitted) + mean(y)
    fit$residuals <- y - fit$fitted
  }
  result <- c(
    fit,
    list(
      type = type,
      rho = if (type == "proper") rho else NA_real_,
      constraint = if (type == "intrinsic") "sum-to-zero spatial effect" else
        "proper precision",
      isolates = which(degree == 0),
      response = maps$response_name,
      domain_hash = ngeo_domain_hash(x),
      weights_method = weights$method
    )
  )
  class(result) <- "ngeo_car"
  result
}

#' @export
print.ngeo_car <- function(x, ...) {
  cat("<ngeo_car>\n  type: ", x$type, "\n  precision: ",
      format(x$precision, digits = 5L), "\n  effective df: ",
      format(x$effective_df, digits = 5L), "\n", sep = "")
  invisible(x)
}

#' Fit a declared spatial model across support maps
#'
#' @param x Source `ngeo` dataset.
#' @param support_maps Complete source-to-target maps.
#' @param targets Aligned target templates.
#' @param weights Aligned target weights or one weight reused for all targets.
#' @inheritParams ngeo_spatial_regression
#'
#' @return An `ngeo_support_model`.
#' @export
ngeo_support_model <- function(
    x,
    support_maps,
    targets,
    response,
    predictors,
    weights = NULL,
    model = c("ols", "slx", "sar", "sem"),
    zero_policy = FALSE) {
  model <- match.arg(model)
  if (inherits(targets, "ngeo")) {
    targets <- rep(list(targets), length(support_maps))
  }
  if (inherits(weights, "ngeo_weights")) {
    weights <- rep(list(weights), length(support_maps))
  }
  if (!is.list(support_maps) || !length(support_maps) ||
      !is.list(targets) || length(targets) != length(support_maps) ||
      (!is.null(weights) && (
        !is.list(weights) || length(weights) != length(support_maps)
      ))) {
    .ngeo_abort("Support-model inputs must align.", "ngeo_error_alignment")
  }
  selected <- c(
    .ngeo_map_selection(x, response),
    .ngeo_map_selection(x, predictors)
  )
  fits <- lapply(seq_along(support_maps), function(i) {
    changed <- ngeo_change_support(
      x, targets[[i]], support_maps[[i]], maps = selected
    )
    ngeo_spatial_regression(
      changed,
      response = 1L,
      predictors = if (length(predictors)) seq.int(2L, length(selected)) else
        character(),
      weights = if (is.null(weights)) NULL else weights[[i]],
      model = model,
      zero_policy = zero_policy
    )
  })
  labels <- names(support_maps) %||% paste0("support_", seq_along(fits))
  coefficient <- do.call(rbind, lapply(seq_along(fits), function(i) {
    data.frame(
      support = labels[[i]],
      fits[[i]]$coefficients,
      stringsAsFactors = FALSE
    )
  }))
  result <- list(
    model = model,
    fits = fits,
    coefficients = coefficient,
    support_map_hashes = vapply(
      support_maps, ngeo_support_map_hash, character(1)
    ),
    claim = "model comparison across declared supports; not invariant coefficients"
  )
  class(result) <- "ngeo_support_model"
  result
}

#' @export
print.ngeo_support_model <- function(x, ...) {
  cat("<ngeo_support_model>\n  model: ", x$model,
      "\n  supports: ", length(x$fits), "\n", sep = "")
  invisible(x)
}
