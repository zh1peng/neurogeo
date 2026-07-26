.ngeo_model_covariance_matrix <- function(covariance, x) {
  .ngeo_validate_covariance_domain(covariance, x)
  maximum <- getOption("neurogeo.max_model_covariance_dimension", 2000L)
  if (covariance$dimension > maximum) {
    .ngeo_abort(
      "Model covariance exceeds the configured dense reference limit.",
      "ngeo_error_resource"
    )
  }
  result <- diag(covariance$variance, covariance$dimension)
  if (identical(covariance$representation, "low_rank")) {
    result <- result + tcrossprod(covariance$factor)
  } else if (identical(covariance$representation, "matrix")) {
    result <- result + as.matrix(covariance$covariance)
  }
  result
}

.ngeo_interval <- function(estimate, standard_error, level) {
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    .ngeo_abort("`level` must lie strictly between zero and one.",
                "ngeo_error_argument")
  }
  critical <- stats::qnorm((1 + level) / 2)
  cbind(
    lower = estimate - critical * standard_error,
    upper = estimate + critical * standard_error
  )
}

.ngeo_variogram_uncertain_empirical <- function(
    x,
    map,
    covariance,
    metric,
    breaks,
    max_distance) {
  map_index <- .ngeo_map_selection(x, map)
  if (length(map_index) != 1L) {
    .ngeo_abort("Select one variogram map.", "ngeo_error_argument")
  }
  values <- as.numeric(x$values[, map_index])
  if (any(!is.finite(values))) {
    .ngeo_abort("Uncertain variograms require finite values.",
                "ngeo_error_missing")
  }
  covariance_matrix <- .ngeo_model_covariance_matrix(covariance, x)
  n <- length(values)
  pair_count <- n * (n - 1) / 2
  maximum <- getOption("neurogeo.max_variogram_pairs", 1e6)
  if (pair_count > maximum) {
    .ngeo_abort(
      "Uncertain variogram pair count exceeds the configured limit.",
      "ngeo_error_dense_distance"
    )
  }
  distance <- corrected <- raw <- numeric(pair_count)
  position <- 1L
  for (i in seq_len(n - 1L)) {
    target <- seq.int(i + 1L, n)
    current <- seq.int(position, length.out = length(target))
    distance[current] <- as.numeric(ngeo_distance(
      x, from = i, to = target, metric = metric,
      max_distance = max_distance
    ))
    raw[current] <- 0.5 * (values[[i]] - values[target])^2
    error_variance <- covariance_matrix[i, i] +
      diag(covariance_matrix)[target] -
      2 * covariance_matrix[i, target]
    corrected[current] <- pmax(0, raw[current] - 0.5 * error_variance)
    position <- position + length(target)
  }
  keep <- is.finite(distance) & distance <= max_distance
  distance <- distance[keep]
  raw <- raw[keep]
  corrected <- corrected[keep]
  if (!length(distance)) {
    .ngeo_abort("No finite variogram pairs remain.",
                "ngeo_error_statistic")
  }
  boundaries <- if (length(breaks) == 1L) {
    breaks <- .ngeo_as_integer(breaks, "breaks")
    if (length(breaks) != 1L || breaks < 1L || max(distance) <= 0) {
      .ngeo_abort("Variogram breaks are invalid.", "ngeo_error_argument")
    }
    seq(0, max(distance), length.out = breaks + 1L)
  } else {
    boundaries <- as.numeric(breaks)
    if (anyNA(boundaries) || any(!is.finite(boundaries)) ||
        is.unsorted(boundaries, strictly = TRUE)) {
      .ngeo_abort("Variogram boundaries must be strictly increasing.",
                  "ngeo_error_argument")
    }
    boundaries
  }
  bin <- cut(distance, boundaries, include.lowest = TRUE)
  result <- do.call(rbind, lapply(levels(bin), function(label) {
    selected <- which(bin == label)
    data.frame(
      bin = label,
      distance = mean(distance[selected]),
      semivariance = mean(corrected[selected]),
      raw_semivariance = mean(raw[selected]),
      measurement_correction = mean(raw[selected] - corrected[selected]),
      n_pairs = length(selected),
      stringsAsFactors = FALSE
    )
  }))
  result <- result[result$n_pairs > 0L, , drop = FALSE]
  rownames(result) <- NULL
  attr(result, "map_id") <- x$maps$map_id[[map_index]]
  attr(result, "map_name") <- x$maps$name[[map_index]]
  attr(result, "domain_hash") <- ngeo_domain_hash(x)
  attr(result, "metric") <- .ngeo_metric_name(metric %||% switch(
    x$domain$type,
    surface = "edge_geodesic",
    volume = "world_euclidean",
    points = "euclidean",
    regions = "region_centroid",
    grayordinates = "edge_geodesic"
  ))
  attr(result, "pair_count") <- length(distance)
  class(result) <- c("ngeo_variogram", "data.frame")
  result
}

#' Fit a measurement-uncertainty-aware variogram
#'
#' @param x An `ngeo` dataset.
#' @param map One numeric map.
#' @param value_covariance Matching domain-bound measurement covariance.
#' @param metric,breaks,max_distance,model Passed to variogram estimation.
#' @param nsim Number of parameter simulations.
#' @param seed Reproducible seed.
#' @param workers Deterministic worker-group count.
#' @param level Interval level.
#'
#' @return An `ngeo_variogram_uncertainty`.
#' @export
ngeo_variogram_uncertainty <- function(
    x,
    map,
    value_covariance,
    metric = NULL,
    breaks = 10L,
    max_distance = Inf,
    model = c("spherical", "exponential", "gaussian"),
    nsim = 199L,
    seed = NULL,
    workers = 1L,
    level = 0.95) {
  model <- match.arg(model)
  workers <- .ngeo_as_integer(workers, "workers")
  if (length(workers) != 1L || workers < 1L) {
    .ngeo_abort("`workers` must be positive.", "ngeo_error_argument")
  }
  .ngeo_interval(0, 1, level)
  empirical <- .ngeo_variogram_uncertain_empirical(
    x, map, value_covariance, metric, breaks, max_distance
  )
  fit <- ngeo_fit_variogram(empirical, model = model)
  nsim <- .ngeo_nsim(nsim)
  map_index <- .ngeo_map_selection(x, map)
  draws <- .ngeo_with_seed(
    seed,
    function() .ngeo_draw_covariance(value_covariance, nsim)
  )
  simulations <- do.call(rbind, .ngeo_simulate(
    nsim, seed, workers,
    function(i) {
      current <- x
      current$values[, map_index] <-
        as.numeric(x$values[, map_index]) + draws[, i]
      current_fit <- tryCatch(
        ngeo_fit_variogram(
          current, map = map_index, metric = metric, breaks = breaks,
          max_distance = max_distance, model = model
        ),
        error = function(...) NULL
      )
      if (is.null(current_fit)) {
        c(nugget = NA_real_, partial_sill = NA_real_, range = NA_real_)
      } else {
        current_fit$parameters
      }
    }
  ))
  simulations <- simulations[apply(is.finite(simulations), 1L, all),
                             , drop = FALSE]
  if (nrow(simulations) < max(10L, ceiling(nsim * 0.8))) {
    .ngeo_abort("Too few variogram parameter simulations converged.",
                "ngeo_error_model")
  }
  interval <- t(apply(
    simulations, 2L, stats::quantile,
    probs = c((1 - level) / 2, (1 + level) / 2),
    names = FALSE
  ))
  colnames(interval) <- c("lower", "upper")
  result <- list(
    fit = fit,
    empirical = empirical,
    parameter_simulations = simulations,
    parameter_interval = interval,
    measurement_covariance = value_covariance,
    nsim = nsim,
    successful_simulations = nrow(simulations),
    seed = .ngeo_seed(seed),
    workers = workers,
    level = level,
    assumptions = paste(
      "additive zero-mean Gaussian measurement error;",
      "measurement error independent of the latent spatial field"
    ),
    domain_hash = ngeo_domain_hash(x)
  )
  class(result) <- "ngeo_variogram_uncertainty"
  result
}

#' Decompose kriging prediction uncertainty
#'
#' @inheritParams ngeo_kriging
#' @param value_covariance Optional matching measurement covariance.
#' @param variogram_uncertainty Optional `ngeo_variogram_uncertainty`.
#' @param support_variance Optional non-negative target-aligned support
#'   variance.
#' @param level Interval level.
#'
#' @return An `ngeo_kriging_uncertainty` data frame.
#' @export
ngeo_kriging_uncertainty <- function(
    x,
    map,
    variogram,
    targets = NULL,
    predictors = character(),
    target_predictors = NULL,
    neighbors = 50L,
    metric = NULL,
    value_covariance = NULL,
    variogram_uncertainty = NULL,
    support_variance = NULL,
    level = 0.95) {
  base <- ngeo_kriging(
    x, map, variogram, targets, predictors, target_predictors,
    neighbors, metric
  )
  linear <- attr(base, "linear_weights")
  measurement <- rep.int(0, nrow(base))
  if (!is.null(value_covariance)) {
    covariance <- .ngeo_model_covariance_matrix(value_covariance, x)
    measurement <- rowSums((as.matrix(linear %*% covariance)) *
                             as.matrix(linear))
  }
  parameter <- rep.int(0, nrow(base))
  if (!is.null(variogram_uncertainty)) {
    if (!inherits(variogram_uncertainty, "ngeo_variogram_uncertainty") ||
        !identical(variogram_uncertainty$domain_hash, ngeo_domain_hash(x))) {
      .ngeo_abort("Variogram uncertainty does not match the domain.",
                  "ngeo_error_domain_mismatch")
    }
    draws <- variogram_uncertainty$parameter_simulations
    predictions <- vapply(seq_len(nrow(draws)), function(i) {
      current <- variogram
      current$parameters <- draws[i, ]
      ngeo_kriging(
        x, map, current, targets, predictors, target_predictors,
        neighbors, metric
      )$prediction
    }, numeric(nrow(base)))
    if (is.null(dim(predictions))) {
      predictions <- matrix(predictions, nrow = nrow(base))
    }
    parameter <- apply(predictions, 1L, stats::var)
  }
  if (is.null(support_variance)) {
    support_variance <- rep.int(0, nrow(base))
  }
  if (!is.numeric(support_variance) ||
      length(support_variance) != nrow(base) ||
      any(!is.finite(support_variance)) || any(support_variance < 0)) {
    .ngeo_abort("Support variance must be non-negative and target-aligned.",
                "ngeo_error_uncertainty")
  }
  total <- base$variance + measurement + parameter + support_variance
  interval <- .ngeo_interval(base$prediction, sqrt(total), level)
  result <- data.frame(
    base,
    process_variance = base$variance,
    measurement_variance = measurement,
    parameter_variance = parameter,
    support_variance = support_variance,
    total_variance = total,
    total_standard_error = sqrt(total),
    lower = interval[, "lower"],
    upper = interval[, "upper"],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  class(result) <- c("ngeo_kriging_uncertainty", "data.frame")
  attr(result, "level") <- level
  attr(result, "assumptions") <- paste(
    "independent process, measurement, variogram-parameter,",
    "and support components"
  )
  result
}

#' Estimate GWR coefficient covariance and bandwidth sensitivity
#'
#' @inheritParams ngeo_gwr
#' @param value_covariance Matching response measurement covariance.
#' @param bandwidths Optional positive bandwidth sensitivity set.
#' @param level Interval level.
#'
#' @return An `ngeo_gwr_uncertainty`.
#' @export
ngeo_gwr_uncertainty <- function(
    x,
    response,
    predictors,
    bandwidth,
    value_covariance,
    bandwidths = NULL,
    metric = NULL,
    kernel = c("gaussian", "bisquare"),
    targets = NULL,
    support = c("none", "domain"),
    singular = c("na", "error"),
    level = 0.95) {
  kernel <- match.arg(kernel)
  support <- match.arg(support)
  singular <- match.arg(singular)
  value <- if (inherits(bandwidth, "ngeo_gwr_bandwidth")) {
    bandwidth$bandwidth
  } else {
    bandwidth
  }
  base <- ngeo_gwr(
    x, response, predictors, value, metric, kernel,
    targets, support, singular
  )
  covariance <- .ngeo_model_covariance_matrix(value_covariance, x)
  maps <- .ngeo_model_maps(x, response, predictors)
  y <- as.numeric(x$values[, maps$response])
  predictor <- x$values[, maps$predictors, drop = FALSE]
  training <- which(is.finite(y) & apply(
    cbind(1, predictor), 1L, function(row) all(is.finite(row))
  ))
  design <- cbind(`(Intercept)` = 1, predictor[training, , drop = FALSE])
  colnames(design) <- c("(Intercept)", maps$predictor_names)
  target_index <- base$target_index
  support_weight <- rep.int(1, length(training))
  if (support == "domain") {
    support_weight <- ngeo_support_size(x)[training]
  }
  covariance_list <- vector("list", length(target_index))
  rows <- vector("list", length(target_index))
  for (i in seq_along(target_index)) {
    distance <- as.numeric(ngeo_distance(
      x, from = target_index[[i]], to = training, metric = metric,
      max_distance = value * if (kernel == "gaussian") 3 else 1
    ))
    weight <- .ngeo_kernel_weights(distance, value, kernel, 3) *
      support_weight
    selected <- which(weight > 0)
    if (length(selected) < ncol(design)) next
    local_design <- design[selected, , drop = FALSE]
    local_weight <- weight[selected]
    information <- crossprod(
      local_design,
      local_weight * local_design
    )
    inverse <- tryCatch(solve(information), error = function(...) NULL)
    if (is.null(inverse)) next
    coefficient <- as.numeric(
      base[i, colnames(design), drop = TRUE]
    )
    residual <- y[training[selected]] -
      as.numeric(local_design %*% coefficient)
    residual_df <- length(selected) - ncol(design)
    residual_scale <- if (residual_df > 0) {
      sum(local_weight * residual^2) / residual_df
    } else {
      0
    }
    bread <- inverse %*% sweep(t(local_design), 2L, local_weight, "*")
    measurement <- bread %*%
      covariance[training[selected], training[selected], drop = FALSE] %*%
      t(bread)
    sampling <- residual_scale * inverse %*%
      crossprod(local_design, local_weight^2 * local_design) %*% inverse
    total <- measurement + sampling
    covariance_list[[i]] <- total
    standard_error <- sqrt(pmax(diag(total), 0))
    interval <- .ngeo_interval(coefficient, standard_error, level)
    rows[[i]] <- data.frame(
      element_id = base$element_id[[i]],
      target_index = target_index[[i]],
      term = colnames(design),
      estimate = coefficient,
      standard_error = standard_error,
      lower = interval[, "lower"],
      upper = interval[, "upper"],
      effective_n = base$effective_n[[i]],
      condition_number = base$condition_number[[i]],
      stringsAsFactors = FALSE
    )
  }
  coefficient <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  bandwidths <- sort(unique(c(value, bandwidths)))
  if (any(!is.finite(bandwidths)) || any(bandwidths <= 0)) {
    .ngeo_abort("Sensitivity bandwidths must be positive.",
                "ngeo_error_argument")
  }
  sensitivity_fits <- lapply(bandwidths, function(current) {
    ngeo_gwr(
      x, response, predictors, current, metric, kernel,
      targets, support, singular
    )
  })
  sensitivity <- do.call(rbind, lapply(seq_along(target_index), function(i) {
    do.call(rbind, lapply(colnames(design), function(term) {
      estimate <- vapply(
        sensitivity_fits,
        function(fit) as.numeric(fit[i, term]),
        numeric(1)
      )
      data.frame(
        element_id = base$element_id[[i]],
        term = term,
        minimum = min(estimate, na.rm = TRUE),
        maximum = max(estimate, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  }))
  result <- list(
    coefficients = coefficient,
    covariance = covariance_list,
    sensitivity = sensitivity,
    bandwidths = bandwidths,
    base = base,
    level = level,
    domain_hash = ngeo_domain_hash(x),
    assumptions = paste(
      "local linearization; Gaussian intervals;",
      "bandwidth ranges are sensitivity ranges, not confidence intervals"
    )
  )
  class(result) <- "ngeo_gwr_uncertainty"
  result
}

#' Simulate SAR or SEM parameter and prediction uncertainty
#'
#' @inheritParams ngeo_spatial_regression
#' @param value_covariance Matching response measurement covariance.
#' @param nsim Number of Gaussian simulations.
#' @param seed Reproducible seed.
#' @param workers Deterministic worker-group count.
#' @param level Interval level.
#'
#' @return An `ngeo_spatial_regression_uncertainty`.
#' @export
ngeo_spatial_regression_uncertainty <- function(
    x,
    response,
    predictors = character(),
    weights,
    model = c("sar", "sem"),
    value_covariance,
    nsim = 199L,
    seed = NULL,
    workers = 1L,
    level = 0.95,
    zero_policy = FALSE) {
  model <- match.arg(model)
  .ngeo_validate_covariance_domain(value_covariance, x)
  if (!is.matrix(x$values)) {
    .ngeo_abort("Model simulation requires an in-memory aligned values block.",
                "ngeo_error_resource")
  }
  nsim <- .ngeo_nsim(nsim)
  workers <- .ngeo_as_integer(workers, "workers")
  if (length(workers) != 1L || workers < 1L) {
    .ngeo_abort("`workers` must be positive.", "ngeo_error_argument")
  }
  .ngeo_interval(0, 1, level)
  base <- ngeo_spatial_regression(
    x, response, predictors, weights, model,
    zero_policy = zero_policy
  )
  map_index <- .ngeo_map_selection(x, response)
  draws <- .ngeo_with_seed(
    seed,
    function() .ngeo_draw_covariance(value_covariance, nsim)
  )
  simulation <- .ngeo_simulate(
    nsim, seed, workers,
    function(i) {
      current <- x
      current$values[, map_index] <-
        x$values[, map_index] + draws[, i]
      tryCatch(
        ngeo_spatial_regression(
          current, response, predictors, weights, model,
          zero_policy = zero_policy
        ),
        error = function(...) NULL
      )
    }
  )
  keep <- !vapply(simulation, is.null, logical(1))
  if (sum(keep) < max(10L, ceiling(nsim * 0.8))) {
    .ngeo_abort("Too few spatial-regression simulations converged.",
                "ngeo_error_model")
  }
  simulation <- simulation[keep]
  coefficient <- do.call(rbind, lapply(simulation, function(fit) {
    c(
      stats::setNames(fit$spatial_parameter, fit$parameter_name),
      stats::setNames(fit$coefficients$estimate, fit$coefficients$term)
    )
  }))
  prediction <- do.call(cbind, lapply(simulation, `[[`, "fitted"))
  coefficient_estimate <- c(
    stats::setNames(base$spatial_parameter, base$parameter_name),
    stats::setNames(base$coefficients$estimate, base$coefficients$term)
  )
  coefficient_se <- apply(coefficient, 2L, stats::sd)
  coefficient_interval <- t(apply(
    coefficient, 2L, stats::quantile,
    probs = c((1 - level) / 2, (1 + level) / 2),
    names = FALSE
  ))
  result <- list(
    fit = base,
    coefficient_summary = data.frame(
      term = names(coefficient_estimate),
      estimate = as.numeric(coefficient_estimate),
      standard_error = coefficient_se[names(coefficient_estimate)],
      lower = coefficient_interval[names(coefficient_estimate), 1L],
      upper = coefficient_interval[names(coefficient_estimate), 2L],
      stringsAsFactors = FALSE
    ),
    prediction = data.frame(
      element_id = base$element_id,
      estimate = base$fitted,
      variance = apply(prediction, 1L, stats::var),
      lower = apply(
        prediction, 1L, stats::quantile,
        probs = (1 - level) / 2, names = FALSE
      ),
      upper = apply(
        prediction, 1L, stats::quantile,
        probs = (1 + level) / 2, names = FALSE
      ),
      stringsAsFactors = FALSE
    ),
    coefficient_simulations = coefficient,
    nsim = nsim,
    successful_simulations = length(simulation),
    seed = .ngeo_seed(seed),
    workers = workers,
    level = level,
    domain_hash = ngeo_domain_hash(x),
    assumptions = "additive Gaussian measurement covariance"
  )
  class(result) <- "ngeo_spatial_regression_uncertainty"
  result
}

#' Compute Gaussian CAR MAP and posterior uncertainty
#'
#' @inheritParams ngeo_car
#' @param value_covariance Positive-definite observation covariance.
#' @param level Interval level.
#'
#' @return An `ngeo_car_uncertainty`.
#' @export
ngeo_car_uncertainty <- function(
    x,
    response,
    weights,
    value_covariance,
    type = c("proper", "intrinsic"),
    rho = 0.95,
    precision = NULL,
    zero_policy = FALSE,
    level = 0.95) {
  type <- match.arg(type)
  covariance <- .ngeo_model_covariance_matrix(value_covariance, x)
  eigenvalue <- eigen(covariance, symmetric = TRUE, only.values = TRUE)$values
  if (min(eigenvalue) <= value_covariance$tolerance) {
    .ngeo_abort("CAR observation covariance must be positive definite.",
                "ngeo_error_uncertainty")
  }
  base <- ngeo_car(
    x, response, weights, type, rho, precision, zero_policy
  )
  map <- .ngeo_map_selection(x, response)
  y <- as.numeric(x$values[, map])
  weight <- .ngeo_model_weights(
    x, weights, seq_along(y), zero_policy
  )
  weight <- (weight + Matrix::t(weight)) / 2
  degree <- Matrix::rowSums(abs(weight))
  q <- as.matrix(Matrix::Diagonal(x = degree) - if (type == "proper") {
    rho * weight
  } else {
    weight
  })
  inverse_covariance <- solve(covariance)
  posterior <- solve(inverse_covariance + base$precision * q)
  if (type == "proper") {
    estimate <- as.numeric(posterior %*% inverse_covariance %*% y)
    posterior_covariance <- posterior
    constraint <- "proper Gaussian posterior"
  } else {
    projection <- diag(length(y)) - matrix(1 / length(y), length(y), length(y))
    estimate <- mean(y) + as.numeric(
      projection %*% posterior %*% inverse_covariance %*%
        projection %*% y
    )
    mean_variance <- sum(covariance) / length(y)^2
    posterior_covariance <- projection %*% posterior %*% projection +
      matrix(mean_variance, length(y), length(y))
    constraint <- "sum-to-zero spatial effect around observed global mean"
  }
  standard_error <- sqrt(pmax(diag(posterior_covariance), 0))
  interval <- .ngeo_interval(estimate, standard_error, level)
  result <- list(
    map = data.frame(
      element_id = x$domain$elements$element_id,
      estimate = estimate,
      standard_error = standard_error,
      lower = interval[, "lower"],
      upper = interval[, "upper"],
      stringsAsFactors = FALSE
    ),
    posterior_covariance = posterior_covariance,
    precision = base$precision,
    rho = if (type == "proper") rho else NA_real_,
    type = type,
    constraint = constraint,
    level = level,
    domain_hash = ngeo_domain_hash(x),
    assumptions = "Gaussian observation model with declared covariance"
  )
  class(result) <- "ngeo_car_uncertainty"
  result
}

.ngeo_model_effect <- function(x) {
  if (inherits(x, "ngeo_spatial_regression_uncertainty")) {
    return(x$coefficient_summary[
      , c("term", "estimate", "standard_error"), drop = FALSE
    ])
  }
  if (inherits(x, "ngeo_spatial_lm") ||
      inherits(x, "ngeo_spatial_regression")) {
    if (!"std.error" %in% names(x$coefficients)) {
      .ngeo_abort(
        "A model without coefficient uncertainty needs simulation first.",
        "ngeo_error_uncertainty"
      )
    }
    result <- x$coefficients[
      , c("term", "estimate", "std.error"), drop = FALSE
    ]
    names(result)[[3L]] <- "standard_error"
    return(result)
  }
  .ngeo_abort("Unsupported model uncertainty input.", "ngeo_error_argument")
}

#' Combine model effects across declared supports
#'
#' @param fits Model fits with coefficient standard errors.
#' @param weights Optional non-negative support-family weights.
#' @param level Interval level.
#'
#' @return An `ngeo_support_model_ensemble`.
#' @export
ngeo_support_model_ensemble <- function(
    fits,
    weights = NULL,
    level = 0.95) {
  if (!is.list(fits) || length(fits) < 2L) {
    .ngeo_abort("At least two support-model fits are required.",
                "ngeo_error_argument")
  }
  effect <- lapply(fits, .ngeo_model_effect)
  terms <- effect[[1L]]$term
  if (!all(vapply(
    effect,
    function(current) identical(current$term, terms),
    logical(1)
  ))) {
    .ngeo_abort("Support-model coefficient terms do not align.",
                "ngeo_error_alignment")
  }
  if (is.null(weights)) weights <- rep.int(1 / length(fits), length(fits))
  if (!is.numeric(weights) || length(weights) != length(fits) ||
      any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) {
    .ngeo_abort("Support-model weights are invalid.", "ngeo_error_argument")
  }
  weights <- weights / sum(weights)
  estimate <- do.call(cbind, lapply(effect, `[[`, "estimate"))
  variance <- do.call(cbind, lapply(
    effect, function(current) current$standard_error^2
  ))
  consensus <- as.numeric(estimate %*% weights)
  within <- as.numeric(variance %*% weights)
  between <- rowSums(
    sweep((estimate - consensus)^2, 2L, weights, "*")
  )
  total <- within + between
  interval <- .ngeo_interval(consensus, sqrt(total), level)
  result <- list(
    summary = data.frame(
      term = terms,
      estimate = consensus,
      within_support_variance = within,
      between_support_variance = between,
      total_variance = total,
      standard_error = sqrt(total),
      lower = interval[, "lower"],
      upper = interval[, "upper"],
      stringsAsFactors = FALSE
    ),
    effects = effect,
    weights = weights,
    supports = names(fits) %||% paste0("support_", seq_along(fits)),
    level = level,
    claim = paste(
      "law-of-total-variance summary over the declared support family;",
      "not parcellation-invariant inference"
    )
  )
  class(result) <- "ngeo_support_model_ensemble"
  result
}

#' Summarize model uncertainty calibration
#'
#' @param truth True values.
#' @param estimate Estimated values.
#' @param lower,upper Optional interval bounds.
#' @param residual_moran Optional residual Moran estimates.
#'
#' @return An `ngeo_model_calibration` data frame.
#' @export
ngeo_model_calibration <- function(
    truth,
    estimate,
    lower = NULL,
    upper = NULL,
    residual_moran = NULL) {
  if (!is.numeric(truth) || !is.numeric(estimate) ||
      length(truth) != length(estimate) || !length(truth) ||
      any(!is.finite(truth)) || any(!is.finite(estimate))) {
    .ngeo_abort("Calibration truth and estimates must align.",
                "ngeo_error_argument")
  }
  if (xor(is.null(lower), is.null(upper)) ||
      (!is.null(lower) && (
        length(lower) != length(truth) ||
          length(upper) != length(truth) ||
          any(!is.finite(lower)) || any(!is.finite(upper)) ||
          any(lower > upper)
      ))) {
    .ngeo_abort("Calibration intervals must be complete and aligned.",
                "ngeo_error_argument")
  }
  error <- estimate - truth
  result <- data.frame(
    n = length(error),
    bias = mean(error),
    rmse = sqrt(mean(error^2)),
    coverage = if (is.null(lower)) NA_real_ else
      mean(lower <= truth & truth <= upper),
    mean_interval_width = if (is.null(lower)) NA_real_ else
      mean(upper - lower),
    mean_residual_moran = if (is.null(residual_moran)) NA_real_ else {
      if (!is.numeric(residual_moran) ||
          any(!is.finite(residual_moran))) {
        .ngeo_abort("Residual Moran values must be finite.",
                    "ngeo_error_argument")
      }
      mean(residual_moran)
    },
    stringsAsFactors = FALSE
  )
  class(result) <- c("ngeo_model_calibration", "data.frame")
  result
}

#' @export
print.ngeo_variogram_uncertainty <- function(x, ...) {
  cat("<ngeo_variogram_uncertainty>\n  simulations: ",
      x$successful_simulations, "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_gwr_uncertainty <- function(x, ...) {
  cat("<ngeo_gwr_uncertainty>\n  coefficient rows: ",
      nrow(x$coefficients), "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_spatial_regression_uncertainty <- function(x, ...) {
  cat("<ngeo_spatial_regression_uncertainty>\n  model: ",
      x$fit$model, "\n  simulations: ", x$successful_simulations,
      "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_car_uncertainty <- function(x, ...) {
  cat("<ngeo_car_uncertainty>\n  type: ", x$type,
      "\n  elements: ", nrow(x$map), "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_support_model_ensemble <- function(x, ...) {
  cat("<ngeo_support_model_ensemble>\n  supports: ",
      length(x$effects), "\n", sep = "")
  invisible(x)
}
