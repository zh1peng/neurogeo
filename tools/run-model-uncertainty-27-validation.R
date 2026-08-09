args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "model-uncertainty-27-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Model uncertainty validation requires jsonlite.")
}
if (!exists("ngeo_variogram_uncertainty", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

coordinates <- as.matrix(expand.grid(x = 0:4, y = 0:4))
predictor <- coordinates[, 1L] + 0.25 * coordinates[, 2L]
base <- ngeo_point(
  coordinates,
  values = cbind(response = 1 + 2 * predictor, predictor = predictor),
  measures = rbind(
    ngeo_measure(support_behavior = "intensive"),
    ngeo_measure(support_behavior = "intensive")
  )
)
spatial_weights <- ngeo_spatial_weights(
  base,
  method = "distance_band",
  threshold = 1.01,
  style = "W"
)
measurement_variance <- 0.01
covariance <- ngeo_support_covariance(
  base,
  variance = rep(measurement_variance, nrow(coordinates))
)

variogram_fit <- structure(
  list(
    model = "exponential",
    parameters = c(nugget = 0.05, partial_sill = 1, range = 3)
  ),
  class = "ngeo_variogram_fit"
)
kriging <- ngeo_kriging_uncertainty(
  base,
  "response",
  variogram_fit,
  targets = 1:6,
  neighbors = 8L,
  value_covariance = covariance
)
linear <- attr(
  ngeo_kriging(
    base, "response", variogram_fit, targets = 1:6, neighbors = 8L
  ),
  "linear_weights"
)
analytic <- rowSums(as.matrix(linear)^2 * measurement_variance)
set.seed(27001)
measurement_draws <- matrix(
  stats::rnorm(nrow(coordinates) * 5000L, sd = sqrt(measurement_variance)),
  nrow = nrow(coordinates)
)
monte_carlo <- apply(
  as.matrix(linear) %*% measurement_draws,
  1L,
  stats::var
)
analytic_error <- max(abs(analytic - kriging$measurement_variance))
monte_carlo_relative_error <- max(
  abs(monte_carlo - analytic) / pmax(analytic, 1e-12)
)
if (analytic_error > 1e-12 || monte_carlo_relative_error > 0.1) {
  stop("Kriging analytic/Monte Carlo covariance calibration failed.")
}

car <- ngeo_car_uncertainty(
  base,
  "response",
  spatial_weights,
  covariance,
  type = "proper",
  rho = 0.8,
  precision = 2
)
weight_matrix <- (spatial_weights$matrix + Matrix::t(spatial_weights$matrix)) / 2
q <- as.matrix(
  Matrix::Diagonal(x = Matrix::rowSums(abs(weight_matrix))) -
    0.8 * weight_matrix
)
observation_precision <- diag(1 / measurement_variance, nrow(q))
posterior_reference <- solve(observation_precision + 2 * q)
car_reference <- as.numeric(
  posterior_reference %*% observation_precision %*%
    base$values[, "response"]
)
car_estimate_error <- max(abs(car$map$estimate - car_reference))
car_covariance_error <- max(
  abs(car$posterior_covariance - posterior_reference)
)
if (car_estimate_error > 1e-10 || car_covariance_error > 1e-10) {
  stop("CAR direct matrix reference failed.")
}

set.seed(27002)
known_effect <- replicate(200L, {
  current <- base
  current$values[, "response"] <- 1 + 2 * predictor +
    stats::rnorm(length(predictor), sd = sqrt(measurement_variance))
  fit <- ngeo_spatial_regression(
    current, "response", "predictor", model = "ols"
  )
  coefficient <- fit$coefficients[
    fit$coefficients$term == "predictor", , drop = FALSE
  ]
  critical <- stats::qt(0.975, fit$df.residual)
  residual_data <- current
  residual_data$values[, "response"] <- fit$residuals
  c(
    estimate = coefficient$estimate,
    lower = coefficient$estimate - critical * coefficient$std.error,
    upper = coefficient$estimate + critical * coefficient$std.error,
    residual_moran = ngeo_moran(
      residual_data, spatial_weights, "response"
    )$estimate
  )
})
calibration <- ngeo_model_calibration(
  truth = rep(2, ncol(known_effect)),
  estimate = known_effect["estimate", ],
  lower = known_effect["lower", ],
  upper = known_effect["upper", ],
  residual_moran = known_effect["residual_moran", ]
)
if (abs(calibration$bias) > 0.03 ||
    calibration$rmse > 0.08 ||
    calibration$coverage < 0.88 ||
    calibration$coverage > 1 ||
    abs(calibration$mean_residual_moran) > 0.25) {
  stop("Known-effect bias, RMSE, coverage, or residual Moran gate failed.")
}

set.seed(27003)
spatial_response <- as.numeric(
  solve(
    diag(nrow(coordinates)) - 0.3 * as.matrix(spatial_weights$matrix),
    1 + 2 * predictor + stats::rnorm(nrow(coordinates), sd = 0.03)
  )
)
spatial <- base
spatial$values[, "response"] <- spatial_response
one_worker <- ngeo_spatial_regression_uncertainty(
  spatial,
  "response",
  "predictor",
  spatial_weights,
  model = "sar",
  value_covariance = covariance,
  nsim = 30L,
  seed = 27004,
  workers = 1L
)
two_workers <- ngeo_spatial_regression_uncertainty(
  spatial,
  "response",
  "predictor",
  spatial_weights,
  model = "sar",
  value_covariance = covariance,
  nsim = 30L,
  seed = 27004,
  workers = 2L
)
worker_identical <- identical(
  one_worker$coefficient_simulations,
  two_workers$coefficient_simulations
)
if (!worker_identical) {
  stop("Seeded SAR simulations changed across worker counts.")
}

wrong_base <- ngeo_point(
  cbind(x = 1:4, y = 0),
  values = cbind(response = 1:4)
)
wrong_covariance <- ngeo_support_covariance(
  wrong_base, variance = rep(1, 4)
)
mutation_rejected <- inherits(tryCatch(
  {
    ngeo_car_uncertainty(
      base, "response", spatial_weights, wrong_covariance, precision = 1
    )
    NULL
  },
  error = identity
), "ngeo_error_base_mismatch")
if (!mutation_rejected) {
  stop("Model covariance base mutation was not rejected.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  direct_references = list(
    kriging_analytic_maximum_error = analytic_error,
    kriging_monte_carlo_maximum_relative_error =
      monte_carlo_relative_error,
    kriging_monte_carlo_draws = 5000L,
    car_estimate_maximum_error = car_estimate_error,
    car_covariance_maximum_error = car_covariance_error
  ),
  known_effect = list(
    simulations = ncol(known_effect),
    truth = 2,
    bias = calibration$bias,
    rmse = calibration$rmse,
    interval_coverage = calibration$coverage,
    mean_interval_width = calibration$mean_interval_width,
    mean_residual_moran = calibration$mean_residual_moran
  ),
  deterministic_simulation = list(
    nsim = 30L,
    seed = 27004L,
    one_two_worker_identical = worker_identical,
    successful_simulations = one_worker$successful_simulations
  ),
  boundaries = list(
    covariance_base_mutation_rejected = mutation_rejected,
    sensitivity_is_confidence_interval = FALSE,
    deterministic_car_claimed_bayesian = FALSE
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
