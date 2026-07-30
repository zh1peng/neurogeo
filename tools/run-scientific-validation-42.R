args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "scientific-validation-42.json")
}

required <- c(
  "jsonlite", "spdep", "spatialreg", "gstat", "GWmodel", "sf"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Scientific validation requires: ",
    paste(missing, collapse = ", ")
  )
}
suppressPackageStartupMessages(library(neurogeo))

checks <- list()

add_agreement <- function(name, observed, reference, tolerance) {
  observed <- as.numeric(observed)
  reference <- as.numeric(reference)
  if (!identical(length(observed), length(reference)) ||
      any(!is.finite(observed)) || any(!is.finite(reference))) {
    pass <- FALSE
    maximum_error <- Inf
  } else {
    maximum_error <- max(abs(observed - reference), 0)
    pass <- maximum_error <= tolerance
  }
  checks[[name]] <<- list(
    kind = "reference_agreement",
    observed = observed,
    reference = reference,
    maximum_absolute_error = maximum_error,
    tolerance = tolerance,
    pass = pass
  )
  invisible(pass)
}

add_interval <- function(name, observed, lower, upper) {
  observed <- as.numeric(observed)
  pass <- length(observed) == 1L && is.finite(observed) &&
    observed >= lower && observed <= upper
  checks[[name]] <<- list(
    kind = "calibration_interval",
    observed = observed,
    lower = lower,
    upper = upper,
    pass = pass
  )
  invisible(pass)
}

add_boolean <- function(name, observed) {
  pass <- isTRUE(observed)
  checks[[name]] <<- list(
    kind = "invariant",
    observed = pass,
    pass = pass
  )
  invisible(pass)
}

has_error_class <- function(expr, class) {
  tryCatch(
    {
      force(expr)
      FALSE
    },
    error = function(condition) inherits(condition, class)
  )
}

grid_coordinates <- function(size) {
  as.matrix(expand.grid(x = 0:(size - 1L), y = 0:(size - 1L)))
}

make_rook_weights <- function(x, style = "W") {
  ngeo_weights(
    x,
    method = "distance_band",
    threshold = 1.01,
    style = style
  )
}

# Global and local association against spdep.
coordinates <- grid_coordinates(5L)
signal <- sin(coordinates[, 1L]) +
  0.3 * coordinates[, 2L] +
  cos(coordinates[, 1L] * coordinates[, 2L] / 4)
association_data <- ngeo_points(
  coordinates,
  values = cbind(signal = signal)
)

for (style in c("W", "B")) {
  weights <- make_rook_weights(association_data, style)
  listw <- as_spdep_listw(weights)
  n <- length(signal)
  s0 <- spdep::Szero(listw)
  moran_reference <- spdep::moran(
    signal, listw, n = n, S0 = s0, zero.policy = TRUE
  )$I
  geary_reference <- spdep::geary(
    signal, listw, n = n, n1 = n - 1L, S0 = s0,
    zero.policy = TRUE
  )$C
  add_agreement(
    paste0("moran_", style),
    ngeo_moran(association_data, weights)$estimate,
    moran_reference,
    1e-10
  )
  add_agreement(
    paste0("geary_", style),
    ngeo_geary(association_data, weights)$estimate,
    geary_reference,
    1e-10
  )
}

weights_w <- make_rook_weights(association_data, "W")
listw_w <- as_spdep_listw(weights_w)
local_observed <- ngeo_local_moran(
  association_data, weights_w, permutations = 0L
)$local_i
local_reference <- spdep::localmoran(
  signal,
  listw_w,
  zero.policy = TRUE,
  conditional = TRUE,
  mlvar = TRUE
)[, "Ii"]
add_agreement(
  "local_moran_population_variance",
  local_observed,
  local_reference,
  1e-10
)

# SLX against an independently assembled design matrix.
predictor <- coordinates[, 1L] + 0.25 * coordinates[, 2L]
slx_base <- ngeo_points(
  coordinates,
  values = cbind(predictor = predictor)
)
slx_weights <- make_rook_weights(slx_base, "W")
lagged_predictor <- as.numeric(slx_weights$matrix %*% predictor)
slx_response <- 2 + 3 * predictor + 1.5 * lagged_predictor +
  0.01 * sin(seq_along(predictor))
slx_data <- ngeo_points(
  coordinates,
  values = cbind(response = slx_response, predictor = predictor)
)
slx_fit <- ngeo_spatial_lm(
  slx_data,
  response = "response",
  predictors = "predictor",
  weights = slx_weights,
  model = "slx"
)
slx_reference <- stats::lm.fit(
  cbind(
    `(Intercept)` = 1,
    predictor = predictor,
    lag_predictor = lagged_predictor
  ),
  slx_response
)$coefficients
add_agreement(
  "slx_coefficients",
  slx_fit$coefficients$estimate,
  slx_reference,
  1e-10
)

# SAR and SEM against spatialreg on a matched row-standardized graph.
model_coordinates <- grid_coordinates(7L)
model_predictor <- as.numeric(scale(
  sin(model_coordinates[, 1L] / 2) +
    0.2 * model_coordinates[, 2L]
))
model_base <- ngeo_points(
  model_coordinates,
  values = cbind(predictor = model_predictor)
)
model_weights <- make_rook_weights(model_base, "W")
model_matrix <- as.matrix(model_weights$matrix)
model_design <- cbind(1, model_predictor)
model_n <- nrow(model_coordinates)
set.seed(4201)
model_error <- stats::rnorm(model_n, sd = 0.35)
sar_response <- as.numeric(solve(
  diag(model_n) - 0.35 * model_matrix,
  model_design %*% c(1, 1.5) + model_error
))
sem_response <- as.numeric(
  model_design %*% c(1, 1.5) +
    solve(diag(model_n) - 0.30 * model_matrix, model_error)
)
sar_data <- ngeo_points(
  model_coordinates,
  values = cbind(response = sar_response, predictor = model_predictor)
)
sem_data <- ngeo_points(
  model_coordinates,
  values = cbind(response = sem_response, predictor = model_predictor)
)
sar_fit <- ngeo_spatial_regression(
  sar_data, "response", "predictor", model_weights, model = "sar"
)
sem_fit <- ngeo_spatial_regression(
  sem_data, "response", "predictor", model_weights, model = "sem"
)
model_listw <- as_spdep_listw(model_weights)
sar_reference <- spatialreg::lagsarlm(
  response ~ predictor,
  data = data.frame(
    response = sar_response,
    predictor = model_predictor
  ),
  listw = model_listw,
  method = "eigen",
  zero.policy = TRUE,
  quiet = TRUE
)
sem_reference <- spatialreg::errorsarlm(
  response ~ predictor,
  data = data.frame(
    response = sem_response,
    predictor = model_predictor
  ),
  listw = model_listw,
  method = "eigen",
  zero.policy = TRUE,
  quiet = TRUE
)
add_agreement(
  "sar_parameter",
  sar_fit$spatial_parameter,
  sar_reference$rho,
  1e-6
)
add_agreement(
  "sar_coefficients",
  sar_fit$coefficients$estimate,
  stats::coef(sar_reference)[c("(Intercept)", "predictor")],
  1e-6
)
add_agreement(
  "sar_log_likelihood",
  sar_fit$logLik,
  as.numeric(stats::logLik(sar_reference)),
  1e-6
)
add_agreement(
  "sem_parameter",
  sem_fit$spatial_parameter,
  sem_reference$lambda,
  1e-6
)
add_agreement(
  "sem_coefficients",
  sem_fit$coefficients$estimate,
  stats::coef(sem_reference)[c("(Intercept)", "predictor")],
  1e-6
)
add_agreement(
  "sem_log_likelihood",
  sem_fit$logLik,
  as.numeric(stats::logLik(sem_reference)),
  1e-6
)

# Spherical variogram and ordinary kriging against gstat.
training_coordinates <- matrix(
  c(
    0, 0, 1, 0, 0, 1, 1, 1,
    2, 0, 0, 2, 2, 2, 1, 2
  ),
  ncol = 2L,
  byrow = TRUE
)
training_values <- sin(training_coordinates[, 1L]) +
  cos(training_coordinates[, 2L])
kriging_data <- ngeo_points(
  training_coordinates,
  values = cbind(signal = training_values)
)
variogram_parameters <- c(
  nugget = 0.1,
  partial_sill = 1.2,
  range = 3
)
variogram_fit <- structure(
  list(
    model = "spherical",
    parameters = variogram_parameters,
    metric = "euclidean",
    domain_hash = ngeo_domain_hash(kriging_data)
  ),
  class = "ngeo_variogram_fit"
)
distance_fixture <- c(0, 0.25, 1, 2.5, 3, 4)
variogram_observed <- neurogeo:::.ngeo_variogram_curve(
  distance_fixture,
  "spherical",
  variogram_parameters[["nugget"]],
  variogram_parameters[["partial_sill"]],
  variogram_parameters[["range"]]
)
gstat_model <- gstat::vgm(
  psill = variogram_parameters[["partial_sill"]],
  model = "Sph",
  range = variogram_parameters[["range"]],
  nugget = variogram_parameters[["nugget"]]
)
variogram_reference <- gstat::variogramLine(
  gstat_model,
  dist_vector = distance_fixture
)$gamma
add_agreement(
  "spherical_variogram_curve",
  variogram_observed,
  variogram_reference,
  1e-10
)

empirical <- ngeo_variogram(
  kriging_data,
  map = "signal",
  metric = "euclidean",
  breaks = c(0, 1, 2, 3)
)
pair <- utils::combn(seq_len(nrow(training_coordinates)), 2L)
pair_distance <- sqrt(rowSums(
  (
    training_coordinates[pair[1L, ], , drop = FALSE] -
      training_coordinates[pair[2L, ], , drop = FALSE]
  )^2
))
pair_semivariance <- 0.5 * (
  training_values[pair[1L, ]] - training_values[pair[2L, ]]
)^2
pair_bin <- cut(
  pair_distance,
  c(0, 1, 2, 3),
  include.lowest = TRUE,
  right = TRUE
)
manual_empirical <- do.call(
  rbind,
  lapply(levels(pair_bin), function(level) {
    selected <- which(pair_bin == level)
    if (!length(selected)) return(NULL)
    c(
      distance = mean(pair_distance[selected]),
      semivariance = mean(pair_semivariance[selected]),
      n_pairs = length(selected)
    )
  })
)
add_agreement(
  "empirical_variogram_distance",
  empirical$distance,
  manual_empirical[, "distance"],
  1e-12
)
add_agreement(
  "empirical_variogram_semivariance",
  empirical$semivariance,
  manual_empirical[, "semivariance"],
  1e-12
)
add_agreement(
  "empirical_variogram_pair_count",
  empirical$n_pairs,
  manual_empirical[, "n_pairs"],
  0
)

target_coordinates_2d <- matrix(
  c(0.5, 0.5, 1.5, 1.5),
  ncol = 2L,
  byrow = TRUE
)
target_coordinates_3d <- cbind(target_coordinates_2d, 0)
kriging_observed <- ngeo_kriging(
  kriging_data,
  "signal",
  variogram_fit,
  targets = target_coordinates_3d,
  neighbors = nrow(training_coordinates),
  metric = "euclidean"
)
training_sf <- sf::st_as_sf(
  data.frame(
    signal = training_values,
    x = training_coordinates[, 1L],
    y = training_coordinates[, 2L]
  ),
  coords = c("x", "y")
)
target_sf <- sf::st_as_sf(
  data.frame(
    x = target_coordinates_2d[, 1L],
    y = target_coordinates_2d[, 2L]
  ),
  coords = c("x", "y")
)
invisible(utils::capture.output(
  kriging_reference <- suppressMessages(gstat::krige(
    signal ~ 1,
    training_sf,
    target_sf,
    model = gstat_model,
    nmax = nrow(training_coordinates)
  ))
))
add_agreement(
  "ordinary_kriging_prediction",
  kriging_observed$prediction,
  kriging_reference$var1.pred,
  1e-8
)
add_agreement(
  "ordinary_kriging_variance",
  kriging_observed$variance,
  kriging_reference$var1.var,
  1e-8
)

# Gaussian GWR against GWmodel. Every pair is within three bandwidths.
gwr_coordinates <- grid_coordinates(5L)
gwr_predictor <- sin(gwr_coordinates[, 1L]) +
  0.2 * gwr_coordinates[, 2L]
gwr_response <- 1 + 2 * gwr_predictor +
  0.1 * gwr_coordinates[, 1L] * gwr_predictor
gwr_data <- ngeo_points(
  gwr_coordinates,
  values = cbind(response = gwr_response, predictor = gwr_predictor)
)
gwr_observed <- ngeo_gwr(
  gwr_data,
  "response",
  "predictor",
  bandwidth = 3,
  metric = "euclidean",
  kernel = "gaussian"
)
gwr_sf <- sf::st_as_sf(
  data.frame(
    response = gwr_response,
    predictor = gwr_predictor,
    x = gwr_coordinates[, 1L],
    y = gwr_coordinates[, 2L]
  ),
  coords = c("x", "y")
)
gwr_reference <- GWmodel::gwr.basic(
  response ~ predictor,
  data = gwr_sf,
  bw = 3,
  kernel = "gaussian",
  adaptive = FALSE,
  longlat = FALSE
)
gwr_reference_data <- sf::st_drop_geometry(gwr_reference$SDF)
add_agreement(
  "gwr_intercept",
  gwr_observed[["(Intercept)"]],
  gwr_reference_data$Intercept,
  1e-8
)
add_agreement(
  "gwr_predictor",
  gwr_observed$predictor,
  gwr_reference_data$predictor,
  1e-8
)

# Edge cases: isolates, missing values, disconnected graphs, and seeds.
isolate_coordinates <- rbind(
  matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2L, byrow = TRUE),
  c(10, 10)
)
isolate_data <- ngeo_points(
  isolate_coordinates,
  values = cbind(signal = c(1, 2, 4, 8, 16))
)
isolate_weights <- ngeo_weights(
  isolate_data,
  method = "distance_band",
  threshold = 1.01,
  style = "W"
)
add_boolean(
  "isolate_rejected_without_zero_policy",
  has_error_class(
    ngeo_moran(isolate_data, isolate_weights),
    "ngeo_error_zero_policy"
  )
)
add_boolean(
  "isolate_allowed_with_zero_policy",
  is.finite(ngeo_moran(
    isolate_data,
    isolate_weights,
    zero_policy = TRUE
  )$estimate)
)

missing_values <- signal
missing_values[[1L]] <- NA_real_
missing_data <- ngeo_points(
  coordinates,
  values = cbind(signal = missing_values)
)
missing_result <- ngeo_moran(
  missing_data,
  weights_w,
  na_action = "omit",
  zero_policy = TRUE
)
complete <- which(is.finite(missing_values))
missing_matrix <- weights_w$raw_matrix[complete, complete, drop = FALSE]
row_sum <- Matrix::rowSums(missing_matrix)
inverse <- ifelse(row_sum > 0, 1 / row_sum, 0)
missing_matrix <- Matrix::Diagonal(x = inverse) %*% missing_matrix
centered_missing <- missing_values[complete] -
  mean(missing_values[complete])
manual_moran <- length(complete) / sum(missing_matrix) *
  as.numeric(crossprod(
    centered_missing,
    missing_matrix %*% centered_missing
  )) / sum(centered_missing^2)
add_agreement(
  "missing_omit_moran",
  missing_result$estimate,
  manual_moran,
  1e-10
)
add_boolean("missing_omit_count", missing_result$omitted == 1L)

disconnected_coordinates <- rbind(
  matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2L, byrow = TRUE),
  matrix(c(10, 0, 11, 0, 10, 1, 11, 1), ncol = 2L, byrow = TRUE)
)
disconnected_data <- ngeo_points(
  disconnected_coordinates,
  values = cbind(signal = seq_len(8L))
)
disconnected_weights <- ngeo_weights(
  disconnected_data,
  method = "distance_band",
  threshold = 1.01,
  style = "W"
)
disconnected_components <- ngeo_components(
  disconnected_weights$raw_matrix
)
add_boolean(
  "disconnected_components",
  length(unique(disconnected_components)) == 2L &&
    is.finite(ngeo_moran(
      disconnected_data,
      disconnected_weights
    )$estimate)
)

seeded_first <- ngeo_moran(
  association_data,
  weights_w,
  permutations = 99L,
  seed = 4242,
  zero_policy = TRUE
)
seeded_second <- ngeo_moran(
  association_data,
  weights_w,
  permutations = 99L,
  seed = 4242,
  zero_policy = TRUE
)
add_boolean(
  "seeded_permutation_reproducibility",
  identical(seeded_first$simulated, seeded_second$simulated) &&
    identical(seeded_first$p.value, seeded_second$p.value)
)

# Known-parameter calibration.
set.seed(4210)
moran_p <- vapply(seq_len(80L), function(i) {
  current <- ngeo_points(
    model_coordinates,
    values = cbind(signal = stats::rnorm(model_n))
  )
  ngeo_moran(
    current,
    model_weights,
    permutations = 199L,
    seed = 8000L + i,
    zero_policy = TRUE
  )$p.value
}, numeric(1))
moran_type1 <- mean(moran_p <= 0.05)
add_interval("moran_type1_error", moran_type1, 0.01, 0.12)

set.seed(4202)
spatial_parameter <- replicate(50L, {
  current_error <- stats::rnorm(model_n, sd = 0.5)
  current_sar <- as.numeric(solve(
    diag(model_n) - 0.35 * model_matrix,
    model_design %*% c(1, 1.5) + current_error
  ))
  current_sem <- as.numeric(
    model_design %*% c(1, 1.5) +
      solve(diag(model_n) - 0.30 * model_matrix, current_error)
  )
  current_sar_data <- ngeo_points(
    model_coordinates,
    values = cbind(
      response = current_sar,
      predictor = model_predictor
    )
  )
  current_sem_data <- ngeo_points(
    model_coordinates,
    values = cbind(
      response = current_sem,
      predictor = model_predictor
    )
  )
  c(
    sar = ngeo_spatial_regression(
      current_sar_data,
      "response",
      "predictor",
      model_weights,
      model = "sar"
    )$spatial_parameter,
    sem = ngeo_spatial_regression(
      current_sem_data,
      "response",
      "predictor",
      model_weights,
      model = "sem"
    )$spatial_parameter
  )
})
parameter_truth <- c(sar = 0.35, sem = 0.30)
parameter_bias <- rowMeans(spatial_parameter) - parameter_truth
parameter_rmse <- sqrt(rowMeans(
  (spatial_parameter - parameter_truth)^2
))
add_interval(
  "sar_parameter_absolute_bias",
  abs(parameter_bias[["sar"]]),
  0,
  0.12
)
add_interval(
  "sem_parameter_absolute_bias",
  abs(parameter_bias[["sem"]]),
  0,
  0.12
)
add_interval(
  "sar_parameter_rmse",
  parameter_rmse[["sar"]],
  0,
  0.25
)
add_interval(
  "sem_parameter_rmse",
  parameter_rmse[["sem"]],
  0,
  0.25
)

spherical_structure <- function(distance, range) {
  ratio <- distance / range
  ifelse(ratio < 1, 1.5 * ratio - 0.5 * ratio^3, 1)
}
all_kriging_coordinates <- rbind(
  training_coordinates,
  target_coordinates_2d
)
kriging_distance <- as.matrix(stats::dist(all_kriging_coordinates))
kriging_covariance <- variogram_parameters[["partial_sill"]] *
  (1 - spherical_structure(
    kriging_distance,
    variogram_parameters[["range"]]
  ))
diag(kriging_covariance) <-
  variogram_parameters[["partial_sill"]] +
  variogram_parameters[["nugget"]]
kriging_cholesky <- chol(
  kriging_covariance +
    diag(1e-10, nrow(kriging_covariance))
)
set.seed(4203)
kriging_error <- numeric()
kriging_standardized <- numeric()
for (i in seq_len(120L)) {
  realization <- as.numeric(
    t(kriging_cholesky) %*%
      stats::rnorm(nrow(kriging_covariance))
  )
  current_data <- ngeo_points(
    training_coordinates,
    values = cbind(
      signal = realization[seq_len(nrow(training_coordinates))]
    )
  )
  current_fit <- variogram_fit
  current_fit$domain_hash <- ngeo_domain_hash(current_data)
  current_prediction <- ngeo_kriging(
    current_data,
    "signal",
    current_fit,
    targets = target_coordinates_3d,
    neighbors = nrow(training_coordinates),
    metric = "euclidean"
  )
  target_index <- nrow(training_coordinates) +
    seq_len(nrow(target_coordinates_2d))
  error <- realization[target_index] -
    current_prediction$prediction
  kriging_error <- c(kriging_error, error)
  kriging_standardized <- c(
    kriging_standardized,
    error / current_prediction$standard_error
  )
}
kriging_bias <- mean(kriging_error)
kriging_rmse <- sqrt(mean(kriging_error^2))
kriging_coverage <- mean(
  abs(kriging_standardized) <= stats::qnorm(0.975)
)
add_interval(
  "kriging_absolute_bias",
  abs(kriging_bias),
  0,
  0.20
)
add_interval(
  "kriging_nominal_coverage",
  kriging_coverage,
  0.88,
  0.99
)
add_interval(
  "kriging_standardized_error_sd",
  stats::sd(kriging_standardized),
  0.80,
  1.25
)

set.seed(4204)
gwr_calibration <- replicate(60L, {
  current_response <- 1 + 2 * model.matrix(
    ~ gwr_predictor
  )[, "gwr_predictor"] + stats::rnorm(length(gwr_predictor), sd = 0.35)
  current_data <- ngeo_points(
    gwr_coordinates,
    values = cbind(
      response = current_response,
      predictor = gwr_predictor
    )
  )
  current_fit <- ngeo_gwr(
    current_data,
    "response",
    "predictor",
    bandwidth = 3,
    metric = "euclidean",
    kernel = "gaussian"
  )
  c(
    intercept_mean = mean(current_fit[["(Intercept)"]]),
    predictor_mean = mean(current_fit$predictor),
    intercept_rmse = sqrt(mean(
      (current_fit[["(Intercept)"]] - 1)^2
    )),
    predictor_rmse = sqrt(mean(
      (current_fit$predictor - 2)^2
    ))
  )
})
gwr_summary <- rowMeans(gwr_calibration)
add_interval(
  "gwr_intercept_absolute_bias",
  abs(gwr_summary[["intercept_mean"]] - 1),
  0,
  0.05
)
add_interval(
  "gwr_predictor_absolute_bias",
  abs(gwr_summary[["predictor_mean"]] - 2),
  0,
  0.05
)
add_interval(
  "gwr_intercept_rmse",
  gwr_summary[["intercept_rmse"]],
  0,
  0.15
)
add_interval(
  "gwr_predictor_rmse",
  gwr_summary[["predictor_rmse"]],
  0,
  0.15
)

passed <- vapply(checks, function(x) isTRUE(x$pass), logical(1))
report <- list(
  schema_version = "1",
  package = "neurogeo",
  package_version = as.character(utils::packageVersion("neurogeo")),
  specification = "NGCS 3.5",
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  reference_packages = as.list(vapply(
    c("spdep", "spatialreg", "gstat", "GWmodel"),
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  )),
  seeds = list(
    reference_models = 4201L,
    spatial_parameter_calibration = 4202L,
    kriging_calibration = 4203L,
    gwr_calibration = 4204L,
    moran_null_fields = 4210L,
    permutation_reproducibility = 4242L
  ),
  simulation_counts = list(
    moran_fields = 80L,
    moran_permutations = 199L,
    sar_sem_fields = 50L,
    kriging_fields = 120L,
    gwr_fields = 60L
  ),
  checks = checks,
  summaries = list(
    moran_type1_error = moran_type1,
    spatial_parameter_bias = as.list(parameter_bias),
    spatial_parameter_rmse = as.list(parameter_rmse),
    kriging_bias = kriging_bias,
    kriging_rmse = kriging_rmse,
    kriging_coverage = kriging_coverage,
    gwr = as.list(gwr_summary)
  ),
  claim_boundary = paste(
    "Matched small-domain estimands and seeded calibration only;",
    "not clinical validation, unknown registration, universal asymptotics,",
    "Bayesian CAR, large-domain exact SAR/SEM, or group inference."
  ),
  passed = all(passed)
)

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report,
  output,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!all(passed)) {
  stop(
    "Scientific validation failed: ",
    paste(names(passed)[!passed], collapse = ", ")
  )
}
