args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "spatial-models-24-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Spatial-model validation requires jsonlite.")
}
if (!exists("ngeo_fit_variogram", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

distance <- seq(0.5, 4, length.out = 8)
empirical <- data.frame(
  bin = seq_along(distance),
  distance = distance,
  semivariance = 0.2 + 1.5 * (1 - exp(-3 * distance / 2)),
  n_pairs = rep.int(100L, length(distance))
)
class(empirical) <- c("ngeo_variogram", "data.frame")
variogram <- ngeo_fit_variogram(
  empirical,
  model = "exponential",
  start = c(nugget = 0.1, partial_sill = 1, range = 1)
)
variogram_error <- max(abs(
  variogram$parameters -
    c(nugget = 0.2, partial_sill = 1.5, range = 2)
))
if (variogram_error > 0.03) {
  stop("Variogram recovery exceeds tolerance.")
}

coordinates <- as.matrix(expand.grid(x = 0:4, y = 0:4))
predictor <- coordinates[, 1L] + 0.25 * coordinates[, 2L]
base <- ngeo_points(coordinates, values = cbind(predictor = predictor))
weights <- ngeo_weights(
  base, method = "distance_band", threshold = 1.01, style = "W"
)
set.seed(2401L)
rho <- 0.35
response <- as.numeric(solve(
  diag(25) - rho * as.matrix(weights$matrix),
  1 + 2 * predictor + stats::rnorm(25, sd = 0.03)
))
x <- ngeo_points(
  coordinates,
  values = cbind(response = response, predictor = predictor),
  measures = rbind(
    ngeo_measure(spatial_semantics = "intensive"),
    ngeo_measure(spatial_semantics = "intensive")
  )
)
weights$domain_hash <- ngeo_domain_hash(x)
sar <- ngeo_spatial_regression(
  x, "response", "predictor", weights, model = "sar"
)
rho_error <- abs(sar$spatial_parameter - rho)
if (rho_error > 0.12 ||
    !identical(sar$log_determinant_method, "exact_dense")) {
  stop("SAR reference recovery exceeds tolerance.")
}

linear <- x
linear$values[, "response"] <- 1 + 2 * predictor
bandwidth <- ngeo_gwr_bandwidth(
  linear, "response", "predictor",
  candidates = c(1.5, 2.5, 4.5)
)
gwr <- ngeo_gwr(
  linear, "response", "predictor", bandwidth,
  singular = "error"
)
gwr_error <- max(abs(gwr$predictor - 2))
if (gwr_error > 1e-8 || any(!is.finite(gwr$condition_number))) {
  stop("GWR recovery or conditioning validation failed.")
}

car <- ngeo_car(
  x, "response", weights,
  type = "intrinsic", precision = 1
)
if (!identical(car$constraint, "sum-to-zero spatial effect") ||
    !is.finite(car$gcv)) {
  stop("CAR constraint validation failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  variogram = list(
    model = variogram$model,
    maximum_parameter_error = variogram_error,
    tolerance = 0.03
  ),
  sar = list(
    expected_rho = rho,
    estimated_rho = sar$spatial_parameter,
    absolute_error = rho_error,
    tolerance = 0.12,
    log_determinant_method = sar$log_determinant_method
  ),
  gwr = list(
    selected_bandwidth = bandwidth$bandwidth,
    maximum_coefficient_error = gwr_error,
    finite_condition_numbers = TRUE
  ),
  car = list(
    type = car$type,
    constraint = car$constraint,
    precision = car$precision
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
