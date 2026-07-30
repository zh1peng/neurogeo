args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "simulation.json")
}
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("pkgload", quietly = TRUE)) {
  stop("Simulation validation requires jsonlite and pkgload.")
}
pkgload::load_all(export_all = FALSE, helpers = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

coordinates <- as.matrix(expand.grid(x = 0:4, y = 0:4))
template <- ngeo_points(
  coordinates,
  values = cbind(signal = rep.int(0, nrow(coordinates))),
  measures = ngeo_measure(spatial_semantics = "intensive")
)
weights <- ngeo_weights(
  template,
  method = "distance_band",
  threshold = 1.01,
  style = "W"
)

set.seed(20260726)
type_i_p <- vapply(seq_len(100L), function(i) {
  current <- template
  current$values[, 1L] <- stats::rnorm(nrow(coordinates))
  ngeo_moran(
    current,
    weights,
    permutations = 99L,
    seed = 1000L + i,
    zero_policy = TRUE
  )$p.value
}, numeric(1))
type_i_rate <- mean(type_i_p <= 0.05)
if (type_i_rate < 0.01 || type_i_rate > 0.10) {
  stop("Moran permutation type-I rate is outside [0.01, 0.10].")
}

predictor <- coordinates[, 1L] + coordinates[, 2L] / 4
coefficient <- replicate(200L, {
  response <- 2 + 3 * predictor + stats::rnorm(length(predictor), sd = 0.5)
  current <- ngeo_points(
    coordinates,
    values = cbind(response = response, predictor = predictor)
  )
  fit <- ngeo_spatial_lm(current, "response", "predictor")
  fit$coefficients$estimate
})
coefficient_bias <- rowMeans(coefficient) - c(2, 3)
if (max(abs(coefficient_bias)) > 0.1) {
  stop("OLS coefficient bias exceeds 0.1.")
}

linear <- ngeo_points(
  cbind(x = 0:9, y = 0),
  values = cbind(response = 1 + 2 * (0:9), predictor = 0:9)
)
kernel <- ngeo_kernel_regression(
  linear,
  "response",
  "predictor",
  bandwidth = 3.1,
  kernel = "bisquare",
  singular = "error"
)
kernel_max_error <- max(abs(kernel$fitted - linear$values[, "response"]))
if (kernel_max_error > 1e-8) {
  stop("Kernel regression exact-field error exceeds 1e-8.")
}

spatial <- template
spatial$values[, 1L] <- predictor
null <- ngeo_moran_null(
  spatial,
  weights,
  nsim = 20L,
  seed = 99L,
  zero_policy = TRUE
)
null_moran <- apply(
  null$simulations,
  2L,
  function(values) neurogeo:::.ngeo_moran_value(values, weights$matrix)
)
moran_preservation_error <- max(abs(null_moran - null$observed_moran))
variance_preservation_error <- max(abs(
  apply(null$simulations, 2L, stats::var) -
    stats::var(spatial$values[, 1L])
))
if (moran_preservation_error > 1e-6 ||
    variance_preservation_error > 1e-6) {
  stop("Moran spectral preservation error exceeds 1e-6.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  cases = list(
    moran_type_i = list(
      simulations = 100L,
      permutations = 99L,
      alpha = 0.05,
      observed_rate = type_i_rate,
      accepted_interval = c(0.01, 0.10)
    ),
    ols_bias = list(
      simulations = 200L,
      coefficient_bias = coefficient_bias,
      absolute_tolerance = 0.1
    ),
    kernel_linear_field = list(
      maximum_absolute_error = kernel_max_error,
      tolerance = 1e-8
    ),
    moran_spectral = list(
      simulations = 20L,
      moran_preservation_error = moran_preservation_error,
      variance_preservation_error = variance_preservation_error,
      tolerance = 1e-6
    )
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
