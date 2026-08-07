args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "support-uncertainty-validation.json")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Support uncertainty validation requires jsonlite.")
}
if (!exists("ngeo_support_uncertainty", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

source <- ngeo_point(
  cbind(x = 1:6, y = 0),
  values = cbind(signal = c(2, 4, 3, 8, 6, 10)),
  measures = ngeo_measure(support_behavior = "intensive"),
  coordinate_space = ngeo_coordinate_space("uncertainty-validation")
)
target <- ngeo_parcellation(
  data.frame(region_id = c("A", "B", "C")),
  support_size = rep(NA_real_, 3),
  coordinate_space = source$base$coordinate_space
)
first <- ngeo_support_map(
  source,
  target,
  c("A", "A", "B", "B", "C", "C"),
  source_support = c(1, 2, 1, 2, 1, 2)
)
operator <- rbind(
  c(1, 0.8, 0.2, 0, 0, 0),
  c(0, 0.2, 0.8, 1, 0, 0),
  c(0, 0, 0, 0, 1, 1)
)
second <- ngeo_support_map(
  source,
  target,
  operator,
  type = "probabilistic",
  source_support = c(1, 2, 1, 2, 1, 2)
)
covariance <- ngeo_support_covariance(
  source,
  variance = rep(0.5, 6),
  factor = matrix(rep(0.2, 6), ncol = 1L)
)
analytic <- ngeo_support_uncertainty(
  source,
  target,
  second,
  covariance,
  output = "covariance"
)
monte_carlo <- ngeo_support_uncertainty(
  source,
  target,
  second,
  covariance,
  method = "monte_carlo",
  nsim = 5000L,
  seed = 2201L
)
relative_error <- max(
  abs(monte_carlo$variance - analytic$variance) /
    pmax(analytic$variance, 1e-12)
)
relative_error_limit <- 0.08
if (relative_error > relative_error_limit) {
  stop("Analytic and Monte Carlo covariance exceed tolerance.")
}

ensemble <- ngeo_registration_ensemble(
  list(crisp = first, probabilistic = second),
  spatial_weights = c(0.4, 0.6)
)
sensitivity <- ngeo_support_sensitivity(
  source,
  target,
  ensemble
)
conditioning <- ngeo_support_condition(second)
if (!all(is.finite(sensitivity$distribution$total_variance)) ||
    conditioning$stable_rank <= 0 ||
    !identical(
      sensitivity$ensemble_hash,
      ensemble$ensemble_hash
    )) {
  stop("Ensemble sensitivity or conditioning validation failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  covariance = list(
    representation = covariance$representation,
    rank = ncol(covariance$factor),
    monte_carlo_draws = monte_carlo$nsim,
    maximum_relative_variance_error = relative_error,
    relative_error_limit = relative_error_limit
  ),
  ensemble = list(
    kind = ensemble$kind,
    layers = length(ensemble$layers),
    hash = ensemble$ensemble_hash,
    positive_between_operator_variance = any(
      sensitivity$distribution$between_operator_variance > 0
    )
  ),
  conditioning = list(
    stable_rank = conditioning$stable_rank,
    numerical_rank = conditioning$numerical_rank,
    condition_number = conditioning$condition_number
  ),
  seed = 2201L,
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
