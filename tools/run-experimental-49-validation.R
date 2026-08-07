args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "experimental-49-validation.json")
required <- c("jsonlite", "adespatial", "ade4", "spdep", "gstat", "sf",
              "GWmodel")
missing <- required[!vapply(required, requireNamespace, logical(1),
                           quietly = TRUE)]
if (length(missing)) {
  stop("4.9 validation requires: ", paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

set.seed(4900L)
n <- 40L
coordinates <- cbind(runif(n), runif(n))
p1 <- scale(coordinates[, 1] + coordinates[, 2])[, 1] + rnorm(n, sd = 0.1)
p2 <- 0.7 * p1 + scale(coordinates[, 1] - coordinates[, 2])[, 1] * 0.3 +
  rnorm(n, sd = 0.1)
response <- 1 + 1.8 * p1 - 0.8 * p2 + rnorm(n, sd = 0.25)
x <- ngeo_point(
  coordinates,
  values = cbind(response = response, p1 = p1, p2 = p2)
)
spatial_weights <- ngeo_spatial_weights(
  x, method = "knn", k = 4L, style = "W", symmetry = "union"
)

ordination <- ngeo_spatial_ordination(x, c("p1", "p2"), spatial_weights, axes = 2L)
ordination_check <- list(
  backend = ordination$backend,
  axes = ncol(ordination$loadings),
  population_inference = ordination$population_inference,
  hash = ordination$ordination_hash,
  pass = identical(ordination$backend, "adespatial::multispati") &&
    !ordination$population_inference && all(is.finite(ordination$loadings))
)

pair_count <- n * (n - 1L) / 2L
empirical <- neurogeo:::.ngeo_cross_variograms(
  x, c("p1", "p2"), pair_sample = pair_count,
  breaks = 6L, seed = 4901L
)
cross_check <- list(
  requested_pairs = empirical$sampling$requested_pairs,
  retained_pairs = empirical$sampling$retained_pairs,
  distance_method = empirical$distance_method,
  convention = empirical$sampling$convention,
  hash = empirical$sampling$hash,
  pass = empirical$sampling$retained_pairs == pair_count &&
    identical(empirical$sampling$seed, 4901L) &&
    all(is.finite(empirical$table$gamma))
)

lmc <- ngeo_coregionalization(
  x, c("p1", "p2"), pair_sample = 500L,
  breaks = 6L, model = "Exp", range = 0.5, seed = 4902L
)
lmc_check <- list(
  backend = lmc$backend,
  minimum_sill_eigenvalue = min(lmc$psd_diagnostics$min_eigenvalue),
  singular_models = sum(lmc$convergence$singular),
  co_kriging = lmc$capabilities$co_kriging,
  hash = lmc$model_hash,
  pass = all(lmc$psd_diagnostics$positive_semidefinite) &&
    !lmc$capabilities$co_kriging
)

mgwr <- ngeo_mgwr(
  x, "response", c("p1", "p2"),
  bandwidths = c(0.9, 0.7, 0.8), max_iterations = 8L
)
mgwr_check <- list(
  backend = mgwr$backend,
  elements = nrow(mgwr$local),
  bandwidths = unname(mgwr$bandwidths),
  finite_effective_n = sum(is.finite(mgwr$local$effective_n)),
  finite_condition_number = sum(is.finite(mgwr$local$condition_number)),
  nominal_local_p_values = mgwr$inference$nominal_local_p_values,
  promotion_blockers = mgwr$promotion_blockers,
  hash = mgwr$model_hash,
  pass = nrow(mgwr$local) == n &&
    !mgwr$inference$nominal_local_p_values &&
    any(is.finite(mgwr$local$condition_number))
)

checks <- list(
  spatial_ordination = ordination_check,
  sampled_cross_variogram = cross_check,
  coregionalization = lmc_check,
  mgwr_feasibility = mgwr_check
)
pass <- all(vapply(checks, `[[`, logical(1), "pass"))
report <- list(
  schema = "neurogeo/experimental-49-validation",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  checks = checks,
  pass = pass,
  claim = paste(
    "Bounded experimental feasibility only; no population inference,",
    "co-kriging, nominal local p layers, or full-cortex MGWR claim."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE,
                     digits = 16)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) stop("Experimental 4.9 validation failed.", call. = FALSE)
