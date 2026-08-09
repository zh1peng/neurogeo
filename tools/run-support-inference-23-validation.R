args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "support-inference-23-validation.json")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Support inference validation requires jsonlite.")
}
if (!exists("ngeo_common_support_test", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

coordinates <- as.matrix(expand.grid(x = 0:3, y = 0:2))
source <- ngeo_point(
  coordinates,
  values = cbind(outcome = seq_len(12), predictor = seq_len(12)),
  measures = rbind(
    ngeo_measure(support_behavior = "intensive"),
    ngeo_measure(support_behavior = "intensive")
  ),
  coordinate_space = ngeo_coordinate_space("inference-23-validation")
)
labels <- list(
  rows = rep(c("A", "B", "C", "D"), each = 3),
  columns = rep(c("A", "B", "C", "D"), times = 3),
  shifted = c("A", "A", "B", "B", "B", "C",
              "C", "C", "D", "D", "D", "A")
)
first <- ngeo_atlas_map(
  source,
  labels[[1L]],
  source_support = rep.int(1, 12)
)
target <- first$target
layers <- c(
  list(rows = first),
  lapply(labels[-1L], function(value) {
    ngeo_atlas_map(
      source,
      value,
      target = target,
      source_support = rep.int(1, 12)
    )
  })
)
targets <- rep(list(target), length(layers))

observed <- c(2, -1, 0.5)
simulated <- rbind(
  c(0, 0, 0),
  c(1, -2, 0.25),
  c(3, 0.5, -1)
)
maximum <- apply(abs(simulated), 1L, max)
reference_max_t <- vapply(abs(observed), function(value) {
  (1 + sum(maximum >= value)) / (nrow(simulated) + 1)
}, numeric(1))
implemented_max_t <- ngeo_support_adjust(
  method = "maxT",
  observed = observed,
  simulated = simulated
)$adjusted
if (!isTRUE(all.equal(implemented_max_t, reference_max_t))) {
  stop("max-T family adjustment does not match the direct reference.")
}

set.seed(2301L)
predictor <- stats::rnorm(12)
outcome <- 1 + 2 * predictor + stats::rnorm(12, sd = 0.08)
source$values[, "predictor"] <- predictor
source$values[, "outcome"] <- outcome
effect <- ngeo_atlas_robust_effect(
  source,
  layers,
  targets,
  outcome = "outcome",
  predictor = "predictor"
)
effect_bias <- max(abs(effect$estimates$estimate - 2))
effect_bias_limit <- 0.2
coverage <- all(
  effect$estimates$confidence_lower <= 2 &
    effect$estimates$confidence_upper >= 2
)
if (effect_bias > effect_bias_limit || !coverage) {
  stop("Known-effect bias or interval coverage exceeds tolerance.")
}

first_test <- ngeo_common_support_test(
  source,
  layers,
  targets,
  outcome = "outcome",
  predictor = "predictor",
  nsim = 49L,
  seed = 2302L
)
second_test <- ngeo_common_support_test(
  source,
  layers,
  targets,
  outcome = "outcome",
  predictor = "predictor",
  nsim = 49L,
  seed = 2302L
)
if (!identical(first_test$simulated, second_test$simulated) ||
    !identical(first_test$estimates, second_test$estimates)) {
  stop("Common-source simulations are not reproducible.")
}

set.seed(2303L)
experiments <- 30L
family_rejection <- logical(experiments)
for (i in seq_len(experiments)) {
  source$values[, "outcome"] <- stats::rnorm(12)
  source$values[, "predictor"] <- stats::rnorm(12)
  current <- ngeo_common_support_test(
    source,
    layers,
    targets,
    outcome = "outcome",
    predictor = "predictor",
    statistic = "correlation",
    nsim = 49L,
    seed = 2400L + i
  )
  family_rejection[[i]] <- any(
    current$estimates$adjusted_p_value <= 0.05
  )
}
family_error <- mean(family_rejection)
family_error_limit <- 0.2
if (family_error > family_error_limit) {
  stop("Empirical max-T family error exceeds tolerance.")
}

consensus <- ngeo_cross_atlas_consensus(
  c(1, 2, 4),
  c(1, 2, 1),
  method = "fixed",
  labels = c("a", "b", "c"),
  independence = TRUE
)
reference_consensus <- sum(c(1, 2, 4) / c(1, 2, 1)^2) /
  sum(1 / c(1, 2, 1)^2)
if (!isTRUE(all.equal(consensus$estimate, reference_consensus))) {
  stop("Consensus does not match inverse-variance reference.")
}
descriptive <- ngeo_cross_atlas_consensus(c(1, 2, 4), c(1, 2, 1))
if (!identical(descriptive$inference_mode, "descriptive") ||
    !is.na(descriptive$p_value)) {
  stop("Cross-atlas consensus did not default to descriptive output.")
}
atlas_correlation <- outer(1:3, 1:3, function(i, j) 0.6^abs(i - j))
atlas_covariance <- atlas_correlation * tcrossprod(c(1, 2, 1))
aware <- ngeo_cross_atlas_consensus(
  c(1, 2, 4), covariance = atlas_covariance
)
precision <- solve(atlas_covariance)
one <- rep(1, 3L)
denominator <- as.numeric(crossprod(one, precision %*% one))
reference_aware <- sum(as.numeric(precision %*% one) * c(1, 2, 4)) /
  denominator
if (!identical(aware$inference_mode, "covariance-aware") ||
    !isTRUE(all.equal(aware$estimate, reference_aware, tolerance = 1e-12)) ||
    !isTRUE(all.equal(
      aware$standard_error, sqrt(1 / denominator), tolerance = 1e-12
    ))) {
  stop("Covariance-aware consensus differs from the GLS reference.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  max_t = list(
    tests = length(observed),
    simulations = nrow(simulated),
    direct_reference_match = TRUE
  ),
  known_effect = list(
    expected_slope = 2,
    maximum_absolute_bias = effect_bias,
    bias_limit = effect_bias_limit,
    all_atlas_intervals_cover = coverage
  ),
  cross_atlas = list(
    descriptive_default = identical(descriptive$inference_mode, "descriptive") &&
      is.na(descriptive$p_value),
    independence_reference_error = abs(consensus$estimate - reference_consensus),
    covariance_reference_error = abs(aware$estimate - reference_aware),
    covariance_standard_error = aware$standard_error
  ),
  null_calibration = list(
    experiments = experiments,
    simulations_per_experiment = 49L,
    alpha = 0.05,
    empirical_family_error = family_error,
    family_error_limit = family_error_limit
  ),
  reproducibility = list(
    seed = 2302L,
    identical_common_simulations = TRUE
  ),
  consensus = list(
    independent_reference_match = TRUE,
    estimate = consensus$estimate
  ),
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
