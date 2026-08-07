args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "support-inference-validation.json")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Support inference validation requires jsonlite.")
}
if (!exists("ngeo_support_test", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

make_case <- function(outcome, predictor) {
  n <- length(outcome)
  source <- ngeo_point(
    cbind(x = seq_len(n), y = sin(seq_len(n) / 8)),
    values = cbind(outcome = outcome, predictor = predictor),
    measures = rbind(
      ngeo_measure(support_behavior = "intensive"),
      ngeo_measure(support_behavior = "intensive")
    ),
    coordinate_space = ngeo_coordinate_space("support-simulation")
  )
  first_labels <- rep(paste0("R", seq_len(12L)), each = n / 12L)
  second_labels <- rep(
    paste0("R", seq_len(12L)),
    length.out = n
  )
  first <- ngeo_atlas_map(
    source,
    first_labels,
    source_support = rep.int(1, n)
  )
  second <- ngeo_atlas_map(
    source,
    second_labels,
    source_support = rep.int(1, n)
  )
  list(
    source = source,
    layers = list(block = first, interleaved = second),
    targets = list(first$target, second$target)
  )
}

set.seed(210726)
n_case <- 40L
alpha <- 0.05
null_rejected <- logical(n_case)
null_p <- matrix(NA_real_, nrow = n_case, ncol = 2L)
for (case in seq_len(n_case)) {
  fixture <- make_case(stats::rnorm(120L), stats::rnorm(120L))
  test <- ngeo_support_test(
    fixture$source,
    fixture$layers,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    nsim = 99L,
    seed = 5000L + case
  )
  null_p[case, ] <- test$estimates$adjusted_p_value
  null_rejected[[case]] <- any(null_p[case, ] <= alpha)
}
type_one_error <- mean(null_rejected)
type_one_limits <- c(0, 0.15)
if (type_one_error < type_one_limits[[1L]] ||
    type_one_error > type_one_limits[[2L]]) {
  stop("Support-aware null calibration exceeded declared tolerance.")
}

n_effect <- 20L
effect <- numeric(n_effect)
for (case in seq_len(n_effect)) {
  predictor <- stats::rnorm(120L)
  fixture <- make_case(
    1 + 2 * predictor + stats::rnorm(120L, sd = 0.5),
    predictor
  )
  fit <- ngeo_atlas_robust_effect(
    fixture$source,
    fixture$layers,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor"
  )
  effect[[case]] <- fit$consensus[["median"]]
}
mean_bias <- mean(effect) - 2
bias_limit <- 0.15
if (abs(mean_bias) > bias_limit) {
  stop("Atlas-robust effect bias exceeded declared tolerance.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  validation = "passed",
  null_calibration = list(
    scenarios = n_case,
    permutations_per_scenario = 99L,
    alpha = alpha,
    family = "two atlases with BH adjustment",
    empirical_type_one_error = type_one_error,
    accepted_interval = type_one_limits,
    permutation_domain = "common_source",
    spatial_constraint = FALSE
  ),
  effect_calibration = list(
    scenarios = n_effect,
    true_slope = 2,
    mean_median_slope = mean(effect),
    mean_bias = mean_bias,
    absolute_bias_limit = bias_limit
  ),
  seed = 210726L,
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
