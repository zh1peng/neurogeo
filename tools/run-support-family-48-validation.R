args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "support-family-48-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Support-family 4.8 validation requires jsonlite.")
}
suppressPackageStartupMessages(library(neurogeo))

make_features <- function(values, ids, support_id, scale_type = "rank_matched",
                          boundary = NULL) {
  values <- as.matrix(values)
  if (is.null(colnames(values))) {
    colnames(values) <- paste0("endpoint_", seq_len(ncol(values)))
  }
  support_hash <- paste0("sha256-", support_id)
  endpoints <- data.frame(
    endpoint_id = colnames(values), family = "simulation",
    estimand = rep(c("spectral_coupling", "band_energy_x"),
                   length.out = ncol(values)),
    layer_x = "x", layer_y = "y", direction = "none",
    component = "cortex", band = rep(c("low", "high"),
                                     length.out = ncol(values)),
    scale_type = scale_type,
    eigenvalue_min = 0.1, eigenvalue_max = 1,
    mode_count = 8L,
    bounds = rep(c("[-1,1]", "unbounded"), length.out = ncol(values)),
    recommended_transform = rep(c("fisher_z", "none"),
                                length.out = ncol(values)),
    support_hash = support_hash,
    stringsAsFactors = FALSE
  )
  structure(list(
    values = values,
    units = data.frame(unit_id = ids, stringsAsFactors = FALSE),
    endpoints = endpoints,
    diagnostics = list(boundary_sensitivity = boundary),
    provenance = list(support_hash = support_hash)
  ), class = "ngeo_subject_features")
}

correlated_support_null <- function(n, p, support_count) {
  common <- matrix(rnorm(n * p), n, p)
  lapply(seq_len(support_count), function(i) {
    sqrt(0.75) * common + sqrt(0.25) * matrix(rnorm(n * p), n, p)
  })
}

full <- identical(
  tolower(Sys.getenv("NEUROGEO_SUPPORT_FULL_CALIBRATION")), "true"
)
replicates <- if (full) 100L else 20L
permutations <- if (full) 199L else 99L
alpha <- 0.05
set.seed(4800L)

# Correlated support-family null and full-family FWER.
n <- 40L
p <- 4L
support_count <- 4L
ids <- paste0("s", seq_len(n))
group <- factor(rep(c("a", "b"), each = n / 2L))
design <- data.frame(unit_id = ids, group = group)
schedule <- ngeo_exchangeability(
  ids, permutations = permutations, seed = 4811L
)
family_rejection <- logical(replicates)
endpoint_rejection <- 0L
for (iteration in seq_len(replicates)) {
  generated <- correlated_support_null(n, p, support_count)
  features <- stats::setNames(lapply(seq_len(support_count), function(i) {
    colnames(generated[[i]]) <- paste0("e", seq_len(p))
    make_features(generated[[i]], ids, paste0("support", i))
  }), paste0("support", seq_len(support_count)))
  result <- ngeo_group_test(
    features, design, ~ group, "group", schedule, transform = "none"
  )
  family_rejection[[iteration]] <- any(result$tests$p_maxT <= alpha)
  endpoint_rejection <- endpoint_rejection + sum(result$tests$p_raw <= alpha)
}
upper <- stats::qbinom(0.995, replicates, alpha) / replicates
checks <- list(
  correlated_family_null = list(
    replicates = replicates, supports = support_count,
    endpoints_per_support = p, permutations = permutations,
    endpoint_type1 = endpoint_rejection /
      (replicates * support_count * p),
    family_wise_error = mean(family_rejection),
    binomial_99_5_upper_gate = upper
  )
)
checks$correlated_family_null$pass <-
  checks$correlated_family_null$family_wise_error <= upper &&
  checks$correlated_family_null$endpoint_type1 <= upper

# Identical supports and declared rank matching.
signal <- matrix(rnorm(n * 2L), n, 2L)
signal[group == "b", 1L] <- signal[group == "b", 1L] + 0.8
signal[, 1L] <- tanh(signal[, 1L])
colnames(signal) <- c("coupling", "energy")
identical_result <- ngeo_group_test(
  list(
    fine = make_features(signal, ids, "fine"),
    repeated = make_features(signal, ids, "repeated")
  ),
  design, ~ group, "group", schedule
)
checks$identical_support <- list(
  stability_rows = nrow(identical_result$support$stability),
  maximum_effect_range = max(identical_result$support$stability$effect_range),
  common_schedule = identical_result$diagnostics$common_schedule_all_supports,
  schedule_hash = identical_result$support$schedule_hash,
  pass = nrow(identical_result$support$stability) == 2L &&
    max(identical_result$support$stability$effect_range) <= 1e-12 &&
    identical(identical_result$diagnostics$common_schedule_all_supports, TRUE)
)

# Boundary perturbation and one-support sign reversal remain visible.
group_numeric <- as.numeric(group == "b")
noise <- matrix(rnorm(n * 2L, sd = 0.2), n, 2L)
stable_a <- noise + cbind(0.7 * group_numeric, 0.5 * group_numeric)
stable_b <- noise + cbind(0.6 * group_numeric, 0.45 * group_numeric)
reversed <- noise + cbind(-1.2 * group_numeric, -0.9 * group_numeric)
colnames(stable_a) <- colnames(stable_b) <- colnames(reversed) <-
  c("coupling", "energy")
perturbed_result <- suppressWarnings(ngeo_group_test(
  list(
    atlas_a = make_features(stable_a, ids, "atlas-a",
                            boundary = list(mean = 0.1)),
    atlas_b = make_features(stable_b, ids, "atlas-b",
                            boundary = list(mean = 0.2)),
    boundary_driver = make_features(reversed, ids, "boundary-driver",
                                    boundary = list(mean = 0.8))
  ),
  design, ~ group, "group", schedule, transform = "none"
))
checks$boundary_sign_reversal <- list(
  direction_disagreement = any(
    !perturbed_result$support$stability$direction_agreement
  ),
  driving_supports = unique(
    perturbed_result$support$stability$driving_support
  ),
  boundary_diagnostics = sum(perturbed_result$support$boundary$available),
  boundary_p_values_reused =
    perturbed_result$support$diagnostics$boundary_p_values_reused,
  pass = any(!perturbed_result$support$stability$direction_agreement) &&
    "boundary_driver" %in%
      perturbed_result$support$stability$driving_support &&
    sum(perturbed_result$support$boundary$available) == 3L &&
    identical(
      perturbed_result$support$diagnostics$boundary_p_values_reused, FALSE
    )
)

# Rank matched and unmatched scale behavior.
unmatched_result <- ngeo_group_test(
  list(
    first = make_features(signal, ids, "unmatched-a", "unmatched"),
    second = make_features(signal, ids, "unmatched-b", "unmatched")
  ),
  design, ~ group, "group", schedule
)
checks$scale_contract <- list(
  rank_matched_rows = nrow(identical_result$support$stability),
  unmatched_rows = nrow(unmatched_result$support$stability),
  unmatched_endpoints =
    unmatched_result$support$diagnostics$unmatched_endpoints,
  pass = nrow(identical_result$support$stability) > 0L &&
    nrow(unmatched_result$support$stability) == 0L &&
    unmatched_result$support$diagnostics$unmatched_endpoints == 4L
)

# Larger full-family streaming and one support-family hash.
performance_n <- if (full) 160L else 80L
performance_supports <- if (full) 10L else 6L
performance_p <- if (full) 64L else 32L
performance_b <- if (full) 999L else 199L
ids_perf <- paste0("p", seq_len(performance_n))
group_perf <- factor(rep(c("a", "b"), length.out = performance_n))
design_perf <- data.frame(unit_id = ids_perf, group = group_perf)
common <- matrix(rnorm(performance_n * performance_p),
                 performance_n, performance_p)
colnames(common) <- paste0("e", seq_len(performance_p))
features_perf <- stats::setNames(lapply(seq_len(performance_supports), function(i) {
  current <- sqrt(0.8) * common + sqrt(0.2) *
    matrix(rnorm(performance_n * performance_p), performance_n, performance_p)
  colnames(current) <- colnames(common)
  make_features(current, ids_perf, paste0("perf", i))
}), paste0("support", seq_len(performance_supports)))
schedule_perf <- ngeo_exchangeability(
  ids_perf, permutations = performance_b, seed = 4899L
)
timing <- system.time({
  performance_result <- ngeo_group_test(
    features_perf, design_perf, ~ group, "group", schedule_perf,
    transform = "none"
  )
})
checks$streaming_family <- list(
  subjects = performance_n, supports = performance_supports,
  endpoints_per_support = performance_p,
  family_endpoints = nrow(performance_result$tests),
  permutations = performance_b,
  elapsed_seconds = unname(timing[["elapsed"]]),
  result_bytes = as.numeric(object.size(performance_result)),
  endpoint_null_retained = "endpoint" %in% names(performance_result$null),
  schedule_hash_equal = identical(
    performance_result$support$schedule_hash,
    schedule_perf$schedule_hash
  ),
  support_dispersion_combined =
    performance_result$support$diagnostics$
      support_dispersion_combined_with_sampling_variance,
  pass = nrow(performance_result$tests) ==
      performance_supports * performance_p &&
    !"endpoint" %in% names(performance_result$null) &&
    identical(performance_result$support$schedule_hash,
              schedule_perf$schedule_hash) &&
    identical(
      performance_result$support$diagnostics$
        support_dispersion_combined_with_sampling_variance,
      FALSE
    )
)

pass <- all(vapply(checks, `[[`, logical(1), "pass"))
report <- list(
  schema = "neurogeo/support-family-48-validation",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  full_calibration = full,
  checks = checks,
  pass = pass,
  claim = paste(
    "Common subject schedule and full-family correction over a declared",
    "support family; dispersion is descriptive and not parcellation-invariant."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, auto_unbox = TRUE, pretty = TRUE, digits = 16
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) stop("Support-family 4.8 validation failed.", call. = FALSE)
