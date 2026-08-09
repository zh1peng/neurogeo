args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--smoke" %in% args) "smoke" else "full"
args <- args[args != "--smoke"]
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val304-cross-atlas-60.json")
required <- c("digest", "jsonlite", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-304 requires: ", paste(missing, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
design <- jsonlite::fromJSON(design_path, simplifyVector = FALSE)
validation <- Filter(
  function(x) identical(x$id, "VAL-304"), design$validations
)[[1L]]
common <- design$common
replicates <- if (identical(run_mode, "full")) {
  common$replicates_per_calibration_cell
} else {
  500L
}
alpha <- common$alpha

wilson_interval <- function(successes, attempted, confidence = 0.95) {
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  estimate <- successes / attempted
  denominator <- 1 + z^2 / attempted
  center <- (estimate + z^2 / (2 * attempted)) / denominator
  half_width <- z * sqrt(
    estimate * (1 - estimate) / attempted + z^2 / (4 * attempted^2)
  ) / denominator
  list(
    estimate = estimate,
    lower = max(0, center - half_width),
    upper = min(1, center + half_width),
    successes = successes,
    attempted = attempted
  )
}

run_cell <- function(correlation, subjects, effect, mode, seed) {
  set.seed(seed)
  atlas_count <- 5L
  marginal_standard_error <- 1 / sqrt(subjects)
  correlation_matrix <- matrix(correlation, atlas_count, atlas_count)
  diag(correlation_matrix) <- 1
  covariance <- correlation_matrix * marginal_standard_error^2
  estimates <- matrix(
    stats::rnorm(replicates * atlas_count),
    nrow = replicates,
    ncol = atlas_count
  ) %*% chol(covariance) + effect
  one <- rep.int(1, atlas_count)
  precision <- solve(covariance)
  denominator <- as.numeric(crossprod(one, precision %*% one))
  covariance_weight <- as.numeric(precision %*% one) / denominator
  covariance_standard_error <- sqrt(1 / denominator)
  independence_weight <- rep.int(1 / atlas_count, atlas_count)
  independence_standard_error <- marginal_standard_error / sqrt(atlas_count)
  covariance_aware <- identical(mode, "covariance-aware")
  weight <- if (covariance_aware) covariance_weight else independence_weight
  pooled_standard_error <- if (covariance_aware) {
    covariance_standard_error
  } else {
    independence_standard_error
  }
  pooled <- as.numeric(estimates %*% weight)
  critical <- stats::qnorm(1 - alpha / 2)
  covered <- abs(pooled - effect) <= critical * pooled_standard_error
  rejected <- abs(pooled / pooled_standard_error) >= critical
  coverage <- wilson_interval(sum(covered), replicates)
  type1 <- if (effect == 0) {
    wilson_interval(sum(rejected), replicates)
  } else {
    NULL
  }
  standard_error <- rep.int(marginal_standard_error, atlas_count)
  public_result <- if (covariance_aware) {
    ngeo_cross_atlas_consensus(
      estimates[1L, ], covariance = covariance
    )
  } else {
    ngeo_cross_atlas_consensus(
      estimates[1L, ], standard_error,
      method = "fixed", independence = TRUE
    )
  }
  descriptive <- ngeo_cross_atlas_consensus(
    estimates[1L, ], standard_error
  )
  expected_first <- sum(weight * estimates[1L, ])
  api_estimate_error <- abs(public_result$estimate - expected_first)
  api_standard_error_error <- abs(
    public_result$standard_error - pooled_standard_error
  )
  independence_opt_in <- identical(descriptive$inference_mode, "descriptive") &&
    is.na(descriptive$p_value) &&
    (!covariance_aware || identical(
      public_result$inference_mode, "covariance-aware"
    )) &&
    (covariance_aware || identical(
      public_result$inference_mode, "independence"
    ))
  bias <- mean(pooled) - effect
  coverage_pass <- coverage$lower >= 0.93 && coverage$upper <= 0.97
  type1_pass <- is.null(type1) || type1$upper <= 0.065
  primary_gate_pass <- covariance_aware && coverage_pass && type1_pass &&
    abs(bias) <= 0.02
  implementation_pass <- api_estimate_error <= 1e-12 &&
    api_standard_error_error <= 1e-12 && independence_opt_in
  list(
    atlas_correlation = correlation,
    subjects = subjects,
    effect = effect,
    mode = mode,
    seed = seed,
    atlas_count = atlas_count,
    replicates_attempted = replicates,
    failed_fits = 0L,
    failed_fit_rate = 0,
    pooled_standard_error = pooled_standard_error,
    bias = bias,
    coverage = coverage,
    type1 = type1,
    api_estimate_error = api_estimate_error,
    api_standard_error_error = api_standard_error_error,
    independence_requires_explicit_opt_in = independence_opt_in,
    primary_gate_applicable = covariance_aware,
    primary_gate_pass = if (covariance_aware) primary_gate_pass else NULL,
    implementation_pass = implementation_pass,
    pass = implementation_pass && (!covariance_aware || primary_gate_pass)
  )
}

correlations <- unlist(
  validation$factors$atlas_correlation, use.names = FALSE
)
subjects <- unlist(validation$factors$subjects, use.names = FALSE)
effects <- unlist(validation$factors$effect, use.names = FALSE)
modes <- unlist(validation$factors$mode, use.names = FALSE)
cells <- list()
cell_index <- 0L
for (correlation in correlations) {
  for (n_subjects in subjects) {
    for (effect in effects) {
      for (mode in modes) {
        cell_index <- cell_index + 1L
        cells[[cell_index]] <- run_cell(
          correlation, n_subjects, effect, mode,
          validation$seed_base + cell_index
        )
      }
    }
  }
}

expected_cells <- length(correlations) * length(subjects) *
  length(effects) * length(modes)
cell_keys <- vapply(cells, function(cell) paste(
  cell$atlas_correlation, cell$subjects, cell$effect, cell$mode, sep = "|"
), character(1))
coverage_complete <- length(cells) == expected_cells &&
  !anyDuplicated(cell_keys)
implementation_passed <- all(vapply(
  cells, `[[`, logical(1), "implementation_pass"
))
primary_cells <- cells[vapply(
  cells, `[[`, logical(1), "primary_gate_applicable"
)]
primary_passed <- all(vapply(
  primary_cells, `[[`, logical(1), "primary_gate_pass"
))
passed <- coverage_complete && implementation_passed && primary_passed
result <- list(
  schema = "neurogeo/phase3-validation/1",
  validation_id = validation$id,
  simulation_id = validation$simulation_id,
  design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  dependency_versions = as.list(vapply(
    required, function(package) as.character(utils::packageVersion(package)),
    character(1)
  )),
  platform = R.version$platform,
  r_version = R.version.string,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  run_mode = run_mode,
  seed_base = validation$seed_base,
  replicates_per_cell = replicates,
  primary_evidence_eligible = identical(run_mode, "full") && passed,
  validation = if (passed && identical(run_mode, "full")) {
    "passed"
  } else if (passed) {
    "debug-passed"
  } else {
    "failed"
  },
  registered_cell_count = expected_cells,
  observed_cell_count = length(cells),
  registered_cell_coverage_complete = coverage_complete,
  primary_cell_count = length(primary_cells),
  ablation_cell_count = length(cells) - length(primary_cells),
  cells = unname(cells)
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
