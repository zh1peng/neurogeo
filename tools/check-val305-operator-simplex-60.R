args <- commandArgs(trailingOnly = TRUE)
result_path <- if (length(args)) args[[1L]] else
  file.path("check-output", "val305-operator-simplex-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-305 result checking requires: ", paste(missing, collapse = ", "))
if (!file.exists(result_path)) stop("VAL-305 result does not exist: ", result_path)

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
if (!identical(design_hash, locked_hash)) stop("Phase 3 design hash lock failed.")
design <- jsonlite::fromJSON(design_path, simplifyVector = FALSE)
validation <- Filter(
  function(x) identical(x$id, "VAL-305"), design$validations
)[[1L]]
result <- jsonlite::fromJSON(result_path, simplifyVector = FALSE)
stopifnot(
  identical(result$schema, "neurogeo/phase3-validation/1"),
  identical(result$validation_id, validation$id),
  identical(result$simulation_id, validation$simulation_id),
  identical(result$design_sha256, design_hash),
  identical(result$package_version, "6.0.0"),
  identical(result$run_mode, "full"),
  identical(result$replicates_per_cell, 5000L),
  isTRUE(result$primary_evidence_eligible),
  identical(result$validation, "passed"),
  isTRUE(result$registered_cell_coverage_complete)
)

distributions <- unlist(
  validation$factors$operator_distribution, use.names = FALSE
)
ensemble_sizes <- unlist(
  validation$factors$ensemble_size, use.names = FALSE
)
concentrations <- unlist(
  validation$factors$concentration, use.names = FALSE
)
expected <- expand.grid(
  operator_distribution = distributions,
  ensemble_size = ensemble_sizes,
  concentration = concentrations,
  stringsAsFactors = FALSE
)
expected_keys <- with(expected, paste(
  operator_distribution, ensemble_size, concentration, sep = "|"
))
observed_keys <- vapply(result$cells, function(cell) paste(
  cell$operator_distribution, cell$ensemble_size, cell$concentration,
  sep = "|"
), character(1))
stopifnot(
  length(result$cells) == nrow(expected),
  identical(result$registered_cell_count, nrow(expected)),
  identical(result$observed_cell_count, nrow(expected)),
  identical(result$primary_cell_count, 12L),
  identical(result$ablation_cell_count, 4L),
  !anyDuplicated(observed_keys),
  setequal(observed_keys, expected_keys)
)

for (cell in result$cells) {
  stopifnot(
    identical(cell$replicates_attempted, 5000L),
    identical(cell$failed_fits, 0L),
    cell$failed_fit_rate == 0,
    identical(cell$measurement_noise_variance, 0.05),
    identical(
      cell$estimand,
      "future noisy mass allocated to targets A and B"
    ),
    isTRUE(cell$implementation_pass),
    isTRUE(cell$pass)
  )
  if (isTRUE(cell$primary_gate_applicable)) {
    stopifnot(
      isTRUE(cell$primary_gate_pass),
      cell$simplex_error <= 1e-12,
      abs(cell$bias) <= 0.02,
      isTRUE(cell$rmse_gate_pass),
      cell$coverage$lower >= 0.93,
      cell$coverage$upper <= 0.97,
      cell$api_mean_error <= 1e-12,
      cell$api_variance_error <= 1e-12
    )
  } else {
    stopifnot(
      cell$simplex_error > 1e-6,
      isTRUE(cell$public_api_rejected_non_simplex_ablation)
    )
  }
}

cat(
  "VAL-305 full result:", length(result$cells),
  "registered cells passed against design", design_hash, "\n"
)
