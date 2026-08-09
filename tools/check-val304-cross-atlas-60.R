args <- commandArgs(trailingOnly = TRUE)
result_path <- if (length(args)) args[[1L]] else
  file.path("check-output", "val304-cross-atlas-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-304 result checking requires: ", paste(missing, collapse = ", "))
if (!file.exists(result_path)) stop("VAL-304 result does not exist: ", result_path)

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
  function(x) identical(x$id, "VAL-304"), design$validations
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

correlations <- unlist(
  validation$factors$atlas_correlation, use.names = FALSE
)
subjects <- unlist(validation$factors$subjects, use.names = FALSE)
effects <- unlist(validation$factors$effect, use.names = FALSE)
modes <- unlist(validation$factors$mode, use.names = FALSE)
expected <- expand.grid(
  atlas_correlation = correlations,
  subjects = subjects,
  effect = effects,
  mode = modes,
  stringsAsFactors = FALSE
)
expected_keys <- with(expected, paste(
  atlas_correlation, subjects, effect, mode, sep = "|"
))
observed_keys <- vapply(result$cells, function(cell) paste(
  cell$atlas_correlation, cell$subjects, cell$effect, cell$mode, sep = "|"
), character(1))
stopifnot(
  length(result$cells) == nrow(expected),
  identical(result$registered_cell_count, nrow(expected)),
  identical(result$observed_cell_count, nrow(expected)),
  identical(result$primary_cell_count, 16L),
  identical(result$ablation_cell_count, 16L),
  !anyDuplicated(observed_keys),
  setequal(observed_keys, expected_keys)
)

for (cell in result$cells) {
  stopifnot(
    identical(cell$replicates_attempted, 5000L),
    identical(cell$failed_fits, 0L),
    cell$failed_fit_rate == 0,
    isTRUE(cell$independence_requires_explicit_opt_in),
    cell$api_estimate_error <= 1e-12,
    cell$api_standard_error_error <= 1e-12,
    isTRUE(cell$implementation_pass),
    isTRUE(cell$pass)
  )
  if (identical(cell$mode, "covariance-aware")) {
    stopifnot(
      isTRUE(cell$primary_gate_applicable),
      isTRUE(cell$primary_gate_pass),
      abs(cell$bias) <= 0.02,
      cell$coverage$lower >= 0.93,
      cell$coverage$upper <= 0.97
    )
    if (cell$effect == 0) stopifnot(cell$type1$upper <= 0.065)
  } else {
    stopifnot(!isTRUE(cell$primary_gate_applicable))
  }
}

cat(
  "VAL-304 full result:", length(result$cells),
  "registered cells passed against design", design_hash, "\n"
)
