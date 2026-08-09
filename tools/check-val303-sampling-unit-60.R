args <- commandArgs(trailingOnly = TRUE)
result_path <- if (length(args)) args[[1L]] else
  file.path("check-output", "val303-sampling-unit-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-303 result checking requires: ", paste(missing, collapse = ", "))
if (!file.exists(result_path)) stop("VAL-303 result does not exist: ", result_path)

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
  function(x) identical(x$id, "VAL-303"), design$validations
)[[1L]]
result <- jsonlite::fromJSON(result_path, simplifyVector = FALSE)
stopifnot(
  identical(result$schema, "neurogeo/phase3-validation/1"),
  identical(result$validation_id, validation$id),
  identical(result$simulation_id, validation$simulation_id),
  identical(result$design_sha256, design_hash),
  identical(result$package_version, "6.0.0"),
  identical(result$run_mode, "full"),
  identical(result$replicates_per_calibration_cell, 5000L),
  isTRUE(result$attempted_replicates_are_denominator),
  isTRUE(result$primary_evidence_eligible),
  identical(result$validation, "passed-with-restriction"),
  isTRUE(result$registered_cell_coverage_complete)
)

units <- unlist(validation$factors$declared_unit, use.names = FALSE)
designs <- unlist(validation$factors$design, use.names = FALSE)
schedules <- unlist(validation$factors$schedule, use.names = FALSE)
expected <- expand.grid(
  declared_unit = units, design = designs, schedule = schedules,
  stringsAsFactors = FALSE
)
expected_keys <- with(expected, paste(declared_unit, design, schedule, sep = "|"))
observed_keys <- vapply(result$cells, function(cell) paste(
  cell$declared_unit, cell$design, cell$schedule, sep = "|"
), character(1))
stopifnot(
  length(result$cells) == nrow(expected),
  identical(result$registered_cell_count, nrow(expected)),
  identical(result$observed_cell_count, nrow(expected)),
  identical(result$primary_cell_count, 24L),
  identical(result$restriction_cell_count, 3L),
  identical(result$separation_cell_count, 9L),
  !anyDuplicated(observed_keys), setequal(observed_keys, expected_keys)
)

for (cell in result$cells) {
  stopifnot(
    identical(cell$failed_fits, 0L), cell$failed_fit_rate == 0,
    isTRUE(cell$wrong_unit_rejection$pass),
    cell$wrong_unit_rejection$estimate == 1,
    isTRUE(cell$implementation_pass), isTRUE(cell$pass)
  )
  if (isTRUE(cell$post_primary_restriction)) {
    stopifnot(
      !isTRUE(cell$primary_gate_applicable),
      identical(cell$design, "site-confounded"),
      identical(cell$schedule, "free"),
      !isTRUE(cell$original_calibration_gate_pass),
      cell$type1$upper > 0.065,
      isTRUE(cell$runtime_free_with_blocks_rejected),
      is.null(cell$schedule_order_difference)
    )
  } else if (identical(cell$declared_unit, "map-null")) {
    stopifnot(
      !isTRUE(cell$primary_gate_applicable),
      identical(cell$replicates_attempted, 0L),
      is.null(cell$coverage), is.null(cell$type1),
      isTRUE(cell$wrong_unit_rejection$map_null_separation)
    )
  } else {
    stopifnot(
      isTRUE(cell$primary_gate_applicable),
      isTRUE(cell$primary_gate_pass),
      identical(cell$replicates_attempted, 5000L),
      isTRUE(cell$exact_enumeration),
      cell$coverage$lower >= 0.93, cell$coverage$upper <= 0.97,
      cell$type1$upper <= 0.065,
      cell$schedule_order_difference <= 1e-12,
      cell$api_p_value_error <= 1e-12,
      cell$api_statistic_error <= 1e-10,
      isTRUE(cell$palm_column_schedule_compatible)
    )
  }
}

cat(
  "VAL-303 full result:", length(result$cells),
  "registered cells passed against design", design_hash, "\n"
)
