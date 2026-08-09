args <- commandArgs(trailingOnly = TRUE)
result_path <- if (length(args)) args[[1L]] else
  file.path("check-output", "val306-spatial-models-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-306 result checking requires: ", paste(missing, collapse = ", "))
if (!file.exists(result_path)) stop("VAL-306 result does not exist: ", result_path)

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
  function(x) identical(x$id, "VAL-306"), design$validations
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
  isTRUE(result$primary_evidence_eligible),
  identical(result$validation, "passed-with-inferential-restriction"),
  isTRUE(result$registered_cell_coverage_complete),
  identical(result$coverage_cell_count, 9L),
  identical(result$type1_cell_count, 0L)
)
methods <- unlist(validation$factors$method, use.names = FALSE)
geometries <- unlist(validation$factors$geometry, use.names = FALSE)
parameters <- unlist(validation$factors$spatial_parameter, use.names = FALSE)
expected <- expand.grid(
  method = methods, geometry = geometries, spatial_parameter = parameters,
  stringsAsFactors = FALSE
)
expected_keys <- with(expected, paste(method, geometry, spatial_parameter, sep = "|"))
observed_keys <- vapply(result$cells, function(cell) paste(
  cell$method, cell$geometry, cell$spatial_parameter, sep = "|"
), character(1))
stopifnot(
  length(result$cells) == nrow(expected),
  identical(result$registered_cell_count, nrow(expected)),
  identical(result$observed_cell_count, nrow(expected)),
  !anyDuplicated(observed_keys), setequal(observed_keys, expected_keys)
)
for (cell in result$cells) {
  stopifnot(
    identical(cell$failed_fits, 0L), cell$failed_fit_rate == 0,
    !isTRUE(cell$type1_applicable), is.null(cell$type1),
    cell$point_estimate_error <= 1e-6, isTRUE(cell$pass)
  )
  if (identical(cell$method, "kriging")) {
    stopifnot(
      isTRUE(cell$coverage_applicable),
      identical(cell$replicates_attempted, 5000L),
      cell$standard_error_error <= 1e-6,
      cell$coverage$lower >= 0.93, cell$coverage$upper <= 0.97
    )
  } else {
    stopifnot(!isTRUE(cell$coverage_applicable), is.null(cell$coverage))
  }
  if (identical(cell$method, "gwr")) {
    stopifnot(
      isTRUE(cell$row_order_cv_applicable),
      cell$row_order_cv_difference <= 1e-12
    )
  } else {
    stopifnot(!isTRUE(cell$row_order_cv_applicable))
  }
}
cat(
  "VAL-306 full result:", length(result$cells),
  "registered cells passed with inferential restrictions against design",
  design_hash, "\n"
)
