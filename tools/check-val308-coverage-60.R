args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val308-coverage-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-308 coverage checking requires: ", paste(missing, collapse = ", "))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
design <- jsonlite::read_json(design_path, simplifyVector = FALSE)
validation <- design$validations[
  vapply(design$validations, function(x) identical(x$id, "VAL-308"), logical(1))
][[1L]]
factor_counts <- vapply(validation$factors, length, integer(1))
registered_cells <- prod(factor_counts)

performance_path <- if (length(args) >= 2L) args[[2L]] else file.path(
  "check-output", "registry", "full-performance-60.json"
)
if (!file.exists(performance_path)) {
  stop("Run tools/run-full-performance.R before the VAL-308 coverage audit.")
}
performance <- jsonlite::read_json(performance_path, simplifyVector = FALSE)
expected_cases <- c(
  "surface_164k", "grayordinates_91k", "surface_diagnostic_32k",
  "coordinate_knn_100k", "support_change_100k_by_1k",
  "affine_support_builder_100k", "uncertain_support_100k"
)
reported_cases <- names(performance$cases)
checks <- list(
  phase3_hash_locked = identical(design_hash, locked_hash),
  core_suite_passed = identical(performance$validation, "passed"),
  seven_registered_core_cases = identical(reported_cases, expected_cases),
  core_report_declares_partial_scope = identical(
    performance$full_val308_evidence, FALSE
  ),
  coverage_audit_present = file.exists(file.path(
    "inst", "validation", "val308-coverage-audit-6.0.md"
  ))
)
passed <- all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/phase3-coverage-audit/1",
  validation_id = "VAL-308",
  design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = if (passed) "partial-performance-evidence-full-grid-required" else
    "coverage-audit-failed",
  validation_evidence = FALSE,
  release_gate_satisfied = FALSE,
  registered_factor_counts = as.list(factor_counts),
  registered_cell_count = registered_cells,
  explicitly_reported_factorial_cell_count = 0L,
  partial_core_case_count = length(reported_cases),
  checks = checks,
  missing_evidence = c(
    "an explicit result row for every frozen factorial cell",
    "registered-small exact-result comparisons and relative error",
    "iterative convergence observations",
    "file-backed path observations",
    "support-family path observations across the registered scales",
    "per-cell peak memory elapsed time and failed-fit rate"
  ),
  evidence_boundary = paste(
    "The seven passing core cases support the narrow C08 claim only.",
    "They are not assignable to the frozen Cartesian cells without",
    "post-result reinterpretation and therefore cannot satisfy VAL-308."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
