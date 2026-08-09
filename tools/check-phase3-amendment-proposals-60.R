args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "phase3-amendment-proposals-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Phase 3 amendment checking requires: ", paste(missing, collapse = ", "))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
proposal_path <- file.path(
  "inst", "validation", "phase3-amendment-proposals-6.0.json"
)
proposal <- jsonlite::read_json(proposal_path, simplifyVector = FALSE)
proposal_ids <- vapply(proposal$proposals, `[[`, character(1), "amendment_id")
validation_ids <- vapply(proposal$proposals, `[[`, character(1), "validation_id")
val301 <- proposal$proposals[[match("VAL-301", validation_ids)]]
val308 <- proposal$proposals[[match("VAL-308", validation_ids)]]
procedure_fields <- c(
  "procedure_id", "public_call", "statistic", "generative_null",
  "tested_hypothesis", "multiplicity_family", "comparator",
  "calibration_cells", "power_or_stress_cells", "failure_behavior"
)
path_fields <- c("path", "workload", "scale_axes", "required_metrics")
checks <- list(
  original_design_hash_locked = identical(design_hash, locked_hash),
  proposal_binds_original_design = identical(
    proposal$original_design_sha256, design_hash
  ),
  proposal_status_unapproved = identical(
    proposal$status, "proposal-awaiting-independent-scientific-review"
  ),
  no_primary_results_claimed = identical(
    proposal$primary_results_generated_under_proposal, FALSE
  ),
  approvals_empty = all(vapply(
    proposal$approvals, is.null, logical(1)
  )),
  proposal_ids_unique = !anyDuplicated(proposal_ids),
  required_validations_present = setequal(validation_ids, c("VAL-301", "VAL-308")),
  val301_procedures_complete = length(val301$registered_procedures) == 3L &&
    all(vapply(val301$registered_procedures, function(item) {
      all(procedure_fields %in% names(item)) &&
        all(vapply(item[procedure_fields], function(value) {
          is.character(value) && length(value) == 1L && nzchar(value)
        }, logical(1)))
    }, logical(1))),
  experimental_nulls_excluded = length(val301$excluded_from_c02) == 2L,
  val308_paths_complete = setequal(vapply(
    val308$recommended_path_definitions, `[[`, character(1), "path"
  ), c("exact", "iterative", "file-backed", "support-family")) &&
    all(vapply(val308$recommended_path_definitions, function(item) {
      all(path_fields %in% names(item)) && length(item$scale_axes) > 0L &&
        length(item$required_metrics) > 0L
    }, logical(1))),
  review_packet_present = file.exists(file.path(
    "inst", "validation", "phase3-amendment-review-6.0.md"
  ))
)
passed <- all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/phase3-amendment-preparation/1",
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  original_design_sha256 = design_hash,
  proposal_sha256 = digest::digest(
    proposal_path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = if (passed) "prepared-awaiting-review" else "preparation-invalid",
  amendment_approved = FALSE,
  validation_evidence = FALSE,
  checks = checks,
  evidence_boundary = paste(
    "This check validates proposal completeness only. It cannot approve an",
    "amendment or satisfy VAL-301, VAL-308, C02, or the release gate."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
