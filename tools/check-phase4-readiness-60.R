args <- commandArgs(trailingOnly = TRUE)
strict <- "--require-complete" %in% args
paths <- args[!startsWith(args, "--")]
output <- if (length(paths)) paths[[1L]] else
  file.path("check-output", "phase4-readiness-60.json")
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("Phase 4 checking requires jsonlite and digest.")
}

protocol_path <- file.path(
  "inst", "validation", "phase4-external-study-protocol-6.0.md"
)
schema_path <- file.path(
  "inst", "validation", "phase4-evidence-schema-6.0.json"
)
template_path <- file.path(
  "inst", "validation", "phase4-evidence-template-6.0.json"
)
receipt_path <- file.path("release", "phase4-evidence-6.0.json")
required_files <- c(protocol_path, schema_path, template_path)
if (!all(file.exists(required_files))) stop("Phase 4 contract files are missing.")
evidence_path <- if (file.exists(receipt_path)) receipt_path else template_path
evidence <- jsonlite::read_json(evidence_path, simplifyVector = FALSE)

top_fields <- c(
  "schema", "status", "package_version", "candidate", "operators",
  "cohorts", "benchmarks", "external_reproductions", "open_release",
  "manuscript", "external_reviews"
)
operator_fields <- c(
  "operator_id", "family", "source_space", "target_space", "direction",
  "version", "sha256", "source_uri", "doi", "license",
  "legal_redistribution", "metric", "expected_coverage", "qc_procedure",
  "qc_pass", "reviewer", "review_record"
)
cohort_fields <- c(
  "cohort_id", "role", "site_id", "independence_rule", "participants",
  "data_terms_uri", "ethics_record", "public_reconstruction",
  "preregistration_uri", "endpoint_plan_sha256", "analysis_commit",
  "completed", "positive_null_failed_reported", "reviewer", "review_record"
)
benchmark_fields <- c(
  "benchmark_id", "category", "estimand", "matched_inputs", "reference",
  "analysis_commit", "result_uri", "source_data_sha256", "completed",
  "all_attempts_reported", "reviewer", "review_record"
)
reproduction_fields <- c(
  "reproduction_id", "laboratory", "independent_of_development",
  "public_materials_only", "figure_id", "release_tag", "source_commit",
  "expected_source_data_sha256", "observed_source_data_sha256",
  "reproduced", "deviations_uri", "resolution_uri", "reviewer",
  "review_record"
)
review_fields <- c(
  "reviewer_id", "independent", "novelty", "broad_interest",
  "technical_validity", "independent_evidence", "recommendation",
  "review_record"
)
open_fields <- c(
  "code_doi", "data_doi", "operators_doi", "source_data_doi",
  "environment_uri", "environment_sha256", "rebuild_command",
  "rebuild_log_uri", "all_figures_rebuilt", "licenses_complete",
  "orcids_complete", "contributions_complete", "reviewer", "review_record"
)
manuscript_fields <- c(
  "artifact_uri", "sha256", "claims_consistent",
  "equations_and_statistics_checked", "limitations_complete",
  "api_status_consistent", "availability_statements_complete",
  "references_and_source_data_complete", "reviewer", "review_record"
)

has_fields <- function(item, fields) all(fields %in% names(item))
nonempty_scalar <- function(value) {
  is.character(value) && length(value) == 1L && nzchar(trimws(value))
}
sha256 <- function(value) nonempty_scalar(value) &&
  grepl("^[0-9a-f]{64}$", value)
commit <- function(value) nonempty_scalar(value) &&
  grepl("^[0-9a-f]{40}$", value)
truth <- function(value) is.logical(value) && length(value) == 1L && isTRUE(value)
complete_records <- function(items, fields) {
  length(items) > 0L && all(vapply(items, has_fields, logical(1), fields = fields))
}

task_path <- file.path("inst", "spec", "audit-task-status-6.0.csv")
tasks <- utils::read.csv(task_path, stringsAsFactors = FALSE, check.names = FALSE)
phase4 <- tasks[grepl("^PUB-40[1-7]$", tasks$task_id), , drop = FALSE]
dependency_ids <- unique(unlist(strsplit(phase4$dependencies, ";", fixed = TRUE)))
dependency_ids <- setdiff(dependency_ids[nzchar(dependency_ids)], phase4$task_id)
upstream <- tasks[match(dependency_ids, tasks$task_id), , drop = FALSE]
closed_statuses <- c(
  "implemented-internal", "implemented-internal-with-restriction",
  "complete-external"
)
dependencies_closed <- nrow(phase4) == 7L &&
  nrow(upstream) == length(dependency_ids) &&
  !anyNA(upstream$task_id) && all(upstream$status %in% closed_statuses)
blocking_upstream <- setNames(
  as.list(upstream$status[!(upstream$status %in% closed_statuses)]),
  upstream$task_id[!(upstream$status %in% closed_statuses)]
)

families <- if (length(evidence$operators)) {
  unique(vapply(evidence$operators, function(item) {
    if ("family" %in% names(item) && nonempty_scalar(item$family))
      tolower(item$family) else ""
  }, character(1)))
} else character()
operator_ok <- complete_records(evidence$operators, operator_fields) &&
  all(c("fsaverage", "fslr", "mni", "cifti-density", "atlas") %in% families) &&
  all(vapply(evidence$operators, function(item) {
    sha256(item$sha256) && truth(item$legal_redistribution) &&
      truth(item$qc_pass) && all(vapply(
        item[c("source_uri", "doi", "metric", "qc_procedure", "reviewer",
               "review_record")], nonempty_scalar, logical(1)
      ))
  }, logical(1)))

cohort_ok <- complete_records(evidence$cohorts, cohort_fields) &&
  length(evidence$cohorts) >= 2L
if (cohort_ok) {
  roles <- vapply(evidence$cohorts, `[[`, character(1), "role")
  sites <- vapply(evidence$cohorts, `[[`, character(1), "site_id")
  cohort_ok <- setequal(unique(roles), c("discovery", "replication")) &&
    length(unique(sites)) >= 2L &&
    any(vapply(evidence$cohorts, function(item) {
      truth(item$public_reconstruction)
    }, logical(1))) &&
    all(vapply(evidence$cohorts, function(item) {
      is.numeric(item$participants) && length(item$participants) == 1L &&
        item$participants > 0 && sha256(item$endpoint_plan_sha256) &&
        commit(item$analysis_commit) && truth(item$completed) &&
        truth(item$positive_null_failed_reported)
    }, logical(1)))
}

categories <- c(
  "area-weighting", "metric", "atlas-ensemble", "spatial-null",
  "uncertainty", "guard", "competitor"
)
benchmark_ok <- complete_records(evidence$benchmarks, benchmark_fields)
if (benchmark_ok) {
  observed <- vapply(evidence$benchmarks, `[[`, character(1), "category")
  benchmark_ok <- all(categories %in% observed) &&
    all(vapply(evidence$benchmarks, function(item) {
      commit(item$analysis_commit) && sha256(item$source_data_sha256) &&
        truth(item$completed) && truth(item$all_attempts_reported) &&
        all(vapply(item[c("estimand", "matched_inputs", "reference",
                         "result_uri", "reviewer", "review_record")],
                   nonempty_scalar, logical(1)))
    }, logical(1)))
}

reproduction_ok <- complete_records(
  evidence$external_reproductions, reproduction_fields
)
if (reproduction_ok) {
  reproduction_ok <- any(vapply(evidence$external_reproductions, function(item) {
    truth(item$independent_of_development) &&
      truth(item$public_materials_only) && truth(item$reproduced) &&
      commit(item$source_commit) && sha256(item$expected_source_data_sha256) &&
      sha256(item$observed_source_data_sha256) &&
      identical(item$expected_source_data_sha256, item$observed_source_data_sha256)
  }, logical(1)))
}

open_ok <- has_fields(evidence$open_release, open_fields) &&
  all(vapply(evidence$open_release[c(
    "code_doi", "data_doi", "operators_doi", "source_data_doi",
    "environment_uri", "rebuild_command", "rebuild_log_uri", "reviewer",
    "review_record"
  )], nonempty_scalar, logical(1))) &&
  sha256(evidence$open_release$environment_sha256) &&
  all(vapply(evidence$open_release[c(
    "all_figures_rebuilt", "licenses_complete", "orcids_complete",
    "contributions_complete"
  )], truth, logical(1)))

manuscript_ok <- has_fields(evidence$manuscript, manuscript_fields) &&
  all(vapply(evidence$manuscript[c("artifact_uri", "reviewer", "review_record")],
             nonempty_scalar, logical(1))) &&
  sha256(evidence$manuscript$sha256) &&
  all(vapply(evidence$manuscript[c(
    "claims_consistent", "equations_and_statistics_checked",
    "limitations_complete", "api_status_consistent",
    "availability_statements_complete", "references_and_source_data_complete"
  )], truth, logical(1)))

reviews_ok <- length(evidence$external_reviews) %in% 2:3 &&
  all(vapply(evidence$external_reviews, has_fields, logical(1),
             fields = review_fields))
if (reviews_ok) {
  reviews_ok <- all(vapply(evidence$external_reviews, function(item) {
    truth(item$independent) &&
      item$recommendation %in% c("go", "no-go", "revise") &&
      all(vapply(item[c("reviewer_id", "novelty", "broad_interest",
                        "technical_validity", "independent_evidence",
                        "review_record")], nonempty_scalar, logical(1)))
  }, logical(1)))
}

candidate_ok <- has_fields(
  evidence$candidate,
  c("release_tag", "source_commit", "p0_attestation_sha256")
) && nonempty_scalar(evidence$candidate$release_tag) &&
  grepl("^v6[.]0[.]0-rc[0-9]+$", evidence$candidate$release_tag) &&
  commit(evidence$candidate$source_commit) &&
  sha256(evidence$candidate$p0_attestation_sha256)
checks <- list(
  contract_files_present = TRUE,
  top_level_schema = has_fields(evidence, top_fields) &&
    identical(evidence$schema, "neurogeo/phase4-external-evidence/1"),
  real_receipt_present = identical(evidence_path, receipt_path),
  upstream_dependencies_closed = dependencies_closed,
  candidate_identity = candidate_ok,
  pub401_operator_registry = operator_ok,
  pub402_cohorts = cohort_ok,
  pub403_benchmarks = benchmark_ok,
  pub404_external_reproduction = reproduction_ok,
  pub405_open_release = open_ok,
  pub406_manuscript = manuscript_ok,
  pub407_external_reviews = reviews_ok
)
complete <- identical(evidence$status, "complete") &&
  all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/phase4-readiness/1",
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = if (complete) "complete" else "blocked-or-awaiting-external-evidence",
  phase4_complete = complete,
  evidence_source = gsub("\\\\", "/", evidence_path),
  evidence_sha256 = digest::digest(
    evidence_path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  blocking_upstream = blocking_upstream,
  protocol_sha256 = digest::digest(
    protocol_path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  schema_sha256 = digest::digest(
    schema_path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  checks = checks,
  evidence_boundary = paste(
    "This checker enforces receipt completeness and frozen coverage only.",
    "Named external reviewers must judge scientific validity, independence,",
    "legal and ethical compliance, and journal fit. Empty templates are not evidence."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (strict && !complete) quit(status = 2L)
