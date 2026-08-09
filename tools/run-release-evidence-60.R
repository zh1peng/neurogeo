args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
strict <- "--require-release" %in% args
paths <- args[!startsWith(args, "--")]
output <- if (length(paths)) paths[[1L]] else
  file.path("check-output", "release-evidence-60.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("The 6.0 release evidence audit requires jsonlite and digest.")
}

read_optional <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::read_json(path, simplifyVector = FALSE)
}
scalar <- function(value, default = NULL) {
  if (is.null(value) || !length(value)) default else value[[1L]]
}
git <- function(arguments) {
  suppressWarnings(system2("git", arguments, stdout = TRUE, stderr = TRUE))
}

version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
source_commit <- scalar(git(c("rev-parse", "HEAD")), "unavailable")
source_dirty <- length(git(c("status", "--porcelain"))) > 0L
p0_path <- file.path("check-output", "p0-evidence-60", "attestation.json")
task_path <- file.path("check-output", "registry", "audit-task-status-60.json")
performance_path <- file.path("check-output", "registry", "full-performance-60.json")
val308_path <- file.path("check-output", "registry", "val308-coverage-audit-60.json")
external_release_path <- file.path(
  "check-output", "external-release-readiness-60.json"
)
p0 <- read_optional(p0_path)
tasks <- read_optional(task_path)
performance <- read_optional(performance_path)
val308 <- read_optional(val308_path)
external_release <- read_optional(external_release_path)

claim_path <- file.path("inst", "validation", "claim-evidence-matrix-6.0.csv")
claims <- utils::read.csv(claim_path, stringsAsFactors = FALSE, check.names = FALSE)
formative <- utils::read.csv(
  file.path("inst", "validation", "formative-usability-results-6.0.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
phase3_paths <- stats::setNames(file.path(
  "check-output", "registry", c(
    "val301-safe-boundary-60.json", "val302-external-prerequisites-60.json",
    "val303-sampling-unit-60.json", "val304-cross-atlas-60.json",
    "val305-operator-simplex-60.json", "val306-spatial-models-60.json",
    "val307-support-builder-60.json", "val308-coverage-audit-60.json"
  )
), paste0("VAL-30", 1:8))
phase3 <- lapply(phase3_paths, read_optional)

formative_complete <- nrow(formative) >= 5L &&
  all(tolower(as.character(formative$completed)) == "true") &&
  length(unique(formative$participant_id)) >= 5L
phase3_present <- vapply(phase3, Negate(is.null), logical(1))
checks <- list(
  source_tree_clean = !source_dirty,
  p0_attestation_present = !is.null(p0),
  p0_attestation_matches_current_source = !is.null(p0) &&
    identical(scalar(p0$candidate$package_version), version) &&
    identical(scalar(p0$candidate$source_commit), source_commit),
  task_status_present = !is.null(tasks),
  task_dependency_graph_valid = !is.null(tasks) &&
    isTRUE(scalar(tasks$dependency_graph_acyclic, FALSE)) &&
    isTRUE(scalar(tasks$dependency_phases_monotone, FALSE)),
  all_audit_tasks_complete = !is.null(tasks) &&
    isTRUE(scalar(tasks$all_gates_complete, FALSE)),
  formative_usability_complete = formative_complete,
  all_claims_externally_replicated = nrow(claims) > 0L &&
    all(claims$evidence_status == "external-replicated"),
  all_phase3_reports_present = all(phase3_present),
  val301_calibration_complete = !is.null(phase3$`VAL-301`) &&
    isTRUE(scalar(phase3$`VAL-301`$calibration_evidence, FALSE)),
  val302_external_parity_complete = !is.null(phase3$`VAL-302`) &&
    isTRUE(scalar(phase3$`VAL-302`$validation_evidence, FALSE)),
  seven_core_performance_cases_pass = !is.null(performance) &&
    identical(scalar(performance$validation), "passed") &&
    length(performance$cases) == 7L,
  val308_release_gate_complete = !is.null(val308) &&
    isTRUE(scalar(val308$release_gate_satisfied, FALSE)),
  immutable_external_release_complete = !is.null(external_release) &&
    isTRUE(scalar(external_release$pass, FALSE)),
  independent_external_validation_complete = FALSE
)
pass <- all(unlist(checks, use.names = FALSE))

artifact_paths <- c(
  p0_attestation = p0_path, task_status = task_path,
  core_performance = performance_path, val308_coverage = val308_path,
  external_release = external_release_path,
  claims = claim_path, formative = file.path(
    "inst", "validation", "formative-usability-results-6.0.csv"
  ), phase3_paths
)
artifacts <- unname(lapply(names(artifact_paths), function(name) {
  path <- artifact_paths[[name]]
  present <- file.exists(path)
  list(
    id = name, path = path, present = present,
    bytes = if (present) as.numeric(file.info(path)$size) else NULL,
    sha256 = if (present) digest::digest(
      path, algo = "sha256", file = TRUE, serialize = FALSE
    ) else NULL
  )
}))
report <- list(
  schema = "neurogeo/release-readiness/2",
  package_version = version,
  source_commit = source_commit,
  source_dirty = source_dirty,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = if (pass) "release-ready" else "blocked",
  pass = pass,
  checks = checks,
  phase3_report_presence = as.list(phase3_present),
  artifacts = artifacts,
  evidence_boundary = paste(
    "This readiness report never promotes internal or prerequisite evidence",
    "to external validation. A signed tag, hosted release, DOI, independent",
    "review, and external rerun must be verified outside the source tree."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, auto_unbox = TRUE, pretty = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (strict && !pass) quit(status = 2L)
