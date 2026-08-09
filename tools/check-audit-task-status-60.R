args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "audit-task-status-60.json")
if (!requireNamespace("digest", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Task-status checking requires digest and jsonlite.")
}

path <- file.path("inst", "spec", "audit-task-status-6.0.csv")
tasks <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
required <- c(
  "task_id", "phase", "dependencies", "external_required", "status",
  "evidence"
)
allowed_status <- c(
  "implemented-internal", "implemented-internal-with-restriction",
  "external-evidence-pending", "design-amendment-required",
  "blocked-external-prerequisites", "partial-internal",
  "blocked-by-prerequisite", "complete-reviewed", "external-validated"
)
stopifnot(
  identical(names(tasks), required), nrow(tasks) > 0L,
  !anyDuplicated(tasks$task_id),
  all(grepl("^[A-Z]+(-[A-Z]+)*-[0-9]{3}$", tasks$task_id)),
  all(tasks$phase %in% paste0("Phase", 0:4)),
  all(tasks$status %in% allowed_status),
  all(tolower(as.character(tasks$external_required)) %in% c("true", "false"))
)

dependencies <- lapply(tasks$dependencies, function(value) {
  if (identical(value, "none")) character() else strsplit(value, ";", fixed = TRUE)[[1L]]
})
names(dependencies) <- tasks$task_id
unknown <- setdiff(unique(unlist(dependencies, use.names = FALSE)), tasks$task_id)
if (length(unknown)) stop("Unknown task dependencies: ", paste(unknown, collapse = ", "))
phase_number <- stats::setNames(
  as.integer(sub("Phase", "", tasks$phase, fixed = TRUE)), tasks$task_id
)
invalid_phase_dependencies <- unlist(lapply(names(dependencies), function(id) {
  dependency <- dependencies[[id]]
  dependency[phase_number[dependency] > phase_number[[id]]]
}), use.names = FALSE)
if (length(invalid_phase_dependencies)) {
  stop("Tasks cannot depend on a later phase: ",
       paste(unique(invalid_phase_dependencies), collapse = ", "))
}

remaining <- dependencies
ordered <- character()
while (length(remaining)) {
  ready <- names(remaining)[vapply(
    remaining, function(value) all(value %in% ordered), logical(1)
  )]
  if (!length(ready)) stop("Audit task registry contains a dependency cycle.")
  ordered <- c(ordered, ready)
  remaining[ready] <- NULL
}

evidence <- unique(unlist(strsplit(tasks$evidence, "|", fixed = TRUE)))
missing_evidence <- evidence[!file.exists(evidence)]
if (length(missing_evidence)) {
  stop("Missing task evidence paths: ", paste(missing_evidence, collapse = ", "))
}
critical <- stats::setNames(tasks$status, tasks$task_id)
stopifnot(
  identical(critical[["VAL-301"]], "design-amendment-required"),
  identical(critical[["VAL-302"]], "blocked-external-prerequisites"),
  identical(critical[["VAL-308"]], "partial-internal"),
  identical(critical[["UX-301"]], "external-evidence-pending"),
  all(critical[paste0("PUB-40", 1:7)] == "blocked-by-prerequisite")
)
counts <- as.list(table(factor(tasks$status, levels = allowed_status)))
names(counts) <- allowed_status
blocking <- !tasks$status %in% c(
  "implemented-internal", "implemented-internal-with-restriction",
  "complete-reviewed", "external-validated"
)
result <- list(
  schema = "neurogeo/audit-task-status/1",
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  registry_sha256 = digest::digest(
    path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  task_count = nrow(tasks),
  dependency_graph_acyclic = TRUE,
  dependency_phases_monotone = TRUE,
  all_evidence_paths_present = TRUE,
  all_gates_complete = all(tasks$status %in% c(
    "complete-reviewed", "external-validated"
  )),
  implemented_internal_task_count = sum(tasks$status %in% c(
    "implemented-internal", "implemented-internal-with-restriction"
  )),
  blocking_task_count = sum(blocking),
  status_counts = counts,
  blocking_tasks = unname(lapply(which(blocking), function(index) list(
    task_id = tasks$task_id[[index]], phase = tasks$phase[[index]],
    status = tasks$status[[index]],
    external_required = identical(
      tolower(as.character(tasks$external_required[[index]])), "true"
    )
  ))),
  evidence_boundary = paste(
    "Implemented-internal is not equivalent to independent review, external",
    "validation, immutable release, or publication readiness."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
