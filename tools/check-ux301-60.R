args <- commandArgs(trailingOnly = TRUE)
strict <- "--require-complete" %in% args
paths <- args[!startsWith(args, "--")]
output <- if (length(paths)) paths[[1L]] else
  file.path("check-output", "ux301-60.json")
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("UX-301 checking requires jsonlite and digest.")
}

protocol_path <- file.path("inst", "validation", "ux301-protocol-6.0.md")
results_path <- file.path("inst", "validation", "ux301-results-6.0.csv")
if (!file.exists(protocol_path) || !file.exists(results_path)) {
  stop("UX-301 protocol or result template is missing.")
}
results <- utils::read.csv(
  results_path, stringsAsFactors = FALSE, check.names = FALSE
)
required <- c(
  "study_id", "participant_id", "experience_level", "neuroimaging_years",
  "primary_format", "locale", "contributed_to_neurogeo", "package_tag",
  "source_commit", "task_id", "task_order", "started_utc",
  "duration_minutes", "completed", "wrong_layer_selection_count",
  "help_requested", "help_rule_id", "protocol_deviation", "misuse_type",
  "failure_summary", "issue_url", "observer", "consent_recorded"
)
if (!identical(names(results), required)) stop("Invalid UX-301 result schema.")
tasks <- c(
  "identify-entrypoint", "quickstart", "format-workflow", "layer-recovery",
  "interpret-result"
)
template_valid <- nrow(results) == 0L || all(results$task_id %in% tasks)
if (!template_valid) stop("UX-301 contains an unregistered task ID.")

complete <- FALSE
metrics <- list(
  participants = 0L, attempted_tasks = nrow(results), success_rate = NULL,
  quickstart_median_minutes = NULL, wrong_layer_selections = NULL,
  novice_participants = 0L, expert_participants = 0L,
  protocol_deviation_rows = 0L
)
checks <- list(
  protocol_present = TRUE,
  result_schema_valid = TRUE,
  frozen_tasks_only = template_valid,
  participant_count = FALSE,
  strata = FALSE,
  five_tasks_each = FALSE,
  non_developers = FALSE,
  one_prerelease_identity = FALSE,
  consent = FALSE,
  help_rules = FALSE,
  success_rate = FALSE,
  quickstart_time = FALSE,
  wrong_layer_selection = FALSE
)
if (nrow(results)) {
  logical_value <- function(value) tolower(as.character(value)) %in% c("true", "false")
  if (!all(logical_value(results$completed)) ||
      !all(logical_value(results$contributed_to_neurogeo)) ||
      !all(logical_value(results$help_requested)) ||
      !all(logical_value(results$protocol_deviation)) ||
      !all(logical_value(results$consent_recorded))) {
    stop("UX-301 logical fields must contain only true or false.")
  }
  participants <- unique(results$participant_id)
  strata <- unique(results[c("participant_id", "experience_level")])
  counts <- table(strata$experience_level)
  count_or_zero <- function(group) {
    if (group %in% names(counts)) unname(counts[[group]]) else 0L
  }
  task_key <- paste(results$participant_id, results$task_id, sep = "\u001f")
  one_study <- length(unique(results$study_id)) == 1L &&
    identical(unique(results$study_id), "UX-301-01")
  prerelease <- unique(results$package_tag)
  commits <- unique(results$source_commit)
  help_requested <- tolower(results$help_requested) == "true"
  completed <- tolower(results$completed) == "true"
  deviations <- tolower(results$protocol_deviation) == "true"
  quickstart <- results$duration_minutes[results$task_id == "quickstart"]
  checks$participant_count <- length(participants) >= 8L &&
    length(participants) <= 15L && one_study
  checks$strata <- all(c("novice", "expert") %in% names(counts)) &&
    all(counts[c("novice", "expert")] >= 3L)
  checks$five_tasks_each <- !anyDuplicated(task_key) &&
    all(table(results$participant_id) == length(tasks)) &&
    all(vapply(split(results$task_id, results$participant_id), function(value) {
      setequal(value, tasks)
    }, logical(1)))
  checks$non_developers <- all(
    tolower(results$contributed_to_neurogeo) == "false"
  )
  checks$one_prerelease_identity <- length(prerelease) == 1L &&
    grepl("^v6[.]0[.]0-rc[0-9]+$", prerelease) &&
    length(commits) == 1L && grepl("^[0-9a-f]{40}$", commits)
  checks$consent <- all(tolower(results$consent_recorded) == "true")
  checks$help_rules <- all(results$help_rule_id %in% c("H0", "H1", "H2", "H3")) &&
    all(results$help_rule_id[!help_requested] == "H0") &&
    all(results$help_rule_id[help_requested] != "H0")
  checks$success_rate <- mean(completed) >= 0.80
  checks$quickstart_time <- length(quickstart) == length(participants) &&
    all(is.finite(quickstart) & quickstart >= 0) &&
    stats::median(quickstart) <= 15
  checks$wrong_layer_selection <- all(
    is.finite(results$wrong_layer_selection_count) &
      results$wrong_layer_selection_count >= 0
  ) && sum(results$wrong_layer_selection_count) == 0
  metrics <- list(
    participants = length(participants), attempted_tasks = nrow(results),
    success_rate = mean(completed),
    quickstart_median_minutes = if (length(quickstart)) stats::median(quickstart) else NULL,
    wrong_layer_selections = sum(results$wrong_layer_selection_count),
    novice_participants = count_or_zero("novice"),
    expert_participants = count_or_zero("expert"),
    protocol_deviation_rows = sum(deviations)
  )
  complete <- all(unlist(checks, use.names = FALSE))
}
result <- list(
  schema = "neurogeo/ux301-readiness/1",
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  protocol_sha256 = digest::digest(
    protocol_path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = if (complete) "complete" else "awaiting-real-participants",
  study_complete = complete,
  checks = checks,
  metrics = metrics,
  evidence_boundary = paste(
    "An empty or partial template is preparation evidence only. UX-301",
    "requires 8-15 real non-developer participants on one signed prerelease."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (strict && !complete) quit(status = 2L)
