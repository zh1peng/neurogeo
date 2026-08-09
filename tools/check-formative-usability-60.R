args <- commandArgs(trailingOnly = TRUE)
require_complete <- "--require-complete" %in% args
path <- file.path(
  "inst", "validation", "formative-usability-results-6.0.csv"
)
protocol <- file.path("inst", "validation", "formative-usability-6.0.md")
if (!file.exists(path) || !file.exists(protocol)) {
  stop("Missing the Phase 1 formative usability protocol or result template.")
}
results <- utils::read.csv(
  path, stringsAsFactors = FALSE, check.names = FALSE,
  na.strings = character()
)
required <- c(
  "round_id", "participant_id", "background", "neuroimaging_years",
  "primary_format", "locale", "package_ref", "quickstart_source_sha256",
  "started_utc", "duration_minutes", "completed", "clarification_count",
  "wrong_layer_selection_count", "failure_summary", "issue_url", "observer",
  "consent_recorded"
)
if (!identical(names(results), required)) {
  stop("Formative usability results do not match the reviewed schema.")
}

if (!nrow(results)) {
  message(
    "Formative usability schema is valid, but no real participant evidence ",
    "has been recorded. Phase 1 exit gate remains pending."
  )
  if (require_complete) quit(status = 2L)
  quit(status = 0L)
}

logical_value <- function(value, field) {
  normalized <- tolower(trimws(value))
  if (any(!normalized %in% c("true", "false"))) {
    stop("`", field, "` must contain only true or false.")
  }
  normalized == "true"
}
completed <- logical_value(results$completed, "completed")
consent <- logical_value(results$consent_recorded, "consent_recorded")
numeric_fields <- c(
  "neuroimaging_years", "duration_minutes", "clarification_count",
  "wrong_layer_selection_count"
)
for (field in numeric_fields) {
  converted <- suppressWarnings(as.numeric(results[[field]]))
  if (anyNA(converted) || any(!is.finite(converted)) || any(converted < 0)) {
    stop("`", field, "` must contain finite non-negative numbers.")
  }
  results[[field]] <- converted
}
if (length(unique(results$round_id)) != 1L ||
    !identical(unique(results$round_id), "P1-formative-01")) {
  stop("Results must contain exactly the reviewed P1-formative-01 round.")
}
if (any(!nzchar(results$participant_id)) ||
    anyDuplicated(results$participant_id)) {
  stop("Participant IDs must be non-empty and unique within the round.")
}
if (any(!results$background %in% c("novice", "experienced")) ||
    any(!results$primary_format %in% c("NIfTI", "surface", "CIFTI", "ROI/cohort")) ||
    any(!results$locale %in% c("zh-CN", "en"))) {
  stop("Background, primary_format, or locale contains an unreviewed value.")
}
if (any(!grepl("^[0-9a-f]{40}$", results$package_ref)) ||
    any(!grepl("^[0-9a-f]{64}$", results$quickstart_source_sha256))) {
  stop("Each session must pin a 40-character commit and quickstart SHA-256.")
}
if (any(!grepl(
  "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
  results$started_utc
))) {
  stop("`started_utc` must use UTC ISO 8601 timestamps.")
}
if (any(!consent)) stop("Every recorded session must have consent.")
failed <- !completed
if (any(failed & !grepl("^https://", results$issue_url))) {
  stop("Every incomplete session must link to an issue.")
}
if (nrow(results) < 5L) {
  message("Only ", nrow(results), " real sessions are recorded; at least 5 are required.")
  if (require_complete) quit(status = 2L)
  quit(status = 0L)
}
median_minutes <- stats::median(results$duration_minutes)
if (median_minutes > 15) {
  stop("Median quickstart time is ", median_minutes, " minutes; revise and rerun the round.")
}
cat(
  "Formative usability complete:", nrow(results), "participants; median",
  median_minutes, "minutes;", sum(failed), "incomplete sessions.\n"
)
