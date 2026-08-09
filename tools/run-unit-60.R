args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "unit-60.json")
candidate_tar <- Sys.getenv("NEUROGEO_CANDIDATE_TAR", unset = "")
if (!requireNamespace("testthat", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("The 6.0 unit evidence runner requires testthat, jsonlite, and digest.")
}
if (!nzchar(candidate_tar)) {
  stop("Set NEUROGEO_CANDIDATE_TAR to the tested source archive.", call. = FALSE)
}
sys.source("tools/evidence-identity-60.R", envir = environment())
started <- Sys.time()
timing <- system.time(result <- testthat::test_local(
  ".", reporter = "summary", stop_on_failure = TRUE
))
failures <- sum(vapply(result, function(case) {
  sum(vapply(case$results, inherits, logical(1), "expectation_failure"))
}, integer(1)))
errors <- sum(vapply(result, function(case) {
  sum(vapply(case$results, inherits, logical(1), "expectation_error"))
}, integer(1)))
skips <- sum(vapply(result, function(case) {
  sum(vapply(case$results, inherits, logical(1), "expectation_skip"))
}, integer(1)))
report <- list(
  schema = "neurogeo/evidence-report/1",
  suite = "unit-6.0",
  candidate = ngeo_evidence_identity(
    candidate_tar,
    c(test_entry = "tests/testthat.R")
  ),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  started_at_utc = format(started, tz = "UTC", usetz = TRUE),
  elapsed_seconds = unname(timing[["elapsed"]]),
  test_blocks = length(result), failures = failures, errors = errors,
  skips = skips,
  checks = list(no_failures = failures == 0L, no_errors = errors == 0L),
  pass = failures == 0L && errors == 0L
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
