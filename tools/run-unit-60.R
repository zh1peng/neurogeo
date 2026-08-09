args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "unit-60.json")
if (!requireNamespace("testthat", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The 6.0 unit evidence runner requires testthat and jsonlite.")
}
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
  schema = "neurogeo/unit-6.0",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  started_at_utc = format(started, tz = "UTC", usetz = TRUE),
  elapsed_seconds = unname(timing[["elapsed"]]),
  test_blocks = length(result), failures = failures, errors = errors,
  skips = skips, pass = failures == 0L && errors == 0L
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
