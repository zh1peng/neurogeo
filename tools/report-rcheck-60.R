args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
if (length(args) < 3L) {
  stop("Usage: report-rcheck-60.R CANDIDATE_TAR CHECK_LOG OUTPUT",
       call. = FALSE)
}
candidate_tar <- args[[1L]]
check_log <- args[[2L]]
output <- args[[3L]]
if (!requireNamespace("digest", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("R CMD check evidence requires digest and jsonlite.")
}
if (!file.exists(check_log)) stop("Missing R CMD check log: ", check_log)
sys.source("tools/evidence-identity-60.R", envir = environment())
status <- grep(
  "^Status:",
  readLines(check_log, warn = FALSE, encoding = "UTF-8"),
  value = TRUE
)
checks <- list(status_ok = identical(status, "Status: OK"))
report <- list(
  schema = "neurogeo/evidence-report/1",
  suite = "r-cmd-check-6.0",
  candidate = ngeo_evidence_identity(
    candidate_tar,
    c(description = "DESCRIPTION")
  ),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  check_log = list(
    path = basename(check_log),
    sha256 = ngeo_sha256_file(check_log),
    status = status
  ),
  checks = checks,
  pass = all(unlist(checks, use.names = FALSE))
)
if (!report$pass) stop("R CMD check evidence is not Status: OK.", call. = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
