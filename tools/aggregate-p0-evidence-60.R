args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
if (length(args) < 3L) {
  stop("Usage: aggregate-p0-evidence-60.R CANDIDATE_TAR EVIDENCE_DIR OUTPUT",
       call. = FALSE)
}
candidate_tar <- normalizePath(args[[1L]], mustWork = TRUE)
evidence_dir <- normalizePath(args[[2L]], mustWork = TRUE)
output <- args[[3L]]
if (!requireNamespace("digest", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("P0 evidence aggregation requires digest and jsonlite.")
}
sys.source("tools/evidence-identity-60.R", envir = environment())

paths <- file.path(evidence_dir, c(
  "unit-60.json",
  "audit-corpus-60.json",
  "doc-entrypoints-60.json",
  "r-cmd-check-60.json"
))
if (any(!file.exists(paths))) {
  stop("Missing P0 evidence: ", paste(paths[!file.exists(paths)], collapse = ", "))
}
reports <- lapply(paths, jsonlite::read_json, simplifyVector = FALSE)
names(reports) <- vapply(reports, `[[`, character(1), "suite")

validation <- lapply(reports, function(report) {
  fixture_paths <- vapply(
    report$candidate$fixtures,
    `[[`,
    character(1),
    "path"
  )
  expected <- ngeo_evidence_identity(candidate_tar, fixture_paths)
  ngeo_validate_evidence_identity(
    report,
    expected,
    seeds = identical(report$suite, "audit-regression-corpus-6.0")
  )
})
identity_pass <- vapply(validation, `[[`, logical(1), "pass")
if (!all(identity_pass)) {
  details <- vapply(names(validation), function(name) {
    paste0(name, ": ", paste(validation[[name]]$failures, collapse = ", "))
  }, character(1))
  stop("P0 identity gate rejected evidence:\n", paste(details, collapse = "\n"),
       call. = FALSE)
}

candidate <- reports[[1L]]$candidate
package_version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
artifacts <- lapply(paths, function(path) list(
  path = basename(path),
  sha256 = ngeo_sha256_file(path),
  bytes = unname(file.info(path)$size)
))
names(artifacts) <- names(reports)
checks <- list(
  required_suites = identical(
    sort(names(reports)),
    sort(c(
      "unit-6.0", "audit-regression-corpus-6.0",
      "documentation-entrypoints-6.0", "r-cmd-check-6.0"
    ))
  ),
  identity = all(identity_pass),
  suite_pass = all(vapply(reports, `[[`, logical(1), "pass")),
  package_version = identical(candidate$package_version, package_version)
)
attestation <- list(
  schema = "neurogeo/p0-attestation/1",
  candidate = candidate[c(
    "package_version", "source_commit", "candidate_tar", "dependencies"
  )],
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  suites = artifacts,
  checks = checks,
  pass = all(unlist(checks, use.names = FALSE))
)
if (!attestation$pass) stop("P0 attestation failed.", call. = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(attestation, output, auto_unbox = TRUE, pretty = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
